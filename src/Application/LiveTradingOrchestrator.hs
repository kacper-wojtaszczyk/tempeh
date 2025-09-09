{-# LANGUAGE OverloadedStrings #-}
module Application.LiveTradingOrchestrator
  ( -- Main functions
    orchestrateLiveTrading
  , defaultLiveTradingConfig
    -- Types
  , LiveTradingConfig'(..)
  , LiveTradingState(..)
    -- Test helpers
  , ticksToCandles
  , groupTicksByMinute
  , tickGroupToCandle
  , generateSignal
  ) where

import Domain.Services.LiveDataService
import Domain.Types
import Application.Strategy.Types (StrategyParameters(..), StrategyInstance(..), SignalGenerator)
import Application.Strategy.Registry (StrategyRegistry, createStrategyFromKeyword)
import Adapter.BrokerDataProvider (BrokerDataProviderM, runBrokerDataProviderIO, executeEnterSignal, executeExitSignal)
import Util.Config (AppConfig(..), loadAppConfig, LiveTradingConfig(..), defaultAppConfig)
import Util.Error (Result, TempehError(..), strategyError, configError)
import Util.Logger (MonadLogger, ComponentName(..), logInfo, logError, logWarn)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (STM, atomically, readTVar)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when, void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime, UTCTime, toGregorian, utctDay, timeToTimeOfDay, TimeOfDay(..), UTCTime(..), fromGregorian, timeOfDayToTime)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

-- Live trading configuration
data LiveTradingConfig' = LiveTradingConfig'
  { ltcInstrument :: Instrument
  , ltcStrategy :: StrategyParameters
  , ltcConfig :: AppConfig
  } deriving (Show)

-- Live trading state
data LiveTradingState = LiveTradingState
  { ltsConnectionId :: ConnectionId
  , ltsStrategyState :: Text  -- Serialized strategy state
  , ltsSignalCount :: Int
  , ltsLastSignalTime :: Maybe UTCTime
  , ltsIsRunning :: Bool
  } deriving (Show)

-- Live trading orchestrator
orchestrateLiveTrading :: StrategyRegistry -> LiveTradingConfig' -> IO (Result LiveTradingState)
orchestrateLiveTrading registry config = do
  putStrLn $ "Starting live trading orchestration for " <> show (ltcInstrument config)

  -- Load application configuration
  configResult <- loadAppConfig
  case configResult of
    Left configErr -> do
      putStrLn $ "Failed to load configuration: " <> T.unpack configErr
      return $ Left $ configError configErr
    Right appConfig -> do
      putStrLn "Configuration loaded successfully"

      -- Initialize broker connection
      runBrokerDataProviderIO $ do
        connectionResult <- connect
        case connectionResult of
          Left brokerErr -> do
            liftIO $ putStrLn $ "Failed to connect to broker: " <> show brokerErr
            return $ Left brokerErr
          Right connId -> do
            liftIO $ putStrLn $ "Connected to broker with connection ID: " <> show connId

            -- Subscribe to instrument data
            subscribeResult <- subscribeToInstrument connId (ltcInstrument config)
            case subscribeResult of
              Left subscribeErr -> do
                liftIO $ putStrLn $ "Failed to subscribe to instrument: " <> show subscribeErr
                return $ Left subscribeErr
              Right _ -> do
                liftIO $ putStrLn $ "Successfully subscribed to " <> show (ltcInstrument config)

                -- Start live trading loop
                result <- startLiveTradingLoop registry config connId
                return result

-- Main live trading loop
startLiveTradingLoop :: StrategyRegistry
                    -> LiveTradingConfig'
                    -> ConnectionId
                    -> BrokerDataProviderM IO (Result LiveTradingState)
startLiveTradingLoop registry config connId = do
  liftIO $ putStrLn "Starting live trading loop..."

  -- Create strategy instance - extract strategy type from parameters
  let strategyType = case ltcStrategy config of
        StrategyParameters sType _ _ _ _ -> T.unpack sType
  case createStrategyFromKeyword strategyType Nothing registry of
    Nothing -> do
      liftIO $ putStrLn $ "Failed to create strategy: unknown strategy type " <> strategyType
      return $ Left $ strategyError ("Unknown strategy type: " <> T.pack strategyType)
    Just strategyInstance -> do
      liftIO $ putStrLn $ "Strategy created: " <> T.unpack (siName strategyInstance)

      -- Initialize live trading state
      now <- liftIO getCurrentTime
      let initialState = LiveTradingState
            { ltsConnectionId = connId
            , ltsStrategyState = T.pack (show (siInitialState strategyInstance))
            , ltsSignalCount = 0
            , ltsLastSignalTime = Nothing
            , ltsIsRunning = True
            }

      -- Start the trading loop
      finalState <- runTradingLoop strategyInstance config connId initialState
      return $ Right finalState

-- Core trading loop
runTradingLoop :: StrategyInstance
               -> LiveTradingConfig'
               -> ConnectionId
               -> LiveTradingState
               -> BrokerDataProviderM IO LiveTradingState
runTradingLoop strategyInstance config connId state = do
  if not (ltsIsRunning state)
    then do
      liftIO $ putStrLn "Trading loop stopped"
      return state
    else do
      -- Check connection status
      statusResult <- getConnectionStatus connId
      case statusResult of
        Left err -> do
          liftIO $ putStrLn $ "Connection error: " <> show err
          return state { ltsIsRunning = False }
        Right status -> do
          case status of
            Connected _ -> do
              -- Process tick data and generate signals
              newState <- processTicks strategyInstance config connId state

              -- Sleep for a short interval (e.g., 100ms)
              liftIO $ threadDelay 100000  -- 100ms

              -- Continue the loop
              runTradingLoop strategyInstance config connId newState

            Disconnected -> do
              liftIO $ putStrLn "Connection lost, stopping trading"
              return state { ltsIsRunning = False }

            Reconnecting attempt -> do
              liftIO $ putStrLn $ "Reconnecting... attempt " <> show attempt
              liftIO $ threadDelay 1000000  -- 1 second
              runTradingLoop strategyInstance config connId state

            _ -> do
              liftIO $ putStrLn $ "Connection status: " <> show status
              liftIO $ threadDelay 1000000  -- 1 second
              runTradingLoop strategyInstance config connId state

-- Process incoming tick data and generate trading signals
processTicks :: StrategyInstance
             -> LiveTradingConfig'
             -> ConnectionId
             -> LiveTradingState
             -> BrokerDataProviderM IO LiveTradingState
processTicks strategyInstance config connId state = do
  -- Get tick stream
  tickStreamResult <- getTickStream connId (ltcInstrument config)
  case tickStreamResult of
    Left err -> do
      liftIO $ putStrLn $ "Failed to get tick stream: " <> show err
      return state
    Right tickStream -> do
      -- Read current ticks
      ticks <- liftIO $ atomically tickStream

      if null ticks
        then return state  -- No new ticks
        else do
          liftIO $ putStrLn $ "Processing " <> show (length ticks) <> " ticks"

          -- Convert ticks to candles (1-minute candles for now)
          now <- liftIO getCurrentTime
          let candles = ticksToCandles ticks (ltcInstrument config)

          if null candles
            then do
              liftIO $ putStrLn "No complete candles generated from ticks yet"
              return state { ltsSignalCount = ltsSignalCount state + length ticks }
            else do
              liftIO $ putStrLn $ "Generated " <> show (length candles) <> " candles from ticks"

              -- Generate trading signal using strategy
              signal <- liftIO $ generateSignal strategyInstance candles

              case signal of
                Hold -> do
                  liftIO $ putStrLn "Strategy signal: HOLD"
                  return state
                    { ltsSignalCount = ltsSignalCount state + length ticks
                    , ltsLastSignalTime = Just now
                    }

                Enter side -> do
                  liftIO $ putStrLn $ "Strategy signal: ENTER " <> show side
                  -- Execute actual position creation through broker API
                  let positionSize = 0.1  -- Default position size for now
                  executeResult <- executeEnterSignal connId (ltcInstrument config) side positionSize
                  case executeResult of
                    Left tradeErr -> do
                      liftIO $ putStrLn $ "Failed to execute ENTER signal: " <> show tradeErr
                    Right dealRef -> do
                      liftIO $ putStrLn $ "Successfully executed ENTER signal, deal reference: " <> T.unpack dealRef
                  return state
                    { ltsSignalCount = ltsSignalCount state + length ticks
                    , ltsLastSignalTime = Just now
                    }

                Exit -> do
                  liftIO $ putStrLn "Strategy signal: EXIT"
                  -- Execute actual position closure through broker API
                  executeResult <- executeExitSignal connId (ltcInstrument config)
                  case executeResult of
                    Left tradeErr -> do
                      liftIO $ putStrLn $ "Failed to execute EXIT signal: " <> show tradeErr
                    Right dealRef -> do
                      liftIO $ putStrLn $ "Successfully executed EXIT signal, deal reference: " <> T.unpack dealRef
                  return state
                    { ltsSignalCount = ltsSignalCount state + length ticks
                    , ltsLastSignalTime = Just now
                    }

                _ -> do
                  liftIO $ putStrLn $ "Strategy signal: " <> show signal
                  return state
                    { ltsSignalCount = ltsSignalCount state + length ticks
                    , ltsLastSignalTime = Just now
                    }

-- Convert raw ticks to 1-minute candles
ticksToCandles :: [Tick] -> Instrument -> [Candle]
ticksToCandles ticks instrument =
  let filteredTicks = filter (\t -> tInstr t == instrument) ticks
      groupedTicks = groupTicksByMinute filteredTicks
  in map tickGroupToCandle groupedTicks

-- Group ticks by minute for candle generation
groupTicksByMinute :: [Tick] -> [[Tick]]
groupTicksByMinute [] = []
groupTicksByMinute ticks =
  let sortedTicks = sortBy (comparing tTime) ticks
      groupedByMinute = groupBy sameMinute sortedTicks
  in filter (not . null) groupedByMinute
  where
    sameMinute t1 t2 =
      let time1 = tTime t1
          time2 = tTime t2
          minute1 = truncateToMinute time1
          minute2 = truncateToMinute time2
      in minute1 == minute2

    truncateToMinute time =
      let (year, month, day) = toGregorian (utctDay time)
          TimeOfDay hour minute _ = timeToTimeOfDay (utctDayTime time)
      in UTCTime (fromGregorian year month day) (timeOfDayToTime (TimeOfDay hour minute 0))

-- Convert a group of ticks within the same minute to a candle
tickGroupToCandle :: [Tick] -> Candle
tickGroupToCandle [] = error "Cannot create candle from empty tick group"
tickGroupToCandle ticks@(firstTick:_) =
  let midPrices = map tickMidPrice ticks
      openPrice = head midPrices
      closePrice = last midPrices
      highPrice = Price $ maximum $ map unPrice midPrices
      lowPrice = Price $ minimum $ map unPrice midPrices
      candleTime = truncateToMinute (tTime firstTick)
  in Candle
    { cTime = candleTime
    , cOpen = openPrice
    , cHigh = highPrice
    , cLow = lowPrice
    , cClose = closePrice
    }
  where
    tickMidPrice tick =
      let bid = unPrice (tBid tick)
          ask = unPrice (tAsk tick)
          mid = (bid + ask) / 2
      in Price mid

    truncateToMinute time =
      let (year, month, day) = toGregorian (utctDay time)
          TimeOfDay hour minute _ = timeToTimeOfDay (utctDayTime time)
      in UTCTime (fromGregorian year month day) (timeOfDayToTime (TimeOfDay hour minute 0))

-- Generate trading signal using strategy instance
generateSignal :: StrategyInstance -> [Candle] -> IO Signal
generateSignal strategyInstance candles = do
  putStrLn $ "Generating signal from " <> show (length candles) <> " candles using " <> T.unpack (siName strategyInstance)

  -- Use the strategy's signal generator with current candles and initial state
  let initialState = siInitialState strategyInstance
      (signal, _newState) = (siSignalGenerator strategyInstance) candles initialState

  putStrLn $ "Strategy generated signal: " <> show signal
  return signal

-- Default live trading configuration
defaultLiveTradingConfig :: Instrument -> StrategyParameters -> IO LiveTradingConfig'
defaultLiveTradingConfig instrument strategyParams = do
  let config = defaultAppConfig
  return LiveTradingConfig'
    { ltcInstrument = instrument
    , ltcStrategy = strategyParams
    , ltcConfig = config
    }
