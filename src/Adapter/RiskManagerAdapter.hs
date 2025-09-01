{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module Adapter.RiskManagerAdapter where

import Domain.Services.BacktestService (RiskManager(..), DrawdownAnalysis(..), DrawdownPeriod(..), RiskAssessment(..), RiskLevel(..))
import Domain.Types
import Util.Error (Result, AppError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime, diffUTCTime)
import Data.List (sortOn, maximumBy, minimumBy, groupBy)
import Data.Function (on)
import qualified Data.Text as T

-- Basic risk manager adapter implementation
data BasicRiskManager = BasicRiskManager
  { brmMaxDrawdownThreshold :: Scientific
  , brmPositionSizeLimit :: Scientific
  , brmLeverageLimit :: Scientific
  }

newtype BasicRiskManagerM a = BasicRiskManagerM
  { runBasicRiskManager :: ReaderT BasicRiskManager IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader BasicRiskManager)

instance RiskManager BasicRiskManagerM where
  validateRiskLimits limits = do
    -- Inline the validation logic since validateBasicRiskLimits was removed with Port layer
    let basicValidation =
          if | rlMaxDrawdown limits <= 0 -> Left $ ValidationError "Max drawdown must be positive"
             | rlMaxPositionSize limits <= 0 -> Left $ ValidationError "Max position size must be positive"
             | rlStopLossThreshold limits < 0 -> Left $ ValidationError "Stop loss threshold cannot be negative"
             | otherwise -> Right ()

    case basicValidation of
      Left err -> pure $ Left err
      Right () -> do
        -- Additional validation logic
        if rlMaxDrawdown limits > fromFloatDigits 5000.0
          then pure $ Left $ ValidationError "Max drawdown limit too high for safety"
          else pure $ Right ()

  checkRiskLimits result limits = do
    drawdownResult <- calculateDrawdown []  -- Would use actual equity curve
    case drawdownResult of
      Left err -> pure $ Left err
      Right drawdownAnalysis -> do
        let currentDrawdown = daCurrentDrawdown drawdownAnalysis
            maxAllowed = rlMaxDrawdown limits

        if currentDrawdown > maxAllowed
          then pure $ Left $ RiskViolationError $ "Drawdown exceeded: " <> T.pack (show currentDrawdown) <> " > " <> T.pack (show maxAllowed)
          else pure $ Right ()

  calculateDrawdown equityCurve = do
    if null equityCurve
      then pure $ Right $ DrawdownAnalysis 0 0 [] 0 Nothing
      else do
        let analysis = analyzeDrawdown equityCurve
        pure $ Right analysis

  validatePositionSize position maxSize = do
    let currentSize = unQty (pQty position)
    if currentSize > maxSize
      then pure $ Left $ ValidationError $ "Position size exceeds limit: " <> T.pack (show currentSize)
      else pure $ Right ()

  assessPortfolioRisk positions account = do
    let totalExposure = sum $ map (unQty . pQty) positions
        accountBalance = aBalance account
        leverageRatio = if accountBalance > 0 then totalExposure / accountBalance else 0

        positionRisk = assessPositionSizingRisk positions account
        leverageRisk = assessLeverageRisk leverageRatio
        drawdownRisk = Low  -- Simplified for now

        overallRisk = maximum [positionRisk, leverageRisk, drawdownRisk]

        recommendations = generateRiskRecommendations overallRisk positions account

        assessment = RiskAssessment
          { raOverallRisk = overallRisk
          , raPositionSizing = positionRisk
          , raDrawdownRisk = drawdownRisk
          , raLeverageRisk = leverageRisk
          , raRecommendations = recommendations
          }

    pure $ Right assessment

-- Implementation functions
analyzeDrawdown :: [(UTCTime, Scientific)] -> DrawdownAnalysis
analyzeDrawdown equityCurve =
  let sortedCurve = sortOn fst equityCurve
      runningPeaks = scanl1 max (map snd sortedCurve)
      drawdowns = zipWith (-) runningPeaks (map snd sortedCurve)
      maxDrawdown = maximum drawdowns
      currentDrawdown = last drawdowns

      -- Calculate drawdown percentage safely to avoid Scientific precision issues
      finalBalance = snd $ last sortedCurve
      maxDrawdownPercent = if finalBalance > 0
        then fromFloatDigits $ realToFrac maxDrawdown / realToFrac finalBalance * 100
        else 0

      -- Identify drawdown periods
      drawdownPeriods = identifyDrawdownPeriods sortedCurve runningPeaks

  in DrawdownAnalysis
    { daMaxDrawdown = maxDrawdown
    , daMaxDrawdownPercent = maxDrawdownPercent
    , daDrawdownPeriods = drawdownPeriods
    , daCurrentDrawdown = currentDrawdown
    , daRecoveryTime = calculateRecoveryTime drawdownPeriods
    }

identifyDrawdownPeriods :: [(UTCTime, Scientific)] -> [Scientific] -> [DrawdownPeriod]
identifyDrawdownPeriods equityCurve runningPeaks =
  let combined = zip3 (map fst equityCurve) (map snd equityCurve) runningPeaks
      -- Group consecutive periods where equity < peak
      drawdownGroups = groupConsecutiveDrawdowns combined
  in map convertToDrawdownPeriod drawdownGroups

groupConsecutiveDrawdowns :: [(UTCTime, Scientific, Scientific)] -> [[(UTCTime, Scientific, Scientific)]]
groupConsecutiveDrawdowns [] = []
groupConsecutiveDrawdowns points =
  let isInDrawdown (_, equity, peak) = equity < peak
      drawdownPoints = filter isInDrawdown points
  in groupBy (\a b -> isConsecutive (fst3 a) (fst3 b)) drawdownPoints
  where
    fst3 (a, _, _) = a
    isConsecutive _ _ = True  -- Simplified - would check actual time proximity

convertToDrawdownPeriod :: [(UTCTime, Scientific, Scientific)] -> DrawdownPeriod
convertToDrawdownPeriod [] = DrawdownPeriod (read "2025-01-01 00:00:00 UTC") Nothing 0 0
convertToDrawdownPeriod points =
  let startTime = minimum $ map (\(t, _, _) -> t) points
      endTime = maximum $ map (\(t, _, _) -> t) points
      maxDD = maximum $ map (\(_, equity, peak) -> peak - equity) points
      duration = fromFloatDigits $ realToFrac $ diffUTCTime endTime startTime / (24 * 3600)  -- Days
  in DrawdownPeriod startTime (Just endTime) maxDD duration

calculateRecoveryTime :: [DrawdownPeriod] -> Maybe Scientific
calculateRecoveryTime [] = Nothing
calculateRecoveryTime periods =
  let completedPeriods = filter (\dp -> dpEnd dp /= Nothing) periods
      recoveryTimes = map dpDuration completedPeriods
  in if null recoveryTimes then Nothing else Just (sum recoveryTimes / fromIntegral (length recoveryTimes))

assessPositionSizingRisk :: [Position] -> Account -> RiskLevel
assessPositionSizingRisk positions account =
  let totalExposure = sum $ map (unQty . pQty) positions
      accountBalance = aBalance account
      exposureRatio = if accountBalance > 0 then totalExposure / accountBalance else 0
  in case () of
    _ | exposureRatio > 10 -> Critical
      | exposureRatio > 5  -> High
      | exposureRatio > 2  -> Medium
      | otherwise         -> Low

assessLeverageRisk :: Scientific -> RiskLevel
assessLeverageRisk leverage
  | leverage > 20 = Critical
  | leverage > 10 = High
  | leverage > 5  = Medium
  | otherwise     = Low

generateRiskRecommendations :: RiskLevel -> [Position] -> Account -> [T.Text]
generateRiskRecommendations overallRisk positions account =
  let baseRecommendations = case overallRisk of
        Critical -> [ "URGENT: Reduce position sizes immediately"
                    , "Consider closing some positions"
                    , "Review risk management strategy"
                    ]
        High     -> [ "Consider reducing position sizes"
                    , "Monitor drawdown closely"
                    , "Review stop loss levels"
                    ]
        Medium   -> [ "Monitor risk levels"
                    , "Consider tightening stop losses"
                    ]
        Low      -> [ "Risk levels acceptable"
                    , "Continue monitoring"
                    ]

      -- Additional position-specific recommendations
      positionRecommendations = analyzePositionRisks positions account

  in baseRecommendations ++ positionRecommendations

analyzePositionRisks :: [Position] -> Account -> [T.Text]
analyzePositionRisks positions account =
  let totalPositions = length positions
      accountBalance = aBalance account
  in case () of
    _ | totalPositions > 10 -> ["Consider reducing number of open positions"]
      | accountBalance < 1000 -> ["Account balance getting low - consider reducing risk"]
      | otherwise -> []

-- Default risk manager configuration
defaultBasicRiskManager :: BasicRiskManager
defaultBasicRiskManager = BasicRiskManager
  { brmMaxDrawdownThreshold = fromFloatDigits 1000.0
  , brmPositionSizeLimit = fromFloatDigits 5000.0
  , brmLeverageLimit = fromFloatDigits 10.0
  }
