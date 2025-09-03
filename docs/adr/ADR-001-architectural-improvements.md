# ADR-001: Architectural Improvements for Tempeh Trading Bot

**Status:** Partially Implemented  
**Date:** 2025-09-03  
**Deciders:** Development Team  
**Technical Story:** Post-backtesting engine completion → Prioritizing live tick data integration

## Context

Following the successful completion of the backtesting engine (95% complete), we've identified several architectural improvement opportunities through comprehensive analysis of the codebase. **The immediate next goal is to integrate live broker connectivity for real-time tick data streaming**, establishing the foundation for live trading while deferring order management complexity.

The current hexagonal architecture provides a solid foundation, but we need to prioritize improvements that directly support real-time data integration and the infrastructure required for live broker connectivity.

## Status Update (2025-09-03)

The following live-trading foundations are now implemented in code and covered by tests/E2E scenarios:
- ✅ Broker connection abstraction with IG and Demo support (safe Demo fallback)
- ✅ IG REST authentication and logout flows (implemented; disabled in tests via demo mode)
- ✅ Subscription registry and per-instrument tick buffers (STM-based)
- ✅ Basic data quality reporting API (stub metrics)
- ✅ Live trading orchestrator: tick grouping → candle generation (1-minute) → strategy signal
- ✅ Centralized configuration with global/local config and environment overrides for tests/CI

## Current Architecture Assessment

### Strengths ✅
- Clean hexagonal/ports-and-adapters architecture
- Excellent separation of concerns across Domain/Application/Adapter layers
- Comprehensive testing strategy (Testing Diamond approach)
- Abstract strategy registry system with pluggable strategies
- Pure functional backtesting engine
- Strong type safety with custom domain types

### Areas for Improvement 🔧
- Performance optimization for large datasets
- Enhanced error handling and observability
- Configuration management centralization
- Live trading preparation requirements
- Concurrent processing capabilities

## Proposed Architectural Improvements

### 1. Strategy Pattern Enhancement 🎯

**Priority:** Medium  
**Status:** Not Implemented  
**Effort:** Medium  

**Current State:** Good strategy registry with individual strategy execution  
**Proposed Enhancement:** Add strategy composition and portfolio capabilities

```haskell
-- Add strategy combination capabilities
data StrategyComposition = 
    Single StrategyInstance
  | Weighted [(StrategyInstance, Scientific)]     -- Weight-based combination
  | Sequential [StrategyInstance]                 -- Chain strategies
  | ConditionalSwitch MarketCondition StrategyInstance StrategyInstance

-- Portfolio-level strategy management
data PortfolioStrategy = PortfolioStrategy
  { psStrategies :: [StrategyComposition]
  , psAllocation :: AllocationStrategy
  , psRebalanceFreq :: RebalanceFrequency
  }
```

**Benefits:**
- Multi-strategy portfolios
- Dynamic strategy switching based on market conditions
- Risk diversification across strategies

---

### 2. Performance & Memory Optimization 🚀

**Priority:** High  
**Status:** Not Implemented  
**Effort:** High  

**Current Issues:**
- Tick data processing loads entire datasets into memory
- List-based processing for large price series
- No streaming capabilities for real-time processing

**Proposed Solutions:**

```haskell
-- Streaming data processing
data StreamingProcessor m = StreamingProcessor
  { spChunkSize :: Int
  , spProcessor :: [Tick] -> m (Result [Candle])
  , spStateManager :: ProcessorState -> ProcessorState
  }

-- Lazy evaluation for candle generation
generateCandlesLazy :: [Tick] -> Producer Candle IO ()

-- Vector-based price data for performance
import qualified Data.Vector.Unboxed as V

type PriceVector = V.Vector Double
```

**Implementation Plan:**
1. Replace List with Vector for price data storage
2. Implement chunked/streaming processing for large datasets
3. Add lazy evaluation for candle generation
4. Benchmark memory usage improvements

---

### 3. Enhanced Error Handling & Context 📋 ✅ **COMPLETED**

**Priority:** High  
**Effort:** Medium  
**Status:** ✅ **IMPLEMENTED** - Comprehensive error hierarchy with context and recovery strategies

**Previous State:** Basic `Result` type with string errors  
**✅ COMPLETED Components:**
- ✅ Structured error hierarchy with domain-specific error types
- ✅ Rich error context with correlation IDs and component tracing
- ✅ Recovery strategies (Retry, RetryWithBackoff, Fallback, SkipAndContinue, Reconnect)
- ✅ Error severity classification (Recoverable vs Fatal)
- ✅ Integration with logging system for error tracing
- ✅ JSON serialization for error persistence and debugging

**Implemented Architecture:**

```haskell
-- ✅ IMPLEMENTED: Structured error hierarchy
data TempehError = 
    DataError DataErrorDetails
  | StrategyError StrategyErrorDetails  
  | RiskError RiskErrorDetails
  | ConfigurationError ConfigErrorDetails
  | SystemError SystemErrorDetails
  | BrokerError BrokerErrorDetails      -- Ready for broker integration
  deriving (Show, Eq, Generic)

-- ✅ IMPLEMENTED: Error severity with recovery strategies
data ErrorSeverity = Recoverable RecoveryStrategy | Fatal
data RecoveryStrategy =
    Retry Int                           -- Simple retry
  | RetryWithBackoff Int Int           -- Exponential backoff
  | Fallback Text                      -- Alternative approach
  | SkipAndContinue                    -- Continue operation
  | Reconnect                          -- Broker reconnection
  deriving (Show, Eq, Generic)

-- ✅ IMPLEMENTED: Rich error context with tracing
data ErrorContext = ErrorContext
  { ecCorrelationId :: Maybe CorrelationId
  , ecTimestamp :: UTCTime
  , ecComponent :: Maybe ComponentName
  , ecOperation :: Maybe Text
  , ecSeverity :: ErrorSeverity
  } deriving (Show, Generic)

-- ✅ IMPLEMENTED: Domain-specific error details
data DataErrorDetails = DataErrorDetails
  { dedMessage :: Text
  , dedDataSource :: Maybe Text
  , dedInstrument :: Maybe Text
  , dedRecordCount :: Maybe Int
  , dedQualityScore :: Maybe Double
  } deriving (Show, Eq, Generic)

data BrokerErrorDetails = BrokerErrorDetails  -- Ready for live trading
  { bedMessage :: Text
  , bedBrokerType :: Maybe Text
  , bedConnectionId :: Maybe Text
  , bedHttpStatus :: Maybe Int
  , bedRateLimit :: Maybe Int
  } deriving (Show, Eq, Generic)
```

**✅ DELIVERED Features:**
- **Comprehensive error types**: Domain-specific errors for Data, Strategy, Risk, Configuration, System, and Broker operations
- **Automated recovery**: Built-in recovery strategies with retry logic and fallback mechanisms
- **Error tracing**: Integration with logging system using correlation IDs and component names
- **Rich context**: Detailed error information including timestamps, operations, and severity
- **Broker-ready**: Pre-built error handling for future broker API integration
- **JSON serialization**: Errors can be persisted and transmitted for debugging

**✅ CURRENT USAGE:**
The error system is actively used throughout the application:
- Data loading operations with detailed failure context
- Strategy execution with parameter and signal information
- Risk management with position and drawdown details
- Configuration validation with field-level error reporting

**Implementation Impact:**
This robust error handling foundation provides the reliability infrastructure needed for live trading. The recovery strategies and structured error types will be essential when dealing with broker API failures, network interruptions, and real-time data issues.

---

### 4. Centralized Configuration Management ⚙️ ✅ **COMPLETED**

**Priority:** Medium  
**Effort:** Medium  
**Status:** ✅ **IMPLEMENTED** - Global/local JSON config with test overrides.

**Current Issues:**
- Configuration scattered across modules
- No environment-specific configurations
- Hard-coded defaults in multiple places

**Proposed Solution:**

```haskell
-- Environment-aware configuration
data Environment = Development | Testing | Production | Sandbox
data AppConfig = AppConfig 
  { acEnvironment :: Environment
  , acDataSources :: DataSourceConfig
  , acRiskLimits :: RiskLimitsConfig
  , acStrategyDefaults :: StrategyDefaultsConfig
  , acLogging :: LogConfig
  , acPerformance :: PerformanceConfig
  } deriving (Generic, FromJSON, ToJSON)

-- Configuration loading with validation
loadConfig :: Environment -> IO (Either ConfigError AppConfig)
validateConfig :: AppConfig -> Either [ValidationError] ()
```

**Implementation:**
- JSON configuration files with global/local split
- Environment variable overrides (TEMPEH_TEST_MODE, TEMPEH_CONFIG_GLOBAL_ONLY)
- Configuration validation at startup
- Hooks for hot-reloading non-critical settings

---

### 5. Observability & Monitoring 📊 ✅ **COMPLETED**

**Priority:** High → **Critical (Essential for live data)**  
**Effort:** Medium  
**Status:** ✅ **IMPLEMENTED** - File-based logging system with component-specific logs

**Previous State:** Basic console logging  
**✅ COMPLETED Components:**
- ✅ Structured logging with correlation IDs and component names
- ✅ File-based logging with automatic log file generation
- ✅ Clean separation between user output and technical logs  
- ✅ Component-specific log files with timestamp naming
- ✅ Multiple log levels (Debug, Info, Warn, Error) with structured format

**Implemented Architecture:**

```haskell
-- ✅ IMPLEMENTED: Structured logging with context
data LogEvent = LogEvent
  { leLevel :: LogLevel
  , leTimestamp :: UTCTime
  , leCorrelationId :: Maybe CorrelationId
  , leComponent :: Maybe ComponentName
  , leMessage :: Text
  , leMetadata :: Map.Map Text Value
  } deriving (Show, Generic)

-- ✅ IMPLEMENTED: File-based logging with component organization
data LogContext = LogContext
  { lcCorrelationId :: Maybe CorrelationId
  , lcComponent :: Maybe ComponentName
  , lcLogFile :: Maybe FilePath
  } deriving (Show)

-- ✅ IMPLEMENTED: Automatic log file generation
generateLogFileName :: ComponentName -> IO FilePath
runFileLoggerWithComponent :: ComponentName -> ReaderT LogContext IO a -> IO a

-- ✅ IMPLEMENTED: Clean MonadLogger interface
class Monad m => MonadLogger m where
  logAtLevel :: LogLevel -> Text -> Map.Map Text Value -> m ()
```

**✅ DELIVERED Features:**
- **File-based logging**: Each backtest creates dedicated log files (`backtest-{INSTRUMENT}-{STRATEGY}-{TIMESTAMP}.log`)
- **Clean console output**: User-facing results separated from technical logs
- **Structured log format**: Timestamp, level, component, and message in each entry
- **Component tracing**: Easy identification of which system component generated each log
- **Production ready**: Persistent logs essential for live trading debugging

**Remaining Work for Live Data (Future):**
- Performance metrics collection for live tick data
- Real-time data quality monitoring
- Connection health monitoring for broker APIs
- Latency tracking for live feeds

**Implementation Impact:**
This foundational logging system provides the observability infrastructure needed for all future live trading components. The clean architecture and structured approach will seamlessly support the additional monitoring requirements when broker integration is implemented.

---

### 6. Testing Strategy Enhancements 🧪

**Priority:** Medium  
**Status:** 🔄 **PARTIALLY IMPLEMENTED**  
**Effort:** Medium  

**Current State:** Excellent testing diamond approach with property-based testing via `tasty-quickcheck`.  
**Proposed Enhancements:**

```haskell
-- Property-based testing for strategy invariants
prop_strategy_respects_risk_limits :: Strategy -> MarketData -> Property
prop_strategy_signal_consistency :: Strategy -> [Candle] -> Property
prop_performance_metrics_bounds :: BacktestResult -> Property

-- Market condition simulation
data MarketSimulator = MarketSimulator
  { msCondition :: MarketCondition  -- Trending, Ranging, Volatile
  , msRegime :: MarketRegime        -- Bull, Bear, Sideways  
  , msNoiseLevel :: NoiseLevel
  }

generateSyntheticMarketData :: MarketSimulator -> Gen [Candle]
```

**Benefits:**
- Automated invariant checking
- Comprehensive market condition testing
- Strategy robustness validation

---

### 7. Data Pipeline Architecture 🔄

**Priority:** Medium → **High (Critical for live broker integration)**  
**Status:** 🔄 **PARTIALLY IMPLEMENTED**  
**Effort:** High  

**Current State:** CSV-based with basic validation. A `LiveDataProvider` exists with a basic REST polling loop for the IG broker.  
**Proposed Enhancements for Live Integration:**

```haskell
-- Multi-source data pipeline with live broker support
data DataSource = 
    CSVFiles FilePath
  | Database ConnectionString
  | WebAPI APIConfig
  | MessageQueue QueueConfig
  | LiveBroker BrokerConfig      -- NEW: Live broker connection

-- Broker configuration
data BrokerConfig = BrokerConfig
  { bcBrokerType :: BrokerType
  , bcCredentials :: Credentials
  , bcEndpoints :: EndpointConfig
  , bcReconnectPolicy :: ReconnectPolicy
  } deriving (Generic, FromJSON)

data BrokerType = OANDA | InteractiveBrokers | Alpaca | MetaTrader
  deriving (Show, Eq, Generic, FromJSON)

-- Real-time data streaming
class LiveDataProvider m where
  connectToBroker :: BrokerConfig -> m (Either ConnectionError Connection)
  streamTicks :: Connection -> Instrument -> m (STM (TVar [Tick]))
  subscribeTo :: Connection -> [Instrument] -> m (Either SubscriptionError ())
  disconnect :: Connection -> m ()

-- Connection management
data Connection = Connection
  { connId :: ConnectionId
  , connBroker :: BrokerType
  , connStatus :: TVar ConnectionStatus
  , connHeartbeat :: TVar UTCTime
  , connReconnectCount :: TVar Int
  }

data ConnectionStatus = Connected | Disconnected | Reconnecting | Error Text
  deriving (Show, Eq)
```

Note: A basic live-broker data path is implemented via the LiveDataProvider instance in Adapter.BrokerDataProvider with per-instrument tick buffers and an initial REST polling loop. Lightstreamer streaming is planned.

---

### 8. Concurrency & Parallelization ⚡

**Priority:** Medium → **High (Essential for live data streams)**  
**Status:** 🔄 **PARTIALLY IMPLEMENTED**  
**Effort:** High  

Focused on Live Data Requirements:
- **Real-time tick data processing**
- **Concurrent broker connections**
- **Thread-safe live data handling**
- **Background connection monitoring**

Current status: Basic concurrency in place (STM TVars for state/ticks, async polling); scalable multi-instrument streaming remains planned.

---

### 9. Domain Model Enrichment 🏗️

**Priority:** Low  
**Status:** Not Implemented  
**Effort:** Medium  

**Proposed Extensions:**

```haskell
-- Rich market context
data MarketCondition = Trending TrendStrength | Ranging | Volatile VolatilityLevel | Calm
data MarketRegime = BullMarket | BearMarket | Sideways
data TrendStrength = Weak | Moderate | Strong
data VolatilityLevel = Low | Medium | High | Extreme

-- Strategy performance by market condition
data StrategyPerformanceProfile = StrategyPerformanceProfile
  { sppByCondition :: Map MarketCondition PerformanceMetrics
  , sppByRegime :: Map MarketRegime PerformanceMetrics
  , sppByVolatility :: Map VolatilityLevel PerformanceMetrics
  , sppOptimalConditions :: [MarketCondition]
  }

-- Market regime detection
class MarketRegimeDetector m where
  detectRegime :: [Candle] -> m MarketRegime
  detectCondition :: [Candle] -> m MarketCondition
```

---

### 10. Live Trading Preparation 🎯

**Priority:** Future (Critical for production) → **Partial Implementation (Live Data Only)**  
**Status:** 🔄 **PARTIALLY IMPLEMENTED**  
**Effort:** Very High → **High (Focused scope)**  

**Phase 1: Live Data Integration (Current Focus)**

```haskell
-- Broker API Abstraction - Data Only
class BrokerDataAPI m where
  getAccount :: m Account  -- Read-only account info
  streamMarketData :: Instrument -> STM (TVar [Tick])
  getHistoricalData :: Instrument -> DateRange -> m [Tick]
  getInstrumentInfo :: Instrument -> m InstrumentDetails

-- Connection management
data BrokerConnection m = BrokerConnection
  { bcConnect :: BrokerConfig -> m (Either ConnectionError Connection)
  , bcDisconnect :: Connection -> m ()
  , bcHealthCheck :: Connection -> m Bool
  , bcReconnect :: Connection -> m (Either ReconnectionError Connection)
  }

-- Live data integration with existing system
integrateWithBacktesting :: LiveDataManager -> BacktestConfig -> IO ()
runLiveDataBacktest :: LiveDataManager -> StrategyInstance -> IO BacktestResult
```

**Phase 2: Order Management (Future)**
- Order placement and management
- Position tracking
- Risk management integration
- Portfolio management

---

## Implementation Roadmap

### Phase 1: Live Data Foundation (Immediate Priority)
- [x] **Enhanced Error Handling (#3)** - ✅ COMPLETED
- [x] **Observability & Monitoring (#5)** - ✅ COMPLETED
- [x] **Configuration System (#4)** - ✅ COMPLETED
- [ ] **Data Pipeline Architecture (#7)** - 🔄 **IN PROGRESS** (Lightstreamer streaming and hardened polling pending)
- [ ] **Concurrency & Parallelization (#8)** - 🔄 **IN PROGRESS** (Scalable multi-instrument streaming pending)

### Phase 2: Live Data Integration (Next 2-3 months)
- [ ] **Live Trading Preparation (#10 - Phase 1)** - 🔄 **IN PROGRESS** (Broker API integration needs hardening)
- [ ] **Performance & Memory Optimization (#2)** - Handle real-time data efficiently
- [ ] **Testing Improvements (#6)** - Live data integration testing

### Phase 3: Enhanced Capabilities  
- [ ] **Strategy Pattern Enhancement (#1)** - Real-time strategy execution
- [ ] **Domain Model Enrichment (#9)** - Live market condition detection

### Phase 4: Full Live Trading (Future)
- [ ] **Live Trading Preparation (#10 - Phase 2)** - Order management
- [ ] Advanced risk management
- [ ] Portfolio management

## Broker Integration Priorities

### Immediate (Next 2 weeks)
1. **Enhanced Error Handling** - ✅ COMPLETED - Robust connection error handling
2. **Basic Observability** - ✅ COMPLETED - Connection monitoring and logging
3. **Configuration System** - ✅ COMPLETED - Global/local config + env overrides (tests default to Demo mode)

### Short Term (Next month)
1. **Data Pipeline Refactoring** - Add broker data source support (Lightstreamer)
2. **Concurrency Infrastructure** - Real-time tick streaming scale-out
3. **Connection Management** - Automatic reconnection and health monitoring

### Medium Term (2-3 months)
1. **Live Data API** - Complete broker integration
2. **Real-time Backtesting** - Run strategies on live data
3. **Performance Optimization** - Handle high-frequency tick streams

## Recommended Broker Integration Approach

### Start with IG.com REST API
- **EU-regulated broker** - Better for EU tax compliance
- Well-documented REST and Streaming APIs - [IG Labs Documentation](https://labs.ig.com/)
- Established European presence with strong regulatory compliance
- Good HTTP client support in Haskell
- You already have an existing account

### Implementation Steps:
1. **Basic connection** - Authentication and account info
2. **Historical data** - Integrate with existing data pipeline  
3. **Live pricing** - Real-time tick streaming via REST API
4. **Streaming API upgrade** - Higher performance real-time data (later)

### Sample Integration:

```haskell
-- Basic IG.com integration
data IGConfig = IGConfig
  { igApiKey :: Text
  , igUsername :: Text
  , igPassword :: Text
  , igEnvironment :: IGEnvironment  -- Demo vs Live
  , igBaseUrl :: Text
  } deriving (Generic, FromJSON)

data IGEnvironment = Demo | Live
  deriving (Show, Eq, Generic, FromJSON)

-- Authentication flow (IG uses session-based auth)
data IGSession = IGSession
  { igSessionToken :: Text
  , igClientSessionToken :: Text
  , igAccountId :: Text
  , igLightstreamerEndpoint :: Text  -- For streaming
  } deriving (Show)

-- Start with REST polling
authenticateIG :: IGConfig -> IO (Either IGError IGSession)
pollIGTicks :: IGSession -> Instrument -> IO (Either IGError [Tick])
streamIGTicks :: IGSession -> Instrument -> IO (STM (TVar [Tick]))

-- IG-specific error handling
data IGError = 
    AuthenticationError Text
  | RateLimitError Int  -- Seconds to wait
  | MarketClosedError
  | InstrumentNotFound Text
  | APIError Int Text   -- HTTP status + message
  deriving (Show, Eq)
```
