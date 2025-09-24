# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting and **live trading capabilities** built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and a semi-serious attempt at making me rich.

## 🚧 Project Status

**Current Phase: All IG Adapter Phases Complete (✅ COMPLETED) → Performance Optimization (🎯 Next Priority)**

This project has successfully evolved from backtesting to **live trading with real-time market data** via comprehensive IG broker integration. The backtesting engine is complete and battle-tested, and we now have a **fully operational live trading system** with **complete modular IG adapter architecture** processing real market data via both REST API and **complete WebSocket streaming**.

**Major Achievement**: Complete IG adapter modular refactoring successfully deployed with **442/442 tests passing** and zero legacy debt.

### ✅ Completed Features

#### Backtesting Engine
- Historical data analysis with EMA, RSI, and Bollinger Bands strategies
- Performance reporting and risk analysis
- CSV data ingestion from histdata.com

#### Live Trading System  
- **Real-time market data** from IG broker via REST API polling
- **Real-time WebSocket streaming** with sub-second latency via Lightstreamer TLCP
- **Automated strategy execution** with configurable parameters
- **Multi-instrument trading** support (EURUSD, GBPUSD, USDJPY, etc.)
- **Live signal generation** from real-time market data
- **Production-ready REST API integration** with comprehensive error handling
- **Complete TLCP WebSocket implementation** with proper authentication and session management

### ✅ WebSocket Streaming: BREAKTHROUGH ACHIEVED
- **Lightstreamer WebSocket integration** - FULLY OPERATIONAL ✅
  - ✅ Complete TLCP protocol implementation with proper authentication
  - ✅ Session creation, binding, and subscription management  
  - ✅ Real-time tick data streaming with sub-second latency
  - ✅ Connection stability with proper error recovery
  - ✅ Complete message parsing (CONOK, SUBOK, U-format updates)
  - ✅ Timestamp parsing and tick creation pipeline
  - ✅ Live EURUSD bid/ask prices: `Tick {tTime = 2025-09-05 14:50:48.695 UTC, tBid = Price 1.17314, tAsk = Price 1.17323}`

### ✅ IG Adapter Modular Architecture: FULLY COMPLETED
- **Complete modular refactoring** - PRODUCTION READY ✅
  - ✅ **Phase 1**: Foundation modules (Session, Connection, Trading, Error)
  - ✅ **Phase 2**: BrokerDataProvider migration and integration
  - ✅ **Phase 3**: Complete BrokerAdapter orchestration with error recovery
  - ✅ **Phase 4**: Legacy cleanup - all temporary artifacts and deprecated patterns removed
  - ✅ Multi-connection support for concurrent IG operations
  - ✅ Automatic session management with renewal and lifecycle handling
  - ✅ Connection-aware subscriptions with proper state isolation
  - ✅ Environment-aware error recovery (demo vs production mode)
  - ✅ Thread-safe STM-based state management
  - ✅ **442/442 tests passing** with comprehensive coverage across all modules

### 🎯 Next Phase: Performance Optimization
**Priority Focus**: System resilience and monitoring enhancements
- **Phase 5**: Performance optimization and monitoring
  - Circuit breaker patterns for external API calls
  - Enhanced metrics collection and performance monitoring
  - Resource pooling for improved efficiency
  - Sophisticated rate limiting for IG API calls
- **Phase 6**: Advanced resilience features
  - Comprehensive health check endpoints
  - Proactive alerting system for system health and performance
  - Load testing of the modular architecture
  - Memory usage optimization and profiling

## 🏗️ Architecture

Clean architecture with hexagonal design across three layers:

- **Domain Layer**: Core business logic, types, and strategies
- **Application Layer**: Orchestration, CLI interface, and live trading coordination  
- **Adapters Layer**: Broker connectivity, data loading, and infrastructure

## 📈 Trading Strategies

- **EMA Crossover**: Trend-following with configurable fast/slow periods (`ema [fast] [slow] [threshold]`)
- **RSI Mean Reversion**: Counter-trend with overbought/oversold levels (`rsi [period] [overbought] [oversold]`)
- **Bollinger Bands**: Volatility-based mean reversion (`bb [period] [std_multiplier] [threshold]`)

## 🚀 Getting Started

### Prerequisites
- GHC 9.6.7+ and Cabal
- IG trading account (demo or live) with API access

### Configuration
```bash
cp config/local.json.template config/local.json
# Add your IG credentials to config/local.json
```

### Usage

#### Backtesting
```bash
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001
cabal run tempeh -- backtest GBPUSD 2025 1 2025 6 rsi 14 70 30
```

#### Live Trading
```bash
cabal run tempeh -- live EURUSD ema 5 20 0.0001
cabal run tempeh -- live GBPUSD rsi 14 70 30
```

**Demo/Real Mode**: Tests use `config/test.json` (safe demo). For live trading, ensure `config/local.json` contains valid IG credentials.

**✅ Note**: WebSocket streaming is now fully operational with sub-second latency. The system automatically uses WebSocket streaming as the primary data source with REST API as failover backup.

## 📊 Data Requirements

**Backtesting**: CSV files from [histdata.com](https://www.histdata.com/) in `data/backtesting/`
- Format: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- Structure: `datetime,bid_price,ask_price,volume`

**Live Trading**: IG trading account with API access for real-time market data

---

*Built with Haskell 🎯 • Powered by Clean Architecture 🏗️ • Tested with Diamond Approach 💎*
