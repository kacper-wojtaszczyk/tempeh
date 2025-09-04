# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting and **live trading capabilities** built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and a semi-serious attempt at making me rich.

## 🚧 Project Status

**Current Phase: WebSocket Streaming COMPLETED ✅ → Trading Operations (Next)**

This project has successfully evolved from backtesting to **production-ready live trading** with comprehensive IG broker integration. The backtesting engine is complete and battle-tested, and we now have a **fully functional live trading system** processing real market data via both REST API and **real-time WebSocket streaming**.

### ✅ Completed Features

#### Backtesting Engine
- Historical data analysis with EMA, RSI, and Bollinger Bands strategies
- Performance reporting and risk analysis
- CSV data ingestion from histdata.com

#### Live Trading System  
- **Real-time market data** from IG broker via REST API and WebSocket streaming
- **Sub-second tick latency** through Lightstreamer WebSocket integration
- **Complete TLCP protocol implementation** for IG's streaming platform
- **Automated strategy execution** with configurable parameters
- **Multi-instrument trading** support (EURUSD, GBPUSD, USDJPY, etc.)
- **Live signal generation** from real-time streaming market data
- **Automatic failover** between WebSocket streaming and REST API polling

### 🎯 Current Work: Trading Operations
**Next Priority**: Implement IG deals API for order placement and position management
- Account and positions synchronization via IG API
- Order placement and management (market, limit, stop orders)
- Stop loss and take profit automation
- Position sizing and risk controls

📋 **See [ADR-002: IG Broker Integration](docs/adr/ADR-002-IG-broker-integration.md) for detailed technical implementation status**

### 🔮 Future Plans
- Multi-broker support (OANDA, Interactive Brokers)
- Real-time monitoring dashboard
- Advanced portfolio management

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

## 📊 Data Requirements

**Backtesting**: CSV files from [histdata.com](https://www.histdata.com/) in `data/backtesting/`
- Format: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- Structure: `datetime,bid_price,ask_price,volume`

**Live Trading**: IG trading account with API access for real-time market data

---

*Built with Haskell 🎯 • Powered by Clean Architecture 🏗️ • Tested with Diamond Approach 💎*
