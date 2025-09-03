# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting and **live trading capabilities** built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and a serious attempt at making me rich (results not guaranteed).

I'm using Copilot extensively, at least in the initial stages. Feel free to comment on any hallucinations you notice.

## 🚧 Project Status

**Current Phase: Live Trading Foundation (Phase 1) Completed → Market Data Streaming (Next)**

This project has successfully evolved from backtesting to the **live trading foundation**. The backtesting engine is complete and battle-tested, and we now have the core live trading components integrated and working end-to-end in Demo mode, with streaming and orders as the next milestones.

### ✅ Completed Features

#### Backtesting Engine (100% Complete)
- Core backtesting engine with hexagonal architecture
- EMA Crossover, RSI Mean Reversion, and Bollinger Bands strategies
- Comprehensive test suite (unit, integration, e2e)
- CSV data ingestion from histdata.com
- Detailed performance reporting and analytics
- Risk management and position sizing
- Configurable strategy parameters via CLI

#### Live Trading Foundation (Phase 1)
- 🔐 IG Auth & Session: REST API authentication and logout implemented (demo verified)
- 🔧 Configuration Management: Global/local config with secure credentials; test override config
- 📡 Connection & Subscriptions: STM-based connection status and per-instrument tick buffers
- 🧬 Live Orchestrator Pipeline: Ticks → 1-minute candles → strategy signal generation
- 📊 Data Quality API: Basic metrics available (stub values for now)
- 🛠️ CLI Live Commands: Live mode wired to orchestrator and broker adapter

Notes:
- The test suite uses a testing-safe configuration file (Demo broker) loaded from `config/test.json`.

### 🔄 Current Work: Market Data Streaming
- Harden REST-based tick polling (retry, rate limiting, metrics)
- Implement Lightstreamer WebSocket streaming for real-time market data
- Scale multi-instrument concurrency and backpressure handling
- Live candle generation monitoring and metrics

### 🎯 Future Plans
- Order management and position handling via IG deals API
- Account and positions APIs
- Multi-broker support (OANDA, Interactive Brokers)
- Real-time monitoring dashboard
- Advanced portfolio management across strategies
- Performance analytics for live trading

## 🏗️ Architecture

The project follows clean architecture principles with clear separation of concerns across three layers:

- **Domain Layer**: Core business logic including types, strategies, and services
- **Application Layer**: Orchestration, CLI interface, live trading, and reporting services  
- **Adapters Layer**: Infrastructure for data loading, broker connectivity, risk management, and reporting

## ✨ Features

### Trading Strategies
- 🔄 EMA Crossover: Trend-following strategy with configurable fast/slow periods
- 📈 RSI Mean Reversion: Counter-trend strategy with overbought/oversold levels
- 📉 Bollinger Bands: Volatility-based mean reversion strategy

### Trading Modes
- 📊 Backtesting Mode: Historical data analysis with comprehensive reporting
- 🔴 Live Trading Mode: Live trading foundation implemented; streaming/orders forthcoming
- ⚙️ Configurable Parameters: Customize strategy parameters via command line

### Infrastructure
- 🏗️ Hexagonal Architecture: Clean separation of concerns with ports and adapters
- 🛡️ Type Safety: Strong typing throughout with comprehensive error handling
- 🔧 Configuration System: Global/local config, plus a dedicated test config override
- 📝 Comprehensive Logging: Structured logging with component-specific files
- 🧪 Extensive Testing: Unit, integration, and end-to-end test coverage

## 📈 Available Strategies

### EMA Crossover (Trend Following)
- **Parameters**: `ema [fast] [slow] [threshold]`
- **Default**: `5 20 0.0001`
- **Logic**: Buy when fast EMA crosses above slow EMA, sell when it crosses below
- **Best For**: Trending markets

### RSI Mean Reversion (Counter-Trend)
- **Parameters**: `rsi [period] [overbought] [oversold]`
- **Default**: `14 70 30`  
- **Logic**: Buy when RSI drops below oversold level, sell when above overbought level
- **Best For**: Range-bound markets

### Bollinger Bands (Volatility Mean Reversion)
- **Parameters**: `bb [period] [std_multiplier] [threshold]`
- **Default**: `20 2.0 0.0001`
- **Logic**: Buy when price touches lower band (oversold), sell when price touches upper band (overbought)
- **Best For**: Range-bound markets with mean-reverting price action

## 🚀 Getting Started

### Prerequisites
- GHC 9.6.7+ and Cabal
- IG trading account (demo or live) with API access

### Configuration
1. Copy the local config template:
   ```bash
   cp config/local.json.template config/local.json
   ```
2. Add your IG credentials to `config/local.json`.
3. For tests/CI, no env flags are required; the suite loads `config/test.json` automatically.

### Usage

#### Backtesting
```bash
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001
cabal run tempeh -- backtest GBPUSD 2025 1 2025 6 rsi 14 70 30
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 bb 20 2.0 0.0001
```

#### Live Trading
```bash
cabal run tempeh -- live EURUSD ema 5 20 0.0001
cabal run tempeh -- live GBPUSD rsi 14 70 30
cabal run tempeh -- live EURUSD bb 20 2.0 0.0001
```

Demo/real mode:
- Tests and CI use `config/test.json` (Demo broker, no network creds).
- For real runs, ensure `config/local.json` includes valid credentials.

## 📊 Data Requirements

Backtesting data can be sourced from [histdata.com](https://www.histdata.com/). The application expects CSV files with the format:

- Filename: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- CSV Structure: `datetime,bid_price,ask_price,volume`
- Datetime Format: `YYYYMMDD HHMMSSTTT`

Place CSV files in the `data/backtesting/` directory.

## 🔗 Live Trading Requirements

For live trading, you need:
- IG Trading Account: Demo or live account with API access
- API Key: Generated from IG's developer portal
- Valid Credentials: Username, password, and account ID

The system currently supports IG's demo environment and includes a Demo broker mode for safe testing. Streaming via Lightstreamer and order management are planned next.

## 📄 License

This project is licensed under the terms specified in the LICENSE file.

---

*Built with Haskell 🎯 • Powered by Clean Architecture 🏗️ • Tested with Diamond Approach 💎*
