# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting capabilities built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and a serious attempt at making me rich (results not guaranteed).

I'm using Copilot extensively, at least in the initial stages. Feel free to comment on any hallucinations you notice.

## 🚧 Project Status

**Current Phase: Backtesting Engine (95% Complete)**

This is a **work-in-progress** project currently focused on building a robust backtesting foundation before moving to live trading capabilities. The backtesting engine is nearly complete and battle-tested with historical forex data.

### ✅ Completed Features
- Core backtesting engine with hexagonal architecture
- EMA Crossover and RSI Mean Reversion strategies
- Comprehensive test suite (unit, integration, e2e)
- CSV data ingestion from histdata.com
- Detailed performance reporting and analytics
- Risk management and position sizing
- Configurable strategy parameters via CLI

### 🔄 Current Work
- Performance optimizations for large datasets
- Additional technical indicators and strategies
- Enhanced reporting with drawdown analysis
- Strategy parameter optimization tools

### 🎯 Future Plans: Live Trading
- Real-time data feeds integration
- Broker API adapters (OANDA, Interactive Brokers)
- Live execution engine with order management
- Real-time monitoring and alerting
- Paper trading mode for strategy validation

## 🏗️ Architecture

The project follows clean architecture principles with clear separation of concerns across three layers:

- **Domain Layer**: Core business logic including types, strategies, and services
- **Application Layer**: Orchestration, CLI interface, and reporting services  
- **Adapters Layer**: Infrastructure components for data loading, risk management, and report generation

## ✨ Features

- **🔄 Multiple Trading Strategies**: EMA Crossover, RSI Mean Reversion, and Bollinger Bands with parameterizable settings
- **⚙️ Configurable Parameters**: Customize strategy parameters directly from the command line
- **📊 Tick Data Processing**: Converts raw tick data to OHLC candles for strategy analysis
- **⚡ Pure Functional Backtesting**: Immutable, composable backtesting engine (time machine for seeing how broke I would have been)
- **📅 Date Range Filtering**: Run backtests on specific time periods
- **📋 Comprehensive Reporting**: Detailed P&L analysis, trade history, and performance metrics
- **🛡️ Risk Management**: Built-in position sizing and risk controls
- **📝 Structured Logging**: File-based logging system with component-specific log files and clean console output
- **🧪 Extensive Testing**: Tests following the Testing Diamond approach
- **🏗️ Clean Architecture**: Hexagonal architecture ready for live trading extensions

## 📊 Available Strategies

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

## 📈 Strategy Comparison Examples

```bash
# Test different EMA sensitivities on the same data
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001    # Fast
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 12 26 0.0005   # Standard
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 21 50 0.001    # Slow

# Compare trend-following vs mean-reversion on the same period  
cabal run tempeh backtest GBPUSD 2025 7 2025 8 ema 12 26 0.0005   # Trend following
cabal run tempeh backtest GBPUSD 2025 7 2025 8 rsi 14 70 30       # Mean reversion

# Test Bollinger Bands mean reversion strategy
cabal run tempeh backtest GBPUSD 2025 7 2025 8 bb 20 2.0 0.0001   # Bollinger Bands
```

## 📊 Data Requirements

Backtesting data can be sourced from [histdata.com](https://www.histdata.com/). The application expects CSV files with the format:

- **Filename**: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- **CSV Structure**: `datetime,bid_price,ask_price,volume`
- **Datetime Format**: `YYYYMMDD HHMMSSTTT`

Place CSV files in the `data/backtesting/` directory.

## 📝 Logging System

The application features a comprehensive structured logging system that separates technical logs from user-facing output:

### 🎯 Clean Output Separation
- **Console Output**: Clean, user-friendly backtest results and summaries
- **Log Files**: Detailed application logs stored in the `log/` directory

### 📁 Log File Organization
Each backtest run creates a dedicated log file with the naming pattern:
```
log/backtest-{INSTRUMENT}-{STRATEGY}-{TIMESTAMP}.log
```

**Example log files:**
```
log/backtest-EURUSD-rsi-20250902-134838.log
log/backtest-GBPUSD-ema-20250902-141225.log
```

### 🔍 Log Content Structure
Log entries include structured information for debugging and monitoring:
```
2025-09-02 13:48:38 [Info] (backtest-EURUSD-rsi)  Loading ticks for EURUSD from 2025-1 to 2025-1
2025-09-02 13:48:38 [Debug] (backtest-EURUSD-rsi)  Successfully loaded 125,847 ticks
2025-09-02 13:48:39 [Info] (backtest-EURUSD-rsi)  Executing backtest with 1,440 candles
```

**Each log entry contains:**
- **Timestamp**: Precise execution time
- **Log Level**: Debug, Info, Warn, Error
- **Component**: Which part of the system generated the log
- **Message**: Detailed information about the operation

### 💡 Benefits
- **Clean User Experience**: No technical logs cluttering console output
- **Detailed Debugging**: Full application flow captured in persistent files
- **Component Tracing**: Easy identification of which system component generated each log
- **Production Ready**: Essential for live trading monitoring and troubleshooting

## 🧪 Test Suite

The project uses a **Testing Diamond** approach with comprehensive test coverage:
- **End-to-End Tests**: Complete workflows with real data flow
- **Integration Tests**: Component interactions and data flow
- **Unit Tests**: Critical business logic validation

```bash
# Run all tests
cabal test

# Run with verbose output
cabal test --test-show-details=always
```

## 🔧 Development

```bash
# Build the project
cabal build

# Run tests
cabal test

# Clean build artifacts
cabal clean
```

### Adding New Strategies
1. Implement the strategy in `src/Strategy/`
2. Add strategy parameters to `Domain.Services.BacktestService`
3. Update `Adapter.StrategyFactory` to handle the new strategy
4. Add CLI parsing support in `Application.CLI`
5. Add tests in the appropriate test directories

## 📋 Command Reference

### Full Command Syntax
```
tempeh backtest <instrument> <start_year> <start_month> <end_year> <end_month> <strategy> [params]
```

### Parameters
- **instrument**: Currency pair (EURUSD, GBPUSD, USDJPY, etc.)
- **start_year/month**: Start date (e.g., 2025 1)
- **end_year/month**: End date (e.g., 2025 3)
- **strategy**: `ema`, `rsi`, or `bb` with optional parameters

### EMA Parameters (optional)
- **fast**: Fast EMA period (default: 5)
- **slow**: Slow EMA period (default: 20)
- **threshold**: Signal threshold in pips (default: 0.0001)

### RSI Parameters (optional)
- **period**: RSI calculation period (default: 14)
- **overbought**: Overbought level (default: 70)
- **oversold**: Oversold level (default: 30)

### Bollinger Bands Parameters (optional)
- **period**: Bollinger Bands period (default: 20)
- **deviation**: Standard deviation multiplier (default: 2)
- **threshold**: Signal threshold in pips (default: 0.0001)

## 🎯 Performance

The backtesting engine can process millions of ticks and generate detailed performance metrics including win rates, profit factors, and drawdown analysis.

Example output:
```
=== TEMPEH BACKTEST ===
Instrument: EURUSD
Strategy: EmaCrossParams {ecpFastPeriod = 12, ecpSlowPeriod = 26, ecpSignalThreshold = 5.0e-4}

=== BACKTEST RESULTS ===
Final Balance: $10,245.80
Total P&L: $245.80
Total Trades: 23
```

## 📝 License

This project is licensed under the terms specified in the LICENSE file.

---

*Built with Haskell 🎯 • Powered by Clean Architecture 🏗️ • Tested with Diamond Approach 💎*
