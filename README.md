# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting capabilities built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and an attempt at making me rich.

I'm using Copilot extensively, at least in the initial stages. Feel free to comment on any hallucinations you notice.

## 🏗️ Architecture

The project follows clean architecture principles with clear separation of concerns across three layers:

- **Domain Layer**: Core business logic including types, strategies, and services
- **Application Layer**: Orchestration, CLI interface, and reporting services  
- **Adapters Layer**: Infrastructure components for data loading, risk management, and report generation

## ✨ Features

- **🔄 Flexible Instrument Support**: Works with any forex currency pair (EURUSD, GBPUSD, USDJPY, etc.)
- **📊 Tick Data Processing**: Converts raw tick data to OHLC candles for strategy analysis
- **📈 EMA Crossover Strategy**: Implements exponential moving average crossover strategy with configurable parameters
- **⚡ Pure Functional Backtesting**: Immutable, composable backtesting engine
- **📅 Date Range Filtering**: Run backtests on specific time periods
- **📋 Comprehensive Reporting**: Detailed P&L analysis, trade history, and performance metrics
- **🛡️ Risk Management**: Built-in position sizing and risk controls
- **🧪 Extensive Testing**: Tests following the Testing Diamond approach

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

## 📊 Data Requirements

Backtesting data can be sourced from [histdata.com](https://www.histdata.com/). The application expects CSV files with the format:

- **Filename**: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- **CSV Structure**: `datetime,bid_price,ask_price,volume`
- **Datetime Format**: `YYYYMMDD HHMMSSTTT`

Place CSV files in the `data/backtesting/` directory.

## 🚀 Usage

```bash
# Show help
cabal run tempeh -- --help

# Run backtest examples
cabal run tempeh backtest EURUSD 2025 1 2025 3
cabal run tempeh backtest GBPUSD 2025 7 2025 8
cabal run tempeh backtest USDJPY 2025 6 2025 6
```

### Command Syntax
```
tempeh backtest <instrument> <start_year> <start_month> <end_year> <end_month>
```

## 📈 Strategy Configuration

The default EMA Crossover strategy uses:
- **Fast EMA**: 5 periods
- **Slow EMA**: 20 periods
- **Signal Threshold**: 0.0001 (1 pip for most pairs)
- **Initial Balance**: $10,000
- **Position Size**: $1,000 per trade

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
4. Add tests in the appropriate test directories

## 🎯 Performance

The backtesting engine can process millions of ticks and generate detailed performance metrics including win rates, profit factors, and drawdown analysis.

---

*Built with Haskell 🎯 • Powered by Clean Architecture 🏗️ • Tested with Diamond Approach 💎*
