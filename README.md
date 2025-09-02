# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting capabilities built using **Ports & Adapters (Hexagonal) Architecture**. This project serves as both a Haskell learning exercise and a serious attempt at making me rich (results not guaranteed).

I'm using Copilot extensively, at least in the initial stages. Feel free to comment on any hallucinations you notice.

## 🏗️ Architecture

The project follows clean architecture principles with clear separation of concerns across three layers:

- **Domain Layer**: Core business logic including types, strategies, and services
- **Application Layer**: Orchestration, CLI interface, and reporting services  
- **Adapters Layer**: Infrastructure components for data loading, risk management, and report generation

## ✨ Features

- **🔄 Multiple Trading Strategies**: EMA Crossover and RSI Mean Reversion with parameterizable settings
- **⚙️ Configurable Parameters**: Customize strategy parameters directly from the command line
- **📊 Tick Data Processing**: Converts raw tick data to OHLC candles for strategy analysis
- **⚡ Pure Functional Backtesting**: Immutable, composable backtesting engine (time machine for seeing how broke I would have been)
- **📅 Date Range Filtering**: Run backtests on specific time periods
- **📋 Comprehensive Reporting**: Detailed P&L analysis, trade history, and performance metrics
- **🛡️ Risk Management**: Built-in position sizing and risk controls
- **🧪 Extensive Testing**: Tests following the Testing Diamond approach

## 🚀 Usage

### Basic Commands

```bash
# Show help
cabal run tempeh -- --help

# EMA strategy with default parameters (5/20 periods, 0.0001 threshold)
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema

# RSI strategy with default parameters (14-period, 70/30 levels)
cabal run tempeh backtest GBPUSD 2025 7 2025 8 rsi
```

### Advanced Parameter Customization

```bash
# Custom EMA parameters: 12/26 periods with 0.0005 threshold
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 12 26 0.0005

# Custom RSI parameters: 21-period with 80/20 overbought/oversold levels
cabal run tempeh backtest GBPUSD 2025 7 2025 8 rsi 21 80 20

# Conservative RSI: Wider bands (25/75) for fewer but stronger signals
cabal run tempeh backtest USDJPY 2025 6 2025 6 rsi 14 75 25
```

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

## 📈 Strategy Comparison Examples

```bash
# Test different EMA sensitivities on the same data
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001    # Fast
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 12 26 0.0005   # Standard
cabal run tempeh backtest EURUSD 2025 1 2025 3 ema 21 50 0.001    # Slow

# Compare trend-following vs mean-reversion on the same period  
cabal run tempeh backtest GBPUSD 2025 7 2025 8 ema 12 26 0.0005   # Trend following
cabal run tempeh backtest GBPUSD 2025 7 2025 8 rsi 14 70 30       # Mean reversion
```

## 📊 Data Requirements

Backtesting data can be sourced from [histdata.com](https://www.histdata.com/). The application expects CSV files with the format:

- **Filename**: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv`
- **CSV Structure**: `datetime,bid_price,ask_price,volume`
- **Datetime Format**: `YYYYMMDD HHMMSSTTT`

Place CSV files in the `data/backtesting/` directory.

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
- **strategy**: `ema` or `rsi` with optional parameters

### EMA Parameters (optional)
- **fast**: Fast EMA period (default: 5)
- **slow**: Slow EMA period (default: 20)
- **threshold**: Signal threshold in pips (default: 0.0001)

### RSI Parameters (optional)
- **period**: RSI calculation period (default: 14)
- **overbought**: Overbought level (default: 70)
- **oversold**: Oversold level (default: 30)

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
