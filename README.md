# Tempeh Trading Bot

A Haskell-based forex trading bot with backtesting capabilities. This is a work-in-progress project that serves as both a Haskell learning exercise and a way to make me rich.

I'm using Copilot extensively, at least in the initial stage of the project. Feel free to comment on/correct any hallucinations you notice.

## Current Features

- **Flexible Instrument Support**: Works with any forex currency pair (EURUSD, GBPUSD, USDJPY, etc.)
- **Tick Data Processing**: Converts raw tick data to OHLC candles for strategy analysis
- **EMA Crossover Strategy**: Implements a simple 5/20 exponential moving average crossover strategy
- **Backtesting Engine**: Pure functional backtesting with configurable parameters (time machine for seeing how broke I would have been)
- **Date Range Filtering**: Run backtests on specific time periods
- **Comprehensive Reporting**: Detailed P&L analysis and trade history

## Data Requirements

### Data Source
Backtesting data can be sourced from [histdata.com](https://www.histdata.com/), which provides free historical forex tick data.

### CSV Format
The application expects CSV files with the following format:
- **Filename**: `DAT_ASCII_[INSTRUMENT]_T_[YYYYMM].csv` (e.g., `DAT_ASCII_EURUSD_T_202508.csv`)
- **CSV Structure**: `datetime,bid_price,ask_price,volume`
- **Datetime Format**: `YYYYMMDD HHMMSSTTT` (e.g., `20250101 170014647`)
- **Price Format**: Decimal numbers (e.g., `1.035030`)
- **Volume**: Integer or decimal (often 0 for tick data)

Example CSV content:
```
20250101 170014647,1.035030,1.035910,0
20250101 170014949,1.035100,1.035850,0
20250101 170019212,1.035140,1.035850,0
```

Place CSV files in the `data/backtesting/` directory.

## Usage

```bash
# Run backtest on all EURUSD data (default)
cabal run tempeh backtest

# Run backtest on specific instrument
cabal run tempeh backtest GBPUSD

# Run backtest on date range (EURUSD)
cabal run tempeh backtest 2025 1 2025 3

# Run backtest on specific instrument and date range
cabal run tempeh backtest GBPUSD 2025 7 2025 8
```

## Architecture

The application is built with a clean, functional architecture:
- **Domain Types**: Core business entities (Instrument, Candle, Trade, etc.)
- **Strategy Layer**: Trading strategy implementations with pure functions
- **Backtest Engine**: Stateful backtesting execution with immutable state transitions
- **Data Processing**: CSV parsing and tick-to-candle aggregation
- **Reporting**: P&L calculation and trade analysis

## Current Status

This is an early-stage project with the following implemented:
- ✅ Tick data loading and processing
- ✅ Candle aggregation (1-minute OHLC)
- ✅ EMA crossover strategy
- ✅ Backtesting engine with trade execution simulation
- ✅ Flexible instrument support
- ✅ Date range filtering
- 🚧 Strategy optimization (planned)
- 🚧 Live trading integration (the "put my money where my mouth is" phase)
- 🚧 Additional technical indicators (planned)

## Building

```bash
cabal build
cabal run tempeh --help
```

## Testing

```bash
cabal test
```
