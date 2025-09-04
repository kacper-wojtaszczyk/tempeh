# Tempeh Trading Bot - Copilot Instructions

## 🎯 Project Overview

**Tempeh** is a Haskell-based forex trading bot with comprehensive backtesting and **production-ready live trading capabilities**. This serves as both a Haskell learning exercise and a serious attempt at automated forex trading.

### Current Status
- **Phase**: WebSocket Streaming ✅ COMPLETED → Trading Operations (Next)
- **Test Coverage**: 332/332 tests passing
- **Production Ready**: Live market data via REST API + WebSocket streaming
- **Next Priority**: IG deals API for order placement and position management

## 🏗️ Architecture & Design Patterns

### Clean Architecture (Hexagonal/Ports & Adapters)

The project follows a three-layer clean architecture pattern:

- **Domain Layer**: Pure business logic, types, and strategies
- **Application Layer**: Orchestration, CLI interface, and live trading coordination
- **Adapters Layer**: Broker connectivity, data providers, and infrastructure

This provides clear separation between business logic and external systems with dependency inversion.

### Key Architectural Decisions
- **Ports & Adapters**: Clear separation between business logic and external systems
- **STM Concurrency**: Thread-safe state management for live trading
- **Error Handling**: Structured error hierarchy with recovery strategies
- **Configuration**: Global/local/test config split with environment overrides
- **Testing**: Diamond approach (Unit → Integration → E2E → Property-based)

## 📋 Tech Stack

### Core Technologies
- **Language**: Haskell (GHC 9.6.7+)
- **Build Tool**: Cabal
- **Concurrency**: STM (Software Transactional Memory)
- **HTTP**: http-conduit, http-client-tls
- **WebSockets**: websockets, wuss (secure WebSocket)
- **Data**: aeson (JSON), cassava (CSV), scientific (precise numbers)
- **Testing**: tasty, tasty-hunit, tasty-quickcheck

### External Integrations
- **Broker**: IG Markets (REST API + Lightstreamer WebSocket)
- **Data Source**: histdata.com (CSV backtesting data)
- **Protocol**: TLCP (Lightstreamer's Text-based Control Protocol)

## 🚀 Current Implementation Status

### ✅ Completed (Production Ready)
1. **Backtesting Engine**: Historical data analysis with multiple strategies
2. **Authentication System**: Complete IG login/logout with session management
3. **REST API Integration**: Real-time market data polling with error handling
4. **WebSocket Streaming**: Complete Lightstreamer TLCP implementation
5. **Live Data Pipeline**: Tick → Candle → Signal → Strategy execution
6. **Error Recovery**: Structured error handling with automatic retry/failover
7. **Configuration Management**: Environment-aware config with test overrides
8. **Logging System**: Component-based file logging with correlation IDs

### 🎯 Next Phase: Trading Operations
- Account/positions APIs and synchronization
- Order placement & management (IG deals API)
- Stop loss / take profit automation
- Position sizing and risk controls

## 📁 Key File Locations

### Configuration
- `config/global.json` - Public defaults (version controlled)
- `config/local.json` - Private credentials (gitignored)
- `config/test.json` - Safe demo config for testing

### Documentation
- `docs/adr/ADR-001-architectural-improvements.md` - Architecture evolution
- `docs/adr/ADR-002-IG-broker-integration.md` - **Current implementation status**
- `README.md` - User-facing project overview

### Core Modules
- `Domain/` - Pure business logic and types
- `Application/` - Orchestration and CLI
- `Adapter/IG/` - IG broker integration (Auth, Polling, Streaming, Types)
- `Strategy/` - Trading strategies (EMA, RSI, Bollinger Bands)
- `Util/` - Configuration, error handling, logging

### Testing
- `test/Unit/` - Unit tests for individual components
- `test/Integration/` - Component interaction tests
- `test/E2E/` - End-to-end workflow validation

## 🔧 Development Guidelines

### Code Patterns
- **Monadic Error Handling**: Use `Result` type for error propagation
- **STM for Concurrency**: Thread-safe state with Software Transactional Memory
- **Component Logging**: Use `ComponentName` for structured logging
- **Configuration Loading**: Environment-aware config with validation
- **Testing Strategy**: Follow the testing diamond (property-based at top)

### Common Commands
```bash
# Run all tests
cabal test --test-show-details=always

# Backtesting
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001

# Live trading (uses config/local.json)
cabal run tempeh -- live EURUSD ema 5 20 0.0001
```

### File Reading Protocol
**IMPORTANT**: When using `read_file`, always read files in their entirety. Never use `endLineNumberBaseZero` equal to `-1`. Read large enough sections to get complete context.

### Terminal Output Handling
**IMPORTANT**: Due to terminal output access issues, always redirect command output to files:
```bash
command 2>&1 | tee ai/output_$(date +%s).txt
```
Then read the file to determine command success/failure. Wait and retry up to 3 times if file is empty/incomplete.

## 💡 Learning Focus Areas

This is a **learning project** emphasizing:
- **Functional Programming**: Pure functions, immutable data, monadic composition
- **Concurrent Programming**: STM, async operations, thread-safe state management
- **Financial Protocols**: TLCP, market data formats, trading system architecture
- **Testing Practices**: Property-based testing, mocking, integration testing
- **Clean Architecture**: Separation of concerns, dependency inversion

## 🎭 Pair Programming Style

The human prefers:
- **Collaborative approach**: Discussion and objection to ideas that don't follow Haskell best practices
- **High-level focus**: Prefer architectural discussions over low-level implementation details
- **Learning-oriented**: This is a hobby project for fun and learning, not production pressure
- **Quality over speed**: Take time to do things properly rather than rush

## 📊 Trading Strategies

### Implemented Strategies
1. **EMA Crossover**: Trend-following with fast/slow EMA periods
2. **RSI Mean Reversion**: Overbought/oversold signal generation
3. **Bollinger Bands**: Volatility-based mean reversion

### Strategy Architecture
- Pluggable strategy registry system
- State persistence between signal generations
- Parameter validation and configuration
- Integration with live data pipeline

## 🔍 Current Challenges & Opportunities

### Recent Achievements
- Fixed Lightstreamer message parsing (CONOK response and update message detection)
- Updated documentation to reflect actual implementation status
- All 332 tests now passing

### Next Technical Challenges
- IG deals API integration for order placement
- Position management and risk controls
- Account synchronization
- Real-time P&L tracking

### Architecture Evolution Opportunities
- Strategy composition and portfolio management
- Performance optimization with Vector-based data structures
- Multi-broker support (OANDA, Interactive Brokers)
- Real-time monitoring dashboard
