# Tempeh Trading Bot - Copilot Instructions

## 🎯 Project Context [ESSENTIAL]

**Tempeh** is a production-ready Haskell forex trading bot following clean architecture patterns. This is both a learning project and serious automated trading system.

**Current Phase**: WebSocket Streaming ✅ → Trading Operations (IG deals API integration)  
**Status**: 332/332 tests passing, live market data operational

## 🏗️ Architecture [ESSENTIAL]

### Clean Architecture (Hexagonal/Ports & Adapters)
- **Domain**: Pure business logic (`Domain/`, `Strategy/`)
- **Application**: Orchestration (`Application/`, CLI)
- **Adapters**: External systems (`Adapter/IG/`, data providers)

### Key Patterns
- **Error Handling**: `Result` type for error propagation
- **Concurrency**: STM (Software Transactional Memory) for thread-safe state
- **Logging**: Component-based with `ComponentName`
- **Testing**: Diamond approach (Unit → Integration → E2E → Property-based)

## 📁 Key File Locations [ESSENTIAL]

### Configuration
- `config/global.json` - Public defaults
- `config/local.json` - Private credentials (gitignored)
- `config/test.json` - Test config

### Core Modules
- `Domain/` - Pure business logic and types
- `Application/` - CLI and orchestration
- `Adapter/IG/` - Broker integration (Auth, Polling, Streaming, Types)
- `Strategy/` - Trading strategies (EMA, RSI, Bollinger Bands)
- `Util/` - Config, error handling, logging

### Documentation
- `docs/adr/ADR-002-IG-broker-integration.md` - Current implementation status

## 🔧 Development Guidelines [ESSENTIAL]

### Commands
```bash
cabal test --test-show-details=always
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001
cabal run tempeh -- live EURUSD ema 5 20 0.0001
```

### macOS Compatibility [CRITICAL]
**IMPORTANT**: Do not use `timeout` command - it's not available on macOS. Use alternative approaches:
- For testing timeouts: Use Ctrl+C to interrupt manually
- For background processes: Use `&` and `kill` if needed
- For bounded execution: Design tests with internal timeouts

### File Reading Protocol [CRITICAL]
**IMPORTANT**: Always read files completely. Never use `endLineNumberBaseZero = -1`. Read large sections for complete context.

### Terminal Output [CRITICAL]
**IMPORTANT**: Redirect all command output to files due to access limitations:
```bash
command 2>&1 | tee ai/output_$(date +%s).txt
```
Read the file to check results. Retry up to 3 times if empty/incomplete.

## 🚀 Current Status [ESSENTIAL]

### Completed ✅
- Backtesting engine with multiple strategies
- IG authentication and session management
- REST API polling with error recovery
- WebSocket streaming (Lightstreamer TLCP)
- Live data pipeline: Tick → Candle → Signal → Strategy

### Next Phase 🎯
- IG deals API for order placement
- Position management and risk controls
- Account synchronization

## 📊 Tech Stack [REFERENCE]

**Core**: Haskell (GHC 9.6.7+), Cabal, STM  
**HTTP**: http-conduit, websockets, wuss  
**Data**: aeson, cassava, scientific  
**Testing**: tasty ecosystem  
**Broker**: IG Markets (REST + Lightstreamer WebSocket)

## 🎭 Working Style [REFERENCE]

This is a **learning project** emphasizing:
- Collaborative approach with Haskell best practices
- Quality over speed
- High-level architectural focus
- Functional programming patterns
- Having fun with the learning process
