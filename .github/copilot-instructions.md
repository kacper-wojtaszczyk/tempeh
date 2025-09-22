# Tempeh Trading Bot - Copilot Instructions

## 🎯 Project Context [ESSENTIAL]

**Tempeh** is a production-ready Haskell forex trading bot following clean architecture patterns. This is both a learning project and serious automated trading system.

**Current Phase**: WebSocket Streaming (✅ COMPLETED) → Trading Operations (🎯 Next Priority)  
**Status**: 332/332 tests passing, live market data fully operational with real-time streaming

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
- `docs/IG/REST.md` - IG REST API documentation
- `docs/IG/TLCP.md` - Complete TLCP protocol specification
- `docs/IG/streaming.md` - IG-specific TLCP integration patterns

## 🔧 Development Guidelines [ESSENTIAL]

### Commands
```bash
cabal test --test-show-details=always
cabal run tempeh -- backtest EURUSD 2025 1 2025 3 ema 5 20 0.0001
cabal run tempeh -- live EURUSD ema 5 20 0.0001
```

### macOS Compatibility [CRITICAL]
**IMPORTANT**: Do not use `timeout` command - it's not available on macOS. Use `gtimeout` from coreutils instead:

### File Reading Protocol [CRITICAL]
**IMPORTANT**: Always read files completely. Never use `endLineNumberBaseZero = -1`. Read large sections for complete context.

### Terminal Output [CRITICAL]
**IMPORTANT**: Redirect all command output to files due to terminal access limitations:
```bash
command 2>&1 | tee ai/output_$(date +%s).txt
```
Read the file to verify command's success and check results instead of relying on direct terminal access. Retry up to 3 times if empty/incomplete.

## 🚀 Current Status [ESSENTIAL]

### Completed ✅
- Backtesting engine with multiple strategies
- IG authentication and session management
- REST API polling with error recovery
- **WebSocket streaming (Lightstreamer TLCP)**: FULLY OPERATIONAL ✅
  - ✅ TLCP protocol implementation with proper authentication
  - ✅ Session creation and subscription management
  - ✅ Real-time tick data streaming (EURUSD live prices)
  - ✅ Connection stability and error recovery
  - ✅ Complete message parsing (CONOK, SUBOK, U-format updates)
  - ✅ Timestamp parsing and tick creation pipeline
- Live data pipeline: Tick → Candle → Signal → Strategy (WebSocket + REST)
- **IG adapter modular refactoring (Phase 1)**: FULLY COMPLETED ✅
  - ✅ Modular architecture with Session, Connection, Trading, Error modules
  - ✅ Comprehensive unit test coverage for new modules
  - ✅ Clean architecture patterns implemented
  - ✅ All compilation issues resolved
  - ✅ Foundation ready for Phase 2 integration

### In Progress 🔄
- **IG deals API integration with new modular architecture (Phase 2)**
- Account synchronization and position tracking

### Next Phase 🎯
- Integration of new modular IG adapter with existing BrokerDataProvider
- Risk controls and position sizing
- Stop loss/take profit automation

## 📊 Tech Stack [REFERENCE]

**Core**: Haskell (GHC 9.6.7+), Cabal, STM  
**HTTP**: http-conduit, websockets, wuss  
**Data**: aeson, cassava, scientific  
**Testing**: tasty ecosystem  
**Broker**: IG Markets (REST + Lightstreamer WebSocket)

## 🎭 Working Style [REFERENCE]

This is a **learning project** emphasizing:
- Collaborative approach with Haskell best practices
- In-depth documentation of architectural decisions and patterns
- Quality over speed
- High-level architectural focus
- Functional programming patterns
- Having fun with the learning process
