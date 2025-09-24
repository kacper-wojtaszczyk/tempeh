# Tempeh Trading Bot - Copilot Instructions

## 🎯 Project Overview [ESSENTIAL]

I'm learning Haskell by building a forex trading bot from scratch. Please assume a mentoring approach, guiding me through best practices, idioms, and architectural patterns. Be critical and descriptive. Argument in detail why you agree or disagree with my ideas and suggest alternatives if needed. Explain the changes you make to the code and reasoning behind them. Always suggest improvements and optimizations.

**Current Phase**: All IG Adapter Phases Complete (✅ COMPLETED) → Performance Optimization (🎯 Next Priority)  
**Status**: 442/442 tests passing, complete modular IG adapter architecture deployed with zero legacy debt

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

**❌ NEVER DO THIS:**
```
endLineNumberBaseZero = -1  // This will make files appear empty!
```

**✅ ALWAYS DO THIS:**
```
# First check file length with wc -l
wc -l /path/to/file
# Then read with proper end line (file_length - 1)
endLineNumberBaseZero = file_length_minus_one
```

**WHY**: Using -1 causes the read_file tool to fail and return empty content, making files appear empty when they actually contain important information.

### Terminal Output [CRITICAL]
**IMPORTANT**: Redirect all command output to files due to terminal access limitations:
```bash
command 2>&1 | tee ai/output_$(date +%s).txt
```
Read the file to verify command's success and check results instead of relying on direct terminal access. Retry up to 3 times if empty/incomplete.

### Development Practices
- Whenever you add new code it must be covered by tests.
- Follow Haskell best practices and idioms.
- Whenever you use documentation as a source of roadmap/next steps remember to update the documentation with current progress after making changes

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
- **IG adapter modular refactoring (ALL PHASES)**: FULLY COMPLETED ✅
  - ✅ **Phase 1**: Modular architecture with Session, Connection, Trading, Error modules
  - ✅ **Phase 2**: BrokerDataProvider migration and LiveDataProvider integration
  - ✅ **Phase 3**: Complete BrokerAdapter orchestration with error recovery
  - ✅ **Phase 4**: Legacy cleanup - all temporary artifacts and deprecated patterns removed
  - ✅ Comprehensive unit test coverage for all modules (442/442 tests passing)
  - ✅ Clean architecture patterns fully implemented
  - ✅ Zero regressions across all implementation phases
  - ✅ Production-ready modular architecture deployed

### Next Phase 🎯
- **Phase 5**: Performance optimization and monitoring
  - Circuit breaker patterns for external API calls
  - Enhanced metrics collection and monitoring
  - Resource pooling for improved efficiency
  - Rate limiting for IG API calls
- **Phase 6**: Advanced resilience features
  - Health check endpoints
  - Alerting system for system health
  - Load testing of modular architecture
  - Memory optimization

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
