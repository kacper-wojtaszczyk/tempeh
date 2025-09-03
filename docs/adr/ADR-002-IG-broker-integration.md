# ADR-002: Comprehensive IG Broker Integration for Live Trading

**Status:** Phase 1 (Auth & Connection) Implemented → Phase 2 (Streaming) In Progress  
**Date:** 2025-09-03  
**Deciders:** Development Team  
**Technical Story:** Live Trading Phase → Broker Connectivity & Streaming

## Context

Following the successful completion of the backtesting engine, the project has transitioned to live trading capabilities. This ADR documents the implemented scope of IG broker integration to date and the plan going forward. The implemented scope focuses on authentication, configuration, connection state, and a subscription/tick buffering pathway for live data, with streaming and order management planned.

The IG broker was selected as the primary integration target due to:
- **Comprehensive API Coverage**: REST API for trading operations, Lightstreamer for real-time data
- **EU Regulatory Compliance**: FCA-regulated broker suitable for EU tax compliance
- **Demo Environment**: Safe testing environment with production-equivalent API
- **Market Coverage**: Major forex pairs with competitive spreads
- **Documentation Quality**: Well-documented APIs at [IG Labs](https://labs.ig.com/)

## Architectural Overview

This integration introduces the broker connectivity system with these implemented capabilities:

1. **Authentication & Session Management** - Implemented with session token extraction and logout
2. **Configuration Management** - Implemented global/local split and env overrides
3. **Connection & Subscription State** - Implemented (STM-based per-instrument tick buffers)
4. **Live Data Path (Initial)** - Implemented scaffold: REST polling loop + candle generation in orchestrator
5. **Streaming & Orders** - Planned

## Phase 1: Authentication & Data Path Foundations (COMPLETED ✅)

### 1.1 Authentication Architecture

**Implementation:** Session management with secure header extraction and basic expiration handling

```haskell
-- Session Management
data IGSession = IGSession
  { igSessionToken :: Text
  , igCST :: Text
  , igXSecurityToken :: Text
  , igExpiresAt :: UTCTime
  } deriving (Show)

-- Authentication Flow Implementation
loginToIG :: BrokerConfig -> Text -> Text -> IO (Result IGSession)
logoutFromIG :: BrokerConfig -> IGSession -> IO (Result ())
```

**Process:** POST /session with credentials + API key → extract CST/X-SECURITY-TOKEN → store with expiry → DELETE /session on disconnect.

**✅ Verified:** Demonstrated locally with demo credentials; default test runs use Demo broker via a dedicated test config.

### 1.2 Configuration Management

**Implementation:** Global (versioned) + Local (gitignored) JSON with environment overrides.

- Global config defines broker defaults and endpoints
- Local config supplies secrets (API key, username, password, account)
- Test/CI config: `config/test.json` is loaded by the test runner via a Util.Config override (no env flags required). Legacy env flags remain supported for backward compatibility.

### 1.3 Connection & Subscription State

**Implementation:** STM-based connection registry with per-instrument tick buffers.

- Connection status tracking (Connected/Disconnected/Reconnecting)
- Subscriptions map Instrument → TVar [Tick]
- Safe Demo fallback when credentials missing or tests force Demo

## Phase 2: Real-time Data Streaming (IN PROGRESS 🔄)

### 2.1 Data Path & Streaming

**Current:**
- REST polling loop scaffolded (`pollIGMarketDataLoop`) and wired when an IG session is present
- Subscription starts polling in background if authenticated
- Tests intentionally run in Demo mode using `config/test.json`

**Next:**
- Replace ad-hoc REST polling with production-grade polling (error handling, rate limiting)
- Implement Lightstreamer WebSocket streaming for true real-time updates
- Expand multi-instrument concurrency and backpressure handling

### 2.2 Tick Processing Pipeline

- Orchestrator converts ticks → 1m candles → runs strategy for signals
- Data quality API returns basic metrics (stubbed values for now)

## Phase 3+: Account, Orders, Risk (PLANNED 📋)

- Account info and positions APIs
- Order placement and deal execution
- Real-time risk controls and position sizing

## Implementation Roadmap

### ✅ Phase 1: Foundation (COMPLETED)
- [x] Authentication & Session Management
- [x] Configuration System (Global/Local + Test override)
- [x] Connection State Management (STM)
- [x] Basic Data Subscription Framework & tick buffers
- [x] CLI integration and live orchestrator pipeline (ticks → candles → signals)

**Status:** Ready for demo authentication and basic connectivity; tests default to Demo via `config/test.json`.

### 🔄 Phase 2: Real-time Data (IN PROGRESS)
- [ ] Harden REST API Tick Polling (retry, rate limit, metrics)
- [ ] Lightstreamer WebSocket Integration
- [ ] Scalable multi-instrument streaming + backpressure
- [ ] Live candle generation monitoring & metrics

### 📋 Phase 3: Account & Orders (PLANNED)
- [ ] Account/positions APIs and synchronization
- [ ] Order placement & management
- [ ] Stop loss / take profit management

### 📋 Phase 4: Risk Management (PLANNED)
- [ ] Real-time risk monitoring and limits
- [ ] Dynamic position sizing
- [ ] Emergency controls

## Testing & Demo Mode Notes

- The test suite loads `config/test.json` at startup using a test override in `Util.Config`, ensuring a Demo broker with no live network credentials.
- End-to-end and integration tests validate the live pipeline behavior using the Demo path.
- For real runs, ensure `config/local.json` provides valid IG credentials; env flags remain optional for legacy workflows.

## Security Considerations

- Local secrets kept out of VCS (gitignored); global configuration is public
- HTTPS enforced; tokens handled via headers; no secrets in logs
- Session invalidation on disconnect; reconnection policies planned

## Conclusion

We’ve implemented the broker connectivity foundation (auth, config, connection state, subscription/tick buffers) and the live trading orchestrator pipeline. Next up is production-grade real-time data via Lightstreamer and hardened polling, followed by account/order features.
