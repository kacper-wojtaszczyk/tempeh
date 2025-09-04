# ADR-003: TLCP Library Extraction for Ecosystem Reusability

**Status:** DRAFT 📋 (Under Consideration)  
**Date:** 2025-09-05  
**Deciders:** Development Team  
**Technical Story:** Architectural Evolution → Extracting TLCP Protocol Implementation as Reusable Library

## Context

During the development of Lightstreamer WebSocket streaming integration (ADR-002), we have implemented a significant portion of the TLCP (Text Lightstreamer Client Protocol) within the Tempeh trading bot. The current implementation in `src/Adapter/IG/Streaming.hs` contains both protocol-specific TLCP logic and IG-specific trading adaptations mixed together.

As we work toward stabilizing the WebSocket streaming implementation, an architectural question has emerged: **Should we extract the TLCP protocol implementation into a separate, reusable Haskell library?**

### Current TLCP Implementation Status

The existing implementation includes:
- WebSocket connection management with TLCP subprotocol
- Session lifecycle management (create_session, bind_session)
- Subscription management and message parsing
- Real-time tick data processing
- Error handling and recovery mechanisms

**Reference Documentation**: Complete TLCP specification is documented in `docs/lightstreamer/docs.md`

### Ecosystem Gap Analysis

Research indicates that the Haskell ecosystem currently lacks a mature, standalone TLCP implementation. Existing Lightstreamer clients are primarily available in Java, JavaScript, and .NET, but no first-class Haskell library exists for TLCP protocol communication.

## Problem Statement

The current architecture mixes two distinct concerns:
1. **Protocol Implementation**: Pure TLCP message parsing, WebSocket management, session handling
2. **Domain Adaptation**: IG-specific epic mapping, tick conversion, trading logic integration

This coupling creates several challenges:
- **Maintainability**: Protocol updates require changes within trading-specific code
- **Testability**: Difficult to isolate TLCP protocol compliance from business logic
- **Reusability**: Other projects cannot leverage our TLCP implementation
- **Ecosystem Contribution**: Missing opportunity to provide value to the broader Haskell community

## Decision

**DRAFT STATUS**: This ADR is under consideration and proposes a phased approach to TLCP library extraction.

### Proposed Solution: Phased TLCP Library Extraction

Extract the TLCP protocol implementation into a separate `lightstreamer-tlcp` library while maintaining clean separation between protocol and domain concerns.

## Detailed Design

### Phase 1: Stabilization (Current Sprint)
**Status**: 🎯 **IMMEDIATE PRIORITY**

Before extraction, stabilize the existing TLCP implementation within Tempeh:
- ✅ Resolve connection stability issues (CloseRequest 1011 errors)
- ✅ Complete message parsing (CONF, EOS message handlers)  
- ✅ Fix timestamp parsing issues
- ✅ Ensure full TLCP protocol compliance

**Rationale**: Extract from a solid, working foundation rather than unstable code.

### Phase 2: Internal Refactoring (Next Sprint)
**Status**: 🔄 **PREPARATION**

Refactor current streaming code for clean internal boundaries:

```haskell
-- Create clear module separation within Tempeh
src/Adapter/IG/Streaming/
  ├── TLCP.hs           -- Pure protocol implementation
  ├── IGAdapter.hs      -- IG-specific adaptations  
  └── Connection.hs     -- Integration layer
```

**Benefits**:
- Establishes clean architectural boundaries
- Prepares codebase for extraction
- Improves maintainability immediately
- Reduces extraction risk

### Phase 3: Library Extraction (Future Sprint)
**Status**: 📋 **PLANNED**

Extract protocol code into standalone `lightstreamer-tlcp` library:

```haskell
-- Proposed lightstreamer-tlcp library structure
lightstreamer-tlcp/
├── src/Network/Lightstreamer/TLCP/
│   ├── Protocol.hs          -- Core TLCP types and parsing
│   ├── Connection.hs        -- WebSocket connection management  
│   ├── Session.hs           -- Session lifecycle (create/bind)
│   ├── Subscription.hs      -- Subscription management
│   ├── Messages.hs          -- Message parsing and formatting
│   └── Types.hs            -- TLCP protocol types
├── Network/Lightstreamer.hs -- Public API
├── test/Protocol/           -- Protocol compliance tests
├── test/Integration/        -- Real Lightstreamer integration tests
└── docs/TLCP-Reference.md   -- Protocol documentation
```

**Clean Abstraction Interface**:
```haskell
-- Generic adapter interface for different brokers
class LightstreamerAdapter a where
  parseItem :: Text -> Maybe a
  formatSubscription :: a -> SubscriptionRequest  
  handleUpdate :: UpdateMessage -> Maybe a
  
-- IG-specific implementation (remains in Tempeh)
instance LightstreamerAdapter IGMarketData where
  parseItem epic = parseIGEpic epic
  formatSubscription = formatIGSubscription
  handleUpdate = parseIGTick
```

## Implementation Strategy

### Library API Design
```haskell
-- Core TLCP connection management
data TLCPConnection = TLCPConnection { ... }
data TLCPSession = TLCPSession { ... }  
data TLCPSubscription = TLCPSubscription { ... }

-- Primary library interface
establishConnection :: TLCPConfig -> IO (Result TLCPConnection)
createSession :: TLCPConnection -> AuthConfig -> IO (Result TLCPSession)
subscribe :: LightstreamerAdapter a => TLCPSession -> [a] -> IO (Result TLCPSubscription)
handleMessages :: TLCPConnection -> (TLCPMessage -> IO ()) -> IO ()
```

### Migration Path
1. **Extract Protocol Core**: Move pure TLCP logic to library
2. **Maintain IG Adapter**: Keep IG-specific code in Tempeh
3. **Update Dependencies**: Tempeh depends on `lightstreamer-tlcp`
4. **Publish Library**: Release to Hackage with documentation
5. **Community Engagement**: Blog post, examples, ecosystem contribution

## Consequences

### Positive Consequences ✅

#### For Tempeh Project
- **Cleaner Architecture**: Clear separation between protocol and business logic
- **Better Testability**: Protocol can be tested in complete isolation
- **Easier Maintenance**: TLCP updates don't affect trading strategies
- **Reduced Complexity**: Smaller, focused modules

#### For Haskell Ecosystem  
- **First-Class TLCP Library**: Fills existing gap in ecosystem
- **Community Benefit**: Other financial applications can leverage Lightstreamer
- **Documentation**: Centralized, authoritative TLCP implementation
- **Protocol Evolution**: Single place for TLCP version updates

#### For Development Quality
- **Focused Testing**: Comprehensive protocol compliance test suite
- **Better Documentation**: Library forces clear API documentation
- **Reusability**: Potential for other trading applications
- **Open Source Contribution**: Valuable ecosystem addition

### Negative Consequences ❌

#### Development Overhead
- **Additional Complexity**: Managing separate repository and package
- **Coordination Effort**: Version management between library and Tempeh
- **Release Management**: Library releases affect Tempeh development cycles

#### Potential Risks
- **Premature Abstraction**: Risk if TLCP requirements change significantly
- **Maintenance Burden**: Responsibility for library maintenance and support
- **Breaking Changes**: Library updates could affect Tempeh stability

### Mitigation Strategies

1. **Stability First**: Only extract after proven, stable implementation
2. **Conservative API**: Design library API for stability over flexibility initially  
3. **Semantic Versioning**: Careful version management to minimize breaking changes
4. **Documentation**: Comprehensive documentation and examples
5. **Community Engagement**: Gather feedback before major API changes

## Risk Assessment

### High Impact, Low Probability
- **Breaking Protocol Changes**: Lightstreamer updates requiring major API changes
- **Performance Issues**: Library abstraction introducing latency
- **Community Adoption**: Low usage leading to maintenance burden

### Medium Impact, Medium Probability  
- **Version Coordination**: Managing library updates with Tempeh development
- **API Evolution**: Need for breaking changes as requirements evolve
- **Testing Complexity**: Ensuring library works across different Lightstreamer configurations

### Mitigation Approach
- **Incremental Development**: Small, focused releases with extensive testing
- **Conservative Design**: Prioritize stability over feature richness initially
- **Real-World Validation**: Validate library with Tempeh production usage
- **Community Feedback**: Engage potential users early in design process

## Alternatives Considered

### Alternative 1: Keep TLCP Embedded
**Pros**: Simpler development, no coordination overhead, faster iteration
**Cons**: Mixed concerns, no ecosystem contribution, difficult to test protocol isolation

### Alternative 2: Immediate Extraction  
**Pros**: Clean architecture from start, early ecosystem contribution
**Cons**: High risk extracting unstable code, potential for significant rework

### Alternative 3: Internal Library (Git Submodule)
**Pros**: Separation benefits without external publishing overhead
**Cons**: No ecosystem benefit, complex dependency management, limited reusability

## Success Criteria

### Phase 1 Success (Stabilization)
- [ ] WebSocket streaming runs without connection errors for >1 hour
- [ ] All TLCP message types handled correctly (including CONF, EOS)  
- [ ] Timestamp parsing produces valid UTCTime values
- [ ] Full test coverage for protocol edge cases

### Phase 2 Success (Internal Refactoring)  
- [ ] Clear module boundaries between protocol and IG-specific code
- [ ] Protocol logic can be tested independently
- [ ] No regression in streaming functionality
- [ ] Improved code maintainability metrics

### Phase 3 Success (Library Extraction)
- [ ] `lightstreamer-tlcp` library published to Hackage
- [ ] Comprehensive documentation and examples
- [ ] Tempeh successfully uses extracted library
- [ ] At least one external project shows interest in adoption
- [ ] Library passes real Lightstreamer server compatibility tests

## Implementation Notes

### Technical Considerations
- **WebSocket Management**: Library should handle connection lifecycle transparently
- **Error Handling**: Comprehensive error types for different failure modes  
- **Configuration**: Flexible configuration for different Lightstreamer setups
- **Performance**: Minimize allocation and latency in message processing
- **Testing**: Mock Lightstreamer server for integration testing

### Documentation Requirements
- **API Reference**: Complete Haddock documentation
- **Protocol Guide**: TLCP protocol explanation with examples
- **Integration Examples**: Sample applications showing library usage
- **Migration Guide**: How to adopt library in existing projects

## Conclusion

The extraction of TLCP protocol implementation into a reusable library represents an excellent architectural evolution that benefits both Tempeh and the broader Haskell ecosystem. However, the **phased approach is critical** to success:

1. **First**: Stabilize existing implementation (immediate priority)
2. **Then**: Refactor for clean boundaries (preparation)  
3. **Finally**: Extract as library (when stable and mature)

This approach maximizes the likelihood of creating a high-quality, valuable library while minimizing risk to Tempeh's development timeline.

**Current Status**: This ADR remains in DRAFT status pending successful completion of Phase 1 (WebSocket streaming stabilization). The decision to proceed with extraction will be revisited after achieving stable TLCP implementation within Tempeh.

**Next Actions**: Focus on resolving current WebSocket streaming issues as documented in ADR-002 before proceeding with architectural extraction.

---

*This ADR captures the architectural vision for TLCP library extraction while maintaining pragmatic focus on current development priorities.*
