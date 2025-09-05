# IG Streaming API - TLCP Integration Specification

Here's a comprehensive breakdown of the **IG Streaming API** for **Lightstreamer WebSocket integration** using the **TLCP (Text Lightstreamer Client Protocol)** format. This document focuses on **authentication** and **subscription operations** with proper TLCP message formatting.

---

## 1. Authentication Flow

### 1.1 REST API Authentication (Prerequisites)

Before establishing WebSocket connection, authenticate via the **REST API `/session` endpoint** to obtain:

* **CST** (Client Session Token)
* **X-SECURITY-TOKEN** (XST)  
* **Lightstreamer endpoint URL** (dynamically retrieved)

**Important**: Do not hard-code the Lightstreamer server address—retrieve it dynamically from `/session`.

### 1.2 Lightstreamer WebSocket Authentication

When connecting via TLCP WebSocket protocol:

**Connection Parameters**:
* **Identifier**: Your IG account ID
* **Password**: Concatenated tokens in format: `CST-<CST_token>|XST-<XST_token>`
* **Adapter Set**: Use the adapter set provided by IG (typically `DEFAULT`)

---

## 2. TLCP Session Management

### 2.1 Create Session Request

**Request Format**:
```
create_session
LS_user={account_id}&LS_password=CST-{cst_token}|XST-{xst_token}&LS_adapter_set=DEFAULT&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg
```

**Parameters**:
* `LS_user` - Required. Your IG account identifier
* `LS_password` - Required. Format: `CST-{token}|XST-{token}`
* `LS_adapter_set` - Optional. Default is `DEFAULT`
* `LS_cid` - Required. Client identifier for custom clients
* `LS_requested_max_bandwidth` - Optional. Bandwidth limit in kbps
* `LS_keepalive_millis` - Optional. Keepalive interval in ms

**Example**:
```
create_session
LS_user=ABC123&LS_password=CST-xyz123abc|XST-def456ghi&LS_adapter_set=DEFAULT&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg&LS_keepalive_millis=30000
```

### 2.2 Bind Session Request

**Request Format**:
```
bind_session
LS_session={session_id}
```

**Parameters**:
* `LS_session` - Optional. Session ID to bind to
* `LS_keepalive_millis` - Optional. Keepalive interval
* `LS_inactivity_millis` - Optional. Max inactivity before timeout

---

## 3. IG Subscription Operations

### 3.1 Price Subscription

**Description**: Real-time streaming of market price ladder data with bid/ask tiers, pricing metadata, and timestamps.

**Item Format**: `PRICE:{account_identifier}:{epic}`
**Mode**: `MERGE`
**Data Adapter**: `DEFAULT`

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=add&LS_subId={subscription_id}&LS_group=PRICE:{account_id}:{epic}&LS_schema={field_list}&LS_mode=MERGE&LS_data_adapter=DEFAULT
```

**Parameters**:
* `LS_reqId` - Required. Unique request identifier
* `LS_op` - Required. Set to `add` for subscription
* `LS_subId` - Required. Unique subscription ID (integer starting from 1)
* `LS_group` - Required. Item group: `PRICE:{account_id}:{epic}`
* `LS_schema` - Required. Space-separated field list
* `LS_mode` - Required. Set to `MERGE`
* `LS_data_adapter` - Optional. Data adapter name (default: `DEFAULT`)
* `LS_snapshot` - Optional. Request snapshot (true/false)
* `LS_requested_max_frequency` - Optional. Max update frequency

**Available Fields**:
* `MID_OPEN` - Opening mid price
* `HIGH` - Intraday high price
* `LOW` - Intraday low price
* `BIDQUOTEID` - Bid quote ID for trading reference
* `ASKQUOTEID` - Ask quote ID for trading reference
* `BIDPRICE1` to `BIDPRICE5` - Bid prices at ladder tiers 1-5
* `ASKPRICE1` to `ASKPRICE5` - Ask prices at ladder tiers 1-5
* `BIDSIZE1` to `BIDSIZE5` - Available bid sizes per tier
* `ASKSIZE1` to `ASKSIZE5` - Available ask sizes per tier
* `CURRENCY0` - Currency of default ladder
* `CURRENCY1` to `CURRENCY5` - Currencies for additional tiers
* `TIMESTAMP` - Update timestamp in UTC milliseconds
* `DLG_FLAG` - Dealing flag indicator

**Example Request**:
```
control
LS_reqId=1&LS_op=add&LS_subId=1&LS_group=PRICE:ABC123:CS.D.EURUSD.CFD.IP&LS_schema=BIDPRICE1 ASKPRICE1 BIDSIZE1 ASKSIZE1 HIGH LOW TIMESTAMP DLG_FLAG&LS_mode=MERGE
```

### 3.2 Account Subscription

**Description**: Streams account-level metrics including funds, equity, margin, P&L.

**Item Format**: `ACCOUNT:{account_identifier}`
**Mode**: `MERGE`

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=add&LS_subId={subscription_id}&LS_group=ACCOUNT:{account_id}&LS_schema={field_list}&LS_mode=MERGE
```

**Available Fields**:
* `PNL` - Overall profit/loss
* `PNL_LR` - Limited-risk P&L
* `PNL_NLR` - Non-limited-risk P&L
* `DEPOSIT` - Minimum required deposit for margin
* `AVAILABLE_CASH` - Cash available after margin and P&L
* `FUNDS` - Total funds
* `MARGIN` - Total margin amount
* `MARGIN_LR` - Limited-risk margin
* `MARGIN_NLR` - Non-limited-risk margin
* `AVAILABLE_TO_DEAL` - Funds available for new deals
* `EQUITY` - Total equity
* `EQUITY_USED` - Equity currently used

**Example Request**:
```
control
LS_reqId=3&LS_op=add&LS_subId=3&LS_group=ACCOUNT:ABC123&LS_schema=AVAILABLE_CASH EQUITY MARGIN PNL&LS_mode=MERGE
```

### 3.3 Trade Subscription

**Description**: Streams trade-related events including confirmations and open position updates.

**Item Format**: `TRADE:{account_identifier}`
**Mode**: `DISTINCT`

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=add&LS_subId={subscription_id}&LS_group=TRADE:{account_id}&LS_schema={field_list}&LS_mode=DISTINCT
```

**Available Fields**:

**CONFIRMS** (Trade Confirmations):
* `direction` - "BUY" or "SELL"
* `limitLevel` - Numeric limit level
* `dealId` - Deal identifier
* `affectedDeals` - Array of affected deal objects
* `status` - Deal status (AMENDED, DELETED, FULLY_CLOSED, OPENED, PARTIALLY_CLOSED)
* `stopLevel` - Stop level
* `expiry` - Expiry timestamp
* `size` - Deal size
* `dealReference` - Deal reference
* `dealStatus` - Deal status
* `repeatDealingWindow` - Repeat dealing window data

**OPU** (Open Position Updates):
* `direction` - "BUY" or "SELL"
* `limitLevel` - Limit level
* `dealId` - Deal identifier
* `dealIdOrigin` - Original deal ID
* `stopLevel` - Stop level
* `expiry` - Expiry timestamp
* `timestamp` - Update timestamp
* `size` - Position size
* `status` - Position status (AMENDED, CLOSED, DELETED, OPEN, PARTIALLY_CLOSED)

**Example Request**:
```
control
LS_reqId=4&LS_op=add&LS_subId=4&LS_group=TRADE:ABC123&LS_schema=CONFIRMS OPU&LS_mode=DISTINCT
```

### 3.4 Chart Tick Subscription

**Description**: Delivers per-tick updates for real-time granular market data.

**Item Format**: `CHART:{epic}:TICK`
**Mode**: `DISTINCT`

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=add&LS_subId={subscription_id}&LS_group=CHART:{epic}:TICK&LS_schema={field_list}&LS_mode=DISTINCT
```

**Available Fields**:
* `BID` - Current bid price
* `OFR` - Current offer price
* `LTP` - Last traded price
* `LTV` - Last traded volume
* `TTV` - Total traded volume
* `UTM` - Update time (timestamp)
* `DAY_OPEN_MID` - Daily opening mid price
* `DAY_NET_CHG_MID` - Daily net change (mid)
* `DAY_PERC_CHG_MID` - Daily percentage change (mid)
* `DAY_HIGH` - Daily high
* `DAY_LOW` - Daily low

**Example Request**:
```
control
LS_reqId=5&LS_op=add&LS_subId=5&LS_group=CHART:CS.D.EURUSD.CFD.IP:TICK&LS_schema=BID OFR LTP LTV DAY_OPEN_MID DAY_NET_CHG_MID DAY_HIGH DAY_LOW&LS_mode=DISTINCT
```

### 3.5 Chart Candlestick Subscription

**Description**: Provides aggregated chart/candlestick data at defined intervals.

**Item Format**: `CHART:{epic}:{scale}`
**Mode**: `MERGE`

**Scale Options**: `SECOND`, `1MINUTE`, `5MINUTE`, `HOUR`, etc.

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=add&LS_subId={subscription_id}&LS_group=CHART:{epic}:{scale}&LS_schema={field_list}&LS_mode=MERGE
```

**Available Fields**:
* `LTV` - Last traded volume
* `TTV` - Total traded volume
* `UTM` - Update time
* `DAY_OPEN_MID` - Daily opening mid price
* `DAY_NET_CHG_MID` - Daily net change (mid)
* `DAY_PERC_CHG_MID` - Daily percentage change (mid)
* `DAY_HIGH` - Daily high
* `DAY_LOW` - Daily low
* `OFR_OPEN` - Offer OHLC: Open
* `OFR_HIGH` - Offer OHLC: High
* `OFR_LOW` - Offer OHLC: Low
* `OFR_CLOSE` - Offer OHLC: Close
* `BID_OPEN` - Bid OHLC: Open
* `BID_HIGH` - Bid OHLC: High
* `BID_LOW` - Bid OHLC: Low
* `BID_CLOSE` - Bid OHLC: Close
* `LTP_OPEN` - Last traded price OHLC: Open
* `LTP_HIGH` - Last traded price OHLC: High
* `LTP_LOW` - Last traded price OHLC: Low
* `LTP_CLOSE` - Last traded price OHLC: Close
* `CONS_END` - Consolidation period end timestamp
* `CONS_TICK_COUNT` - Number of ticks merged in consolidation

**Example Request**:
```
control
LS_reqId=6&LS_op=add&LS_subId=6&LS_group=CHART:CS.D.EURUSD.MINI.IP:1MINUTE&LS_schema=LTV UTM DAY_OPEN_MID DAY_NET_CHG_MID DAY_PERC_CHG_MID DAY_HIGH DAY_LOW OFR_OPEN OFR_HIGH OFR_LOW OFR_CLOSE BID_OPEN BID_HIGH BID_LOW BID_CLOSE LTP_OPEN LTP_HIGH LTP_LOW LTP_CLOSE CONS_END CONS_TICK_COUNT&LS_mode=MERGE
```

## 4. Unsubscription Operations

### 4.1 Unsubscribe Request

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=delete&LS_subId={subscription_id}
```

**Parameters**:
* `LS_reqId` - Required. Unique request identifier
* `LS_op` - Required. Set to `delete` for unsubscription
* `LS_subId` - Required. ID of subscription to remove
* `LS_ack` - Optional. If false, REQOK is skipped

**Example Request**:
```
control
LS_reqId=10&LS_op=delete&LS_subId=1
```

## 5. Connection Management

### 5.1 Heartbeat Request

**Request Format**:
```
heartbeat
LS_session={session_id}
```

### 5.2 Session Destroy Request

**Request Format**:
```
control
LS_reqId={request_id}&LS_op=destroy&LS_cause_code=31&LS_cause_message=Client%20disconnect
```

---

## Summary

* **Authentication**: Use REST API to obtain CST + XST tokens, then authenticate WebSocket with TLCP `create_session`
* **Subscriptions**: Five main types with proper TLCP `control` requests
* **Message Format**: All requests follow TLCP protocol with proper parameter encoding
* **Modes**: `MERGE` for consolidated data, `DISTINCT` for event-based updates
* **Fields**: Each subscription type has specific field schema with detailed descriptions

**Reference**: [IG Streaming API Reference](https://labs.ig.com/streaming-api-reference.html)
