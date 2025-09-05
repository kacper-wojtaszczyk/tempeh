# TLCP WebSocket Transport Specification

---

## General Request Format

All requests must be sent as **UTF-8 encoded text WebSocket messages**.

The format of a request is:

```
<request-name>
param1=value1&param2=value2&...&paramN=valueN
```

Rules:

* The first line contains the request name, for example `create_session`, `bind_session`, `control`, `msg`, `heartbeat`, `wsok`.
* The second line contains the parameters, in the form `key=value` pairs separated by `&`.
* Each request must be entirely contained within a single WebSocket message.
* Line separator is CR-LF. A trailing CR-LF at the end is optional.
* Reserved characters in parameter values must be percent-encoded. The reserved characters are:
  carriage return (CR), line feed (LF), ampersand (&), equals sign (=), percent (%), plus sign (+).
* Percent encoding must be applied on UTF-8 encoded values.
* Request identifiers (`LS_reqId`) must be unique within a WebSocket connection. They can be reused after a sufficient delay.

---

## Requests

### 1. create\_session

Creates a new session and binds it to the current WebSocket connection.

Parameters:

* LS\_cid. Required. Client identifier. For custom clients must be set to `mgQkwtwdysogQz2BJ4Ji%20kOj2Bg`.
* LS\_user. Optional. Username for authentication, interpreted by the Metadata Adapter.
* LS\_password. Optional. Password for authentication.
* LS\_adapter\_set. Optional. Adapter Set logical name. Default is `DEFAULT`.
* LS\_requested\_max\_bandwidth. Optional. Requested bandwidth limit in kbps. Decimal number with dot separator, or `unlimited`.
* LS\_supported\_diffs. Optional. Comma separated list of accepted diff formats for updates.
* LS\_polling. Optional. If true, session will use polling instead of streaming.
* LS\_polling\_millis. Required if LS\_polling is true. Expected time in ms between closing and reopening polling connections.
* LS\_idle\_millis. Optional, only if polling. Max wait time in ms before server closes a poll without data.
* LS\_inactivity\_millis. Optional, only if not polling. Max inactivity time in ms before server assumes client is stuck.
* LS\_keepalive\_millis. Optional, only if not polling. Server’s keepalive interval in ms. If exceeded, a PROBE notification is sent.
* LS\_send\_sync. Optional. If false, SYNC notifications are not sent. Default is true.
* LS\_reduce\_head. Optional. If true, server omits SERVNAME, CLIENTIP and CONS notifications. Default false.
* LS\_ttl\_millis. Optional. Time to live of request in ms. May be integer, `unknown` (default) or `unlimited`.

Example:

```
create_session
LS_user=demo&LS_password=demo&LS_adapter_set=DEMO&LS_cid=mgQkwtwdysogQz2BJ4Ji%20kOj2Bg
```

---

### 2. bind\_session

Rebinds an existing session to the current WebSocket connection.

Parameters:

* LS\_session. Optional. Session ID to bind to. If omitted, the last bound session on this WebSocket is used.
* LS\_recovery\_from. Optional. Progressive number of last data notification received. Used for session recovery.
* LS\_content\_length. Optional. Requested maximum content length for connection.
* LS\_polling. Optional. If true, use polling mode.
* LS\_polling\_millis. Required if polling. Next poll delay in ms.
* LS\_idle\_millis. Optional, only if polling. Wait time in ms for data before closing.
* LS\_inactivity\_millis. Optional, only if not polling. Max inactivity in ms before server assumes client is unresponsive.
* LS\_keepalive\_millis. Optional. Keepalive interval in ms. If exceeded, PROBE notification is sent.
* LS\_send\_sync. Optional. If false, SYNC notifications are suppressed. Default true.
* LS\_reduce\_head. Optional. If true, suppresses certain notifications (CONOK, SERVNAME, CLIENTIP). Default false.

Example:

```
bind_session
LS_session=S1234567890
```

---

### 3. control

Used to send control operations for an existing session. Each control request must include LS\_reqId and LS\_op.

Common parameters:

* LS\_session. Optional on WS. Defaults to last bound session.
* LS\_reqId. Required. Unique identifier for the request.
* LS\_op. Required. Operation type.

Operations:

a) Subscription (LS\_op=add).
Parameters:

* LS\_subId. Required. Unique subscription ID (integer starting from 1).
* LS\_group. Required. Item group name.
* LS\_schema. Required. Field schema name.
* LS\_data\_adapter. Optional. Data Adapter. Default "DEFAULT".
* LS\_selector. Optional. Selector name.
* LS\_mode. Required. RAW, MERGE, DISTINCT, or COMMAND.
* LS\_requested\_buffer\_size. Optional. Buffer size in events or "unlimited".
* LS\_requested\_max\_frequency. Optional. Decimal number, "unlimited" or "unfiltered".
* LS\_snapshot. Optional. true, false, or integer (only for DISTINCT mode). Default false.
* LS\_ack. Optional. If false, REQOK is skipped. Default true.

Example:

```
control
LS_reqId=1&LS_op=add&LS_subId=1&LS_group=item1&LS_schema=last_price&LS_mode=MERGE
```

b) Unsubscription (LS\_op=delete).
Parameters:

* LS\_subId. Required. The ID of subscription to remove.
* LS\_ack. Optional. If false, REQOK is skipped.

Example:

```
control
LS_reqId=2&LS_op=delete&LS_subId=1
```

c) Reconfiguration (LS\_op=reconf).
Parameters:

* LS\_subId. Required.
* LS\_requested\_max\_frequency. Optional.

Example:

```
control
LS_reqId=3&LS_op=reconf&LS_subId=1&LS_requested_max_frequency=2.0
```

d) Session constrain (LS\_op=constrain).
Parameters:

* LS\_requested\_max\_bandwidth. Optional. Decimal number or "unlimited".

Example:

```
control
LS_reqId=4&LS_op=constrain&LS_requested_max_bandwidth=50.0
```

e) Force rebind (LS\_op=force\_rebind).
Parameters:

* LS\_polling\_millis. Optional. Expected time until next binding.
* LS\_close\_socket. Optional. If true, closes the stream connection.

Example:

```
control
LS_reqId=5&LS_op=force_rebind
```

f) Session destroy (LS\_op=destroy).
Parameters:

* LS\_cause\_code. Optional. Numeric code for END notification. Only zero or negative allowed.
* LS\_cause\_message. Optional. Custom message for END. Limited to simple ASCII, single line, max 35 chars.
* LS\_close\_socket. Optional. If true, closes connection.

Example:

```
control
LS_reqId=6&LS_op=destroy
```

---

### 4. msg

Used to send an upstream message to the server or adapters.

Parameters:

* LS\_reqId. Required. Unique request ID.
* LS\_message. Required. Message body.
* LS\_sequence. Optional. Sequence name to order messages.
* LS\_msg\_prog. Conditionally required if LS\_sequence is used or LS\_outcome is true. Progressive number within the sequence.
* LS\_max\_wait. Optional. Timeout in ms.
* LS\_ack. Optional. If false, REQOK is skipped. Default true.
* LS\_outcome. Optional. If false, MSGDONE and MSGFAIL are not sent. Default true.

Example:

```
msg
LS_reqId=7&LS_message=HelloWorld&LS_sequence=chat&LS_msg_prog=1
```

---

### 5. heartbeat

Used to keep the connection alive.

Parameters:

* LS\_session. Optional. Session ID to ping.

Example:

```
heartbeat
LS_session=S1234567890
```

---

### 6. wsok

Used to check WebSocket establishment.

No parameters.

Example:

```
wsok
```

---

## Notifications

Notifications are all server-to-client messages. This includes responses to requests and asynchronous events.

All notifications follow the format:

```
TAG,arg1,arg2,...,argN
```

Arguments are UTF-8 encoded, with reserved characters percent-encoded.

---

### CONOK

`CONOK,sessionId,requestLimit,keepAlive,controlLink`

* sessionId: String. Session identifier.
* requestLimit: Integer. Max allowed request length in bytes.
* keepAlive: Integer. Max inactivity interval before PROBE in ms.
* controlLink: Address or "\*" indicating control channel.

---

### CONERR

`CONERR,errorCode,errorMessage`

* errorCode: Integer. Session creation or binding error code.
* errorMessage: String. Human-readable description.

Error codes (Appendix A — Session errors):
1 — Requested Adapter Set not available.
2 — Requested Adapter not available.
3 — Remote server unavailable.
4 — Connection refused by Metadata Adapter.
5 — User not authorized.
7 — Concurrency conflict.
8 — Session refused because server is full.
9 — Client version not supported.
10 — Connection exceeded configured number of sessions.
11 — Incompatible TLCP version.
20 — Generic error during session creation.
21 — Max number of sessions for this user exceeded.
30 — Max server load exceeded.

---

### END

`END,causeCode,causeMessage`

* causeCode: Integer. Cause of session termination.
* causeMessage: String. Description.

Cause codes (Appendix A — Session errors, extended to END):
31 — Explicit destroy request from client.
32 — Client inactivity timeout.
33 — Server initiated termination (e.g. shutdown).
34 — Rebind failed or expired.
35 — Bandwidth constraints not met.
36 — Illegal operation or malformed request.
37 — Server resources exceeded (e.g. memory).
38 — Internal server error.

---

### REQOK

`REQOK,reqId`

* reqId: String. The request identifier of the successful control or message operation.

---

### REQERR

`REQERR,reqId,errorCode,errorMessage`

* reqId: String. Request identifier.
* errorCode: Integer. Control operation error code.
* errorMessage: String. Description of the error.

Error codes (Appendix B — Control errors):
1 — Invalid subscription mode.
2 — Unknown subscription ID.
3 — Duplicate subscription ID.
4 — Schema not recognized.
5 — Group not recognized.
6 — Selector not recognized.
7 — Data Adapter not recognized.
8 — Mode not compatible with schema/group.
9 — Snapshot not available.
10 — Subscription not permitted by Metadata Adapter.
11 — Frequency not permitted.
12 — Buffer size not permitted.
13 — Bandwidth not permitted.
14 — MPN subscription not permitted.
15 — Invalid MPN device ID.
16 — MPN device not registered.
17 — Message not permitted.
18 — Message sequence not recognized.
19 — Message sequence conflict.
20 — Max number of subscriptions exceeded.
21 — Max frequency exceeded.
22 — Max buffer size exceeded.
23 — Subscription already exists.
24 — Command mode not permitted.
25 — Distinct mode not permitted.
26 — Illegal message format.
27 — Illegal constraint value.
28 — Illegal subscription ID.
29 — Illegal request ID.
30 — Internal control error.

---

### ERROR

`ERROR,errorCode,errorMessage`

* errorCode: Integer. Request format or internal error code.
* errorMessage: String. Description.

Error codes (Appendix A and B, general request errors):
1 — Malformed request syntax.
2 — Missing required parameter.
3 — Illegal value for parameter.
4 — Unsupported operation.
5 — Request too large.
6 — Unknown request type.
7 — Request ID missing or invalid.
8 — Session ID missing or invalid.
9 — Control operation not supported on WS.
10 — Internal server error.

---

### WSOK

`WSOK`

No arguments. Confirms WebSocket establishment.

---

### SUBOK

`SUBOK,subId,numItems,numFields`

* subId: Integer. Subscription ID.
* numItems: Integer. Number of items in group.
* numFields: Integer. Number of fields in schema.

---

### SUBCMD

`SUBCMD,subId,numItems,numFields`

Same arguments as SUBOK. Confirms command-mode subscription.

---

### UNSUB

`UNSUB,subId`

* subId: Integer. Subscription ID that was unsubscribed.

---

### EOS

`EOS,subId`

* subId: Integer. Subscription ID. Marks end of snapshot.

---

### CS

`CS,subId`

* subId: Integer. Subscription ID. Snapshot cleared.

---

### OV

`OV,subId`

* subId: Integer. Subscription ID. Buffer overflow occurred.

---

### CONF

`CONF,subId,newFrequency`

* subId: Integer. Subscription ID.
* newFrequency: Decimal number or string. Applied update frequency.

---

### U

`U,subId,itemId,fieldValues`

* subId: Integer. Subscription ID.
* itemId: Integer. Identifier of item within subscription.
* fieldValues: String. Pipe-separated list of field updates, possibly diff-encoded.

---

### MSGDONE

`MSGDONE,sequence,prog`

* sequence: String. Message sequence name.
* prog: Integer. Progressive number in sequence.

---

### MSGFAIL

`MSGFAIL,sequence,prog`

* sequence: String. Sequence name.
* prog: Integer. Progressive number.

---

### MPNREG

`MPNREG,deviceId`

* deviceId: String. Identifier of registered push device.

---

### MPNBADGE

`MPNBADGE,deviceId`

* deviceId: String. Device identifier whose badge was reset.

---

### MPNOK

`MPNOK,subId`

* subId: Integer. Push notification subscription ID.

---

### MPNCONF

`MPNCONF,subId`

* subId: Integer. Push notification subscription ID.

---

### MPNDIS

`MPNDIS,subId`

* subId: Integer. Push notification subscription ID.

---

### CONS

`CONS,bandwidth`

* bandwidth: Decimal or string. New session bandwidth constraint, or `unlimited`.

---

### SYNC

`SYNC`

No arguments. Time synchronization notification.

---

### CLIENTIP

`CLIENTIP,ipAddress`

* ipAddress: String. The client’s IP address as seen by the server.

---

### SERVNAME

`SERVNAME,serverName`

* serverName: String. The server’s hostname or identifier.

---

### PROG

`PROG,count`

* count: Integer. Progressive sequence number of last sent update. Used for recovery.

---

### NOOP

`NOOP`

No arguments. Keep-alive message.

---

### PROBE

`PROBE`

No arguments. Keep-alive probe.

---

### LOOP

`LOOP`

No arguments. Instructs client to rebind session.
