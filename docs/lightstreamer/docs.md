# 📘 Lightstreamer TLCP WebSocket API Documentation

---

## 1. Transport Setup

**General WS request format:**

```
<request-name>
key1=value1&key2=value2&...&keyN=valueN
```

* Each request is **one UTF-8 text WS frame**.
* First line = request name (`create_session`, `bind_session`, `control`, `msg`, etc.).
* Second line = parameters, `&` separated.
* Multiple control requests may be batched under `control`.
* Reserved characters in parameter values (CR, LF, `&`, `=`, `%`, `+`) must be **percent-encoded**.

---

## 2. Session Lifecycle

### 2.1 `create_session`

**Purpose:** Start a new session with the server and obtain a session ID.

**Parameters:**

* `LS_cid` (**required**) → client identifier. Must always be `mgQkwtwdysogQz2BJ4Ji%20kOj2Bg` for custom clients.
* `LS_user` (**optional**) → username for authentication (interpreted by Metadata Adapter).
* `LS_password` (**optional**) → password for authentication.
* `LS_adapter_set` (**optional**, default: `DEFAULT`) → name of Adapter Set to use.
* `LS_requested_max_bandwidth` (**optional**) → max kbps bandwidth requested (decimal).
* `LS_content_length` (**ignored in WS**) → only used for HTTP transport.
* `LS_supported_diffs` (**optional**) → comma-separated list of diff formats supported by client.
* `LS_polling`, `LS_polling_millis`, `LS_idle_millis` (**ignored in WS**) → polling applies to HTTP.
* `LS_inactivity_millis` (**optional**) → max time (ms) client will stay silent before sending something (heartbeat, etc.).
* `LS_keepalive_millis` (**optional**) → max idle time before server sends keepalive (`PROBE`).
* `LS_send_sync` (**optional**, default: `true`) → whether to receive `SYNC` notifications.
* `LS_reduce_head` (**optional**, default: `false`) → reduce initial server metadata notifications.
* `LS_ttl_millis` (**optional**) → request timeout in ms (`unlimited` = no timeout).

---

### 2.2 `bind_session`

**Purpose:** Reconnect an existing session to a new WS connection, optionally with recovery.

**Parameters:**

* `LS_session` (**optional in WS**) → session ID to rebind. If omitted, last bound session is assumed.
* `LS_recovery_from` (**optional**) → progressive count of last data notification received; used for recovery.
* `LS_inactivity_millis` (**optional**) → max client idle time before server assumes stuck.
* `LS_keepalive_millis` (**optional**) → keepalive interval in ms.
* `LS_send_sync` (**optional**, default: `true`) → whether to receive `SYNC` notifications.
* `LS_reduce_head` (**optional**, default: `false`) → reduce metadata notifications.

---

### 2.3 `control LS_op=constrain`

**Purpose:** Modify session constraints (bandwidth).

**Parameters:**

* `LS_session` (**optional in WS**) → target session ID.
* `LS_reqId` (**required**) → request identifier, unique per connection.
* `LS_op=constrain` (**required**) → operation type.
* `LS_requested_max_bandwidth` (**optional**) → new bandwidth limit in kbps (decimal, `unlimited`).

---

### 2.4 `control LS_op=force_rebind`

**Purpose:** Force server to unbind the session.

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_op=force_rebind` (**required**).
* `LS_polling_millis` (**optional**) → time expected before new binding (ms).
* `LS_close_socket` (**optional**, default: false) → if true, server forcibly closes connection.

---

### 2.5 `control LS_op=destroy`

**Purpose:** Explicitly terminate a session.

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_op=destroy` (**required**).
* `LS_cause_code` (**optional**) → numeric cause code in END notification (only 0 or negative supported).
* `LS_cause_message` (**optional**) → text cause message if cause code supplied.
* `LS_close_socket` (**optional**, default: false) → if true, server forcibly closes connection.

---

## 3. Subscriptions

### 3.1 `control LS_op=add`

**Purpose:** Subscribe to an item group and receive real-time updates.

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_op=add` (**required**).
* `LS_subId` (**required**) → subscription ID, unique progressive integer.
* `LS_data_adapter` (**optional**, default: `DEFAULT`) → Data Adapter name in the Adapter Set.
* `LS_group` (**required**) → item group name (interpreted by Metadata Adapter).
* `LS_schema` (**required**) → field schema name.
* `LS_selector` (**optional**) → selector name, interpreted by Metadata Adapter.
* `LS_mode` (**required**) → one of `RAW`, `MERGE`, `DISTINCT`, `COMMAND`.
* `LS_requested_buffer_size` (**optional**) → buffer size in number of events. (`1` default for MERGE, unlimited for DISTINCT).
* `LS_requested_max_frequency` (**optional**) → max update frequency (`decimal`, `unlimited`, `unfiltered`). Default: unlimited.
* `LS_snapshot` (**optional**) → snapshot request (`true`, `false`, or integer for DISTINCT). Default: false.
* `LS_ack` (**optional, WS only, default: true**) → if false, skip REQOK response.

---

### 3.2 `control LS_op=delete`

**Purpose:** Cancel a subscription.

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_op=delete` (**required**).
* `LS_subId` (**required**) → subscription ID to remove.
* `LS_ack` (**optional, WS only, default: true**) → if false, skip REQOK response.

---

### 3.3 `control LS_op=reconf`

**Purpose:** Change parameters of an existing subscription.

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_op=reconf` (**required**).
* `LS_subId` (**required**) → subscription ID to reconfigure.
* `LS_requested_max_frequency` (**optional**) → new max update frequency (decimal or `unlimited`).

---

## 4. Messaging

### 4.1 `msg`

**Purpose:** Send an upstream message (e.g. chat, command, business event).

**Parameters:**

* `LS_session` (**optional in WS**).
* `LS_reqId` (**required**).
* `LS_sequence` (**required**) → sequence identifier, ensures ordering of related messages.
* `LS_msg_prog` (**required**) → progressive number in the sequence.
* `LS_message` (**required**) → message payload string.

---

## 5. Connection Maintenance

### 5.1 `heartbeat`

**Purpose:** Send a lightweight ping to keep the session alive.

**Parameters:**

* `LS_session` (**required**) → session ID.

---

### 5.2 `wsok`

**Purpose:** Verify WebSocket channel establishment.

**Parameters:**
*None.*

---

## 6. Notifications & Responses Catalog

Each notification has **fixed arguments**:

* **CONOK** → session creation/bind success

  ```
  CONOK,<session-ID>,<request-limit>,<keep-alive>,<control-link>
  ```
* **CONERR** → session creation/bind error

  ```
  CONERR,<error-code>,<error-message>
  ```
* **END** → session forcibly terminated

  ```
  END,<cause-code>,<cause-message>
  ```
* **CONS** → session constraints updated

  ```
  CONS,<bandwidth-limit>
  ```
* **LOOP** → session must be rebound

  ```
  LOOP
  ```

### Subscription notifications

* **SUBOK** → subscription success

  ```
  SUBOK,<subId>,<num-items>,<num-fields>
  ```
* **SUBCMD** → subscription success (COMMAND mode).
* **UNSUB** → subscription terminated

  ```
  UNSUB,<subId>
  ```
* **CONF** → subscription reconfigured

  ```
  CONF,<subId>
  ```
* **U** → real-time update

  ```
  U,<subId>,<item>,<field-values>
  ```

### Message notifications

* **MSGDONE** → message delivered

  ```
  MSGDONE,<sequence>,<msg-prog>
  ```
* **MSGFAIL** → message delivery failed

  ```
  MSGFAIL,<sequence>,<msg-prog>
  ```

### Control responses

* **REQOK** → request succeeded

  ```
  REQOK,<reqId>
  ```
* **REQERR** → request failed

  ```
  REQERR,<reqId>,<error-code>,<error-message>
  ```

### Keepalive / Maintenance

* **PROBE** → keepalive ping from server.
* **WSOK** → WebSocket confirmed.
* **NOOP** → no-op.
