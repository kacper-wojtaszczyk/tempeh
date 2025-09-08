# General headers used everywhere (explicitly)

For each endpoint below the headers are repeated in full. Typical header values and formats:

* `X-IG-API-KEY: <string>` — **required** for every request; your API key from IG.
* Token-based authentication (v1/v2 flow) — **include both**:

    * `CST: <string>` — client session token (returned in headers after v2 login).
    * `X-SECURITY-TOKEN: <string>` — account security token (returned in headers after v2 login).
* OAuth (v3) authentication:

    * `Authorization: Bearer <access_token>` — **required** when using OAuth tokens.
    * `IG-ACCOUNT-ID: <accountId>` — **required** when using OAuth access token to identify the active trading account.
* `Accept: application/json; charset=UTF-8` — recommended on all requests.
* `Content-Type: application/json; charset=UTF-8` — required on requests with JSON bodies.
* `Version: <n>` — **required**. Each endpoint below explicitly states which version to set.

---

## CATEGORY: LOGIN / SESSIONS

### Endpoint: `POST /session` — Version: 3 (OAuth / highest v3)

**Description:** Authenticate using username/password to receive an OAuth access token and refresh token. Use the `access_token` in `Authorization: Bearer <token>` for subsequent OAuth calls. This is the preferred modern login.

**Method & Path:** POST `/session`
**Headers Required (full list):**

* `X-IG-API-KEY: <string>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 3` — required

**Parameters (JSON body):**

1. `identifier` — string — **required** — the username/login id (pattern: alphanumeric, hyphen/underscore allowed; up to \~30 chars).
2. `password` — string — **required** if `encryptedPassword` is absent — plain text password (sensitive).
3. `encryptedPassword` — boolean or string depending on doc variant — **optional** — supply if you perform client-side encryption (server will accept either encrypted or plain based on this flag).

**Example Request (verbose):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 3

Body (JSON):
{
  "identifier": "trader_jane",
  "password": "SuperSecretPassword123!",
  "encryptedPassword": false
}
```

**Example Response (successful, JSON body):**

```json
{
  "accountId": "ABC123",
  "clientId": "CLIENT-0001",
  "lightstreamerEndpoint": "https://push.lightstreamer.ig.com",
  "timezoneOffset": 2,
  "oauthToken": {
    "access_token": "eyJhbGciOiJ...",
    "refresh_token": "def456-refresh",
    "token_type": "Bearer",
    "expires_in": 60,
    "scope": "trading"
  },
  "accounts": [
    {"accountId": "ABC123", "accountName": "Demo CFD Account", "preferred": true}
  ]
}
```

**Return Codes:**

* 200 OK — login succeeded; response includes `oauthToken`.
* 400 Bad Request — `invalid.input`, `invalid.request`.
* 401 Unauthorized — `error.public-api.failure.missing.credentials`, `error.security.oauth-token-invalid`.
* 403 Forbidden — `error.security.api-key-disabled`, `error.security.api-key-restricted`.
* 500 Internal Server Error — `system.error`.

---

### Endpoint: `POST /session` — Version: 2 (CST / X-SECURITY-TOKEN)

**Description:** Legacy login method that returns session tokens in **response headers** (`CST` and `X-SECURITY-TOKEN`). Use these tokens for token-based authentication with other endpoints (v1/v2 endpoints).

**Method & Path:** POST `/session`
**Headers Required (full list):**

* `X-IG-API-KEY: <string>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 2` — required

**Parameters (JSON body):**

1. `identifier` — string — **required** — username.
2. `password` — string — **required** if `encryptedPassword` absent.
3. `encryptedPassword` — boolean or string — **optional**.

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 2

Body:
{
  "identifier": "trader_jane",
  "password": "SuperSecretPassword123!",
  "encryptedPassword": false
}
```

**Example Response (headers + body):**

```
Response Headers (on success):
  CST: AbCdEfGhIjKlMnOpQrSt
  X-SECURITY-TOKEN: XST-0123456789

Response Body:
{
  "clientId": "CLIENT-0001",
  "accounts": [
    {"accountId": "ABC123", "accountName": "Demo CFD Account", "preferred": true}
  ],
  "timezoneOffset": 2
}
```

**Return Codes:**

* 200 OK — login succeeded; `CST` and `X-SECURITY-TOKEN` returned in headers.
* 400 Bad Request — `invalid.input`.
* 401 Unauthorized — `error.public-api.failure.missing.credentials`, `error.security.client-token-invalid`.
* 403 Forbidden — `error.security.api-key-disabled`.
* 500 Internal Server Error — `system.error`.

---

### Endpoint: `GET /session` — Version: 1

**Description:** Retrieve details about the currently authenticated session. If using OAuth access token but you need CST/XST for streaming, use query parameter `fetchSessionTokens=true` to return them (response headers).

**Method & Path:** GET `/session`
**Headers Required (token-based or OAuth):**

* `X-IG-API-KEY: <string>` — required
* If token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required.
* If OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required.
* `Accept: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters (query):**

1. `fetchSessionTokens` — boolean — optional — if `true`, server returns `CST` and `X-SECURITY-TOKEN` (in headers), useful when starting streaming after OAuth login.

**Example Request (OAuth user requesting session tokens):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  Authorization: Bearer eyJhbGciOiJ...
  IG-ACCOUNT-ID: ABC123
  Accept: application/json; charset=UTF-8
  Version: 1

Query:
  fetchSessionTokens = true
```

**Example Response (body + optional headers):**

```
Response Headers (if fetchSessionTokens true):
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321

Body:
{
  "accountId": "ABC123",
  "clientId": "CLIENT-0001",
  "accounts": [{ "accountId": "ABC123", "accountName": "Demo CFD", "preferred": true }],
  "timezoneOffset": 2
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized — `error.security.client-token-invalid`, `error.security.oauth-token-invalid`
* 403 Forbidden — `error.security.api-key-restricted`
* 500 Internal Server Error

---

### Endpoint: `DELETE /session` — Version: 1

**Description:** Explicitly close the current session. Invalidates tokens returned by login.

**Method & Path:** DELETE `/session`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* If token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required
* If OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters:** none

**Example Request (token-based):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 1
```

**Example Response:**

```json
{
  "status": "SUCCESS"
}
```

**Return Codes:**

* 200 OK — session closed.
* 401 Unauthorized — invalid tokens.
* 500 Internal Server Error.

---

### Endpoint: `GET /session/encryptionKey` — Version: 1

**Description:** Retrieve a one-time encryption key and timestamp used to encrypt client passwords before sending (where server requires encrypted credentials).

**Method & Path:** GET `/session/encryptionKey`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters:** none

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  Accept: application/json; charset=UTF-8
  Version: 1
```

**Example Response:**

```json
{
  "encryptionKey": "BASE64_ENCODED_KEY_STRING",
  "timeStamp": 1700000000000
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized — `error.security.api-key-missing`
* 500 Internal Server Error

---

### Endpoint: `POST /session/refresh-token` — Version: 1

**Description:** Exchange a refresh token for a new access token (OAuth flow).

**Method & Path:** POST `/session/refresh-token`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters (JSON body):**

1. `refresh_token` — string — **required** — the refresh token returned by `POST /session` v3.

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 1

Body:
{
  "refresh_token": "def456-refresh"
}
```

**Example Response:**

```json
{
  "access_token": "newAccessToken123",
  "refresh_token": "newRefreshToken456",
  "expires_in": 60,
  "token_type": "Bearer",
  "scope": "trading"
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.refresh.token`
* 401 Unauthorized — `error.security.oauth-token-invalid`
* 500 Internal Server Error

---

## CATEGORY: DEALING (positions, confirms, working orders, repeat dealing window)

> All dealing endpoints require `X-IG-API-KEY` and one of the authentication pairs shown below. Each endpoint below specifies **full header lists** — do not substitute "same as above".

---

### Endpoint: `GET /confirms/{dealReference}` — Version: 1

**Description:** Fetch confirmation details for a deal identified by `dealReference`. Use when streaming confirmation not received.

**Method & Path:** GET `/confirms/{dealReference}`
**Headers Required (explicit):**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters:**

1. `dealReference` — string — **required** — path parameter identifying the deal.

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 1

Path:
  dealReference = "D8VEMD26FGQTYPH"
```

**Example Response:**

```json
{
  "dealReference": "D8VEMD26FGQTYPH",
  "dealId": "DIAAAABBBCCC123",
  "epic": "CS.D.EURUSD.CFD.IP",
  "dealStatus": "ACCEPTED",
  "direction": "BUY",
  "size": 1.0,
  "level": 1.10050,
  "timeStamp": "2025-09-07T12:00:00Z"
}
```

**Return Codes:**

* 200 OK — confirmation returned.
* 400 Bad Request — `invalid.input`.
* 401 Unauthorized — `error.security.client-token-invalid`, `error.security.oauth-token-invalid`.
* 404 Not Found — `error.public-api.deal-not-found`.
* 500 Internal Server Error.

---

### Endpoint: `GET /positions` — Version: 2

**Description:** Return a list of all open positions for the authenticated account.

**Method & Path:** GET `/positions`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 2` — required

**Parameters:**

* None (no query parameters for this call).

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 2
```

**Example Response:**

```json
{
  "positions": [
    {
      "position": {
        "dealId": "DIAAAABBBCCC123",
        "dealReference": "REF-0001",
        "direction": "BUY",
        "size": 1.0,
        "level": 1.1005,
        "stopLevel": 1.0950,
        "limitLevel": 1.1050,
        "currency": "USD",
        "createdDateUTC": "2025-09-06T15:00:00Z"
      },
      "market": {
        "epic": "CS.D.EURUSD.CFD.IP",
        "instrumentName": "EUR/USD",
        "bid": 1.1003,
        "offer": 1.1005,
        "marketStatus": "TRADEABLE",
        "streamingPricesAvailable": true
      }
    }
  ]
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized — invalid tokens
* 500 Internal Server Error

---

### Endpoint: `GET /positions/{dealId}` — Version: 2

**Description:** Get details for a single open position identified by `dealId`.

**Method & Path:** GET `/positions/{dealId}`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 2` — required

**Parameters:**

1. `dealId` — string — **required** — the position deal identifier (path).

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 2

Path:
  dealId = "DIAAAABBBCCC123"
```

**Example Response:**

```json
{
  "position": {
    "dealId": "DIAAAABBBCCC123",
    "dealReference": "REF-0001",
    "direction": "BUY",
    "size": 1.0,
    "level": 1.1005,
    "stopLevel": 1.0950,
    "limitLevel": 1.1050,
    "trailingStop": false,
    "currency": "USD",
    "createdDateUTC": "2025-09-06T15:00:00Z"
  },
  "market": {
    "epic": "CS.D.EURUSD.CFD.IP",
    "instrumentName": "EUR/USD",
    "bid": 1.1003,
    "offer": 1.1005,
    "marketStatus": "TRADEABLE"
  }
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.dealId`
* 401 Unauthorized
* 404 Not Found — `deal.not.found`
* 500 Internal Server Error

---

### Endpoint: `POST /positions/otc` — Version: 2

**Description:** Create (open) an OTC position (market/limit/stop order). Many fields with constraints — both stop/limit and trailing options are available; the API enforces mutually exclusive fields and allowed combos.

**Method & Path:** POST `/positions/otc`
**Headers Required (explicit):**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 2` — required

**Parameters (JSON Body) — full verbose list (each item: name — type — required? — constraints):**

1. `epic` — string — **required** — market epic (e.g., `CS.D.EURUSD.CFD.IP`).
2. `expiry` — string — **required** — expiry identifier (e.g., `-` for non-expiring or `DD-MMM-YY` or `DFB` depending on market).
3. `direction` — enum (`BUY` | `SELL`) — **required**.
4. `size` — decimal/number — **required** — trade size; precision may be up to 12 decimal places depending on instrument.
5. `orderType` — enum (`MARKET` | `LIMIT` | `STOP` | sometimes `QUOTE`) — **required**.
6. `level` — decimal — optional — price level for `LIMIT`/`STOP` orders (if `orderType` requires it).
7. `quoteId` — string — optional — required when order is placed against a specific quote (e.g., `QUOTE`).
8. `currencyCode` — string (3-letter) — optional (e.g., `USD`, `GBP`); default depends on account/instrument.
9. `forceOpen` — boolean — optional — whether to force open an additional position rather than net-off.
10. `guaranteedStop` — boolean — optional — if `true`, indicates guaranteed stop requested (may cause premium).
11. `stopLevel` — decimal — optional — explicit stop level. Must not be set when `trailingStop` is true (mutually exclusive).
12. `stopDistance` — decimal — optional — alternative to `stopLevel` (mutually exclusive).
13. `trailingStop` — boolean — optional — if `true`, requires `trailingStopIncrement` and `stopDistance`.
14. `trailingStopIncrement` — decimal — optional — required if `trailingStop == true`.
15. `limitLevel` — decimal — optional — explicit limit level (mutually exclusive with `limitDistance`).
16. `limitDistance` — decimal — optional — alternative to `limitLevel`.
17. `timeInForce` — enum — optional — (e.g., `EXECUTE_AND_ELIMINATE`, `FILL_OR_KILL`) — depends on order type.
18. `guaranteedStopLevel` — decimal — optional — if guaranteed stop requires specifying premium?
19. `dealReference` — string — optional — client-supplied reference id (alphanumeric/hyphen/underscore max \~30 chars).

**Important constraints & business rules (enforced by API):**

* If `orderType == LIMIT`, supply `level` or `limitLevel`/`limitDistance` as required; do **not** supply `quoteId`.
* If `orderType == MARKET`, do **not** supply `level` or `quoteId`.
* If `trailingStop == true`, **do not** set `stopLevel`; set `stopDistance` and `trailingStopIncrement`.
* Only one of `limitLevel` or `limitDistance` may be set.
* Only one of `stopLevel` or `stopDistance` may be set.
* `guaranteedStop` and `trailingStop` cannot both be true.

**Example Request (verbose; **all optional fields included where applicable** — note some are null to illustrate optionality):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 2

Body:
{
  "epic": "CS.D.EURUSD.CFD.IP",
  "expiry": "-",
  "direction": "BUY",
  "size": 1.0,
  "orderType": "MARKET",
  "level": null,
  "quoteId": "QUOTE-20250907-001",
  "currencyCode": "USD",
  "forceOpen": true,
  "guaranteedStop": false,
  "stopLevel": 1.0950,
  "stopDistance": null,
  "trailingStop": false,
  "trailingStopIncrement": null,
  "limitLevel": 1.1050,
  "limitDistance": null,
  "timeInForce": "EXECUTE_AND_ELIMINATE",
  "dealReference": "client-ref-0001"
}
```

**Example Response (success):**

```json
{
  "dealReference": "D8VEMD26FGQTYPH",
  "dealStatus": "ACCEPTED"
}
```

**Return Codes:**

* 200 OK — `deal accepted` (dealReference in body).
* 400 Bad Request — `invalid.input`, `invalid.orderType`, `invalid.size`, `invalid.level`.
* 401 Unauthorized — `error.security.client-token-invalid`, `error.security.account-token-invalid`.
* 403 Forbidden — `insufficient.funds`, `account.disabled`, `error.public-api.exceeded-account-trading-allowance`.
* 404 Not Found — `epic.not.found`.
* 409 Conflict — `quote.expired` (if using quotes).
* 500 Internal Server Error — `system.error`.

---

### Endpoint: `PUT /positions/otc/{dealId}` — Version: 2

**Description:** Amend an existing OTC position (modify stops, limits, trailing stops, size if allowed).

**Method & Path:** PUT `/positions/otc/{dealId}`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 2` — required

**Parameters (path + body):**

1. `dealId` — string — **required** — path param identifying the open position to amend.
2. Body parameters (all optional; list with constraints):

    * `limitLevel` — decimal — optional — set only if adjusting limit; mutually exclusive with `limitDistance`.
    * `limitDistance` — decimal — optional — alternative to `limitLevel`.
    * `stopLevel` — decimal — optional — set only if adjusting stop; mutually exclusive with `stopDistance`.
    * `stopDistance` — decimal — optional — alternative to `stopLevel`.
    * `guaranteedStop` — boolean — optional.
    * `trailingStop` — boolean — optional.
    * `trailingStopIncrement` — decimal — optional — required if `trailingStop == true`.
    * `size` — decimal — optional — change position size only if supported by account/instrument.
    * `dealReference` — string — optional — client supplied reference for this update.

**Example Request (verbose):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 2

Path:
  dealId = "DIAAAABBBCCC123"

Body:
{
  "limitLevel": 1.1100,
  "limitDistance": null,
  "stopLevel": null,
  "stopDistance": 50,
  "guaranteedStop": false,
  "trailingStop": true,
  "trailingStopIncrement": 0.0005,
  "size": 1,
  "dealReference": "amend-0001"
}
```

**Example Response:**

```json
{
  "dealReference": "AMEND-REF-0001",
  "dealStatus": "AMENDED"
}
```

**Return Codes:**

* 200 OK — amend accepted (dealReference returned).
* 400 Bad Request — `invalid.input`, `invalid.dealId`.
* 401 Unauthorized.
* 404 Not Found — `deal.not.found`.
* 409 Conflict — `amend.not.allowed`.
* 500 Internal Server Error.

---

### Endpoint: `DELETE /positions/otc` — Version: 1 (Close by epic/criteria)

**Description:** Close one or multiple OTC positions by specifying `dealId` values or by specifying `epic`+`expiry`. Accepts an array or selection criteria in body.

**Method & Path:** DELETE `/positions/otc`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters (JSON body; all fields listed):**

* Option A: Close by `dealId` list:

    1. `deals` — array of objects — **optional** (if you supply this the API will close specified deals). Each object:

        * `dealId` — string — **required** inside each object.
        * `direction` — `BUY` | `SELL` — optional; inferred if not provided.
        * `size` — decimal — optional; specify partial close sizes.
        * `level` — decimal — optional; required for `LIMIT`/`QUOTE` types as necessary.
        * `orderType` — enum — optional.
          Example close-by-deal body:

  ```json
  {
    "deals": [
      {"dealId": "DIAAAABBBCCC123", "direction": "SELL", "size": 1}
    ],
    "epic": null,
    "expiry": null
  }
  ```
* Option B: Close by `epic` and `expiry`:

    * `epic` — string — optional — epic of market to close.
    * `expiry` — string — optional — expiry identifier; required if `epic` provided.
    * `direction`, `size`, `orderType`, `level` — optional (similar to above for closing criteria).

**Example Request (close by dealId):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 1

Body:
{
  "deals": [
    {
      "dealId": "DIAAAABBBCCC123",
      "direction": "SELL",
      "size": 1,
      "orderType": "MARKET",
      "level": null
    }
  ],
  "epic": null,
  "expiry": null
}
```

**Example Response (success):**

```json
{
  "dealReference": "CLOSE-REF-0001",
  "dealStatus": "ACCEPTED"
}
```

**Return Codes:**

* 200 OK — close accepted.
* 400 Bad Request — `invalid.input`, `invalid.dealId`.
* 401 Unauthorized.
* 403 Forbidden — `insufficient.funds` (if closing triggers margin events), `account.disabled`.
* 404 Not Found — `deal.not.found`.
* 500 Internal Server Error.

---

### Endpoint: `DELETE /positions/{dealId}` — Version: 1 (Close single position via path)

**Description:** Close a single position specified by `dealId`. Simpler alternative to `DELETE /positions/otc`.

**Method & Path:** DELETE `/positions/{dealId}`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Content-Type: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters (path + JSON body):**

1. `dealId` — string — **required** — path parameter.
2. Body:

    * `direction` — `BUY`|`SELL` — **required** — which side to close.
    * `size` — decimal — required.
    * `orderType` — enum — required.
    * `level` — decimal — optional.

**Example Request:**

```
Headers: ...
Path:
  dealId = "DIAAAABBBCCC123"
Body:
{
  "direction": "SELL",
  "size": 1.0,
  "orderType": "MARKET",
  "level": null
}
```

**Example Response:**

```json
{
  "dealReference": "CLOSE-REF-123"
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.dealId`, `invalid.input`
* 401 Unauthorized
* 403 Forbidden
* 404 Not Found — `deal.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /working-orders` — Version: 2

**Description:** Retrieve all working (pending) orders for the account.

**Method & Path:** GET `/working-orders`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters:** None

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 2
```

**Example Response:**

```json
{
  "workingOrders": [
    {
      "marketData": {
        "epic": "CS.D.EURUSD.CFD.IP",
        "instrumentName": "EUR/USD",
        "bid": 1.1003,
        "offer": 1.1005,
        "marketStatus": "TRADEABLE"
      },
      "workingOrderData": {
        "dealId": "W1234",
        "direction": "BUY",
        "orderType": "LIMIT",
        "orderLevel": 1.0750,
        "orderSize": 1.0,
        "timeInForce": "GOOD_TILL_CANCELLED",
        "goodTillDate": null,
        "createdDateUTC": "2025-09-01T10:00:00Z"
      }
    }
  ]
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `POST /working-orders/otc` — Version: 2

**Description:** Create a new working (pending) order (limit/stop) to be executed when match occurs.

**Method & Path:** POST `/working-orders/otc`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8`
* `Version: 2`

**Parameters (JSON body) — complete list:**

1. `epic` — string — **required**.
2. `expiry` — string — **required** (or `-`).
3. `direction` — enum (`BUY`|`SELL`) — **required**.
4. `size` — decimal — **required**.
5. `orderType` — enum (`LIMIT`|`STOP`) — **required**.
6. `level` — decimal — **required** (level for the working order).
7. `limitLevel` / `limitDistance` — decimal — optional (mutually exclusive).
8. `stopLevel` / `stopDistance` — decimal — optional (mutually exclusive).
9. `guaranteedStop` — boolean — optional.
10. `timeInForce` — enum — optional (`GOOD_TILL_CANCELLED` | `GOOD_TILL_DATE`).
11. `goodTillDate` — string — optional; if `timeInForce` is `GOOD_TILL_DATE`, supply ISO datetime.
12. `currencyCode` — string — optional (3-letter).
13. `forceOpen` — boolean — optional.
14. `dealReference` — string — optional.

**Example Request (verbose):**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Content-Type: application/json; charset=UTF-8
  Version: 2

Body:
{
  "epic": "CS.D.GBPUSD.CFD.IP",
  "expiry": "-",
  "direction": "SELL",
  "size": 2.0,
  "orderType": "LIMIT",
  "level": 1.2500,
  "limitLevel": 1.2400,
  "limitDistance": null,
  "stopLevel": 1.2600,
  "stopDistance": null,
  "guaranteedStop": false,
  "timeInForce": "GOOD_TILL_DATE",
  "goodTillDate": "2025-12-31T23:59:59Z",
  "currencyCode": "USD",
  "forceOpen": true,
  "dealReference": "working-client-001"
}
```

**Example Response:**

```json
{
  "dealReference": "W-REF-98765",
  "dealStatus": "PENDING"
}
```

**Return Codes:**

* 200 OK — working order accepted.
* 400 Bad Request — `invalid.input`, `invalid.orderType`, `invalid.level`.
* 401 Unauthorized.
* 403 Forbidden — `order.not.allowed`.
* 404 Not Found — `epic.not.found`.
* 500 Internal Server Error.

---

### Endpoint: `PUT /working-orders/otc/{dealId}` — Version: 2

**Description:** Amend an existing working order (change level, good till date, etc.).

**Method & Path:** PUT `/working-orders/otc/{dealId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8`
* `Version: 2`

**Parameters:**

1. `dealId` — string — **required** — path param identifying the working order.
2. Body parameters (all optional; list):

    * `level` — decimal — optional — change order level.
    * `limitLevel` / `limitDistance` — optional.
    * `stopLevel` / `stopDistance` — optional.
    * `goodTillDate` — string — optional.
    * `timeInForce` — enum — optional.
    * `dealReference` — string — optional.

**Example Request:**

```
Headers: ...
Path:
  dealId = "W-REF-98765"

Body:
{
  "level": 1.2450,
  "limitLevel": null,
  "limitDistance": null,
  "stopLevel": 1.2550,
  "stopDistance": null,
  "goodTillDate": "2025-12-31T23:59:59Z",
  "timeInForce": "GOOD_TILL_DATE",
  "dealReference": "modify-working-001"
}
```

**Example Response:**

```json
{
  "dealReference": "W-REF-98765",
  "dealStatus": "AMENDED"
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.input`
* 401 Unauthorized
* 404 Not Found — `workingorder.not.found`
* 409 Conflict — `amend.not.allowed`
* 500 Internal Server Error

---

### Endpoint: `DELETE /working-orders/otc/{dealId}` — Version: 1

**Description:** Cancel/delete a working order by `dealId`.

**Method & Path:** DELETE `/working-orders/otc/{dealId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8` — (body often empty, but content-type allowed)
* `Version: 1`

**Parameters:**

1. `dealId` — string — **required** — path param.

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGh...
  X-SECURITY-TOKEN: XST-987...
  Accept: application/json; charset=UTF-8
  Version: 1

Path:
  dealId = "W-REF-98765"
```

**Example Response:**

```json
{"dealReference": "W-REF-98765", "dealStatus": "DELETED"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.dealId`
* 401 Unauthorized
* 404 Not Found — `workingorder.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /repeat-dealing-window` — Version: 1

**Description:** Get current repeat-dealing window settings/availability for the account.

**Method & Path:** GET `/repeat-dealing-window`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:** none

**Example Response:**

```json
{
  "repeatDealingWindowEnabled": true,
  "windowDurationSeconds": 30,
  "nextWindowOpensAtUTC": "2025-09-07T12:00:30Z"
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized
* 500 Internal Server Error

---

## CATEGORY: MARKETS (market navigation, market details, search, historical prices)

> Each markets endpoint includes the full explicit header lists.

---

### Endpoint: `GET /market-navigation` — Version: 1

**Description:** Get top-level market navigation (nodes/categories).

**Method & Path:** GET `/market-navigation`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:** none

**Example Response:**

```json
{
  "nodes": [
    { "id": "FX", "name": "Forex", "hasChildren": true },
    { "id": "IND", "name": "Indices", "hasChildren": true }
  ]
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /market-navigation/{nodeId}` — Version: 1

**Description:** Retrieve the child nodes and markets for the given node.

**Method & Path:** GET `/market-navigation/{nodeId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:**

1. `nodeId` — string — **required** — market navigation node id (use `root` for top-level).

**Example Request:**

```
Path:
  nodeId = "root"
```

**Example Response:**

```json
{
  "nodes": [{ "id": "FX", "name": "Forex", "hasChildren": true }],
  "markets": [
    { "epic": "CS.D.EURUSD.CFD.IP", "instrumentName": "EUR/USD", "marketStatus": "TRADEABLE" }
  ]
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.nodeId`
* 401 Unauthorized
* 404 Not Found — `node.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /markets` — Version: 2

**Description:** Retrieve details for multiple markets (supply up to a limit of epics in a comma-separated `epics` query) or use `filter` param to reduce payload.

**Method & Path:** GET `/markets`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters (query):**

1. `epics` — string — **required** — comma-separated list of epics (max \~50 per request).
2. `filter` — string — optional — `ALL` (default) or `SNAPSHOT_ONLY` (return minimal snapshot fields).

**Example Request:**

```
Headers: ...
Query:
  epics = "CS.D.EURUSD.CFD.IP,CS.D.GBPUSD.CFD.IP"
  filter = "SNAPSHOT_ONLY"
```

**Example Response:**

```json
{
  "markets": [
    {
      "instrument": { "epic": "CS.D.EURUSD.CFD.IP", "name": "EUR/USD", "lotSize": 100000 },
      "snapshot": { "bid": 1.1003, "offer": 1.1005, "marketStatus": "TRADEABLE" },
      "dealingRules": { "minDealSize": 0.01 }
    }
  ]
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.epics`, `too.many.epics`
* 401 Unauthorized
* 404 Not Found — `epic.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /markets/{epic}` — Version: 4 (highest)

**Description:** Get the full, detailed metadata, dealing rules, and snapshot for a single market epic.

**Method & Path:** GET `/markets/{epic}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 4`

**Parameters:**

1. `epic` — string — **required** — market epic identifier.

**Example Request:**

```
Headers: ...
Path:
  epic = "CS.D.EURUSD.CFD.IP"
```

**Example Response (abridged but detailed):**

```json
{
  "dealingRules": {
    "minDealSize": { "unit": "UNITS", "value": 0.01 },
    "minNormalStopOrLimitDistance": { "unit": "POINTS", "value": 10 },
    "maxStopOrLimitDistance": { "unit": "POINTS", "value": 1000 }
  },
  "instrument": {
    "epic": "CS.D.EURUSD.CFD.IP",
    "name": "EUR/USD",
    "type": "CURRENCIES",
    "lotSize": 100000,
    "valueOfOnePip": "0.0001",
    "streamingPricesAvailable": true,
    "limitAllowed": true,
    "stopAllowed": true,
    "expiry": "-"
  },
  "snapshot": {
    "bid": 1.1003,
    "offer": 1.1005,
    "high": 1.1100,
    "low": 1.0900,
    "netChange": 0.0012,
    "percentageChange": 0.11,
    "updateTimestampUTC": 1700000000,
    "marketStatus": "TRADEABLE"
  }
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.epic`
* 401 Unauthorized
* 403 Forbidden — `endpoint.unavailable.for.api-key`
* 404 Not Found — `epic.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /markets?searchTerm={searchTerm}` — Version: 1

**Description:** Free-text search for markets by name, ticker, or partial epic.

**Method & Path:** GET `/markets` with query `searchTerm`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters (query):**

1. `searchTerm` — string — **required** — partial/full name or epic.

**Example Request:**

```
Query:
  searchTerm = "EURUSD"
```

**Example Response:**

```json
{
  "markets": [
    { "epic": "CS.D.EURUSD.CFD.IP", "instrumentName": "EUR/USD", "marketStatus": "TRADEABLE" }
  ]
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.searchTerm`
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /prices/{epic}` — Version: 3 (historical prices default call)

**Description:** Retrieve recent historical prices for a market; defaults to minute resolution for last 10 points if no query params set.

**Method & Path:** GET `/prices/{epic}` or GET `/prices/{epic}?resolution={}&max={}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 3`

**Parameters (query & path — full list):**

1. `epic` — string — **required** — path parameter.
2. `resolution` — enum — optional — e.g., `SECOND`, `MINUTE`, `MINUTE_5`, `MINUTE_15`, `HOUR`, `DAY`, `WEEK`, `MONTH`. Default `MINUTE`.
3. `from` or `startDate` — string — optional — ISO8601 timestamp / date (e.g., `2024-01-01T00:00:00`).
4. `to` or `endDate` — string — optional — ISO8601 timestamp; if `from` present `to` must be present.
5. `max` — integer — optional — limit number of points to return (default 10 if no date range).
6. `pageNumber` — integer — optional (paging), default 1.
7. `pageSize` — integer — optional (paging), default 20.

**Example Request (explicit with date range):**

```
Headers: ...
Path:
  epic = "CS.D.EURUSD.CFD.IP"
Query:
  resolution = "MINUTE"
  from = "2025-08-01T00:00:00"
  to = "2025-08-02T00:00:00"
  pageNumber = 1
  pageSize = 100
```

**Example Response:**

```json
{
  "prices": [
    {
      "snapshotTimeUTC": "2025-08-01T00:01:00",
      "openPrice": {"bid": 1.1000, "ask": 1.1002},
      "closePrice": {"bid": 1.1005, "ask": 1.1007},
      "highPrice": {"bid": 1.1010, "ask": 1.1012},
      "lowPrice": {"bid": 1.0995, "ask": 1.0997},
      "lastTradedVolume": 200
    }
  ],
  "metadata": { "pageNumber": 1, "pageSize": 100, "totalPages": 5 },
  "allowance": { "remainingAllowance": 1000, "allowanceExpiry": 3599 }
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.date.range`, `invalid.resolution`
* 401 Unauthorized
* 404 Not Found — `epic.not.found`
* 500 Internal Server Error

---

### Endpoint: `GET /prices/{epic}/{resolution}/{numPoints}` — Version: 2 (alternative)

**Description:** Return last `numPoints` price points at `resolution` for `epic`. Path-based historical retrieval.

**Method & Path:** GET `/prices/{epic}/{resolution}/{numPoints}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters (path & optional query):**

1. `epic` — string — required.
2. `resolution` — enum — required.
3. `numPoints` — integer — required — number of points to return.
4. Optional `pageNumber` / `pageSize`.

**Example Request:**

```
Path:
  epic = "CS.D.EURUSD.CFD.IP"
  resolution = "MINUTE"
  numPoints = 60
```

**Example Response:**
Same schema as `GET /prices` — returns `prices` array with `openPrice`, `closePrice`, `highPrice`, `lowPrice`.

**Return Codes:**

* 200 OK
* 400 Bad Request
* 401 Unauthorized
* 404 Not Found
* 500 Internal Server Error

---

## CATEGORY: WATCHLISTS (full CRUD)

> All Watchlist endpoints use Version: 1 (explicit below). Include full headers for OAuth & token-based.

---

### Endpoint: `GET /watchlists` — Version: 1

**Description:** Return all watchlists for the authenticated account.

**Method & Path:** GET `/watchlists`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:** none

**Example Response:**

```json
{
  "watchlists": [
    {"watchlistId": "WL123", "name": "My FX", "editable": true, "deletable": true}
  ]
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `POST /watchlists` — Version: 1

**Description:** Create a new watchlist.

**Method & Path:** POST `/watchlists`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8`
* `Version: 1`

**Parameters (JSON body):**

1. `name` — string — **required** — watchlist name (UTF-8; docs allow typical name characters).
2. `epics` — array\[string] — optional — initial list of epic identifiers to include.

**Example Request (verbose):**

```
Body:
{
  "name": "My New Watchlist",
  "epics": ["CS.D.EURUSD.CFD.IP", "IX.D.FTSE.CFD.IP"]
}
```

**Example Response:**

```json
{
  "status": "SUCCESS",
  "watchlistId": "WL456"
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.input`
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /watchlists/{watchlistId}` — Version: 1

**Description:** Get a specific watchlist and the markets it contains.

**Method & Path:** GET `/watchlists/{watchlistId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:**

1. `watchlistId` — string — **required** — path param.

**Example Response:**

```json
{
  "watchlist": {
    "watchlistId": "WL456",
    "name": "My New Watchlist",
    "markets": [
      {"epic": "CS.D.EURUSD.CFD.IP", "instrumentName": "EUR/USD", "bid": 1.1003},
      {"epic": "IX.D.FTSE.CFD.IP", "instrumentName": "FTSE 100", "bid": 7500.0}
    ]
  }
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.watchlistId`
* 401 Unauthorized
* 404 Not Found — `watchlist.not.found`
* 500 Internal Server Error

---

### Endpoint: `PUT /watchlists/{watchlistId}` — Version: 1

**Description:** Rename a watchlist (or update metadata). This variant may be used to set `name`.

**Method & Path:** PUT `/watchlists/{watchlistId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8`
* `Version: 1`

**Parameters (JSON body):**

1. `name` — string — **required** — new name for watchlist.

**Example Request:**

```
Path:
  watchlistId = "WL456"

Body:
{
  "name": "Renamed Watchlist"
}
```

**Example Response:**

```json
{"status": "SUCCESS"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.input`
* 401 Unauthorized
* 404 Not Found — `watchlist.not.found`
* 500 Internal Server Error

---

### Endpoint: `PUT /watchlists/{watchlistId}/{epic}` — Version: 1

**Description:** Add a single epic to an existing watchlist via a PUT (alternative to POST adding arrays). Some docs use PUT to add a single epic.

**Method & Path:** PUT `/watchlists/{watchlistId}/{epic}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8` — although body may be empty
* `Version: 1`

**Parameters:**

1. `watchlistId` — string — **required** — path param.
2. `epic` — string — **required** — path param to add.

**Example Request:**

```
Path:
  watchlistId = "WL456"
  epic = "CS.D.GBPUSD.CFD.IP"
```

**Example Response:**

```json
{"status": "SUCCESS"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request
* 401 Unauthorized
* 404 Not Found
* 500 Internal Server Error

---

### Endpoint: `POST /watchlists/{watchlistId}` — Version: 1

**Description:** Add multiple epics to a watchlist (supply array of epics).

**Method & Path:** POST `/watchlists/{watchlistId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Content-Type: application/json; charset=UTF-8`
* `Version: 1`

**Parameters (JSON body):**

1. `epics` — array\[string] — **required** — list of epic strings to add.

**Example Request:**

```
Path:
  watchlistId = "WL456"

Body:
{
  "epics": ["CS.D.GBPUSD.CFD.IP", "CS.D.AUDUSD.CFD.IP"]
}
```

**Example Response:**

```json
{"status": "SUCCESS"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.input`
* 401 Unauthorized
* 404 Not Found — `watchlist.not.found`
* 500 Internal Server Error

---

### Endpoint: `DELETE /watchlists/{watchlistId}` — Version: 1

**Description:** Delete a user-created watchlist.

**Method & Path:** DELETE `/watchlists/{watchlistId}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:**

1. `watchlistId` — string — **required**.

**Example Response:**

```json
{"status": "SUCCESS"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request
* 401 Unauthorized
* 404 Not Found — `watchlist.not.found`
* 500 Internal Server Error

---

### Endpoint: `DELETE /watchlists/{watchlistId}/{epic}` — Version: 1

**Description:** Remove a market epic from a watchlist.

**Method & Path:** DELETE `/watchlists/{watchlistId}/{epic}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters:**

1. `watchlistId` — string — **required** — path.
2. `epic` — string — **required** — path.

**Example Response:**

```json
{"status": "SUCCESS"}
```

**Return Codes:**

* 200 OK
* 400 Bad Request
* 401 Unauthorized
* 404 Not Found — `watchlist.not.found` or `epic.not.in.watchlist`
* 500 Internal Server Error

---

## CATEGORY: ACCOUNTS

> These endpoints return account information, transactional history, deposit/withdrawal records, activity logs and support date ranges and filters.

---

### Endpoint: `GET /accounts` — Version: 1

**Description:** Returns a list of all accounts associated with the logged-in client, including account details, balances, status, and transfer capabilities.

**Method & Path:** GET `/accounts`
**Headers Required:**

* `X-IG-API-KEY: <string>` — required
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>` — required; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>` — required
* `Accept: application/json; charset=UTF-8` — required
* `Version: 1` — required

**Parameters:** None

**Example Request:**

```
Headers:
  X-IG-API-KEY: YOUR_API_KEY
  CST: AbCdEfGhIjKl...
  X-SECURITY-TOKEN: XST-987654321
  Accept: application/json; charset=UTF-8
  Version: 1
```

**Example Response:**

```json
{
  "accounts": [
    {
      "accountId": "ABC123",
      "accountName": "Demo CFD Account",
      "accountAlias": "My Trading Account",
      "accountType": "CFD",
      "preferred": true,
      "currency": "USD",
      "canTransferFrom": true,
      "canTransferTo": true,
      "status": "ENABLED",
      "balance": {
        "balance": 10000.00,
        "available": 9500.00,
        "deposit": 500.00,
        "profitLoss": 150.00
      }
    },
    {
      "accountId": "DEF456",
      "accountName": "Live Spread Bet Account",
      "accountAlias": "SB Account",
      "accountType": "SPREADBET",
      "preferred": false,
      "currency": "GBP",
      "canTransferFrom": true,
      "canTransferTo": false,
      "status": "ENABLED",
      "balance": {
        "balance": 5000.00,
        "available": 4800.00,
        "deposit": 200.00,
        "profitLoss": -50.00
      }
    }
  ]
}
```

**Response Fields (detailed breakdown):**

* `accounts` — array — list of account objects, each containing:
  * `accountId` — string — unique account identifier (e.g., "ABC123")
  * `accountName` — string — display name of the account
  * `accountAlias` — string — user-defined alias for the account
  * `accountType` — enum — account type: `CFD` | `PHYSICAL` | `SPREADBET`
  * `preferred` — boolean — true if this is the default login account
  * `currency` — string — account base currency (e.g., "USD", "GBP", "EUR")
  * `canTransferFrom` — boolean — true if funds can be transferred from this account
  * `canTransferTo` — boolean — true if funds can be transferred to this account
  * `status` — enum — account status: `ENABLED` | `DISABLED` | `SUSPENDED_FROM_DEALING`
  * `balance` — object — account balance information:
    * `balance` — number — total balance of funds in the account
    * `available` — number — amount available for trading (balance minus margin requirements)
    * `deposit` — number — minimum deposit amount required for margins
    * `profitLoss` — number — current unrealized profit and loss amount

**Return Codes:**

* 200 OK — accounts list returned successfully
* 400 Bad Request — `error.public-api.failure.encryption.required`, `error.request.invalid.date-range`, `error.security.api-key-missing`, `invalid.input`
* 401 Unauthorized — `error.public-api.failure.kyc.required`, `error.public-api.failure.missing.credentials`, `error.public-api.failure.pending.agreements.required`, `error.public-api.failure.preferred.account.disabled`, `error.public-api.failure.preferred.account.not.set`, `error.security.account-token-invalid`, `error.security.account-token-missing`, `error.security.client-token-invalid`, `error.security.client-token-missing`, `error.security.oauth-token-invalid`
* 403 Forbidden — `endpoint.unavailable.for.api-key`, `error.public-api.exceeded-account-allowance`, `error.public-api.exceeded-account-historical-data-allowance`, `error.public-api.exceeded-account-trading-allowance`, `error.public-api.exceeded-api-key-allowance`, `error.public-api.failure.stockbroking-not-supported`, `error.security.api-key-disabled`, `error.security.api-key-invalid`, `error.security.api-key-restricted`, `error.security.api-key-revoked`
* 404 Not Found — `invalid.url`
* 500 Internal Server Error — `system.error`

---

### Endpoint: `GET /history/transactions` — Version: 2

**Description:** Retrieve transaction history for the account over the specified date/time range, optionally filtered by type.

**Method & Path:** GET `/history/transactions` with query parameters
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters (query):**

1. `from` — string — **required** — ISO8601 date/time (e.g., `2024-01-01T00:00:00`) — start of range.
2. `to` — string — **required** — ISO8601 date/time — end of range.
3. `type` — enum — optional — `ALL` | `DEPOSIT` | `WITHDRAWAL` | `TRADE` | `COMMISSION` | `DIVIDEND` etc.
4. `maxSpanSeconds` — integer — optional — maximum allowed span (service may enforce max).

**Example Request:**

```
Query:
  from = "2024-01-01T00:00:00"
  to = "2024-01-31T23:59:59"
  type = "ALL"
  maxSpanSeconds = 2592000
```

**Example Response:**

```json
{
  "transactions": [
    {
      "date": "2024-01-05T12:00:00Z",
      "type": "TRADE",
      "instrumentName": "EUR/USD",
      "dealId": "DIAAAABBBCCC123",
      "amount": -100.0,
      "profitAndLoss": 50.0,
      "currency": "USD",
      "balance": 9950.0
    }
  ],
  "metadata": { "from": "2024-01-01T00:00:00", "to": "2024-01-31T23:59:59" }
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.date.range`, `invalid.type`
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /history/transactions/{transactionType}/{fromDate}/{toDate}` — Version: 2

**Description:** Retrieve transactions of a specific `transactionType` in an explicit date range (path-based).

**Method & Path:** GET `/history/transactions/{transactionType}/{fromDate}/{toDate}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters (path):**

1. `transactionType` — enum — **required** — one of `ALL`, `TRADE`, `DEPOSIT`, `WITHDRAWAL`, etc.
2. `fromDate` — string — **required** — ISO8601.
3. `toDate` — string — **required** — ISO8601.

**Example Request:**

```
Path:
  transactionType = "TRADE"
  fromDate = "2024-01-01T00:00:00"
  toDate = "2024-01-31T23:59:59"
```

**Example Response:** same structure as `GET /history/transactions`.

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.transactionType`, `invalid.date.range`
* 401 Unauthorized
* 404 Not Found
* 500 Internal Server Error

---

### Endpoint: `GET /history/transactions/{transactionType}/{lastPeriod}` — Version: 2

**Description:** Retrieve recent transactions of a specific type for a relative period (`DAY`, `WEEK`, `MONTH`).

**Method & Path:** GET `/history/transactions/{transactionType}/{lastPeriod}`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 2`

**Parameters:**

1. `transactionType` — enum — **required** — `ALL` | `TRADE` | ...
2. `lastPeriod` — string — **required** — `DAY` | `WEEK` | `MONTH` (relative window).

**Example Request:**

```
Path:
  transactionType = "TRADE"
  lastPeriod = "WEEK"
```

**Example Response:**

```json
{
  "transactions": [
    {
      "date": "2025-09-03T09:30:00Z",
      "type": "TRADE",
      "instrumentName": "GBP/USD",
      "profitAndLoss": -25.0,
      "currency": "USD"
    }
  ]
}
```

**Return Codes:**

* 200 OK
* 400 Bad Request — `invalid.transactionType`, `invalid.lastPeriod`
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /history/activity` — Version: 1

**Description:** Retrieve general account activity (login, changes, major events) — variant endpoints exist for activity per date range.

**Method & Path:** GET `/history/activity`
**Headers Required:**

* `X-IG-API-KEY: <string>`
* Token-based: `CST: <string>` and `X-SECURITY-TOKEN: <string>`; OR
* OAuth-based: `Authorization: Bearer <access_token>` and `IG-ACCOUNT-ID: <accountId>`
* `Accept: application/json; charset=UTF-8`
* `Version: 1`

**Parameters (query optional):**

* `from` — ISO8601 — optional
* `to` — ISO8601 — optional

**Example Response:**

```json
{
  "activity": [
    { "date": "2025-09-01T10:00:00Z", "description": "Login from web", "type": "LOGIN" }
  ]
}
```

**Return Codes:**

* 200 OK
* 401 Unauthorized
* 500 Internal Server Error

---

### Endpoint: `GET /history/activity/{fromDate}/{toDate}` — Version: 1

**Description:** Activity for a specific date range.

**Method & Path:** GET `/history/activity/{fromDate}/{toDate}`
**Headers Required:** same as `/history/activity`

**Parameters:**

1. `fromDate` — ISO8601 — required
2. `toDate` — ISO8601 — required

**Example Response:** same structure as `/history/activity`.

**Return Codes:** 200, 400 invalid date, 401, 500.

---

## APPENDIX: ERROR CODES & MESSAGES (Comprehensive list of commonly returned identifiers)

> Each error below is presented as an identifier frequently returned by IG APIs. Many endpoints also return generic HTTP codes (400/401/403/404/409/500); the identifiers below are the common payload error keys/messages you will see in response bodies.

### Authentication / Session Errors

* `error.security.api-key-missing` — X-IG-API-KEY header missing.
* `error.security.api-key-invalid` — API key invalid.
* `error.security.api-key-disabled` — API key disabled.
* `error.security.api-key-restricted` — API key restricted.
* `error.public-api.failure.missing.credentials` — username/password missing.
* `invalid.credentials` — credentials wrong (user/pass).
* `auth.failed` — generic auth failure.
* `error.security.client-token-invalid` — CST invalid.
* `error.security.client-token-missing` — CST missing.
* `error.security.account-token-invalid` — X-SECURITY-TOKEN invalid.
* `error.security.account-token-missing` — X-SECURITY-TOKEN missing.
* `error.security.oauth-token-invalid` — OAuth access token invalid.
* `error.public-api.failure.kyc.required` — KYC required for account.
* `error.public-api.failure.preferred.account.not.set` — preferred account not set.
* `error.public-api.failure.preferred.account.disabled` — chosen preferred account disabled.

### Dealing / Trading Errors

* `invalid.input` — invalid JSON payload or bad parameters.
* `invalid.orderType` — unrecognized order type.
* `invalid.size` — size invalid or extent out of range.
* `invalid.level` — level missing or invalid for order type.
* `invalid.dealId` — provided dealId invalid/format incorrect.
* `deal.not.found` — dealId not found.
* `error.public-api.deal-not-found` — same semantics as above.
* `insufficient.funds` — not enough funds/margin for action.
* `error.dealing.market.orders.not.supported` — market orders not supported for this instrument.
* `quote.expired` — quote used in request expired.
* `amend.not.allowed` — amendment not allowed on this order.
* `error.public-api.exceeded-account-trading-allowance` — account trading allowance exceeded.
* `order.not.allowed` — working order not allowed for market/account.
* `error.trading.otc.market-orders.not-supported` — OTC instrument doesn't support market orders.

### Market / Prices / Navigation Errors

* `invalid.epic` — epic has invalid format.
* `epic.not.found` — epic doesn't exist.
* `invalid.searchTerm` — search term malformed.
* `invalid.nodeId` — navigation node id malformed.
* `node.not.found` — node not present.
* `invalid.resolution` — price resolution invalid.
* `invalid.date.range` — start/end date combination invalid or too wide.

### Watchlists Errors

* `invalid.watchlistId` — watchlist id invalid.
* `watchlist.not.found` — watchlist doesn't exist.
* `epic.not.in.watchlist` — attempt to remove epic that's not present.
* `invalid.input` — generic payload error.

### History / Transactions Errors

* `invalid.transactionType` — unrecognized transaction type filter.
* `invalid.date.range` — transaction history date range invalid.

### Generic / HTTP-level

* HTTP 400 — Bad Request (see identifiers above).
* HTTP 401 — Unauthorized (invalid/missing credentials).
* HTTP 403 — Forbidden (API key/account not permitted).
* HTTP 404 — Not Found (resource missing).
* HTTP 409 — Conflict (race condition or resource conflict e.g., `quote.expired`).
* HTTP 500 — Internal Server Error — `system.error`.

---

## NOTES, GOTCHAS, & USAGE TIPS (explicit)

* **Always set the `Version` header** exactly as specified per endpoint. If you omit it the server may default to `1` or reject the request.
* **Two authentication models**: v3/OAuth returns access tokens in JSON, v1/v2 returns `CST` and `X-SECURITY-TOKEN` in headers. Some endpoints accept both — check the explicit header lists above and always include `X-IG-API-KEY`.
* **When using OAuth (v3)**: include `IG-ACCOUNT-ID` header on requests that require account context (dealing endpoints usually require it).
* **Streaming (Lightstreamer)**: If you use OAuth tokens but need `CST` and `X-SECURITY-TOKEN` for the streaming API, call `GET /session?fetchSessionTokens=true` with your OAuth access token to retrieve them in response headers.
* **Expiry format** for instruments: many endpoints accept `expiry` as `-` for non-expiring instruments or formatted codes for futures/sprint markets — follow the instrument `expiry` value returned by `/markets/{epic}` when creating orders.
* **Mutually exclusive fields**: many dealing endpoints enforce mutually exclusive parameter sets (e.g., `stopLevel` vs `stopDistance`, `limitLevel` vs `limitDistance`) — attempting to set both will return `400` with `invalid.input`.
* **Quote lifecycle**: If an order requires a `quoteId`, the quote expires quickly — handle `quote.expired` by re-quoting.
* **Precision**: sizes and level decimal precision can vary by instrument — the `markets/{epic}` response contains `decimalPlacesFactor`/`lotSize`/`valueOfOnePip` etc. Use those to format numeric fields.
