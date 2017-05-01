limitless_wsapi
=====

A Websocket API interface for Limitless.

The application is using the in-memory distributed no-master document database
[minidb](https://github.com/hachreak/minidb) in a way that you can
have multiple nodes running `limitless_wsapi` and connect them in a cluster.

Build
-----

    $ rebar3 compile

Run
---

    $ make all

Open a websocket client and connect to `ws://127.0.0.1:8080/`.

Then, setup the token configuration sending the message:

```json
{"command": "setup", "context": {"objectids": ["token1"], "group": "token"}}
```

Now, every time you receive a request made with this token, you'll simply ask
if who request the resource has reach some limits
(1000 req/daily or 100 req/15min).

Send the request:

```json
{"command": "is_reached", "context": {"objectids": ["token1"]}}
```

You'll receive something like:

```json
{
	"info": [{
		"extra": [{
			"expiry": 83659,
			"max": 1000,
			"remaining": 990,
			"type": "Token-Daily"
		}, {
			"expiry": 856,
			"max": 100,
			"remaining": 97,
			"type": "Token-15min"
		}],
		"is_reached": false,
		"objectid": "token1"
	}],
	"is_reached": false,
	"objectid": "token1"
}
```

The most important information is last `is_reached`: it's saying to you if
there is a limit reached for the `token1` and if you should block the request.

In this case is `false` and it means that the request can proceeds.

Inside `"extra"` you can find some interesting information useful to costruct
`X-RateLimiter-Token1-XXX` HTTP headers.

In this case:

```
X-RateLimit-Token-Daily-Limit: 1000
X-RateLimit-Token-Daily-Remaining: 990
X-RateLimit-Token-Daily-Reset: 83659
X-RateLimit-Token-15min-Limit: 100
X-RateLimit-Token-15min-Remaining: 97
X-RateLimit-Token-15min-Reset: 856
```
