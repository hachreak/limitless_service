limitless_wsapi
===============

A Websocket API interface for Limitless.

The application is using the in-memory distributed no-master document database
[minidb](https://github.com/hachreak/minidb) in a way that you can
have multiple nodes running `limitless_wsapi` and connect them in a cluster.

Build
-----

    $ rebar3 compile

Run
---

    $ make node1

Websocket API
-------------

Open a websocket client and connect to `ws://127.0.0.1:8080/websocket`.

#### Setup limits for a token

```json
{"path": "/objects/token1/groups/token", "method": "put"}
```

You should receive a response:

```json
{"context":{},"result":"ok"}
```

#### Ask if request reach some limits

Now, every time you receive a request made with this token `token`,
you'll simply ask if who request the resource has reach some limits
(in our example: 1000 req/daily or 100 req/15min).

Send the request:

```json
{"path": "/objects/token1/_isreached", "method": "put"}
```

You'll receive something like:

```json
{
  "context": {
    "info": [{
      "extra": [{
        "expiry": 811,
        "group": "Token-15min",
        "max": 100,
        "remaining": 100
      }, {
        "expiry": 86311,
        "group": "Token-Daily",
        "max": 1000,
        "remaining": 1000
      }],
      "is_reached": false
    }],
    "is_reached": false
  },
  "result": "ok"
}
```

The most important information is last `is_reached`: it's saying to you if
there is a limit reached for the `token1`.
If it's true, if'll block the request.

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

In the next example we'll see the tipical response if a limit is reached:

```json
{
  "context": {
    "info": [{
      "extra": [{
        "expiry": 649,
        "group": "Token-15min",
        "max": 100,
        "remaining": 0
      }, {
        "expiry": 86149,
        "group": "Token-Daily",
        "max": 1000,
        "remaining": 900
      }],
      "is_reached": true
    }],
    "is_reached": true
  },
  "result": "ok"
}
```

REST API
--------

The endpoints are the same but in a REST flavour:

#### Setup limits for a token

```bash
$ http PUT :8080/api/objects/token1/groups/token Content-type:application/json
```

```http
HTTP/1.1 204 No Content
content-length: 0
content-type: application/json
date: Mon, 15 May 2017 21:30:17 GMT
server: Cowboy
```

#### Ask if request reach some limits

```bash
$ http PUT :8080/api/objects/token1/_isreached Content-type:application/json
```

```http
HTTP/1.1 200 OK
content-length: 190
content-type: application/json
date: Mon, 15 May 2017 21:31:32 GMT
server: Cowboy

{
    "info": [
        {
            "extra": [
                {
                    "expiry": 825,
                    "group": "Token-15min",
                    "max": 100,
                    "remaining": 100
                },
                {
                    "expiry": 86325,
                    "group": "Token-Daily",
                    "max": 1000,
                    "remaining": 1000
                }
            ],
            "is_reached": false
        }
    ],
    "is_reached": false
}
```

In case, a limit is reached, you will receive a response like:

```http
HTTP/1.1 200 OK
content-length: 185
content-type: application/json
date: Mon, 15 May 2017 21:33:51 GMT
server: Cowboy

{
    "info": [
        {
            "extra": [
                {
                    "expiry": 685,
                    "group": "Token-15min",
                    "max": 100,
                    "remaining": 0
                },
                {
                    "expiry": 86185,
                    "group": "Token-Daily",
                    "max": 1000,
                    "remaining": 900
                }
            ],
            "is_reached": true
        }
    ],
    "is_reached": true
}
```

Make a servers cluster
---------------------

Open the first console and run:

    $ make node1

In another console run:

    $ make node2
    1> minidb:join('test1@127.0.0.1').

Now you can open a websocket connect to `ws://127.0.0.1:8080/websocket` or to
`ws://127.0.0.1:8081/websocket`.

You can note that the node you are choosing is not important.
Also, at runtime, you can disconnect from one and connect to the other without
loosing information.

It can be useful to create a connection pool from client side.
