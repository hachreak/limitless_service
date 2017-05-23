limitless_service
=================

Rate-limiters As A Service (RLAAS):
REST + Websocket API microservice interface for Limitless.

The application is using the in-memory distributed master-less document
database [minidb](https://github.com/hachreak/minidb) to be able to create
a cluster and scale horizontally.

API
---

API                          | REST         | Websocket
-----------------------------|--------------|-------------
Setup limits for a `object`. | Implemented  | Implemented
Ask if limit reached.        | Implemented  | Implemented

You can connect to the service through a REST or Websocket API.

The endpoints are defined by a unique swagger (OpenAPI) configuration.
The [swagger_routerl](https://github.com/hachreak/swagger_routerl) is used to
instantiate them.

To get the swagger file, run the instance and get
`http://127.0.0.1:8080/1.0.0/docs/swagger.yaml` or
open the file `priv/docs/swagger.yaml`.

You can choose to connect to the service every time you need making a REST call
or leave a connections pool open to the websocket endpoint `/websocket` of
one o more server in a cluster.

Both interface are stateless. It means that you easily scale the service.

### Configure limits

In our example, we configure (see `limitless.config`) the object group
`token`.

```erlang
    {limits, [
      % group name: token.
      {token, [
        % This group incude two limit types:
        [
          % max 1000 req/day
          {type, <<"Token-Daily">>},
          {frequency, 86400}, % 1 day = 3600 * 24h
          {requests, 1000}
        ],
        [
          % max 100 req/15min
          {type, <<"Token-15min">>},
          {frequency, 900}, % 15 min = 60 * 15
          {requests, 100}
        ]
      ]}
    ]}
```

But you can use the group name you want and also define multiple groups
(e.g. a group for tokens and a group for users).

See `etc/limitless.config` for a complete configuration example.

### Setup limits for a object

In your application, at the moment a new token is created, you'll associate
to it the limits defined by the `token` group:

**--> REST example**

    $ http PUT :8080/api/objects/token1/groups/token Content-type:application/json

You should receive a response:

```http
HTTP/1.1 204 No Content
content-length: 0
content-type: application/json
date: Mon, 15 May 2017 21:30:17 GMT
server: Cowboy
```

**--> Websocket example**

Send the message

```
{"path": "/objects/token1/groups/token", "method": "put"}
```

You should receive a response:

```json
{"context": {}, "result": "ok"}
```

### Ask if the object reach one of defined limits

Every time you'll receive a request containing a token, you can ask if it
has reached any defined limits.

**--> REST example**

    $ http PUT :8080/api/objects/token1/_isreached Content-type:application/json

You should receive a response:

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
                    "expiry": 811,
                    "type": "Token-15min",
                    "max": 100,
                    "remaining": 90
                },
                {
                    "expiry": 86311,
                    "type": "Token-Daily",
                    "max": 1000,
                    "remaining": 970
                }
            ],
            "is_reached": false
        }
    ],
    "is_reached": false
}
```

**--> Websocket example**

Send the message

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
        "type": "Token-15min",
        "max": 100,
        "remaining": 90
      }, {
        "expiry": 86311,
        "type": "Token-Daily",
        "max": 1000,
        "remaining": 970
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
If it's true, you'll block the request.

In this case is `false` and it means that the request can be processed.

Inside `"extra"` you can find some interesting information useful to build
general informations about the state of all limits.

In case you are implementing a REST API, they are usefull to build the
`X-RateLimiter-Token-XXX` HTTP headers.

E.g.

```
X-RateLimit-Token-Daily-Limit: 1000
X-RateLimit-Token-Daily-Remaining: 970
X-RateLimit-Token-Daily-Reset: 86311
X-RateLimit-Token-15min-Limit: 100
X-RateLimit-Token-15min-Remaining: 90
X-RateLimit-Token-15min-Reset: 811
```


Make a servers cluster
----------------------

Open the first console and run:

    $ make node1

In another console run:

    $ make node2

Node 2 will automatically connect after 5 second to the node 1 in cluster.

Now you can open a websocket connect to `ws://127.0.0.1:8080/websocket` or to
`ws://127.0.0.1:8081/websocket`.
Or make a REST call to one of them.

The node you are choosing is not relevant. Any node can be used.

At client side can be useful create a connection pool and have connections
with all of them.

Run with docker
---------------

There is an example of instance running on docker:

    $ docker-compose build
    $ docker-compose up

Now you will have a node available on `ws://127.0.0.1:8080/websocket`
or REST API at `http://127.0.0.1:8080/api`.

Run a release
-------------

    $ rebar3 release
    $ rebar3 run
