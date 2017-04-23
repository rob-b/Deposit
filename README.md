# deposit

[![Build Status](https://travis-ci.org/rob-b/Deposit.png)](https://travis-ci.org/rob-b/Deposit)

Simple Http testing tool; akin to https://httpbin.org but much more limited.

##  Installation

```
stack install deposit
```

## Usage

To run the server
```
stack exec -- deposit
```

So far only `POST` and `GET` are supported
```
> curl -XPOST -d '{"somekey": 123}' http://localhost:8080/post
{"origin":"127.0.0.1","args":{},"data":"{\"somekey\": 123}","headers":{"Content-Type":"application/x-www-form-urlencoded","Accept":"*/*","User-Agent":"curl/7.51.0","Host":"localhost:8080","Content-Length":"16"}}
```

```
> curl -XGET "http://localhost:8080/get?foo=bar&foo=baz"
{"origin":"127.0.0.1","args":{"foo":"baz"},"headers":{"Accept":"*/*","User-Agent":"curl/7.51.0","Host":"localhost:8080"}}%
```
