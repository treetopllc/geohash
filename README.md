GeoHash
=======

A NIF based geohash module implementation for Erlang

Example Usage
-------------

```erlang
1> geohash:encode(12.0, 12.0, 5).
{ok,<<"s60s3">>}
2> geohash:decode(<<"abcde">>).
{ok,{32.0, 52.3}}
```
