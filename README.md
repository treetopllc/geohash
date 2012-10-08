GeoHash
=======

A NIF based geohash module implementation for Erlang

Example Usage
-------------

```erlang
$ erl -pa ebin        
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1> {ok, Hash} = geohash:encode(12.2, 12.4, 5).
{ok,<<"s60v6">>}
2> geohash:neighbor(Hash, n).
{ok,<<"s60vd">>}
3> geohash:neighbor(Hash, s).
{ok,<<"s60v4">>}
4> geohash:neighbor(Hash, w).
{ok,<<"s60v7">>}
5> geohash:neighbor(Hash, e).
{ok,<<"s60v3">>}
6> geohash:decode(Hash).     
{ok,{12.216796875,12.48046875}}
7> geohash:decode_bbox(Hash).
{ok,{{12.12890625,12.3046875},{12.3046875,12.65625}}}
8> 
```
