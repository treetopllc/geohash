%% Copyright (c) 2012, Treetop Software LLC
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2012 Treetop Software LLC
%% @doc GeoHash C Function Wrapper

-module(geohash).

-export([decode/1, decode_bbox/1, encode/3, neighbor/2, neighbors/1, expand/1]).

%% @doc Decode a geohash in to latitude and longitude
-spec decode(binary()) -> {float(), float()}.
decode(_GeoHash) ->
    exit(geohash_nif_not_loaded).

%% @doc Decode a geohash in to latitude and longitude bounds
-spec decode_bbox(binary()) -> {{float(), float()}, {float(), float()}}.
decode_bbox(_GeoHash) ->
    exit(geohash_nif_not_loaded).

%% @doc Encode latitude and longitude into a geohash 
-spec encode(float(), float(), pos_integer()) -> binary().
encode(_Latitude, _Longitude, _Precision) ->
    exit(geohash_nif_not_loaded).

%% @doc Calculate a neighoring geohash
-spec neighbor(binary(), u | d | l | r) -> binary().
neighbor(GeoHash, Direction) ->
    exit(geohash_nif_not_loaded).

%% @doc Calculate 8 neighboring geohashes
-spec neighbors(binary()) -> [binary()].
neighbors(C) ->
    N = neighbor(C, u),
    W = neighbor(C, l),
    S = neighbor(C, d),
    E = neighbor(C, r),
    NW = neighbor(N, l),
    NE = neighbor(N, r),
    SW = neighbor(S, l),
    SE = neighbor(S, r),
    [N, W, S, E, NW, NE, SW, SE].

%% @doc Expand a geohash to give a list of itself and 8 neighboring geohashes
-spec expand(binary()) -> [binary()].
expand(C) ->
    [C | neighbors(C)].
