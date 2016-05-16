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
%% @doc GeoHash functions for Erlang with C implementations for core 
%% functionality
%% @end

-module(geohash).

-export([
    decode/1,
    decode_bbox/1,
    encode/3,
    neighbor/2,
    neighbors/1,
    expand/1,
    nearby/3
]).

-on_load(init/0).

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
-spec neighbor(binary(), n | s | w | e) -> binary().
neighbor(_GeoHash, _Direction) ->
    exit(geohash_nif_not_loaded).

%% @doc Calculate 8 neighboring geohashes
-spec neighbors(binary()) -> [binary()].
neighbors(C) ->
    {ok, N} = neighbor(C, n),
    {ok, W} = neighbor(C, w),
    {ok, S} = neighbor(C, s),
    {ok, E} = neighbor(C, e),
    {ok, NW} = neighbor(N, w),
    {ok, NE} = neighbor(N, e),
    {ok, SW} = neighbor(S, w),
    {ok, SE} = neighbor(S, e),
    [N, W, S, E, NW, NE, SW, SE].

%% @doc Expand a geohash to give a list of itself and 8 neighboring geohashes
-spec expand(binary()) -> [binary()].
expand(C) ->
    [C | neighbors(C)].

%% @doc Nearby geohashes useful for searching a region of a map
-spec nearby(float(), float(), float()) -> [binary()].
nearby(Lat, Lon, Rad) ->
    Precision = nearby_precision(Lat, Lon, Rad),
    {ok, GeoHash} = encode(Lat, Lon, Precision),
    expand(GeoHash).

%% @private
init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "geohash_nif"]);
        false ->
            filename:join(["priv", "geohash_nif"])
        end;
    Dir ->
        filename:join(Dir, "geohash_nif")
    end,
    (catch erlang:load_nif(SoName, 0)),
    case erlang:system_info(otp_release) of
    "R13B03" -> true;
    _ -> ok
    end.

%% @doc Best fit geohash precision
-spec nearby_precision(float(), float(), float()) -> integer().
nearby_precision(Lat, Lon, Rad) ->
    {MinLat, MinLon, MaxLat, MaxLon} = earth_bounding_box(Lat, Lon, Rad),
    DeltaLat = MaxLat - MinLat,
    DeltaLon = MaxLon - MinLon,
    Bits = geohash_bits(DeltaLat, DeltaLon, 63),
    max(trunc(Bits/5), 1).

%% @doc Determine the bounding box of coordinates for a point and radius
%% distance on the earth.
%% @end
-spec earth_bounding_box(float(), float(), float()) ->
    {float(), float(), float(), float()}. 
earth_bounding_box(Lat, Lon, Dist)  ->
    MIN_LAT = radians(-90),
    MAX_LAT = radians(90),
    MIN_LON = radians(-180),
    MAX_LON = radians(180),
    AngularDist  = Dist / earth_radius(),
    Lat0 = radians(Lat),
    Lon0 = radians(Lon),
    MinLat = Lat0 - AngularDist,
    MaxLat = Lat0 + AngularDist,
    case (MinLat > MIN_LAT) and (MaxLat < MAX_LAT) of
        true ->
            DeltaLon = math:asin(math:sin(AngularDist) / math:cos(Lat0)),
            MinLon = Lon0 - DeltaLon,
            MinLon0 = case MinLon < MIN_LON of
                true ->
                    MinLon + 2.0*math:pi();
                false ->
                    MinLon
            end,
            MaxLon = Lon0 + DeltaLon,
            MaxLon0 = case MaxLon > MAX_LON of
                true ->
                    MaxLon - 2.0*math:pi();
                false ->
                    MaxLon
            end,
            {degrees(MinLat), degrees(MinLon0),
                degrees(MaxLat), degrees(MaxLon0)};
        false ->
            MinLat0 = max(MinLat, MIN_LAT),
            MaxLat0 = min(MaxLat, MAX_LAT),
            {degrees(MinLat0), degrees(MIN_LON),
                degrees(MaxLat0), degrees(MAX_LON)}
    end.

%% @doc Earth Radius
-spec earth_radius() -> float().
earth_radius() ->
    6371.0.

%% @doc Convert degrees to radians
-spec radians(float()) -> float().
radians(Degrees) ->
    (math:pi()/180.0)*Degrees.

%% @doc Convert radians to degrees
-spec degrees(float()) -> float().
degrees(Radians) ->
    (180.0/math:pi())*Radians.

%% @doc Determine the number of bits required (slices) to encompass a 
%% latitude and longitude range.
geohash_bits(_, _, 0) ->
    0;
geohash_bits(DeltaLat, DeltaLon, Bits) ->
    case (delta_lat(Bits) < DeltaLat) or (delta_lon(Bits) < DeltaLon) of
        true ->
            geohash_bits(DeltaLat, DeltaLon, Bits-1);
        false ->
            Bits
    end.

%% @doc Give the delta for a number of bits used in geohashing latitude
delta_lat(Bits) ->
    180.0 / math:pow(2, Bits / 2).

%% @doc Give the delta for a number of bits used in geohashing longitude
delta_lon(Bits) ->
    360.0 / math:pow(2, (Bits + 1) / 2).

