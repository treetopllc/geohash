/* 
 * Copyright (c) 2008 David Troy, Roundhouse Technologies LLC
 * Copyright (c) 2012 Tom Burdick, Treetop Software LLC
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * 'Software'), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "erl_nif_compat.h"

#define GEOHASH_MAX 64
#define BASE32	"0123456789bcdefghjkmnpqrstuvwxyz"

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })

/**
 * Decode the bounding box represented by a geohash
 */
static void
geohash_decode_bbox(char *geohash, double *lat, double *lon)
{
    static char bits[] = {16,8,4,2,1};

    int i, j, hashlen;
    double lat_err, lon_err;
    char c, cd, mask, is_even=1;

    hashlen = strlen(geohash);

    lat[0] = -90.0;  lat[1] = 90.0;
    lon[0] = -180.0; lon[1] = 180.0;
    lat_err = 90.0;  lon_err = 180.0;

    for (i=0; i<hashlen; i++) {
        c = tolower(geohash[i]);
        cd = strchr(BASE32, c)-BASE32;
        for (j=0; j<5; j++) {
            mask = bits[j];
            if (is_even) {
                lon_err /= 2;
                lon[!(cd&mask)] = (lon[0] + lon[1])/2;
            } else {
                lat_err /= 2;
                lat[!(cd&mask)] = (lat[0] + lat[1])/2;
            }
            is_even = !is_even;
        }
    }
}

/**
 * Decode the mid point of the bounding box represented by a geohash
 */
static void
geohash_decode(char *geohash, double *point)
{
    double lat[2], lon[2];

    geohash_decode_bbox(geohash, lat, lon);

    point[0] = (lat[0] + lat[1]) / 2;
    point[1] = (lon[0] + lon[1]) / 2;
}

/**
 * Encode a point to given precision in to a geohash
 */
static void
geohash_encode(double latitude, double longitude, int precision, char *geohash)
{
    int is_even=1, i=0;
    double lat[2], lon[2], mid;
    char bits[] = {16,8,4,2,1};
    int bit=0, ch=0;

    lat[0] = -90.0;  lat[1] = 90.0;
    lon[0] = -180.0; lon[1] = 180.0;

    while (i < precision) {
        if (is_even) {
            mid = (lon[0] + lon[1]) / 2;
            if (longitude > mid) {
                ch |= bits[bit];
                lon[0] = mid;
            } else
                lon[1] = mid;
        } else {
            mid = (lat[0] + lat[1]) / 2;
            if (latitude > mid) {
                ch |= bits[bit];
                lat[0] = mid;
            } else
                lat[1] = mid;
        }

        is_even = !is_even;
        if (bit < 4)
            bit++;
        else {
            geohash[i++] = BASE32[ch];
            bit = 0;
            ch = 0;
        }
    }
    geohash[i] = 0;
}

/**
 * Calculate a neighbor to a geohash of the same precision
 */
void 
geohash_neighbor(char *str, int dir, int hashlen)
{
    /* Right, Left, Top, Bottom */
    /*     0,    1,   2,      3 */
    static char *neighbors[] = {
        "bc01fg45238967deuvhjyznpkmstqrwx",
        "238967debc01fg45kmstqrwxuvhjyznp",
        "p0r21436x8zb9dcf5h7kjnmqesgutwvy",
        "14365h7k9dcfesgujnmqp0r2twvyx8zb"
    };

    static char *borders[] = {
        "bcfguvyz",
        "0145hjnp",
        "prxz",
        "028b"
    };

    char last_chr, *border, *neighbor;
    int index = ( 2 * (hashlen % 2) + dir) % 4;
    neighbor = neighbors[index];
    border = borders[index];
    last_chr = str[hashlen-1];
    if (strchr(border,last_chr))
        geohash_neighbor(str, dir, hashlen-1);
    str[hashlen-1] = BASE32[strchr(neighbor, last_chr)-neighbor];
}

/**
 * Helper to make a valid erlang atom
 */
static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

/**
 * Helper to make a valid erlang tuple containing {ok, Msg}
 */
static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);   
}

/**
 * Helper to make a valid erlang tuple containing {error, Msg}
 */
static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}

/**
 * Erlang Wrapper for geohash_decode_bbox
 */
ERL_NIF_TERM
erl_geohash_decode_bbox(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    char geohash[GEOHASH_MAX];
    double lat[2], lon[2];
    size_t len;

    if(!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    len = min(input.size, GEOHASH_MAX);
    strncpy(geohash, (char*)input.data, len);
    geohash[len] = '\0';

    geohash_decode_bbox(geohash, lat, lon);

    ERL_NIF_TERM latrange = enif_make_tuple2(env,
            enif_make_double(env, lat[0]),
            enif_make_double(env, lat[1]));
    ERL_NIF_TERM lonrange = enif_make_tuple2(env,
            enif_make_double(env, lon[0]),
            enif_make_double(env, lon[1]));
    ERL_NIF_TERM bbox = enif_make_tuple2(env, latrange, lonrange);

    return make_ok(env, bbox);
}

/**
 * Erlang Wrapper for geohash_decode
 */
ERL_NIF_TERM
erl_geohash_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    char geohash[GEOHASH_MAX];
    double point[2];
    size_t len;

    if(!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    len = min(input.size, GEOHASH_MAX);
    strncpy(geohash, (char*)input.data, len);
    geohash[len] = '\0';
    geohash_decode(geohash, point);

    ERL_NIF_TERM point_tuple = enif_make_tuple2(env,
            enif_make_double(env, point[0]),
            enif_make_double(env, point[1]));

    return make_ok(env, point_tuple);
}

/**
 * Erlang Wrapper for geohash_encode
 */
ERL_NIF_TERM
erl_geohash_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double lat;
    double lon;
    int precision;
    ErlNifBinary bin;

    if(!enif_get_double(env, argv[0], &lat)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_double(env, argv[1], &lon)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[2], &precision)) {
        return enif_make_badarg(env);
    }

    if(precision >= GEOHASH_MAX || precision < 1) {
        return make_error(env, "precision_range");
    }

    if(!enif_alloc_binary(precision, &bin)) {
        return make_error(env, "alloc_error");
    }

    geohash_encode(lat, lon, precision, (char*)bin.data);

    return make_ok(env, enif_make_binary(env, &bin));
}

/**
 * Erlang Wrapper for geohash_neighbor
 */
ERL_NIF_TERM
erl_geohash_neighbor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input, output;
    char geohash[GEOHASH_MAX];
    char dir[2];
    size_t hash_len;
    unsigned int dir_len;
    int dir_val;

    if(!enif_inspect_binary(env, argv[0], &input))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom_length(env, argv[1], &dir_len, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    if(dir_len > sizeof(dir)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom(env, argv[1], dir, sizeof(dir), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    hash_len = min(input.size, GEOHASH_MAX);
    strncpy(geohash, (char*)input.data, hash_len);
    geohash[hash_len] = '\0';

    switch (dir[0]) {
        case 'w':
            dir_val = 0;
            break;
        case 'e':
            dir_val = 1;
            break;
        case 'n':
            dir_val = 2;
            break;
        case 's':
            dir_val = 3;
            break;
        default:
            return enif_make_badarg(env);
    }

    if(!enif_alloc_binary(hash_len, &output)) {
        return make_error(env, "alloc_error");
    }

    geohash_neighbor(geohash, dir_val, hash_len);
    
    memcpy(output.data, geohash, hash_len);

    return make_ok(env, enif_make_binary(env, &output));
}



int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}

static ErlNifFunc nif_functions[] = {
    {"encode", 3, erl_geohash_encode},
    {"decode", 1, erl_geohash_decode},
    {"decode_bbox", 1, erl_geohash_decode_bbox},
    {"neighbor", 2, erl_geohash_neighbor}
};

ERL_NIF_INIT(geohash, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);
