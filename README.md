hastile
=======

A haskell tile server. Send request to `/layer/z/x/y/mvt`, get back a Mapbox vector tile (`.mvt`). In addition, to see the SQL generated, replace `mvt` in the URL with `query`. Likewise, replace `mvt` with `geojson` to see the GeoJSON generated as input to mapnik-vector-tile.

Start server with `hastile --configFile FILEPATH`

Configuration
-------------

Config file should contain a JSON map like

```javascript
{
  "pgConnection": "host=example.com port=5432 user=tiler password=123abc dbname=notoracle"
  "layers": {
    "layer1"": "SELECT ST_AsGeoJSON(wkb_geometry), hstore(layer1_table) FROM layer1_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
    "layer2"": "SELECT ST_AsGeoJSON(wkb_geometry), hstore(layer2_table) FROM layer2_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
  }
}
```

where pgConnection is a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

Like tilesplash, hastile will replace `!bbox_4326!` with the SQL for a bounding box for the requested tile in EPSG4326. This allows your query to dynamically select the features to be included in the requested tile.

The configuration may optionally contain these too:

```javascript
{
  "pgPoolSize": 10,
  "pgTimeout": 5,
  "port": 1234
}
```

Dependencies
------------

### Mapnik

Mapnik is a C++ library that renders the tiles. hastile requires a Mapnik version that supports Mapbox vector tiles - 3.0.9 is currently used, as that's the most recent version I can get building on Mac.

Building it on Mac involves something like the following:

 - `brew install boost --with-icu4c`
 - `brew install` mapnik's dependencies (**not** mapnik itself)
 - `git clone <mapnik>`
 - `git checkout v3.0.9`
 - `./configure && make && make install`
 
You may also need to sym link the library and includes directories to somewhere ghc can find them when it builds its wrapper.

### Mapnik vector tile

This sits on top of mapnik and allows us to produce mapbox vector tiles from GeoJSON.

Download the [source](https://github.com/mapbox/mapnik-vector-tile) and follow its documentation to build it.

### FFI

hastile includes a C wrapper for mapnik-vector-tile that exposes a function to turn GeoJSON into a vector tile. The code for this is in the `mapnik-vector-tile-c` directory. Build this using `build.sh`. Unfortunately this is currently a manual step, and will likely require you to edit build.sh to point to the correct include and library directories.

Building
--------

Once all of the dependencies above are built, you should be able to build the project with `stack build`.

Projections
-----------

We assume tiles are requested in the spherical mercator (EPSG 3857 AKA EPSG 900913 AKA Webby McWebcator). Furthermore, map data is assumed to be stored in EPSG 4326.

Helpful links
-------------

- [Tiles a la Google Maps](http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/)
- [The Google Maps/Bing Maps Spherical Mercator Projection](https://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/)
- [vtile-encode - CLI that does GeoJSON to mvt](https://github.com/mapbox/mapnik-vector-tile/blob/master/bench/vtile-encode.cpp)
