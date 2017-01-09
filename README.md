hastile
=======

A haskell tile server. Send request to `/layer/z/x/y/mvt`, get back a Mapbox vector tile (`.mvt`). In addition, to see the SQL generated, replace `mvt` in the URL with `query`. Likewise, replace `mvt` with `geojson` to see the GeoJSON generated as input to mapnik-vector-tile.

Start server with `hastile --configFile FILEPATH`

**NOTE**: hastile doesn't _quite_ work at the moment. Only the first feature in each tile is put into the returned vector tile. See [my question](http://gis.stackexchange.com/questions/212691/mapnik-vector-tile-produces-tiles-with-only-one-feature) on gis.stackexchange for details.

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

Mapnik is a C++ library that renders the tiles. hastile requires a Mapnik version that supports Mapbox vector tiles - 
3.0.11ish is currently used (supports MVT version 2).

For OSX:
 - `brew install cairo --without-x --without-glib`
 - `brew install boost --with-icu4c`
 - `brew install icu4c`
 - `brew link icu4c`
 - `brew install boost-python`
 - `brew install proj`
 - `brew install jpeg`
 - `brew link jpeg`
 - `brew install libtiff`
 - `brew install gdal --with-libtiff=/usr/local/lib`
 - `brew install ossp-uuid`
 - `brew install postgis`
 - `brew install harfbuzz`
 - `git clone https://github.com/mapnik/mapnik.git`
 - `git checkout 8a8427daedb685b8f37fac487526255d575a715d`
 - `git submodule sync`
 - `git submodule update --init deps/mapbox/variant`
 - `./configure && make && make install`

For Ubuntu 16.04:
 - Add ubuntugis ppa
  - add to /etc/apt/sources.list
    - deb http://ppa.launchpad.net/ubuntugis/ubuntugis-experimental/ubuntu xenial main 
    - deb-src http://ppa.launchpad.net/ubuntugis/ubuntugis-experimental/ubuntu xenial main 
  - `sudo add-apt-repository ppa:ubuntugis/ubuntugis-experimental`
  - `sudo apt-get update`
 - Mapnik
  - `sudo apt-get install libmapnik3.0 libmapnik-dev`

### Mapnik Vector Tile

Then clone and compile:
 - `git clone https://github.com/mapbox/mapnik-vector-tile.git`
 - `cd mapnik-vector-tile`

For OSX:
 - `brew install protobuf`
 - `make`
 - Will produce files in ./build/Release/

For Ubuntu 16.04:
 - `sudo apt-get install -y protobuf-compiler libprotoc-dev libprotoc9v5`
 - `make`
 - Create static libraries:
   - ar -t obj.target/gyp/libmapnik_vector_tile_impl.a | xargs ar rvs ./libmapnik_vector_tile_impl.a
   - ar -t obj.target/gyp/libvector_tile.a | xargs ar rvs ./libvector_tile.a

### FFI

hastile includes a C wrapper for mapnik-vector-tile that exposes a function to turn GeoJSON into a vector tile. The code for this is in the `mapnik-vector-tile-c` directory. The FFI wrapper _should_ be built when you `stack build` as long as you

 - have mapnik installed
 - have downloaded and built mapnik-vector-tile
 - have set the MAPNIK\_VECTOR\_TILE\_SRC environment variable to the path where you checked out
   and built mapnik-vector-tile

If this does not work, build this using `build.sh`. You will likely have to edit `build.sh` to point to the correct include and library directories.   

Building
--------

For example, assuming mapnik-vector-tile and hastile are peers:
 - <code>export MAPNIK_VECTOR_TILE_SRC=\`pwd\`/../mapnik-vector-tile/</code>
 - `stack build`

Projections
-----------

We assume tiles are requested in the spherical mercator (EPSG 3857 AKA EPSG 900913 AKA Webby McWebcator). Furthermore, map data is assumed to be stored in EPSG 4326.

Helpful links
-------------

- [Tiles a la Google Maps](http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/)
- [The Google Maps/Bing Maps Spherical Mercator Projection](https://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/)
- [vtile-encode - CLI that does GeoJSON to mvt](https://github.com/mapbox/mapnik-vector-tile/blob/master/bench/vtile-encode.cpp)
