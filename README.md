hastile
=======

A haskell tile server. Send request to `/layer_name/z/x/y.mvt`, get back a Mapbox vector tile (`.mvt`). Likewise, `/layer_name/z/x/y.geojson` to get back GeoJSON.  In addition, to see the SQL generated 
use `/layer/z/x/y/query`.

Start server with `hastile --configFile FILEPATH`

Configuration
-------------

The file contains settings for the database connection and layer configuration, for example:
```javascript
{
  "db-connection": "host=example.com port=5432 user=tiler password=123abc dbname=notoracle"
  "layers": {
    "layer1": { 
      "query": "SELECT ST_AsGeoJSON(wkb_geometry), hstore(layer1_table)-'wkb_geometry'::text FROM layer1_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      "last-modified": "2017-01-15T23:49:36Z"
    },
    "layer2": {
      "query": "SELECT ST_AsGeoJSON(wkb_geometry), hstore(layer2_table)-'wkb_geometry'::text FROM layer2_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      "last-modified": "2017-01-15T23:49:36Z"
    }
  }
}
```

Where, db-connection is a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

You can configure other database, mapnik and HTTP port settings too:
```javascript
{
  "db-pool-size": 10,
  "db-timeout": 5,
  "mapnik-input-plugins": "/usr/local/lib/mapnik/input"
  "port": 8080
}
```

Hastile will replace `!bbox_4326!` with the SQL for a bounding box for the requested tile in EPSG4326. This allows your query to dynamically select the features to be included in the requested tile.

If you want to combine multiple tables into a single layer you can use UNION and MATERIALIZED VIEWS and then query it directly:
```SQL
create materialized view layers as
  SELECT ST_AsGeoJSON(wkb_geometry) as geojson, * FROM layer1_table
  UNION
  SELECT ST_AsGeoJSON(wkb_geometry) as geojson, * FROM layer2_table
```

Changing the configuration to:
```javascript
  "layers": {
    "layer": {
      "query": "SELECT geojson, hstore(layers)-ARRAY['wkb_geometry','geojson'] FROM layers WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      ...
    }  
  }
```

Restful API
-----------
```
GET  /                            (application/json)         - Returns current configuration
POST /layername                   (application/json)         - New query setting for layername (e.g. {"query": "..."})
GET  /layername/Z/X/Y/query       (text/plain)               - Query for a given layer, Zoom, and (X,Y).
GET  /layername/Z/X/Y[.mvt|.json] (application/octet-stream) - Return JSON or Mapnick Vector Tile for given layer. Zoom, (X,Y).
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
  - `sudo apt-get install libmapnik3.0 libmapnik-dev`

### Mapnik Vector Tile

You need to build this one from source:
 - `git clone https://github.com/mapbox/mapnik-vector-tile.git`
 - `cd mapnik-vector-tile`

For OSX:
 - `brew install protobuf`
 - `make`
 - Will produce files in ./build/Release/

For Ubuntu 16.04:
 - `sudo apt-get install -y protobuf-compiler libprotoc-dev libprotoc9v5`
 - Turn on PIC
   - Edit ./gyp/build.gyp
   - Under "mapnik_vector_tile_impl", "cflags_cc" (line 91 and 112) add '-fPIC'.
 - `make`
 - Create static libraries in:
   - `cd build/Release`
   - `ar -t obj.target/gyp/libmapnik_vector_tile_impl.a | xargs ar rvs ./libmapnik_vector_tile_impl.a`
   - `ar -t obj.target/gyp/libvector_tile.a | xargs ar rvs ./libvector_tile.a`

### FFI

hastile includes a C wrapper for mapnik-vector-tile that exposes a function to turn GeoJSON into a vector tile. The code for this is in the `mapnik-vector-tile-c` directory. The FFI wrapper _should_ be built when you `stack build` as long as you

 - have mapnik installed
 - have downloaded and built mapnik-vector-tile
 - have set the MAPNIK\_VECTOR\_TILE\_SRC environment variable to the path where you checked out
   and built mapnik-vector-tile

If this does not work, build this using `build.sh`. You will likely have to edit `build.sh` to point to the correct include and library directories.   

Building
--------

Assuming mapnik-vector-tile and hastile projects are peers (underneath the same parent directory):
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
