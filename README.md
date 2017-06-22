hastile
=======

A Haskell tile server that produces GeoJSON or MVT (Mapbox Vector Tiles) from a PostGIS database.

RESTful API
-----------
```
GET  /                         (application/json)         - Returns the current configuration.
POST /layername                (application/json)         - Add/overwrite the query setting for layername (e.g. {"query": "..."}).
GET  /layername/Z/X/Y/query    (text/plain)               - Query for a given layername, Zoom, and (X,Y).
GET  /layername/Z/X/Y.mvt      (application/vnd.mapbox-vector-tile) - Return Mapnick Vector Tile for given layername, Zoom, (X,Y).
GET  /layername/Z/X/Y.json     (application/json) - Return GeoJSON for given layername, Zoom, (X,Y).
```

Building
--------

### PostgreSQL

To generate the GeoJSON feature (see below) requires PostgreSQL 9.5+.

### Mapnik

Mapnik is a C++ library that renders the tiles. hastile requires a Mapnik version that supports Mapbox vector tiles - 
3.0.11 is currently used (supports MVT version 2).

Compiling:
 - [OSX](https://github.com/vlc/hastile/wiki/Compiling-Mapnik-3.0.11-for-OSX)
 - [Ubuntu 16.04](https://github.com/vlc/hastile/wiki/Compiling-Mapnik-3.0.11-for-Ubuntu-16.04)

### Mapnik Vector Tile

Run:
 - `mapnik-config --input-plugins`
 - Take the output and set the environment variable: MAPNIK_PLUGINS_DIR. e.g. export MAPNIK_PLUGINS_DIR=/usr/lib/mapnik/3.0/input

You need to build this one from source:
 - `git clone https://github.com/mapbox/mapnik-vector-tile.git`
 - `cd mapnik-vector-tile`
 - `git checkout 55eebb5b4439edec8fb8c1455e5ad9e1dc59670d`

For OSX:
 - `brew install protobuf`
 - `make`
 - Will produce files in ./build/Release/

For Ubuntu 16.04:
 - `sudo apt-get install -y protobuf-compiler libprotoc-dev libprotozero-dev libprotoc9v5`
 - Turn on PIC
   - Edit ./gyp/build.gyp
   - In the section, "target_name": "mapnik_vector_tile_impl":
     - Add '-fPIC' to both "cflags_cc" sections (lines 91 and 112).
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

### Stack

Assuming mapnik-vector-tile and hastile projects are peers (underneath the same parent directory):
 - <code>export MAPNIK_VECTOR_TILE_SRC=\`pwd\`/../mapnik-vector-tile/</code>
 - <code>export MAPNIK_PLUGINS_DIR=\`mapnik-config --input-plugins\`</code>
 - `stack build`
 - `stack test`

Configuration
-------------

The file contains settings for the database connection and layer configuration, for example:
```javascript
{
  "db-connection": "host=example.com port=5432 user=tiler password=123abc dbname=notoracle"
  "layers": {
    "layer1": { 
      "query": "SELECT geojson FROM layer1_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      "last-modified": "2017-01-15T23:49:36Z"
    },
    "layer2": {
      "query": "SELECT geojson FROM layer2_table WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      "last-modified": "2017-01-15T23:49:36Z"
    }
  }
}
```

Where, db-connection is a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

The layer table has two columns: a single GeoJSON formatted feature as JSON and the geometry.  The geometry is used to perform the spatial query and the geojson is the feature returned.

To construct a table with a GeoJSON feature with all properties containing arbitrary columns from a table, create a materialized view like:
```javascript
CREATE MATERIALIZED VIEW layer1_table as SELECT jsonb_build_object(
    'type',      'Feature',
    'id',         ogc_fid,
    'geometry',   ST_AsGeoJSON(wkb_geometry)::jsonb,
    'properties', to_jsonb(row) - 'ogc_fid' - 'wkb_geometry'
)::json as geojson, row.wkb_geometry as wkb_geometry FROM (SELECT * FROM source_layer1_table) row;
```

This will create the two columns required: geojson (a GeoJSON feature in JSON format) and the geometry column.

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
CREATE MATERIALIZED VIEW layers AS
  SELECT geojson FROM layer1_table
  UNION
  SELECT geojson FROM layer2_table
```

Changing the configuration to:
```javascript
  "layers": {
    "layer": {
      "query": "SELECT geojson FROM layers WHERE ST_Intersects(wkb_geometry, !bbox_4326!)",
      ...
    }  
  }
```

Running
-------
Start the server with `hastile --configFile FILEPATH`

Projections
-----------

We assume tiles are requested in the spherical mercator (EPSG 3857 AKA EPSG 900913 AKA Webby McWebcator). Furthermore, map data is assumed to be stored in EPSG 4326.

Helpful links
-------------

- [Mapbox Vector Tile Specification] (https://www.mapbox.com/vector-tiles/specification/)
- [Tiles a la Google Maps](http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/)
- [The Google Maps/Bing Maps Spherical Mercator Projection](https://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/)
- [vtile-encode - CLI that does GeoJSON to mvt](https://github.com/mapbox/mapnik-vector-tile/blob/master/bench/vtile-encode.cpp)
