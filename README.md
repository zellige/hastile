hastile
=======

A Haskell tile server that produces GeoJSON or MVT (Mapbox Vector Tiles) from a PostGIS database.

![Build Status](https://circleci.com/gh/indicatrix/hastile/tree/master.png?circle-token=151e6cea2b027041b06878de8694bbfdaf2b6aba)

RESTful API
-----------
```
GET    /                       (application/json)                   - Returns the current configuration.
POST   /                       (application/json)                   - Add/overwite layername (e.g. {"layer": { "name" : "...", "query": "..." } ... }).
POST   /layername              (application/json)                   - Add/overwrite the query setting for layername (e.g. {"query": "...", ... }).
GET    /layername              (application/json)                   - Return TileJSON
GET    /layername/Z/X/Y/query  (text/plain)                         - Query for a given layername, Zoom, and (X,Y).
GET    /layername/Z/X/Y.mvt    (application/vnd.mapbox-vector-tile) - Return Mapnick Vector Tile for given layername, Zoom, (X,Y).
GET    /layername/Z/X/Y.json   (application/json)                   - Return GeoJSON for given layername, Zoom, (X,Y).
GET    /token                  (application/json)                   - Returns tokens and authorised layers
GET    /token/tokenid          (application/json)                   - Returns the authorised layers for the given token
POST   /token                  (application/json)                   - Post a token and its authorised layers to insert/update the token database
DELETE /token/tokenid          (application/json)                   - Delete the given token from the token database
```

[TileJSON](https://github.com/mapbox/tilejson-spec/tree/master/2.2.0)

Layer API
---------

The ```POST /``` with a layer configuration or ```POST /layername``` with a layer settings allows you to change the layers that Hastile serves up 
and will save the configuration file to disk.

To create a new layer:
- ```curl -d '{ "layer_name": { "table-name": "...", "format": "geojson", "quantize": 2, "simplify": {} } }' -H "Content-Type: application/json" -X POST http://localhost:8080/```

To modify an existing layer:
- ```curl -d '{ "table-name": "...", "format": "geojson", "quantize": 2, "simplify": {} }' -H "Content-Type: application/json" -X POST http://localhost:8080/layer_name```

Token API
---------

To insert or update a token:
- ```curl -d '{ "token": "abcd", "layers": ["layer1", "layer2"] }' -H "Content-Type: application/json" -X POST http://localhost:8080/token```

To delete a token:
- ```curl -H "Content-Type: application/json" -X DELETE http://localhost:8080/token/abcd```


Building
--------

### PostgreSQL

To generate the GeoJSON feature (see below) requires PostgreSQL 9.5+.

Building:
 - `stack build`
 - `stack test`

Set up tokens database
----------------------

* Create a postgres database to store the tokens table:
  `createdb -O sa db_name`
If you don't have the `createdb` utility then use the `migration` tool :  
  `./db/migration createdb db_name sa`
* Initialize the DB :  
  `./db/migration init "postgresql://db_user:password@db_server:db_port/db_name"`
* Run the migrations :  
  `./db/migration migrate "postgresql://db_user:password@db_server:db_port/db_name"`
* Use the `migration` tool for migrations :  
  `./db/migration --help`  


Configuration
-------------

The file contains settings for the database connection and layer configuration, for example:
```javascript
{
  "db-connection": "host=example.com port=5432 user=tiler password=123abc dbname=notoracle"
  "layers": {
    "layer1": { 
      "table_name": "layer1_table",
      "format": "wkb-properties",
      "last-modified": "2017-01-15T23:49:36Z"
    },
    "layer2": {
      "table_name": "layer2_table",
      "format": "geojson",
      "last-modified": "2017-01-15T23:49:36Z"
    }
  }
}
```

Where, db-connection is a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

The layer table has three columns: a single GeoJSON formatted feature as JSON, the properties part of the geojson and the geometry. The geometry is used to perform the spatial query. The GeoJSON column is the feature returned if the layer format is `geojson`. The properties column is returned along side the geometry if the layer format is `wkb-properties`.

To construct a table with a GeoJSON feature with all properties containing arbitrary columns from a table, create a materialized view like:
```javascript
CREATE MATERIALIZED VIEW layer1_table as SELECT jsonb_build_object(
    'type',      'Feature',
    'id',         ogc_fid,
    'geometry',   ST_AsGeoJSON(wkb_geometry)::jsonb,
    'properties', to_jsonb(row) - 'ogc_fid' - 'wkb_geometry'
)::json as geojson, (to_jsonb(row) - 'wkb_geometry') :: JSON as properties,row.wkb_geometry as wkb_geometry FROM (SELECT * FROM source_layer1_table) row;
```

This will create the two columns required: geojson (a GeoJSON feature in JSON format) and the geometry column.

You can configure other database, mapnik and HTTP port settings too:
```javascript
{
  "db-pool-size": 10,
  "db-timeout": 5,
  "port": 8080
}
```

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
      ...
    }  
  }
```

Running
-------
To start the server:
`hastile --configFile FILEPATH`

To run with GHC Metrics:
`./hastile --configFile FILEPATH +RTS -T`

Projections
-----------

We assume tiles are requested in the spherical mercator (EPSG 3857 AKA EPSG 900913 AKA Webby McWebcator). Furthermore, map data is assumed to be stored in EPSG 4326.

Helpful links
-------------

- [Mapbox Vector Tile Specification] (https://www.mapbox.com/vector-tiles/specification/)
- [Tiles a la Google Maps](http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/)
- [The Google Maps/Bing Maps Spherical Mercator Projection](https://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/)
- [vtile-encode - CLI that does GeoJSON to mvt](https://github.com/mapbox/mapnik-vector-tile/blob/master/bench/vtile-encode.cpp)
