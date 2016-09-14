hastile
=======

A haskell tile server. Send request to `/layer/z/x/y`, get back a Mapbox vector tile (`.mvt`).

Start server with `hastile --configFile FILEPATH`

Config file should contain a JSON map like

```javascript
{
  "pgConnection": "host=example.com port=5432 user=tiler password=123abc dbname=notoracle"
}
```

where pgConnection is a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

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

Building
--------

`stack build`

Projections
-----------

We assume tiles are requested in the spherical mercator (EPSG 3857 AKA EPSG 900913 AKA Spherey McSpherecator). Furthermore, map data is assumed to be stored in EPSG 4326 (WGS84).

TODO
----

 - Deal with other projections
 - Develop better/more detailed instructions for installing dependencies (i.e. Mapnik)
 - Allow connections to multiple Postgres servers for different layers
 - Get building and testing on Linux

Helpful links
-------------

[Tiles a la Google Maps](http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/)
[The Google Maps/Bing Maps Spherical Mercator Projection](https://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/)
