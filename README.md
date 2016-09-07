hastile
=======

A haskell tile server. Send request to `/layer/z/x/y`, get back a Mapbox vector tile (`.mvt`).

Start server with `hastile --pgConfig FILEPATH [--port INT]`

`pgConfig` file should contain a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING)

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
