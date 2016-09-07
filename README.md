hastile
=======

A haskell tile server. Send request to `/layer/z/x/y`, get back a Mapbox vector tile (`.mvt`).

Start server with `hastile --pgConfig FILEPATH [--port INT]`

`pgConfig` file should contain a [Postgres connection string](https://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING)

Dependencies
------------

### Mapnik

Mapnik is a C++ library that renders our tiles.

Building it on Mac involves:

 - `brew install boost --with-icu4c`
 - `brew install` mapnik's dependencies (**not** mapnik itself)
 - `git clone <mapnik>`
 - `git checkout v3.0.9`
 - `./configure && make && make install`

Might need to set some paths in `configure`

Building
--------

`stack build`
