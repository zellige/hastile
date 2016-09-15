#!/usr/bin/env bash

#-I$(obj)/gen \

g++ -c mvt_from_geojson.cpp \
	-arch x86_64 \
	-std=c++11 \
	-stdlib=libc++ \
	-fvisibility-inlines-hidden \
	-I/usr/local/include \
	-I/usr/local/include/mapnik/agg \
	-I/usr/local/Cellar/postgresql/9.5.3/include \
	-I/usr/local/Cellar/freetype/2.6.5/include/freetype2 \
	-I/usr/local/include \
	-I/usr/local/opt/icu4c/include \
	-I/usr/include \
	-I/usr/local/Cellar/cairo/1.14.6_1/include/cairo \
	-I/usr/local/Cellar/glib/2.48.1/include/glib-2.0 \
	-I/usr/local/Cellar/glib/2.48.1/lib/glib-2.0/include \
	-I/usr/local/opt/gettext/include \
	-I/usr/local/Cellar/pcre/8.39/include \
	-I/usr/local/Cellar/pixman/0.34.0/include/pixman-1 \
	-I/usr/local/Cellar/fontconfig/2.12.1/include \
	-I/usr/local/Cellar/libpng/1.6.24/include/libpng16 \
	-I/Users/andrew/git/mapnik-vector-tile/src \
	-I/Users/andrew/git/mapnik-vector-tile/deps/protozero/include \
	-I/Users/andrew/git/mapnik-vector-tile/deps/clipper/cpp \
	'-DMAPNIK_VECTOR_TILE_LIBRARY=1' \
	'-DCLIPPER_INTPOINT_IMPL=mapnik::geometry::point<cInt>' \
	'-DCLIPPER_PATH_IMPL=mapnik::geometry::line_string<cInt>' \
	'-DCLIPPER_PATHS_IMPL=mapnik::geometry::multi_line_string<cInt>' \
	'-DCLIPPER_IMPL_INCLUDE=<mapnik/geometry.hpp>' \
	'-DMAPNIK_PLUGINDIR="/usr/local/lib/mapnik/input"' \
	'-DDEFAULT_INPUT_PLUGIN_DIR="/usr/local/lib/mapnik/input"' \
	'-DNDEBUG' \
	-DMAPNIK_MEMORY_MAPPED_FILE \
	-DMAPNIK_HAS_DLCFN \
	-DBIGINT \
	-DBOOST_REGEX_HAS_ICU \
	-DHAVE_JPEG \
	-DMAPNIK_USE_PROJ4 \
	-DHAVE_PNG \
	-DHAVE_WEBP \
	-DHAVE_TIFF \
	-DDARWIN \
	-DMAPNIK_THREADSAFE \
	-DBOOST_SPIRIT_NO_PREDEFINED_TERMINALS=1 \
	-DBOOST_PHOENIX_NO_PREDEFINED_TERMINALS=1 \
	-DBOOST_SPIRIT_USE_PHOENIX_V3=1 \
	-DNDEBUG \
	-DHAVE_CAIRO \
	-DGRID_RENDERER \
	-Wall \
	-ftemplate-depth-300 \
	-Wsign-compare \
	-Wshadow \
	-O3 \
	-Wall \
	-Wno-unknown-pragmas \
	-L/usr/local/lib \
	-lmapnik \
	-L/Users/andrew/git/mapnik-vector-tile/build/Release \
    -lmapnik_vector_tile_impl \
	-lvector_tile \
	-L/usr/local/Cellar/freetype/2.6.5/lib \
	-L/usr/local/opt/icu4c/lib \
	-L/usr/lib \
	-lmapnik-wkt \
	-lmapnik-json \
	-lboost_filesystem-mt \
	-lboost_regex-mt \
	-lcairo \
	-lpng \
	-lproj \
	-ltiff \
	-lwebp \
	-licui18n \
	-lboost_system-mt \
	-lharfbuzz \
	-ljpeg \
	-licuuc \
	-lfreetype \
	-lz \
	-lprotobuf-lite

ar crsv libmvt_from_geojson.a mvt_from_geojson.o
