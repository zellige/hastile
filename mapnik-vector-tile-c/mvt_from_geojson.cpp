#include <string>
#include <mapnik/map.hpp>
#include <mapnik/datasource_cache.hpp>
#include "vector_tile_processor.hpp"

#include "mvt_from_geojson.h"

struct _mvtc_return {
  mvtc_return_code return_code;
  std::string message;
  std::string mvt;
};

mvtc_return * mvtc_from_geo_json(const int tile_size,
                                 const char * geo_json,
                                 const char * layer_name,
                                 const char * mapnik_input_plugins_path,
                                 const int z,
                                 const int x,
                                 const int y)
{
  mvtc_return * rv = new mvtc_return();
  if (rv == NULL) return NULL;

  try
  {
    double area_threshold = 0.1;
    double simplify_distance = 0.0;
    bool strictly_simple = true;
    bool multi_polygon_union = false;
    mapnik::vector_tile_impl::polygon_fill_type fill_type = mapnik::vector_tile_impl::positive_fill;
    bool process_all_rings = false;

    mapnik::datasource_cache::instance().register_datasources(mapnik_input_plugins_path);

    mapnik::Map map(tile_size, tile_size, "+init=epsg:3857");
    mapnik::parameters p;
    p["type"] = "geojson";
    p["inline"] = geo_json;

    mapnik::layer lyr(layer_name,"+init=epsg:4326");
    lyr.set_datasource(mapnik::datasource_cache::instance().create(p));
    map.add_layer(lyr);

    mapnik::vector_tile_impl::merc_tile out_tile(x, y, z, tile_size);
    mapnik::vector_tile_impl::processor ren(map);
    ren.set_area_threshold(area_threshold);
    ren.set_strictly_simple(strictly_simple);
    ren.set_simplify_distance(simplify_distance);
    ren.set_multi_polygon_union(multi_polygon_union);
    ren.set_fill_type(fill_type);
    ren.set_process_all_rings(process_all_rings);
    ren.update_tile(out_tile);

    out_tile.serialize_to_string(rv->mvt);
    rv->return_code = MVTC_SUCCESS;
  }
  catch (std::exception & ex)
  {
    rv->mvt = "";
    rv->return_code = MVTC_FAILURE;
    rv->message = std::string(ex.what());
  }
  return rv;
}

const char * mvtc_get_mvt(mvtc_return * rv)
{
  return rv->mvt.c_str();
}

mvtc_return_code mvtc_get_return_code(mvtc_return * rv)
{
  return rv->return_code;
}

const char * mvtc_get_message(mvtc_return * rv)
{
  return rv->message.c_str();
}

void mvtc_free_mvtc_return(mvtc_return * rv)
{
  if (rv)
  {
    /* mvtc_reset_return(rv); */
    delete rv;
    rv = NULL;
  }
}

int main(int argc, char ** argv)
{
  char * geo_json;
  FILE * f = fopen(argv[1], "r");
  fseek(f, 0, SEEK_END);
  int string_size = ftell(f);
  rewind(f);
  geo_json = (char *) malloc(sizeof(char) * string_size + 1);
  fread(geo_json, sizeof(char), string_size, f);
  geo_json[string_size] = '\0';
  fclose(f);

	mvtc_return * r = mvtc_from_geo_json(256, geo_json, "foolayer", "/usr/local/lib/mapnik/input", 14, 13464, 9727);
	if (r->return_code != MVTC_SUCCESS)
	{
		printf("Bad: %s\n", r->message.c_str());
		return 1;
	}

  free(geo_json);

	printf("%s", r->mvt.c_str());
}
