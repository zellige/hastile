#include "mvt_from_geojson.h"

void write_mvt(const char * name, const char * mvt)
{
  FILE *f = fopen(name, "w");
  if (f)
  {
    fprintf(f, mvt);
    fclose(f);
  }
  else
  {
    printf("OMFG no file!");
  }
}

int main(const int argc, const char** argv)
{
  static const char * geo_json = "{ \
    \"type\": \"FeatureCollection\", \
    \"features\": [{ \
        \"type\": \"Feature\", \
        \"geometry\": {\"type\": \"LineString\", \"coordinates\": [[115.859553,-31.951733], [115.859679,-31.951774]]}, \
        \"properties\": {\"foo\": \"bar\"} \
     }] \
    }";
  mvtc_return * rv = mvtc_from_geo_json(256, geo_json, "test_layer");
  if (!rv) printf("Oh noes, no return value\n");
  if (rv->return_code != MVTC_SUCCESS)
  {
    printf("You had a bad:\n  %s", rv->message.c_str());
  }
  const char * mvt = mvtc_get_mvt(rv);
  write_mvt("tile.mvt", mvt);
  printf("done!\n");
  return 0;
}
