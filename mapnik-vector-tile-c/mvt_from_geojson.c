#include "mvt_from_geojson.h"

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
  mvtc_from_geo_json(256, geo_json, "test_layer");
  printf("done!");
  return 0;
}
