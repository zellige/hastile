#ifndef MVT_FROM_GEOJSON_H
#define MVT_FROM_GEOJSON_H

#ifdef __cplusplus
extern "C"
{
#endif

typedef enum {
  MVTC_SUCCESS,
  MVTC_FAILURE
} mvtc_return_code;

typedef struct _mvtc_return mvtc_return;

mvtc_return * mvtc_from_geo_json(const int tile_size,
                                 const char * geo_json,
                                 const char * layer_name,
                                 const char * mapnik_input_plugins_path,
                                 const int z,
                                 const int x,
                                 const int y);

const char * mvtc_get_mvt(mvtc_return * rv);

const int mvtc_get_mvt_size(mvtc_return * rv);

mvtc_return_code mvtc_get_return_code(mvtc_return * rv);

const char * mvtc_get_message(mvtc_return * rv);

void mvtc_free_mvtc_return(mvtc_return * rv);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // MVT_FROM_GEOJSON_H
