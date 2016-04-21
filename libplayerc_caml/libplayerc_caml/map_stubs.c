/**
 * \file map_stubs.c
 *
 * Stub functions for the map proxy.
 *
 * \author Kevin Rose
 * \date 06-09-2010
 */

#define _POSIX_C_SOURCE 200112L

#include <libplayerc/playerc.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "common.h"
#include "verb.h"

enum map_fields {
  MAP_RES,
  MAP_WIDTH,
  MAP_HEIGHT,
  MAP_ORIGIN_X,
  MAP_ORIGIN_Y,
  MAP_CELLS,

  MAP_NUM
};

/* cleanup a map structure. */
static void map_finalize(value v);


/**
 * Operations for a map custom block.
 */
static struct custom_operations map_ops = {
	.identifier = "playerc map",
	.finalize = map_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void map_finalize(value map_val)
{
	playerc_map_t *map;
	map = Map_val(map_val);

	if (map) {
		DPRINTF("destroying map %p\n", map);
		playerc_map_destroy(map);
	}
}

value map_create_stub(value client_val, value index_val)
{
	CAMLparam2(client_val, index_val);
	CAMLlocal1(map_val);
	int index = Int_val(index_val);
	playerc_client_t *client = Client_val(client_val);
	playerc_map_t *map;

	DPRINTF("creat map on client %p\n", client);

	map = playerc_map_create(client, index);
	if (!map)
		exception_playerc_error();

	DPRINTF("created map %p on clien %p\n", map, client);

	map_val = caml_alloc_custom(&map_ops, sizeof(map), 0, 1);
	Map_val(map_val) = map;

	CAMLreturn(map_val);
}

void map_destroy_stub(value map_val)
{
	CAMLparam1(map_val);
	playerc_map_t *map = Map_val(map_val);

	DPRINTF("destroying map %p\n", map);
	playerc_map_destroy(map);

	CAMLreturn0;
}

void map_subscribe_stub(value map_val, value access_val)
{
	CAMLparam2(map_val, access_val);
	playerc_map_t *map = Map_val(map_val);
	int access = get_access_mode(Int_val(access_val));


	DPRINTF("subscribing to map %p\n", map);

	if (playerc_map_subscribe(map, access))
		exception_playerc_error();

	DPRINTF("map %p subscribed\n", map);

	CAMLreturn0;
}

void map_unsubscribe_stub(value map_val)
{
	CAMLparam1(map_val);
	playerc_map_t *map = Map_val(map_val);

	DPRINTF("unsubscribing from map %p\n", map);

	if (playerc_map_unsubscribe(map))
		exception_playerc_error();

	DPRINTF("map %p unsubscribed\n", map);

	CAMLreturn0;
}

void map_get_map_stub(value map_val)
{
  CAMLparam1(map_val);
  playerc_map_t *map = Map_val(map_val);

  if(playerc_map_get_map(map))
    exception_playerc_error();

  CAMLreturn0;
}

value map_get_resolution_stub(value map_val)
{
  CAMLparam1(map_val);
  CAMLlocal1(res);
  playerc_map_t *map = Map_val(map_val);
  
  res = copy_double(map->resolution);
  CAMLreturn(res);
}

value map_get_width_stub(value map_val)
{
  CAMLparam1(map_val);
  CAMLlocal1(res);
  playerc_map_t *map = Map_val(map_val);
  
  res = Val_int(map->width);
  CAMLreturn(res);
}

value map_get_height_stub(value map_val)
{
  CAMLparam1(map_val);
  CAMLlocal1(res);
  playerc_map_t *map = Map_val(map_val);
  
  res = Val_int(map->height);
  CAMLreturn(res);
}

value map_get_origin_stub(value map_val)
{
  CAMLparam1(map_val);
  CAMLlocal1(res);
  playerc_map_t *map = Map_val(map_val);
  
  res = caml_alloc_tuple(2);
  Store_field(res, 0, copy_double(map->origin[0]));
  Store_field(res, 1, copy_double(map->origin[1]));
  // DPRINTF("ORIGIN: %f, %f\n", map->origin[0], map->origin[1]);
  CAMLreturn(res);
}

value map_get_cells_stub(value map_val)
{
  CAMLparam1(map_val);
  CAMLlocal1(res);
  int i;
  int size;
  playerc_map_t *map = Map_val(map_val);
  
  // allocate the int array
  size = map->width * map->height;
  res = caml_alloc(size, 0);

  for(i = 0; i < size; i++)
  {
    Store_field(res, i, Val_int((int) map->cells[i]));
  }

  CAMLreturn(res);
}

// EOF
