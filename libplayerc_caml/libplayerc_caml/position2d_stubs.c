/**
 * \file position2d_stubs.c
 *
 * Stub functions for the position2d proxy.
 *
 * \author eaburns
 * \date 30-01-2010
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

/* Fields of the position record.  These need to match the order of
 * the fields in the OCaml record. */
enum position_fields {
	POS_PX,
	POS_PY,
	POS_PA,
	POS_VX,
	POS_VY,
	POS_VA,

	POS_NUM
};

/* cleanup a position2d structure. */
static void position2d_finalize(value v);


/**
 * Operations for a position2d custom block.
 */
static struct custom_operations position2d_ops = {
	.identifier = "playerc position2d",
	.finalize = position2d_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void position2d_finalize(value pos2d_val)
{
	playerc_position2d_t *pos2d;
	pos2d = Position2d_val(pos2d_val);

	if (pos2d) {
		DPRINTF("destroying position2d %p\n", pos2d);
		playerc_position2d_destroy(pos2d);
	}
}

value position2d_create_stub(value client_val, value index_val)
{
	CAMLparam2(client_val, index_val);
	CAMLlocal1(pos2d_val);
	int index = Int_val(index_val);
	playerc_client_t *client = Client_val(client_val);
	playerc_position2d_t *pos2d;

	DPRINTF("creat position2d on client %p\n", client);

	pos2d = playerc_position2d_create(client, index);
	if (!pos2d)
		exception_playerc_error();

	DPRINTF("created position2d %p on clien %p\n", pos2d, client);

	pos2d_val = caml_alloc_custom(&position2d_ops, sizeof(pos2d), 0, 1);
	Position2d_val(pos2d_val) = pos2d;

	CAMLreturn(pos2d_val);
}

void position2d_destroy_stub(value pos2d_val)
{
	CAMLparam1(pos2d_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);

	DPRINTF("destroying position2d %p\n", pos2d);
	playerc_position2d_destroy(pos2d);

	CAMLreturn0;
}

void position2d_subscribe_stub(value pos2d_val, value access_val)
{
	CAMLparam2(pos2d_val, access_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
	int access = get_access_mode(Int_val(access_val));


	DPRINTF("subscribing to position2d %p\n", pos2d);

	if (playerc_position2d_subscribe(pos2d, access))
		exception_playerc_error();

	DPRINTF("position2d %p subscribed\n", pos2d);

	CAMLreturn0;
}

void position2d_unsubscribe_stub(value pos2d_val)
{
	CAMLparam1(pos2d_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);

	DPRINTF("unsubscribing from position2d %p\n", pos2d);

	if (playerc_position2d_unsubscribe(pos2d))
		exception_playerc_error();

	DPRINTF("position2d %p unsubscribed\n", pos2d);

	CAMLreturn0;
}

void position2d_enable_stub(value pos2d_val, value enable_val)
{
	CAMLparam2(pos2d_val, enable_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
	int enable = Bool_val(enable_val);

	DPRINTF("position2d %p setting enable to %d\n", pos2d, enable);

	if (playerc_position2d_enable(pos2d, enable))
		exception_playerc_error();

	DPRINTF("set position2d %p enable to %d\n", pos2d, enable);

	CAMLreturn0;
}

value position2d_device_stub(value pos2d_val)
{
	CAMLparam1(pos2d_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);

	CAMLreturn((value) &pos2d->info);
}

value position2d_position_stub(value pos2d_val)
{
	CAMLparam1(pos2d_val);
	CAMLlocal1(res);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);

	res = caml_alloc(POS_NUM, Double_array_tag);
	Store_double_field(res, POS_PX, pos2d->px);
	Store_double_field(res, POS_PY, pos2d->py);
	Store_double_field(res, POS_PA, pos2d->pa);
	Store_double_field(res, POS_VX, pos2d->vx);
	Store_double_field(res, POS_VY, pos2d->vy);
	Store_double_field(res, POS_VA, pos2d->va);

	CAMLreturn(res);
}

void position2d_set_cmd_pose_stub(value pos2d_val,
				  value px_val,
				  value py_val,
				  value pa_val,
				  value state_val)
{
	CAMLparam5(pos2d_val, px_val, py_val, pa_val, state_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
	double px = Double_val(px_val);
	double py = Double_val(py_val);
	double pa = Double_val(pa_val);
	int state = Int_val(state_val);

	DPRINTF("setting position2d %p px=%f py=%f pa=%f state=%d\n",
		pos2d, px, py, pa, state);

	if (playerc_position2d_set_cmd_pose(pos2d, px, py, pa, state))
		exception_playerc_error();

	DPRINTF("position2d %p set to px=%f py=%f pa=%f state=%d\n",
		pos2d, px, py, pa, state);

	CAMLreturn0;
}

void position2d_set_cmd_vel_stub(value pos2d_val,
				 value vx_val,
				 value vy_val,
				 value va_val,
				 value state_val)
{
	CAMLparam5(pos2d_val, vx_val, vy_val, va_val, state_val);
	playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
	double vx = Double_val(vx_val);
	double vy = Double_val(vy_val);
	double va = Double_val(va_val);
	int state = Int_val(state_val);

	DPRINTF("setting position2d %p vx=%f vy=%f va=%f state=%d\n",
		pos2d, vx, vy, va, state);

	if (playerc_position2d_set_cmd_vel(pos2d, vx, vy, va, state))
		exception_playerc_error();

	DPRINTF("position2d %p set to vx=%f vy=%f va=%f state=%d\n",
		pos2d, vx, vy, va, state);

	CAMLreturn0;
}

void position2d_set_odom_stub(value pos2d_val,
         value ox_val,
         value oy_val,
         value oa_val)
{
  CAMLparam4(pos2d_val, ox_val, oy_val, oa_val);
  playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
  double ox = Double_val(ox_val);
  double oy = Double_val(oy_val);
  double oa = Double_val(oa_val);

  DPRINTF("setting position2d %p odom: ox=%f oy=%f oa=%f\n",
      pos2d, ox, oy, oa);

  if(playerc_position2d_set_odom(pos2d, ox, oy, oa))
    exception_playerc_error();

  DPRINTF("setting position2d %p odom: ox=%f oy=%f oa=%f\n",
      pos2d, ox, oy, oa);

  CAMLreturn0;
}

value position2d_get_size_stub(value pos2d_val)
{
  CAMLparam1(pos2d_val);
  CAMLlocal1(res);
  playerc_position2d_t *pos2d = Position2d_val(pos2d_val);
  res = caml_alloc_tuple(2);

  DPRINTF("requesting position2d size\n");

  if(playerc_position2d_get_geom(pos2d))
    exception_playerc_error();

  DPRINTF("got position2d size\n");

  Store_field(res, 0, copy_double(pos2d->size[0]));
  Store_field(res, 1, copy_double(pos2d->size[1]));
  CAMLreturn(res);
}
