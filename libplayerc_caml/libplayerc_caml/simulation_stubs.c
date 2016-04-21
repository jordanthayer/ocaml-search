/**
 * \file simulation_stubs.c
 *
 * Stub functions for the simulation proxy.
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


/* cleanup a simulation structure. */
static void simulation_finalize(value v);


/**
 * Operations for a simulation custom block.
 */
static struct custom_operations simulation_ops = {
	.identifier = "playerc simulation",
	.finalize = simulation_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void simulation_finalize(value sim_val)
{
	playerc_simulation_t *sim;
	sim = Simulation_val(sim_val);

	if (sim) {
		DPRINTF("destroying simulation %p\n", sim);
		playerc_simulation_destroy(sim);
	}
}

value simulation_create_stub(value client_val, value index_val)
{
	CAMLparam2(client_val, index_val);
	CAMLlocal1(sim_val);
	int index = Int_val(index_val);
	playerc_client_t *client = Client_val(client_val);
	playerc_simulation_t *sim;

	DPRINTF("creat simulation on client %p\n", client);

	sim = playerc_simulation_create(client, index);
	if (!sim)
		exception_playerc_error();

	DPRINTF("created simulation %p on client %p\n", sim, client);

	sim_val = caml_alloc_custom(&simulation_ops, sizeof(sim), 0, 1);
	Simulation_val(sim_val) = sim;

	CAMLreturn(sim_val);
}

void simulation_destroy_stub(value sim_val)
{
	CAMLparam1(sim_val);
	playerc_simulation_t *sim = Simulation_val(sim_val);

	DPRINTF("destroying simulation %p\n", sim);
	playerc_simulation_destroy(sim);

	CAMLreturn0;
}

void simulation_subscribe_stub(value sim_val, value access_val)
{
	CAMLparam2(sim_val, access_val);
	playerc_simulation_t *sim = Simulation_val(sim_val);
	int access = get_access_mode(Int_val(access_val));


	DPRINTF("subscribing to simulation %p\n", sim);

	if (playerc_simulation_subscribe(sim, access))
		exception_playerc_error();

	DPRINTF("simulation %p subscribed\n", sim);

	CAMLreturn0;
}

void simulation_unsubscribe_stub(value sim_val)
{
	CAMLparam1(sim_val);
	playerc_simulation_t *sim = Simulation_val(sim_val);

	DPRINTF("unsubscribing from simulation %p\n", sim);

	if (playerc_simulation_unsubscribe(sim))
		exception_playerc_error();

	DPRINTF("simulation %p unsubscribed\n", sim);

	CAMLreturn0;
}

void simulation_set_pose2d_stub(value sim_val, value name_val,
    value gx_val, value gy_val, value ga_val)
{
  CAMLparam5(sim_val, name_val, gx_val, gy_val, ga_val);
	playerc_simulation_t *sim = Simulation_val(sim_val);

  char *name = String_val(name_val);
  double gx = Double_val(gx_val);
  double gy = Double_val(gy_val);
  double ga = Double_val(ga_val);

  DPRINTF("setting sim %p pose2d: name - %s gx = %f gy = %f ga = %f\n",
      sim, name, gx, gy, ga);

  if(playerc_simulation_set_pose2d(sim, name, gx, gy, ga))
    exception_playerc_error();

  DPRINTF("set sim %p pose2d: name - %s gx = %f gy = %f ga = %f\n",
      sim, name, gx, gy, ga);

  CAMLreturn0;
}

value simulation_get_pose2d_stub(value sim_val, value name_val)
{
  CAMLparam2(sim_val, name_val);
  CAMLlocal1(result);

	playerc_simulation_t *sim = Simulation_val(sim_val);
  char *name = String_val(name_val);

  double x, y, a;

  DPRINTF("getting sim %p pose2d: name - %s\n", sim, name);

  if(playerc_simulation_get_pose2d(sim, name, &x, &y, &a))
    exception_playerc_error();

  DPRINTF("got sim %p pose2d: name - %s x = %f y = %f a = %f\n",
      sim, name, x, y, a);

  result = caml_alloc_tuple(3);
  Store_field(result, 0, copy_double(x));
  Store_field(result, 1, copy_double(y));
  Store_field(result, 2, copy_double(a));

  CAMLreturn(result);
}

void simulation_set_pose3d_stub(value sim_val, value name_val,
    value gx_val, value gy_val, value gz_val, value groll_val,
    value gpitch_val, value gyaw_val)
{
  CAMLparam5(sim_val, name_val, gx_val, gy_val, gz_val);
//  CAMLxparam3(groll_val, gpitch_val, gyaw_val);

	playerc_simulation_t *sim = Simulation_val(sim_val);
  char *name = String_val(name_val);
  double gx = Double_val(gx_val);
  double gy = Double_val(gy_val);
  double gz = Double_val(gz_val);
  double groll = Double_val(groll_val);
  double gpitch = Double_val(gpitch_val);
  double gyaw = Double_val(gyaw_val);

  DPRINTF("setting sim %p pose3d: name - %s gx = %f gy = %f gz = %f groll = %f gpitch = %f gyaw = %f\n",
      sim, name, gx, gy, gz, groll, gpitch, gyaw);

  if(playerc_simulation_set_pose3d(sim, name, gx, gy, gz, groll, gpitch, gyaw))
    exception_playerc_error();

  DPRINTF("set sim %p pose3d: name - %s gx = %f gy = %f gz = %f groll = %f gpitch = %f gyaw = %f\n",
      sim, name, gx, gy, gz, groll, gpitch, gyaw);

  CAMLreturn0;
}

//void simulation_set_pose3d_stub2(value groll_val, value gpitch_val, value gyaw_val,
 //   playerc_simulation_t *sim, double gx, double gy, double groll)
//{
//}

value simulation_get_pose3d_stub(value sim_val, value name_val)
{
  CAMLparam2(sim_val, name_val);
  CAMLlocal1(result);

	playerc_simulation_t *sim = Simulation_val(sim_val);
  char *name = String_val(name_val);

  double x, y, z;
  double roll, pitch, yaw;
  double time;

  DPRINTF("getting sim %p pose3d: name - %s\n", sim, name);

  if(playerc_simulation_get_pose3d(sim, name, &x, &y, &z, &roll, &pitch, &yaw, &time))
    exception_playerc_error();

  DPRINTF("set sim %p pose3d: name - %s x = %f y = %f z = %f roll = %f pitch = %f yaw = %f time = %f\n",
      sim, name, x, y, z, roll, pitch, yaw, time);

  result = caml_alloc_tuple(7);
  Store_field(result, 0, copy_double(x));
  Store_field(result, 1, copy_double(y));
  Store_field(result, 2, copy_double(z));
  Store_field(result, 3, copy_double(roll));
  Store_field(result, 4, copy_double(pitch));
  Store_field(result, 5, copy_double(yaw));
  Store_field(result, 6, copy_double(time));

  CAMLreturn(result);
}

void simulation_pause_stub(value sim_val)
{
	CAMLparam1(sim_val);
	playerc_simulation_t *sim = Simulation_val(sim_val);

	DPRINTF("toggling simulation pause %p\n", sim);

	if (playerc_simulation_pause(sim))
		exception_playerc_error();

	DPRINTF("simulation %p pause toggled\n", sim);

	CAMLreturn0;
}


// EOF
