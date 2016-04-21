/**
 * \file device_stubs.c
 *
 * Accessor functions for the device structure fields.  The OCaml
 * value for a playerc_device_t is just a pointer with nothing fancier
 * wrapped around it.  These structures are not allocated explicitly
 * and there is no need to wrap them in a custom block since they will
 * never need to be cleaned up by the garbage collector.
 *
 * \author eaburns
 * \date 30-01-2010
 */

#define _POSIX_C_SOURCE 200112L

#include <libplayerc/playerc.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Must match the order of the fields in the Device.address type. */
enum address_fields {
	DEV_HOST,
	DEV_ROBOT,
	DEV_INTERF,
	DEV_INDEX,

	DEV_NUM
};

value device_address_stub(value dev_val)
{
	CAMLparam1(dev_val);
	CAMLlocal1(addr_val);
	playerc_device_t *dev = (playerc_device_t*) dev_val;

	addr_val = caml_alloc(DEV_NUM, 0);
	/* apparently this always succeeds... */
	Store_field(addr_val, DEV_HOST, Val_int(dev->addr.host));
	Store_field(addr_val, DEV_ROBOT, Val_int(dev->addr.robot));
	Store_field(addr_val, DEV_INTERF, Val_int(dev->addr.interf));
	Store_field(addr_val, DEV_INDEX, Val_int(dev->addr.index));

	CAMLreturn(addr_val);
}

value device_drivername_stub(value dev_val)
{
	CAMLparam1(dev_val);
	playerc_device_t *dev = (playerc_device_t*) dev_val;

	CAMLreturn(caml_copy_string(dev->drivername));
}

value device_subscribed_stub(value dev_val)
{
	CAMLparam1(dev_val);
	playerc_device_t *dev = (playerc_device_t*) dev_val;

	CAMLreturn(Val_bool(dev->subscribed));
}

value device_datatime_stub(value dev_val)
{
	CAMLparam1(dev_val);
	playerc_device_t *dev = (playerc_device_t*) dev_val;

	CAMLreturn(caml_copy_double(dev->datatime));
}

value device_lasttime_stub(value dev_val)
{
	CAMLparam1(dev_val);
	playerc_device_t *dev = (playerc_device_t*) dev_val;

	CAMLreturn(caml_copy_double(dev->lasttime));
}
