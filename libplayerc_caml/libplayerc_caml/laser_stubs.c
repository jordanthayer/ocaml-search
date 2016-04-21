/**
 * \file laser_stubs.c
 *
 * Stubs for laser range finder data.
 *
 * \author eaburns
 * \date 02-02-2010
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

/* Fields of the config record.  These need to match the order of the
 * fields in the OCaml record. */
enum config_fields {
	CONF_MIN_ANGLE,
	CONF_MAX_ANGLE,
	CONF_RESOLUTION,
	CONF_RANGE_RES,
	CONF_INTENSITY,
	CONF_SCANNING_FREQUENCY,

	CONF_NUM
};

/* cleanup a laser structure. */
static void laser_finalize(value v);


/**
 * Operations for a laser custom block.
 */
static struct custom_operations laser_ops = {
	.identifier = "playerc laser",
	.finalize = laser_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

static void laser_finalize(value laser_val)
{
	playerc_laser_t *laser;
	laser = Laser_val(laser_val);

	if (laser) {
		DPRINTF("destroying laser %p\n", laser);
		playerc_laser_destroy(laser);
	}
}

value laser_create_stub(value client_val, value index_val)
{
	CAMLparam2(client_val, index_val);
	CAMLlocal1(laser_val);
	int index = Int_val(index_val);
	playerc_client_t *client = Client_val(client_val);
	playerc_laser_t *laser;

	DPRINTF("create laser on client %p\n", client);

	laser = playerc_laser_create(client, index);
	if (!laser)
		exception_playerc_error();

	DPRINTF("created laser %p on client %p\n", laser, client);

	laser_val = caml_alloc_custom(&laser_ops, sizeof(laser), 0, 1);
	Laser_val(laser_val) = laser;

	CAMLreturn(laser_val);
}

void laser_subscribe_stub(value laser_val, value access_val)
{
	CAMLparam2(laser_val, access_val);
	playerc_laser_t *laser = Laser_val(laser_val);
	int access = get_access_mode(Int_val(access_val));

	DPRINTF("subscribing to laser %p\n", laser);

	if (playerc_laser_subscribe(laser, access))
		exception_playerc_error();

	DPRINTF("subscribed to laser %p\n", laser);

	CAMLreturn0;
}

void laser_unsubscribe_stub(value laser_val)
{
	CAMLparam1(laser_val);
	playerc_laser_t *laser = Laser_val(laser_val);

	DPRINTF("unsubscribing from laser %p\n", laser);

	if (playerc_laser_unsubscribe(laser))
		exception_playerc_error();

	DPRINTF("unsubscribed from laser %p\n", laser);

	CAMLreturn0;
}

void laser_set_config_stub(value laser_val, value conf_val)
{
	CAMLparam2(laser_val, conf_val);
	double min_angle = Double_field(conf_val, CONF_MIN_ANGLE);
	double max_angle = Double_field(conf_val, CONF_MAX_ANGLE);
	double resolution = Double_field(conf_val, CONF_RESOLUTION);
	double range_res = Double_field(conf_val, CONF_RANGE_RES);
	double intensity_d = Double_field(conf_val, CONF_INTENSITY);
	unsigned char intensity = intensity_d;
	double scanning_freq = Double_field(conf_val, CONF_SCANNING_FREQUENCY);
	playerc_laser_t *laser = Laser_val(laser_val);

	if (intensity_d > 255 || intensity_d < 0
	    || intensity_d != intensity) {
		DPRINTF("intensity %f is not a valid 8-bit integer value\n",
			intensity_d);
		caml_invalid_argument("intensity");
	}

	DPRINTF("setting laser %p config to:\n", laser);
	DPRINTF("\tmin_angle=%f\n", min_angle);
	DPRINTF("\tmax_angle=%f\n", max_angle);
	DPRINTF("\tresolution=%f\n", resolution);
	DPRINTF("\trange_ros=%f\n", range_res);
	DPRINTF("\tintensity=%d\n", intensity);
	DPRINTF("\tscanning_frequency=%f\n", scanning_freq);

	if (playerc_laser_set_config(laser, min_angle, max_angle, resolution,
				      range_res, intensity, scanning_freq))
		exception_playerc_error();

	DPRINTF("set laser %p config\n", laser);

	CAMLreturn0;
}

value laser_get_config_stub(value laser_val)
{
	CAMLparam1(laser_val);
	CAMLlocal1(res);
	playerc_laser_t *laser = Laser_val(laser_val);
	double min_angle;
	double max_angle;
	double resolution;
	double range_res;
	unsigned char intensity;
	double scanning_freq;

	DPRINTF("getting laser %p config\n", laser);

	if (playerc_laser_get_config(laser, &min_angle, &max_angle,
				      &resolution, &range_res, &intensity,
				      &scanning_freq))
		exception_playerc_error();

	DPRINTF("got laser %p config:\n", laser);
	DPRINTF("\tmin_angle=%f\n", min_angle);
	DPRINTF("\tmax_angle=%f\n", max_angle);
	DPRINTF("\tresolution=%f\n", resolution);
	DPRINTF("\trange_ros=%f\n", range_res);
	DPRINTF("\tintensity=%d\n", intensity);
	DPRINTF("\tscanning_frequency=%f\n", scanning_freq);

	res = caml_alloc(CONF_NUM, Double_array_tag);
	Store_double_field(res, CONF_MIN_ANGLE, min_angle);
	Store_double_field(res, CONF_MAX_ANGLE, max_angle);
	Store_double_field(res, CONF_RESOLUTION, resolution);
	Store_double_field(res, CONF_RANGE_RES, range_res);
	Store_double_field(res, CONF_INTENSITY, (double) intensity);
	Store_double_field(res, CONF_SCANNING_FREQUENCY, scanning_freq);

	CAMLreturn(res);
}

/* Returns a tuple of xs and ys because this is more compact than
 * storing an array of point records. */
value laser_points_stub(value laser_val)
{
	CAMLparam1(laser_val);
	CAMLlocal3(res, xs, ys);
	playerc_laser_t *laser = Laser_val(laser_val);
	unsigned int i;
	unsigned int num = laser->scan_count;

	xs = caml_alloc(num, Double_array_tag);
	ys = caml_alloc(num, Double_array_tag);
	for (i = 0; i < num; i += 1) {
		Store_double_field(xs, i, laser->point[i].px);
		Store_double_field(ys, i, laser->point[i].py);
	}

	res = caml_alloc_tuple(2);
	Store_field(res, 0, xs);
	Store_field(res, 1, ys);

	CAMLreturn(res);
}

value laser_scan_stub(value laser_val)
{
	CAMLparam1(laser_val);
	CAMLlocal3(res, ms, rs);
	playerc_laser_t *laser = Laser_val(laser_val);
	unsigned int i;
	unsigned int num = laser->scan_count;

	ms = caml_alloc(num, Double_array_tag);
	rs = caml_alloc(num, Double_array_tag);
	for (i = 0; i < num; i += 1) {
		Store_double_field(ms, i, laser->scan[i][0]);
		Store_double_field(rs, i, laser->scan[i][1]);
	}

	res = caml_alloc_tuple(2);
	Store_field(res, 0, ms);
	Store_field(res, 1, rs);

	CAMLreturn(res);
}

value laser_intensities_stub(value laser_val)
{
	CAMLparam1(laser_val);
	CAMLlocal1(intensities);
	playerc_laser_t *laser = Laser_val(laser_val);
	unsigned int i;
	unsigned int num = laser->scan_count;

	intensities = caml_alloc(num, 0);
	for (i = 0; i < num; i += 1)
		Store_field(intensities, i, Val_int(laser->intensity[i]));

	CAMLreturn(intensities);
}

value laser_max_range_stub(value laser_val)
{
	CAMLparam1(laser_val);
	playerc_laser_t *laser = Laser_val(laser_val);

	CAMLreturn(caml_copy_double(laser->max_range));
}

value laser_device_stub(value laser_val)
{
	CAMLparam1(laser_val);
	playerc_laser_t *laser = Laser_val(laser_val);

	CAMLreturn((value) &laser->info);
}
