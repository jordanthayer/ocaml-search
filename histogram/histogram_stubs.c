/**
 * \file histogram_stubs.c
 *
 * OCaml stub functions for accessing the histograms.
 *
 * \author eaburns
 * \date 22-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <errno.h>
#include <limits.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "hist.h"
#include "hist_bins.h"
#include "debug.h"

static void histogram_finalize(value v);

/**
 * Operations for a histogram custom block.
 */
static struct custom_operations histogram_ops = {
	.identifier = "histogram",
	.finalize = histogram_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};

/* The finalization function cleans up the memory for a histogram when
 * the GC is done with it. */
static void histogram_finalize(value v)
{
	struct histogram *h;

	h = Data_custom_val(v);

	DPRINTF(DEBUG_STUBS, "Freeing histogram %p\n", h);

	histogram_free(h);
}

/**
 * Make a new histogram.
 */
value histogram_make_stub(value max)
{
	CAMLparam1(max);
	CAMLlocal1(h_val);
	struct histogram *h;

	h_val = caml_alloc_custom(&histogram_ops, sizeof(*h), 0, 1);
	/* As far as I can tell this will always return with
	 * success...? */

	h = Data_custom_val(h_val);
	histogram_create(h, Int_val(max));
	DPRINTF(DEBUG_STUBS, "Created histogram %p\n", h);

	CAMLreturn(h_val);
}

/**
 * Makes a new bins histogram of the given size.
 */
value histogram_make_bins_stub(value max_val, value start_val, value end_val)
{
	CAMLparam3(max_val, start_val, end_val);
	CAMLlocal1(h_val);
	struct histogram *h;

	h_val = caml_alloc_custom(&histogram_ops, sizeof(*h), 0, 1);
	h = Data_custom_val(h_val);

	histogram_create(h, Int_val(max_val));
	DPRINTF(DEBUG_STUBS, "Created histogram %p\n", h);

	bins_create(h, Double_val(start_val), Double_val(end_val));
	DPRINTF(DEBUG_STUBS, "Created bins %f-%f %p\n", Double_val(start_val),
		Double_val(end_val), h);

	CAMLreturn(h_val);
}


/* type kind variant numbers. */
#define NOTHING 0
#define POINTS 1
#define BINS 2

/**
 * Get the type of the histogram.
 */
value histogram_kind_stub(value h_val)
{
	CAMLparam1(h_val);
	CAMLlocal1(ret);
	struct histogram *h;

	h = Data_custom_val(h_val);
	if (h->type == HISTOGRAM_POINTS)
		ret = POINTS;
	else if (h->type == HISTOGRAM_BINS)
		ret = BINS;
	else
		ret = NOTHING;

	CAMLreturn(Val_int(ret));
}

value histogram_copy_stub(value h_val)
{
	CAMLparam1(h_val);
	CAMLlocal1(g_val);
	struct histogram *h = Data_custom_val(h_val);
	struct histogram *g;


	g_val = caml_alloc_custom(&histogram_ops, sizeof(*g), 0, 1);
	g = Data_custom_val(g_val);
	if (!histogram_copy(g, h)) {
		char error_string[LINE_MAX];
		strerror_r(errno, error_string, LINE_MAX);
		if (errno == EINVAL)
			caml_invalid_argument(error_string);
		else
			caml_failwith(error_string);
	}
	DPRINTF(DEBUG_STUBS, "Copied histogram %p to %p\n", h, g);

	CAMLreturn(g_val);
}

value histogram_add_stub(value a_val, value b_val)
{
	CAMLparam2(a_val, b_val);
	struct histogram *a = Data_custom_val(a_val);
	struct histogram *b = Data_custom_val(b_val);

	if (!histogram_add(a, b)) {
		char error_string[LINE_MAX];
		strerror_r(errno, error_string, LINE_MAX);
		if (errno == EINVAL)
			caml_invalid_argument(error_string);
		else
			caml_failwith(error_string);
	}
	DPRINTF(DEBUG_STUBS, "Added histograms %p and %p into %p\n", a, b, a);

	CAMLreturn(a_val);
}

value histogram_add_mass_stub(value vl_val, value wt_val, value h_val)
{
	CAMLparam3(vl_val, wt_val, h_val);
	double vl = Double_val(vl_val);
	double wt = Double_val(wt_val);
	struct histogram *h = Data_custom_val(h_val);

	if (!histogram_add_mass(h, vl, wt)) {
		char error_string[LINE_MAX];
		strerror_r(errno, error_string, LINE_MAX);
		if (errno == EINVAL)
			caml_invalid_argument(error_string);
		else
			caml_failwith(error_string);
	}
	DPRINTF(DEBUG_STUBS, "Added vl=%f, wt=%f to %p\n", vl, wt, h);

	CAMLreturn(Val_unit);
}

value histogram_total_weight_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	double mass;

	mass = histogram_total_mass(h);
	DPRINTF(DEBUG_STUBS, "Got mass of %p mass=%f\n", h, mass);

	CAMLreturn(caml_copy_double(mass));
}

value histogram_min_value_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	double min;

	min = histogram_min_value(h);
	DPRINTF(DEBUG_STUBS, "Got min of %p min=%f\n", h, min);

	CAMLreturn(caml_copy_double(min));
}

value histogram_max_value_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	double max;

	max = histogram_max_value(h);
	DPRINTF(DEBUG_STUBS, "Got max of %p max=%f\n", h, max);

	CAMLreturn(caml_copy_double(max));
}

value histogram_fill_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	unsigned int fill;

	if (h->type == HISTOGRAM_POINTS)
		fill = h->as.points.fill;
	else if (h->type == HISTOGRAM_BINS)
		fill = h->max;
	else
		fill = 0;

	CAMLreturn(Val_int(fill));
}

value histogram_size_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	CAMLreturn(Val_int(h->max));
}


value histogram_is_empty_stub(value h_val)
{
	CAMLparam1(h_val);
	struct histogram *h = Data_custom_val(h_val);
	int empty;

	empty = histogram_is_empty(h);
	DPRINTF(DEBUG_STUBS, "Testing empty of %p empty=%s\n", h,
		empty ? "true" : "false");

	CAMLreturn(Val_bool(empty));
}

value histogram_prune_above_stub(value b_val, value h_val)
{
	CAMLparam2(b_val, h_val);
	double bound = Double_val(b_val);
	struct histogram *h = Data_custom_val(h_val);

	histogram_prune_above(h, bound);
	DPRINTF(DEBUG_STUBS, "Pruned %p above %f\n", h, bound);

	CAMLreturn(Val_unit);
}

value histogram_scale_stub(value f_val, value h_val)
{
	CAMLparam2(f_val, h_val);
	double fact = Double_val(f_val);
	struct histogram *h = Data_custom_val(h_val);

	histogram_scale(h, fact);
	DPRINTF(DEBUG_STUBS, "Scaled %p by %f\n", h, fact);

	CAMLreturn(Val_unit);
}

value histogram_normalize_stub(value wt_val, value h_val)
{
	CAMLparam2(wt_val, h_val);
	double wt = Double_val(wt_val);
	struct histogram *h = Data_custom_val(h_val);

	histogram_normalize(h, wt);
	DPRINTF(DEBUG_STUBS, "Normalized %p to %f\n", h, wt);

	CAMLreturn(Val_unit);
}

value histogram_weight_left_of_stub(value vl_val, value h_val)
{
	CAMLparam2(vl_val, h_val);
	double vl = Double_val(vl_val);
	struct histogram *h = Data_custom_val(h_val);
	double wt;

	wt = histogram_weight_left_of(h, vl);
	DPRINTF(DEBUG_STUBS, "Weight in %p left of %f is %f\n", h, vl, wt);

	CAMLreturn(caml_copy_double(wt));
}

value histogram_val_for_weight_stub(value wt_val, value h_val)
{
	CAMLparam2(wt_val, h_val);
	double wt = Double_val(wt_val);
	struct histogram *h = Data_custom_val(h_val);
	double vl;

	vl = histogram_val_for_weight(h, wt);
	DPRINTF(DEBUG_STUBS, "Value in %p for weight %f is %f\n", h, wt, vl);

	CAMLreturn(caml_copy_double(vl));
}

value histogram_bound_for_wted_combo_stub(value desired_vl,
					  value a_vl, value b_vl,
					  value factor_vl)
{
	CAMLparam4(desired_vl, a_vl, b_vl, factor_vl);
	double bound, desired, factor;
	struct histogram *a, *b;
	bool error = false;

	desired = Double_val(desired_vl);
	factor = Double_val(factor_vl);
	a = Data_custom_val(a_vl);
	b = Data_custom_val(b_vl);

	bound = histogram_bound_for_wted_combo(&error, desired, a, b, factor);
	if (error) {
		char error_string[LINE_MAX];
		strerror_r(errno, error_string, LINE_MAX);
		if (errno == EINVAL)
			caml_invalid_argument(error_string);
		else
			caml_failwith(error_string);
	}

	CAMLreturn(caml_copy_double(bound));
}

value histogram_convolve_stub(value a_val, value b_val, value bound_val)
{
	CAMLparam3(a_val, b_val, bound_val);
	CAMLlocal1(c_val);
	double bound = Double_val(bound_val);
	struct histogram *a = Data_custom_val(a_val);
	struct histogram *b = Data_custom_val(b_val);
	struct histogram *c;


	/* Allocte a histogram for the result. */
	c_val = caml_alloc_custom(&histogram_ops, sizeof(*c), 0, 1);
	c = Data_custom_val(c_val);
	DPRINTF(DEBUG_STUBS, "Allocating result histogram for convolve %p\n",
		c);

	if (!histogram_convolve(c, a, b, bound)) {
		char error_string[LINE_MAX];
		strerror_r(errno, error_string, LINE_MAX);
		if (errno == EINVAL)
			caml_invalid_argument(error_string);
		else
			caml_failwith(error_string);
	}
	DPRINTF(DEBUG_STUBS, "Convolved %p and %p creating %p\n", a, b, c);

	CAMLreturn(c_val);
}

value histogram_set_debug_mask_stub(value mask_val)
{
	CAMLparam1(mask_val);

#if !defined(NDEBUG)
	unsigned long mask = Long_val(mask_val);

	debug_mask = mask;
	DPRINTF(DEBUG_STUBS, "Set debug mask to %0lx\n", mask);
#else
	caml_failwith("Debugging is compiled out");
#endif
	CAMLreturn(Val_unit);
}

value histogram_get_debug_mask_stub(value unit)
{
	CAMLparam1(unit);
#if !defined(NDEBUG)
	DPRINTF(DEBUG_STUBS, "Getting debug mask %0lx\n", debug_mask);

	CAMLreturn(Val_int(debug_mask));
#else
	caml_failwith("Debugging is compiled out");
	CAMLreturn(Val_unit);
#endif
}
