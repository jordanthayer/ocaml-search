/**
 * \file hist_bins.c
 *
 * Functions for operating on bins histograms.
 *
 * \author eaburns
 * \date 21-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "hist.h"
#include "hist_bins.h"
#include "debug.h"

void bins_sprinkle_mass(struct histogram *h, double min, double max, double wt)
{
	unsigned int i, first, last;
	double default_portion;
#if !defined(NDEBUG)
	double old_mass = bins_total_mass(h);
#endif /* !NDEBUG */

	if (wt == 0)
		return;

	DPRINTF(DEBUG_BINS, "Sprinkling wt=%f between min=%f and max=%f\n",
		wt, min, max);

	first = bin_index(h, min);
	last = bin_index(h, max);
	wt /= (max - min);
	default_portion = wt * h->as.bins.width;

	for (i = first; i <= last; i += 1) {
		if (i == first || i == last) {
			double portion_l, portion_r, bin_min, bin_max;
			bin_min = bin_start(h, i);
			bin_max = bin_min + h->as.bins.width;
			portion_l = min > bin_min ? min : bin_min;
			portion_r = max < bin_max ? max : bin_max;
			h->as.bins.bins[i] += wt * (portion_r - portion_l);
			DPRINTF(DEBUG_BINS,
				"Adding wt=%lf to bin %d (%lf - %lf)\n",
				wt * (portion_r - portion_l), i,
				bin_start(h, i), bin_end(h, i));
		} else {
			h->as.bins.bins[i] += default_portion;
			DPRINTF(DEBUG_BINS,
				"Adding wt=%lf to bin %d (%lf - %lf)\n",
				default_portion, i, bin_start(h, i),
				bin_end(h, i));
		}
	}
	DPRINTF(DEBUG_BINS, "Total wt=%lf\n", bins_total_mass(h));

	DPRINTF(DEBUG_BINS, "old mass=%lf, new mass=%lf\n",
		old_mass, bins_total_mass(h));
}

/**
 * Grows the histogram on the left side to fit 'vl'.
 *
 * This is a direct translation of Wheelers histogram growth code.
 */
static void bins_grow_left(struct histogram *h, double vl)
{
	int src, dest, next;
	double h_max = bins_end(h);
	double h_min = bins_start(h);
	double old_wn = h->as.bins.bins[h->max - 1];
	double f = ceil((h_max - vl) / (h_max - h_min));
#if !defined(NDEBUG)
	double old_mass = histogram_total_mass(h);
#endif	/* !NDEBUG */

	/* Stupid rounding errors. */
	if (f == 1) f += 0.00001;
	assert (f > 1);

	dest = src = h->max - 1;
	while(src >= 0) {
		next = (src - f) > -1 ? (src - f) : -1;
		h->as.bins.bins[dest] = 0.;
		while (src > next) {
			h->as.bins.bins[dest] += h->as.bins.bins[src];
			src -= 1;
		}
		dest -= 1;
	}
	h->as.bins.bins[h->max - 1] += old_wn;
	for (; dest >= 0; dest -= 1)
		h->as.bins.bins[dest] = 0;

	h->as.bins.width *= f;
	h->as.bins.base = h_max - (h->max - 1) * h->as.bins.width;

	assert(bins_start(h) <= vl);
	assert(bins_end(h) >= h_max);

	SANITY_CHECK(effectively_equal(bins_total_mass(h), old_mass));
}


/**
 * Grows the histogram on the right side to fit 'vl'.
 *
 * This is a direct translation of Wheelers histogram growth code.
 */
static void bins_grow_right(struct histogram *h, double vl)
{
	unsigned int src, dest, next, n;
	double old_w0 = h->as.bins.bins[0];
	double f = ceil((vl - h->as.bins.base)
			/ (bins_end(h) - h->as.bins.base));
#if !defined(NDEBUG)
	double old_mass = bins_total_mass(h);
	double h_min = bins_start(h);
#endif	/* !NDEBUG */

	/* Stupid rounding errors. */
	if (f == 1) f += 0.00001;
	assert (f > 1);

	n = h->max - 1;
	dest = src = 0;
	while(src < h->max) {
		next = (src + f) < h->max ? (src + f) : h->max;
		h->as.bins.bins[dest] = 0;
		while (src < next) {
			DPRINTF(DEBUG_BINS,
				"Adding %f from %d to %d\n",
				h->as.bins.bins[src], src, dest);
			assert(dest <= n);
			assert(dest >= 0);
			h->as.bins.bins[dest] += h->as.bins.bins[src];
			src += 1;
		}
		dest += 1;
	}
	h->as.bins.bins[0] += old_w0;
	for (; dest < h->max; dest += 1)
		h->as.bins.bins[dest] = 0;

	h->as.bins.width *= f;

	assert(bins_end(h) >= vl);
	assert(bins_start(h) <= h_min);

	SANITY_CHECK(effectively_equal(bins_total_mass(h), old_mass));
}

/**
 * Grows the histogram to the right to accomidate 'max'.
 */
static inline  void bins_ensure_size(struct histogram *h, double min,
				     double max)
{
	if (max > bins_end(h)) {
		DPRINTF(DEBUG_BINS, "Growing right: max=%f\n", max);
		bins_grow_right(h, max);
	}
	if (min < bins_start(h)) {
		DPRINTF(DEBUG_BINS, "Growing left: min=%f\n", min);
		bins_grow_left(h, min);
	}

	assert(bins_start(h) <= min);
	assert(bins_end(h) >= max);
}

/**
 * Ensure the value fits in the histogram.
 */
static inline void bins_ensure_value(struct histogram *h, double vl)
{
	if (vl > bins_end(h))
		bins_grow_right(h, vl);
	else if (vl < bins_start(h))
		bins_grow_left(h, vl);

	assert(bins_start(h) <= vl);
	assert(bins_end(h) >= vl);
}

/**
 * Sprinkle the weight from a set of bins over the histogram.
 */
static void bins_sprinkle_bins(struct histogram *h, double base,
			       double width, unsigned int num,
			       double *bins)
{
	unsigned int i;

	for (i = 0; i < num; i += 1) {
		double start = base + (width * i);
		double end = start + width;
		if (bins[i] > 0)
			bins_ensure_size(h, start, end);
		bins_sprinkle_mass(h, start, end, bins[i]);
	}
}


bool bins_create(struct histogram *h, double min, double max)
{
	double *bins;
#if !defined(NDEBUG)
	double orig_max = max;
#endif	/* !NDEBUG */

	if (max <= min) {
		/* This is a bit of a hack. */
		max = (max + 1) * 2;
		min = min - max;
		if (max < min) {
			/* If the values were negative, we need to
			 * swap them. */
			double t = max;
			max = min;
			min = t;
		}
		DPRINTF(DEBUG_BINS,
			"re-fitting min and max to min=%f, max=%f\n",
			min, max);
	}

	if (h->max < 1) {
		DPRINTF(DEBUG_BINS, "Must have at least 1 bins\n");
		errno = EINVAL;
		return false;
	}

	bins = calloc(h->max, sizeof(*h->as.bins.bins));
	if (!bins)
		return false;

	/* Wheeler does this to pad out max a bit. */
	max += ((max - min) / 1000000.) + 0.00001;

	h->type = HISTOGRAM_BINS;
	h->as.bins.bins = bins;
	h->as.bins.width = (max - min) / (h->max);
	h->as.bins.base = min;
	assert(bins_start(h) <= min);
	assert(bins_end(h) >= orig_max);
	DPRINTF(DEBUG_BINS, "min=%f, max=%f, base=%f, width=%f\n",
		min, max, h->as.bins.base, h->as.bins.width);

	return true;
}


void bins_free(struct histogram *h)
{
	assert(h);
	assert(h->type == HISTOGRAM_BINS);
	free(h->as.bins.bins);
}


bool bins_copy(struct histogram *to, struct histogram *from)
{
	double *bins;

	assert(from);
	assert(from->type == HISTOGRAM_BINS);

	bins = malloc(from->max * sizeof(*bins));
	if (!bins)
		return false;
	memcpy(bins, from->as.bins.bins, from->max * sizeof(*bins));

	to->type = HISTOGRAM_BINS;
	to->as.bins.width = from->as.bins.width;
	to->as.bins.base = from->as.bins.base;
	to->as.bins.bins = bins;

	DPRINTF(DEBUG_BINS, "Total wt=%f\n", bins_total_mass(to));

	return true;
}


bool bins_add_mass(struct histogram *h, double vl, double wt)
{
	unsigned int index;
#if !defined(NDEBUG)
	double start, end;
#endif	/* !NDEBUG */

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	if (wt == 0)
		return true;

	bins_ensure_value(h, vl);

	index = bin_index(h, vl);
#if !defined(NDEBUG)
	start = bin_start(h, index);
	end = bin_end(h, index);
#endif	/* !NDEBUG */
	DPRINTF(DEBUG_BINS, "Adding wt=%f to bin=%d (%f - %f)\n",
		wt, index, start, end);
	h->as.bins.bins[index] += wt;

	DPRINTF(DEBUG_BINS, "Total wt=%f\n", bins_total_mass(h));

	return true;
}


double bins_prune_above(struct histogram *h, double bound)
{
	unsigned int i, p;
	double pruned = 0;

	if (bound < h->as.bins.base) {
		DPRINTF(DEBUG_BINS | DEBUG_PRUNE, "Clamping to base.\n");
		bound = h->as.bins.base;
		p = 0;
	}
	p = bin_index(h, bound);
	for (i = p + 1; i < h->max; i += 1) {
		DPRINTF(DEBUG_BINS | DEBUG_PRUNE,
			"Pruning bin %d (%f - %f) wt=%f\n",
			i, bin_start(h, i), bin_end(h, i),
			h->as.bins.bins[i]);
		pruned += h->as.bins.bins[i];
		h->as.bins.bins[i] = 0;
	}

	DPRINTF(DEBUG_BINS | DEBUG_PRUNE, "Pruned wt=%f\n", pruned);
	DPRINTF(DEBUG_BINS | DEBUG_PRUNE , "Total wt=%f\n",
		bins_total_mass(h));

	return pruned;
}

double bins_total_mass(struct histogram *h)
{
	unsigned int i;
	double accum = 0;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	for (i = 0; i < h->max; i += 1)
		accum += h->as.bins.bins[i];

	return accum;
}

bool bins_is_empty(struct histogram *h)
{
	unsigned int i;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	for (i = 0; i < h->max; i += 1)
		if (h->as.bins.bins[i] != 0)
			return false;

	return true;
}

bool bins_add_bins(struct histogram *a, struct histogram *b)
{
	assert(a);
  assert(a->type == HISTOGRAM_BINS);
  assert(b);
	assert(b->type == HISTOGRAM_BINS);

	bins_sprinkle_bins(a, b->as.bins.base, b->as.bins.width,
			   b->max, b->as.bins.bins);

	return true;
}

void bins_scale(struct histogram *h, double fact)
{
	unsigned int i;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	DPRINTF(DEBUG_BINS, "Scaling by %f\n", fact);

	for (i = 0; i < h->max; i += 1)
		h->as.bins.bins[i] *= fact;

	DPRINTF(DEBUG_BINS, "Total wt=%f\n", bins_total_mass(h));
}


double bins_weight_left_of(struct histogram *h, double vl)
{
	double accum = 0;
	double frac;
	unsigned int i, bin;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	DPRINTF(DEBUG_BINS, "Finding weight for vl=%f\n", vl);

	if (vl > bins_end(h)) {
		double total = histogram_total_mass(h); /* all of it */
		DPRINTF(DEBUG_BINS, "All Weight=%f\n", total);
		return total;
	}
	if (vl < bins_start(h)) {
		DPRINTF(DEBUG_BINS, "No Weight=%f\n", 0.0);
		return 0;
	}

	bin = bin_index(h, vl);
	for (i = 0; i < bin; i += 1) {
		accum += h->as.bins.bins[i];
	}

	frac = (vl - bin_start(h, bin)) / h->as.bins.width;
	accum += h->as.bins.bins[bin] * frac;
	DPRINTF(DEBUG_BINS, "Wt in final bin=%f, frac=%f\n",
		h->as.bins.bins[bin], frac);

	DPRINTF(DEBUG_BINS, "Some Weight=%f\n", accum);

	return accum;
}


double bins_val_for_weight(struct histogram *h, double wt)
{
	double accum = 0;
	double frac, bound;
	unsigned int i;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	DPRINTF((DEBUG_BOUNDS|DEBUG_BINS), "Finding value for wt=%f\n", wt);
	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "min=%f\n", bins_start(h));
	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "max=%f\n", bins_end(h));

	for (i = 0; i < h->max; i += 1) {
		if (accum + h->as.bins.bins[i] > wt)
			break;
		accum += h->as.bins.bins[i];
	}

	if (i == h->max)
		return INFINITY;

	frac = (wt - accum) / h->as.bins.bins[i];
	bound = bin_start(h, i) + (frac * h->as.bins.width);

	DPRINTF((DEBUG_BOUNDS|DEBUG_BINS), "Bound=%f\n", bound);

	return bound;
}

void bins_output(FILE *out, struct histogram *h)
{
	unsigned int i;

	assert(h);
	assert(h->type == HISTOGRAM_BINS);

	for (i = 0; i < h->max; i += 1) {
		double wt = h->as.bins.bins[i];
		if (wt != 0) {
			double start = bin_start(h, i);
			double end = bin_end(h, i);
			fprintf(out, "%03d: %f (%f - %f)\n", i, wt,
				start, end);
		}
	}
	fprintf(out, "bin width:  %f\n", h->as.bins.width);
	fprintf(out, "total mass: %f\n", bins_total_mass(h));
}

double bins_min_value(struct histogram *h)
{
	unsigned int i;

	for (i = 0; i < h->max; i += 1) {
		if (h->as.bins.bins[i] != 0)
			return bin_start(h, i);
	}

	return INFINITY;
}

double bins_max_value(struct histogram *h)
{
	unsigned int i;

	if (!h->as.bins.bins)
		return -INFINITY;

	for (i = h->max - 1; i >= 0; i -= 1) {
		if (h->as.bins.bins[i] != 0)
			return bin_end(h, i);
		if (i == 0)
			break;
	}

	return -INFINITY;
}
