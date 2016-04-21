/**
 * \file hist.c
 *
 * Functions that operate on both types of histograms (points and
 * bins).
 *
 * \author eaburns
 * \date 20-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "hist.h"
#include "hist_points.h"
#include "hist_bins.h"
#include "debug.h"

/**
 * Add points to a bins histogram given the array of points that need
 * to be added.
 */
static bool bins_add_point_array(struct histogram *h,
				 struct histogram_pt **ary,
				 unsigned int num)
{
	unsigned int i;

	for (i = 0; i < num; i += 1) {
		DPRINTF(DEBUG_CONVERT, "Adding vl=%f, wt=%f\n",
			ary[i]->vl, ary[i]->wt);
		if (!bins_add_mass(h, ary[i]->vl, ary[i]->wt))
			return false;
	}

	return true;
}

/**
 * Convert a points histogram to a bins histogram.
 *
 *
 */
static bool convert_to_bins(struct histogram *h)
{
	struct histogram bins;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);
	assert(!points_is_empty(h));
	assert(h->as.points.fill <= h->max);

	if (h->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_CONVERT, "h has the incorrect type\n");
		goto inval;
	} else if (points_is_empty(h)) {
		DPRINTF(DEBUG_CONVERT, "h is empty\n");
		goto inval;
	}

	DPRINTF(DEBUG_CONVERT, "Converting to points\n");
	bins.max = h->max;
	bins.total = h->total;
	if (!bins_create(&bins, points_min_value(h), points_max_value(h)))
		return false;

	ary = points_array(h, false, false);
	if (!bins_add_point_array(&bins, ary, h->as.points.fill))
		return false;

	/* Free the points and copy over the histogram with the new
	 * bins histogram. */
	points_free(h);
	*h = bins;

	return true;
inval:
	errno = EINVAL;
	return false;
}

/**
 * Add histogram 'b' to histogram 'a'.  Returns true on success and
 * false on error.
 */
static bool bins_add_points(struct histogram *a, struct histogram *b)
{
	struct histogram_pt **ary;

	assert(a);
	assert(b);

	if (a->type != HISTOGRAM_BINS) {
		DPRINTF(DEBUG_ADD, "a has the incorrect type\n");
		goto inval;
	} else if (b->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_ADD, "b has the incorrect type\n");
		goto inval;
	}

	ary = points_array(b, false, false);
	if (!ary)
		return false;
	if (!bins_add_point_array(a, ary, b->as.points.fill))
		return false;

	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}

/**
 * Add histogram 'b' to histogram 'a'.  Returns true on success and
 * false on error.
 */
static bool points_add_points(struct histogram *a, struct histogram *b)
{
	unsigned int i;
	struct histogram_pt **ary;

	assert(a);
	assert(b);

	if (a->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_ADD, "a has the incorrect type\n");
		goto inval;
	} else if (b->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_ADD, "b has the incorrect type\n");
		goto inval;
	}

	if (points_is_empty(b)) {
		DPRINTF(DEBUG_ADD, "Adding an empty histogram\n");
		return true;
	}

	ary = points_array(b, false, false);
	if (!ary)
		return false;
	for (i = 0; i < b->as.points.fill; i += 1) {
		bool success;
		if (points_is_full(a)) {
			DPRINTF(DEBUG_ADD, "Converting to bins\n");
			success = convert_to_bins(a);
			if (!success)
				return false;
			DPRINTF(DEBUG_ADD,
				"Continuing add from ary+%d=%p, len=%d\n",
				i, ary + i, b->as.points.fill - i);
			return bins_add_point_array(a, ary + i,
						    b->as.points.fill - i);
		}
		DPRINTF(DEBUG_CONVERT, "Adding vl=%f, wt=%f\n",
			ary[i]->vl, ary[i]->wt);
		success = points_add_mass(a, ary[i]->vl, ary[i]->wt);
		if (!success)
			return false;
	}

	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}


void histogram_create(struct histogram *h, unsigned int max)
{
	h->type = HISTOGRAM_NONE;
	h->max = max;
	h->total = 0.0;
}


void histogram_free(struct histogram *h)
{
	assert(h);

	switch(h->type) {
	case HISTOGRAM_POINTS:
		points_free(h);
		break;
	case HISTOGRAM_BINS:
		bins_free(h);
		break;
	case HISTOGRAM_NONE:
		/* Do nothing */
		break;
	}
}

bool histogram_add(struct histogram *a, struct histogram *b)
{
	bool success = false;

	if (!a) {
		DPRINTF(DEBUG_ADD, "a is NULL\n");
		goto inval;
	} else if (!b) {
		DPRINTF(DEBUG_ADD, "b is NULL\n");
		goto inval;
	}

	DPRINTF(DEBUG_ADD, "Adding b %p wt=%f min=%f to a %p wt=%f min=%f\n",
		b, b->total, histogram_min_value(b),
		a, a->total, histogram_min_value(a));

	if(histogram_is_empty(a)) {
		DPRINTF(DEBUG_ADD, "Adding an empty histogram: copy\n");
		return histogram_copy(a, b);
	}

	if(histogram_is_empty(b)) {
		DPRINTF(DEBUG_ADD, "Adding an empty histogram: noop\n");
		return true;
	}

	switch(a->type) {
	case HISTOGRAM_POINTS:
		switch (b->type) {
		case HISTOGRAM_POINTS:
			DPRINTF(DEBUG_ADD, "Adding points and points\n");
			success = points_add_points(a, b);
			break;
		case HISTOGRAM_BINS:
			DPRINTF(DEBUG_ADD, "Converting to bins\n");
			success = convert_to_bins(a);
			if (success) {
				DPRINTF(DEBUG_ADD, "Adding bins and bins\n");
				success = bins_add_bins(a, b);
			}
			break;
		default:
			DPRINTF(DEBUG_ADD,
				"Not adding since b is not-created\n");
			return true;
		}
		break;
	case HISTOGRAM_BINS:
		switch (b->type) {
		case HISTOGRAM_POINTS:
			DPRINTF(DEBUG_ADD, "Adding bins and points\n");
			success = bins_add_points(a, b);
			break;
		case HISTOGRAM_BINS:
			DPRINTF(DEBUG_ADD, "Adding bins and bins\n");
			success = bins_add_bins(a, b);
			break;
		default:
			DPRINTF(DEBUG_ADD,
				"Not adding since b is not-created\n");
			return true;
		}
		break;
	case HISTOGRAM_NONE:
		/* This case cannot occur because the 'a' histogram
		 * would be empty and a copy of 'b' would have been
		 * returned above. */
		assert (false);
	}

	if (!success)
		return false;

	a->total += b->total;

	histogram_check_total_mass(a);
	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}

bool histogram_add_mass(struct histogram *h, double vl, double wt)
{
	bool success = false;

	if (!h) {
		DPRINTF(DEBUG_ADD, "h is NULL\n");
		errno = EINVAL;
		return false;
	}

	switch(h->type) {
	case HISTOGRAM_POINTS:
		if (points_is_full(h)) {
			success = convert_to_bins(h);
			if (!success)
				return false;
			return histogram_add_mass(h, vl, wt);
		} else {
			success = points_add_mass(h, vl, wt);
		}
		break;
	case HISTOGRAM_BINS:
		success = bins_add_mass(h, vl, wt);
		break;
	case HISTOGRAM_NONE:
		if (!points_create(h))
			return false;
		return histogram_add_mass(h, vl, wt);
	}

	if (!success)
		return false;

	h->total += wt;
	histogram_check_total_mass(h);

	return true;
}

double histogram_compute_total_mass(struct histogram *h)
{
	switch(h->type) {
	case HISTOGRAM_POINTS:
		return points_total_mass(h);
		break;
	case HISTOGRAM_BINS:
		return bins_total_mass(h);
		break;
	case HISTOGRAM_NONE:
		return 0;
	}

	NOT_REACHABLE();
}

double histogram_min_value(struct histogram *h)
{
	switch(h->type) {
	case HISTOGRAM_POINTS:
		return points_min_value(h);
		break;
	case HISTOGRAM_BINS:
		return bins_min_value(h);
		break;
	case HISTOGRAM_NONE:
		return INFINITY;
	}

	NOT_REACHABLE();
}

double histogram_max_value(struct histogram *h)
{
	switch(h->type) {
	case HISTOGRAM_POINTS:
		return points_max_value(h);
		break;
	case HISTOGRAM_BINS:
		return bins_max_value(h);
		break;
	case HISTOGRAM_NONE:
		return -INFINITY;
	}

	NOT_REACHABLE();
}

/*
 * Instead of this, just check the total weight in a small static inline.
 */
/*
bool histogram_is_empty(struct histogram *h)
{
	switch(h->type) {
	case HISTOGRAM_POINTS:
		return points_is_empty(h);
		break;
	case HISTOGRAM_BINS:
		return bins_is_empty(h);
		break;
	case HISTOGRAM_NONE:
		return true;
	}

	NOT_REACHABLE();
}
*/

void histogram_prune_above(struct histogram *h, double bound)
{
	double pruned_wt = 0;

	if (h->type == HISTOGRAM_NONE || bound > histogram_max_value(h))
		return;

	switch(h->type) {
	case HISTOGRAM_POINTS:
		/* don't prune equal to the bound. */
		pruned_wt = points_prune_above(h, bound, false);
		break;
	case HISTOGRAM_BINS:
		pruned_wt = bins_prune_above(h, bound);
		break;
	case HISTOGRAM_NONE:
		NOT_REACHABLE();
	}

	h->total -= pruned_wt;
	histogram_check_total_mass(h);
}


bool histogram_copy(struct histogram *to, struct histogram *from)
{
	to->max = from->max;
	to->total = from->total;

	DPRINTF(DEBUG_COPY, "Copying %p to %p\n", from, to);

	switch (from->type) {
	case HISTOGRAM_POINTS:
		DPRINTF(DEBUG_COPY, "Copying points\n");
		return points_copy(to, from);
	case HISTOGRAM_BINS:
		DPRINTF(DEBUG_COPY, "Copying bins\n");
		return bins_copy(to, from);
	case HISTOGRAM_NONE:
		DPRINTF(DEBUG_COPY,
			"Copying none (setting type to HISTOGRAM_NONE)\n");
		to->type = HISTOGRAM_NONE;
		break;
	}

	histogram_check_total_mass(to);
	histogram_check_total_mass(from);

	return true;
}

void histogram_scale(struct histogram *h, double fact)
{
	switch (h->type) {
	case HISTOGRAM_POINTS:
		points_scale(h, fact);
		break;
	case HISTOGRAM_BINS:
		bins_scale(h, fact);
		break;
	case HISTOGRAM_NONE:
		/* Do nothing. */
		break;
	}

	h->total *= fact;
	histogram_check_total_mass(h);
}

void histogram_normalize(struct histogram *h, double wt)
{
	double total_wt;

	total_wt = histogram_total_mass(h);
	histogram_scale(h, wt / total_wt);
	h->total = wt;
	histogram_check_total_mass(h);
}

double histogram_weight_left_of(struct histogram *h, double vl)
{
	double wt = 0;

	switch (h->type) {
	case HISTOGRAM_POINTS:
		wt = points_weight_left_of(h, vl, true);
		break;
	case HISTOGRAM_BINS:
		wt = bins_weight_left_of(h, vl);
		break;
	case HISTOGRAM_NONE:
		/* Do nothing. */
		break;
	}

	return wt;
}

double histogram_val_for_weight(struct histogram *h, double wt)
{
	double bound = INFINITY;

	switch (h->type) {
	case HISTOGRAM_POINTS:
		DPRINTF(DEBUG_BOUNDS, "Finding bound for points\n");
		bound = points_val_for_weight(h, wt, true);
		break;
	case HISTOGRAM_BINS:
		DPRINTF(DEBUG_BOUNDS, "Finding bound for bins\n");
		bound = bins_val_for_weight(h, wt);
		break;
	case HISTOGRAM_NONE:
		DPRINTF(DEBUG_BOUNDS, "Finding bound for none\n");
		/* Do nothing. */
		break;
	}

	return bound;
}

/* TODO: Implement this more efficiently than copying histograms
 * everywhere. */
double histogram_bound_for_wted_combo(bool *error,
				      double desired,
				      struct histogram *a,
				      struct histogram *b,
				      double factor)
{
	struct histogram accum, scaled;
	double bound;

	*error = false;

	if (!histogram_copy(&accum, a))
		return EXIT_FAILURE;
	if (!histogram_copy(&scaled, b))
		return EXIT_FAILURE;

	histogram_scale(&scaled, factor);

	if (!histogram_add(&accum, &scaled)) {
		*error = true;
		return NAN;
	}

	bound = histogram_val_for_weight(&accum, desired);

	histogram_free(&accum);
	histogram_free(&scaled);

	return bound;
}

/**
 * Makes a histogram that will hold the result of a convolution of a
 * and b.
 */
static bool create_convolution_result(struct histogram *res,
				      struct histogram *a,
				      struct histogram *b)
{
	double amin, bmin, amax, bmax, min, max;

	amin = histogram_min_value(a);
	amax = histogram_max_value(a);
	bmin = histogram_min_value(b);
	bmax = histogram_max_value(b);
	min = amin + bmin;
	max = amax + bmax;
	DPRINTF(DEBUG_CONVOLVE, "creating result: min=%f, max=%f\n", min, max);
	if (!bins_create(res, min, max)) {
		unsigned int e = errno;
		DPRINTF(DEBUG_CONVOLVE, "creating bins failed\n");
		errno = e;
		return false;
	}
	return true;
}

/**
 * Convolve two points histograms.
 */
static bool convolve_points(struct histogram *res, struct histogram *a,
			    struct histogram *b, double bound)
{
	unsigned int i, j;
	bool asorted, bsorted;
	struct histogram_pt **aary, **bary;

	assert(a);
	assert(b);

	if (a->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_CONVOLVE, "a has the incorrect type\n");
		goto inval;
	} else if (b->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_CONVOLVE, "b has the incorrect type\n");
		goto inval;
	}

	asorted = a->as.points.ary_state == POINTS_ARY_SORTED;
	bsorted = b->as.points.ary_state == POINTS_ARY_SORTED;
	histogram_create(res, a->max > b->max ? a->max : b->max);
	if (a->as.points.fill * b->as.points.fill > a->max) {
		DPRINTF(DEBUG_CONVOLVE, "Result is bins\n");
		if (!create_convolution_result(res, a, b))
			return false;
	} else {
		DPRINTF(DEBUG_CONVOLVE, "Result is points\n");
		if (!points_create(res))
			return false;
	}

	aary = points_array(a, false, false);
	bary = points_array(b, false, false);
	for (i = 0; i < a->as.points.fill; i += 1) {
		double avl = aary[i]->vl;
		double awt = aary[i]->wt;
		if (avl > bound) {
			if (asorted) {
				DPRINTF(DEBUG_CONVOLVE,
					"Sorted, stoping avl=%f\n", avl);
				break;
			} else {
				DPRINTF(DEBUG_CONVOLVE,
					"Unsorted, skipping avl=%f\n", avl);
				continue; /* not sorted, don't break. */
			}
		}
		assert(awt != 0);
		for (j = 0; j < b->as.points.fill; j += 1) {
			double bvl = bary[j]->vl;
			double bwt = bary[j]->wt;
			double wt = awt * bwt;
			double vl = avl + bvl;
			if (vl > bound) {
				if (bsorted) {
					DPRINTF(DEBUG_CONVOLVE,
						"Sorted, stoping bvl=%f\n",
						bvl);
					break;
				} else {
					DPRINTF(DEBUG_CONVOLVE,
						"Unsorted, skipping bvl=%f\n",
						bvl);
					continue;
				}
			}
			if (wt == 0)
				continue;
			DPRINTF(DEBUG_CONVOLVE,
				"avl=%f, awt=%f, bvl=%f, bvl=%f\n",
				avl, awt, bvl, bwt);
			DPRINTF(DEBUG_CONVOLVE, "Adding mass vl=%f, wt=%f\n",
				vl, wt);
			if (!histogram_add_mass(res, vl, wt)) {
				bins_free(res);
				return false;
			}
		}
	}

	histogram_check_total_mass(res);
	histogram_check_total_mass(a);
	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}

/**
 * Convolve two bins histograms.
 */
static bool convolve_bins(struct histogram *res, struct histogram *a,
			       struct histogram *b, double bound)
{
	unsigned int i, j;

	assert(a);
	assert(b);

	if (a->type != HISTOGRAM_BINS) {
		DPRINTF(DEBUG_CONVOLVE, "a has the incorrect type\n");
		goto inval;
	} else if (b->type != HISTOGRAM_BINS) {
		DPRINTF(DEBUG_CONVOLVE, "b has the incorrect type\n");
		goto inval;
	}

	histogram_create(res, a->max > b->max ? a->max : b->max);
	if (!create_convolution_result(res, a, b))
		return false;

	/* This is mostly a copy of whatever Wheeler did in his ML
	 * histogram. */
	for (i = 0; i < a->max; i += 1) {
		double awt = a->as.bins.bins[i];
		double aleft = bin_start(a, i);
		double shift = aleft + (a->as.bins.width / 2);
		if (shift > bound)
			break;
		if (awt == 0)
			continue;
		for (j = 0; j < b->max; j += 1) {
			double bwt = b->as.bins.bins[j];
			double bleft = bin_start(b, j);
			double left = bleft + shift;
			double right = left + b->as.bins.width;
			double wt = awt * bwt;
			if (wt == 0)
				continue;
			assert (left <= bins_end(res));
			assert (right <= bins_end(res));
			if (left > bound)
				break;
			if (right > bound) {
				/* Prune some weight */
				right = bound;
				wt *= (bound - left) / b->as.bins.width;
			}
			DPRINTF(DEBUG_CONVOLVE,
				"Sprinkling i=%d, j=%d, wt=%f (%f - %f)\n",
				i, j, wt, left, right);
			bins_sprinkle_mass(res, left, right, wt);
			res->total += wt;
		}
	}

	histogram_check_total_mass(res);
	histogram_check_total_mass(a);
	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}


/**
 * Convolve a points histograms (a) with a bins histogram (b).
 */
static bool convolve_points_bins(struct histogram *res, struct histogram *a,
				 struct histogram *b, double bound)
{
	unsigned int i, j;
	struct histogram_pt **aary;

	assert(a);
	assert(b);

	if (a->type != HISTOGRAM_POINTS) {
		DPRINTF(DEBUG_CONVOLVE, "a has the incorrect type\n");
		goto inval;
	} else if (b->type != HISTOGRAM_BINS) {
		DPRINTF(DEBUG_CONVOLVE, "b has the incorrect type\n");
		goto inval;
	}

	histogram_create(res, a->max > b->max ? a->max : b->max);
	if (!create_convolution_result(res, a, b))
		return false;

	aary = points_array(a, false, false);
	if (!aary)
		return false;

	/* This is mostly a copy of whatever Wheeler did in his ML
	 * histogram. */
	for (i = 0; i < a->as.points.fill; i += 1) {
		double avl = aary[i]->vl;
		double awt = aary[i]->wt;
		if (avl > bound) {
			if (a->as.points.ary_state == POINTS_ARY_SORTED) break;
			else continue;
		}
		if (awt == 0)
			continue;
		for (j = 0; j < b->max; j += 1) {
			double bwt, wt, left, right;
			bwt = b->as.bins.bins[j];
			wt = awt * bwt;
			if (wt == 0)
				continue;
			left = bin_start(b, j) + avl;
			right = left + b->as.bins.width;
			assert (left <= bins_end(res));
			assert (right <= bins_end(res));
			if (left > bound)
				break;
			if (right > bound) {
				/* Prune some weight */
				right = bound;
				wt *= (bound - left) / b->as.bins.width;
			}
			DPRINTF(DEBUG_CONVOLVE,
				"Sprinkling i=%d, j=%d, wt=%f (%f - %f)\n",
				i, j, wt, left, right);
			bins_sprinkle_mass(res, left, right, wt);
			res->total += wt;
		}
	}

	histogram_check_total_mass(res);
	histogram_check_total_mass(a);
	histogram_check_total_mass(b);

	return true;
inval:
	errno = EINVAL;
	return false;
}

bool histogram_convolve(struct histogram *res, struct histogram *a,
			struct histogram *b, double bound)
{
	bool success = false;

	if (!a) {
		DPRINTF(DEBUG_CONVOLVE, "a is NULL\n");
		goto inval;
	} else if (!b) {
		DPRINTF(DEBUG_CONVOLVE, "b is NULL\n");
		goto inval;
	}

	DPRINTF(DEBUG_CONVOLVE, "Convolving with bound=%f\n", bound);

	/*
	 * Convolution when some histograms are empty either makes an
	 * empty histogram (if both a and b are empty) or makes a copy
	 * of the non-empty histogram.
	 */
	if (histogram_is_empty(a) && histogram_is_empty(b)) {
		DPRINTF(DEBUG_CONVOLVE, "Convolving empty histograms makes "
			"an empty histogram\n");
		histogram_create(res, a->max);
		return true;
	} else if (histogram_is_empty(a)) {
		DPRINTF(DEBUG_CONVOLVE, "Convolving an empty histogram "
			"make a copy of b\n");
		histogram_copy(res, b);
		return true;
	} else if (histogram_is_empty(b)) {
		DPRINTF(DEBUG_CONVOLVE, "Convolving an empty histogram "
			"make a copy of a\n");
		histogram_copy(res, a);
		return true;
	}

	switch(a->type) {
	case HISTOGRAM_POINTS:
		switch (b->type) {
		case HISTOGRAM_POINTS:
			success = convolve_points(res, a, b, bound);
			break;
		case HISTOGRAM_BINS:
			success = convolve_points_bins(res, a, b, bound);
			break;
		case HISTOGRAM_NONE:
			/* This would have been caught as an empty
			 * case. */
			NOT_REACHABLE();
		}
		break;
	case HISTOGRAM_BINS:
		switch (b->type) {
		case HISTOGRAM_POINTS:
			success = convolve_points_bins(res, b, a, bound);
			break;
		case HISTOGRAM_BINS:
			success = convolve_bins(res, a, b, bound);
			break;
		case HISTOGRAM_NONE:
			/* This would have been caught as an empty
			 * case. */
			NOT_REACHABLE();
		}
		break;
	case HISTOGRAM_NONE:
		/* This would have been caught as an empty
		 * case. */
		NOT_REACHABLE();
	}

	DPRINTF(DEBUG_CONVOLVE, "Final weight=%f\n",
		histogram_total_mass(res));

	return success;

inval:
	errno = EINVAL;
	return false;
}

void histogram_output(FILE *out, struct histogram *h)
{
	switch(h->type) {
	case HISTOGRAM_POINTS:
		points_output(out, h);
		break;
	case HISTOGRAM_BINS:
		bins_output(out, h);
		break;
	case HISTOGRAM_NONE:
		fprintf(out, "Not yet created\n");
		break;
	}
}
