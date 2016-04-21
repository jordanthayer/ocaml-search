/**
 * \file hist_points.c
 *
 * Functions dealing only with the points histogram.  These functions
 * should never be called with anything except a points histogram.
 * Functions that are advertised in the header file should assert
 * this.  No need for non-debug time testing because it *should* never
 * happen.
 *
 * \author eaburns
 * \date 20-12-2009
 */

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <errno.h>
#include <math.h>		/* NAN */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "hist.h"
#include "hist_points.h"
#include "debug.h"

/* By how much should the hash table over allocate (to be sparse) */
#define OVER_ALLOC_FACTOR 3

/* Get the maximum size of the hash table. */
#define TBL_MAX(h) ((h)->max * OVER_ALLOC_FACTOR)

/**
 * Get the hash index for the given histogram value.  The hash value
 * is just some made up hack based on the bits of the float.
 */
static inline unsigned int hash_value(struct histogram *h, double vl)
{
	union { uint64_t i; double d; } cast_union;
	unsigned int hash;

	cast_union.d = vl;
	hash = cast_union.i;
	cast_union.d /= 3.1415926535;
	hash |= cast_union.i;

	return hash % (TBL_MAX(h) - 1);
}

/**
 * Test if the point is marked for deferred removal.
 */
static bool point_is_removed(struct histogram_pt *p)
{
	/* NAN is the only floating point that is not equal to
	 * itself. */
	return p->vl != p->vl;
}

/**
 * Frees the memory for the point.
 *
 * NOTE: This function does *not* unlink the point from its chain and
 * it does not change the fill counter.  this must be handled by the
 * calling function.
 *
 * The next field of the point is returned.
 */
static struct histogram_pt *point_deferred_remove(struct histogram_pt *p)
{
	struct histogram_pt *next;

	DPRINTF(DEBUG_POINTS, "Freeing point vl=%f (%p)\n", p->vl, p);

	next = p->next;
	free(p);

	return next;
}

/**
 * Remove a point from the histogram which was not marked for deferred
 * removal.
 */
static struct histogram_pt *point_remove(struct histogram *h,
					 struct histogram_pt *p)
{
	assert (!point_is_removed(p));

	if (h->as.points.ary != POINTS_ARY_INVALID) {
		DPRINTF(DEBUG_POINTS, "Invalidating cached array of points\n");
		h->as.points.ary_state = POINTS_ARY_INVALID;
	}

	DPRINTF(DEBUG_POINTS, "Decrementing fill\n");
	h->as.points.fill -= 1;

	return point_deferred_remove(p);
}

/**
 * Marks a point for deferred removal and sets the array state to the
 * desired value.
 */
static void point_mark_removal(struct histogram *h,
			       struct histogram_pt *p,
			       enum points_ary_state ary_state)
{
	struct histogram_pt *next;

	p->vl = NAN;
	next = p->next;

#if !defined(NDEBUG)
	if (ary_state == POINTS_ARY_INVALID)
		DPRINTF(DEBUG_POINTS, "Invalidating cached array of points\n");
	else if (ary_state == POINTS_ARY_UNSORTED)
		DPRINTF(DEBUG_POINTS, "Marking array of points unsorted\n");
#endif	/* !NDEBUG */

	h->as.points.ary_state = ary_state;
}

/**
 * Add the point to the cached array if it is valid and increment the
 * fill.
 *
 * This should probably be called when ever fill is going to be
 * incremented.
 */
static void point_add(struct histogram *h, struct histogram_pt *p)
{
	if (h->as.points.ary_state != POINTS_ARY_INVALID) {
		DPRINTF(DEBUG_POINTS, "Marking cached array as unsorted\n");
		h->as.points.ary_state = POINTS_ARY_UNSORTED;
		DPRINTF(DEBUG_POINTS, "Adding %f to array at %d\n", p->vl,
			h->as.points.fill);
		h->as.points.ary[h->as.points.fill] = p;
	}
	h->as.points.fill += 1;
}

/**
 * Free a histogram point chain.
 */
static void points_chain_free(struct histogram_pt *p)
{
	if (p) {
		points_chain_free(p->next);
		free(p);
	}
}

/**
 * Get the length of a chain of points.
 */
static unsigned int points_chain_length(struct histogram_pt *p)
{
	if (p)
		return 1 + points_chain_length(p->next);
	else
		return 0;
}

/**
 * Copies a chain of points.  The result is the new chain.  On error,
 * the 'error' argument is set to true.  It is set to false on
 * success.
 *
 * Additionally this adds the copied points to the UNSORTED array in
 * 'ary' using the 'index' argument as the index into the array (which
 * is updated appropriately).
 */
static struct histogram_pt *points_chain_copy(bool *error,
					      unsigned int *index,
					      struct histogram_pt **ary,
					      struct histogram_pt *p)
{
	struct histogram_pt *q;

	*error = false;
	if (!p)
		return NULL;

	if (point_is_removed(p))
		return points_chain_copy(error, index, ary, p->next);

	q = malloc(sizeof(*q));
	if(!q) {
		*error = true;
		return NULL;
	}
	*q = *p;

	q->next = points_chain_copy(error, index, ary, q->next);
	if (*error) {
		int e = errno;	/* save errno across call to free() */
		free(q);
		errno = e;
		return NULL;
	}

	if (ary) {
		assert (index);
		ary[*index] = q;
		*index += 1;
	}

	return q;
}

/**
 * Adds a new point to the histogram.  Allocates the point, increments
 * the fill counter, and adds the point to the cached array of p
 */
static struct histogram_pt *point_create(struct histogram *h, double vl,
					 double wt)
{
	struct histogram_pt *p;

	p = malloc(sizeof(*p));
	if (!p)
		return NULL;

	DPRINTF(DEBUG_POINTS, "Allocating point (%p) for value %f, wt %f\n",
		p, vl, wt);

	p->vl = vl;
	p->wt = wt;
	p->next = NULL;
	point_add(h, p);

	return p;
}

/**
 * Add mass to a chain of points.  If the point is found then add to
 * its current weight, if not then add a new link in the chain and an
 * entry is added to the array.
 *
 * Any points with a weight of zero are removed from the chain.
 *
 * The first argument, 'error', will be set to false if there was no
 * error and will be set to true if there was.
 */
static struct histogram_pt *points_chain_add_mass(bool *error,
						  struct histogram *h,
						  struct histogram_pt *p,
						  double vl,
						  double wt)
{
	if (wt == 0) {
		DPRINTF(DEBUG_POINTS, "called with wt==0.0\n");
		errno = EINVAL;
		*error = true;
		return NULL;
	}

	*error = false;
	if (!p) {
		/* Not found, allocate a new point, update fill and
		 * add it to the array (if valid). */
		return point_create(h, vl, wt);
	} else if (p->vl == vl) {
		/* Found, add the mass. */
		DPRINTF(DEBUG_POINTS, "Adding mass %f to value %f (%p)\n",
			wt, vl, p);

		assert (p->wt != 0);
		p->wt += wt;

		/* Remove the point if the mass has become zero. */
		if (p->wt == 0)
			return point_remove(h, p);
		return p;
	} else {
		/* Not found, keep searching down the chain. */
		struct histogram_pt *next ;
		DPRINTF(DEBUG_POINTS, "Looking for value %f\n", vl);

		next = points_chain_add_mass(error, h, p->next, vl, wt);
		if (*error)
			return NULL;
		p->next = next;

		/* Remove a point marked for deferred removal. */
		if (point_is_removed(p))
			return point_deferred_remove(p);

		return p;
	}
}


/**
 * Compare two struct histogram_pt pointers cast into void pointers.
 */
static int cmp_points(const void *_a, const void *_b)
{
	const struct histogram_pt *a = *(struct histogram_pt**) _a;
	const struct histogram_pt *b = *(struct histogram_pt**) _b;

	assert(a);
	assert(b);

	if (a->vl < b->vl)
		return -1;
	if (a->vl > b->vl)
		return 1;
	return 0;
}


bool points_create(struct histogram *h)
{
	struct histogram_pt **tbl;
	struct histogram_pt **ary;

	assert(h);

	if (h->max == 0) {
		DPRINTF(DEBUG_POINTS, "h->max == 0\n");
		errno = EINVAL;
		return false;
	}

	tbl = calloc(TBL_MAX(h), sizeof(*h->as.points.tbl));
	if (!tbl)
		return false;
	DPRINTF(DEBUG_POINTS, "Creating points tbl=%p\n", tbl);

	ary = malloc(h->max * sizeof(*h->as.points.tbl));
	if (!ary) {
		free(tbl);
		return false;
	}
	DPRINTF(DEBUG_POINTS, "Creating points ary=%p\n", ary);

	h->type = HISTOGRAM_POINTS;
	h->as.points.fill = 0;
	h->as.points.tbl = tbl;
	h->as.points.ary = ary;
	h->as.points.ary_state = POINTS_ARY_SORTED;

	return h;
}

void points_free(struct histogram *h)
{
	unsigned int i;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	for(i = 0; i < TBL_MAX(h); i += 1)
		points_chain_free(h->as.points.tbl[i]);

	free(h->as.points.tbl);
	free(h->as.points.ary);
}

bool points_copy(struct histogram *to, struct histogram *from)
{
	bool error;
	unsigned int i;
	struct histogram_pt **from_tbl, **to_tbl;

	assert(from);
	assert(from->type == HISTOGRAM_POINTS);

	if (!points_create(to))
		return false;
	to->as.points.ary_state = POINTS_ARY_UNSORTED;

	from_tbl = from->as.points.tbl;
	to_tbl = to->as.points.tbl;
	for (i = 0; i < TBL_MAX(from); i += 1) {
		to_tbl[i] = points_chain_copy(&error,
					      &to->as.points.fill,
					      to->as.points.ary,
					      from_tbl[i]);
		if (error) {
			points_free(to);
			return false;
		}
	}

	assert (from->as.points.fill == to->as.points.fill);
	assert (to->as.points.fill <= to->max);

	return true;
}

bool points_add_mass(struct histogram *h, double vl, double wt)
{
	bool error;
	unsigned int hash;
	struct histogram_pt *p;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	if (wt == 0)
		return true;

	if (h->as.points.fill == h->max) {
		DPRINTF(DEBUG_POINTS, "Fail: adding to a full histogram\n");
		errno = EINVAL;
		return false;
	}

	hash = hash_value(h, vl);
	p = points_chain_add_mass(&error, h, h->as.points.tbl[hash], vl, wt);
	if (error)
		return false;

	h->as.points.tbl[hash] = p;

	assert (h->as.points.fill <= h->max);

	return true;
}

struct histogram_pt **points_array(struct histogram *h, bool sorted, bool copy)
{

	if (h->as.points.ary_state == POINTS_ARY_INVALID) {
		unsigned int i, j;
		struct histogram_pt *p;

		DPRINTF(DEBUG_POINTS, "Building points array\n");
		j = 0;
		for (i = 0; i < TBL_MAX(h); i += 1) {
			for (p = h->as.points.tbl[i]; p; p = p->next) {
				if (!point_is_removed(p)) {
					h->as.points.ary[j] = p;
					j += 1;
				}
			}
		}
		assert(j == h->as.points.fill);
		h->as.points.ary_state = POINTS_ARY_UNSORTED;
	}

	assert(h->as.points.ary_state != POINTS_ARY_INVALID);
	if (sorted && h->as.points.ary_state != POINTS_ARY_SORTED) {
		DPRINTF(DEBUG_POINTS, "Sorting points array\n");
		qsort(h->as.points.ary, h->as.points.fill,
		      sizeof(*h->as.points.ary), cmp_points);
		h->as.points.ary_state = POINTS_ARY_SORTED;
	}

	assert (h->as.points.fill <= h->max);

	if (copy) {
		struct histogram_pt **ary = calloc(h->as.points.fill,
						   sizeof(*ary));
		if (!ary)
			return NULL;
		return memcpy(ary, h->as.points.ary,
			      h->as.points.fill * sizeof(*ary));
	}
	return h->as.points.ary;
}

double points_prune_above(struct histogram *h, double bound, bool equal)
{
	unsigned int i;
	double pruned = 0;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	if (bound > points_max_value(h)) {
		DPRINTF(DEBUG_POINTS, "Nothing pruned\n");
		return 0;
	}

	ary = points_sorted_array(h, false);

	for (i = 0; i < h->as.points.fill; i += 1) {
		if (ary[i]->vl > bound || (equal && ary[i]->vl == bound))
			break;
	}

	if (i < h->as.points.fill) {
		/* Some values were pruned. */
		unsigned int old_fill = h->as.points.fill;

		h->as.points.fill = i;
		DPRINTF(DEBUG_POINTS, "Fill after pruning=%d\n",
			h->as.points.fill);

		/* mark points out of the bound for deferred removal. */
		for (; i < old_fill; i += 1) {
			DPRINTF(DEBUG_POINTS,
				"Marking vl=%f (%p) for deferred removal\n",
				ary[i]->vl, ary[i]);
			pruned += ary[i]->wt;
			point_mark_removal(h, ary[i], h->as.points.ary_state);
		}
	}

	DPRINTF(DEBUG_POINTS, "Pruned wt=%f\n", pruned);
	DPRINTF(DEBUG_POINTS, "Total wt=%f\n", points_total_mass(h));
	assert (h->as.points.fill <= h->max);

	return pruned;
}

double points_total_mass(struct histogram *h)
{
	unsigned int i;
	double accum = 0;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	ary = points_array(h, false, false);
	for (i = 0; i < h->as.points.fill; i += 1)
		accum += h->as.points.ary[i]->wt;

	return accum;
}

double points_min_value(struct histogram *h)
{
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	if (h->as.points.fill == 0)
		return INFINITY;

	ary = points_sorted_array(h, false);
	return ary[0]->vl;
}

double points_max_value(struct histogram *h)
{
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	if (h->as.points.fill == 0)
		return -INFINITY;

	ary = points_sorted_array(h, false);
	return ary[h->as.points.fill - 1]->vl;
}

void points_output(FILE *out, struct histogram *h)
{
	unsigned int i;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	ary = points_sorted_array(h, false);

	for (i = 0; i < h->as.points.fill; i += 1) {
		double vl = ary[i]->vl;
		double wt = ary[i]->wt;
		fprintf(out, "vl=%f, wt=%f\n", vl, wt);
	}
	fprintf(out, "fill: %d\n", h->as.points.fill);
	fprintf(out, "total mass: %f\n", points_total_mass(h));
}

unsigned int points_max_chain_length(struct histogram *h)
{
	unsigned int i;
	unsigned int max = 0;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	for(i = 0; i < TBL_MAX(h); i += 1) {
		unsigned int len = points_chain_length(h->as.points.tbl[i]);
		if (len > max)
			max = len;
	}

	return max;
}

void points_scale(struct histogram *h, double fact)
{
	unsigned int i;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	DPRINTF(DEBUG_POINTS, "Scaling by %f\n", fact);

	ary = points_array(h, false, false);
	for (i = 0; i < h->as.points.fill; i += 1)
		ary[i]->wt *= fact;
}

double points_weight_left_of(struct histogram *h, double vl, bool equal)
{
	unsigned int i;
	double accum = 0;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	DPRINTF(DEBUG_POINTS, "Finding weight for vl=%f\n", vl);

	ary = points_sorted_array(h, false);
	for (i = 0; i < h->as.points.fill; i += 1) {
		if (ary[i]->vl >= vl) {
			if (equal && ary[i]->vl == vl)
				accum += ary[i]->wt;
			break;
		}
		accum += ary[i]->wt;
		DPRINTF(DEBUG_POINTS, "vl=%f, wt=%f, sum=%f\n",
			ary[i]->vl, ary[i]->wt, accum);
	}

	DPRINTF(DEBUG_POINTS, "Weight = %f\n", accum);

	return accum;
}

double points_val_for_weight(struct histogram *h, double wt, bool equal)
{
	unsigned int i;
	double accum = 0;
	double bound = INFINITY;
	struct histogram_pt **ary;

	assert(h);
	assert(h->type == HISTOGRAM_POINTS);

	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "Finding bound for wt=%f\n", wt);
	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "min=%f\n", points_min_value(h));
	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "max=%f\n", points_max_value(h));

	ary = points_sorted_array(h, false);
	for (i = 0; i < h->as.points.fill; i += 1) {
		if (equal) {
			if (accum + ary[i]->wt >= wt) {
				bound = ary[i]->vl;
				break;
			}
		} else {
			if (accum >= wt) {
				bound = ary[i]->vl;
				break;
			}
		}
		accum += ary[i]->wt;
		DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "vl=%f, wt=%f, sum=%f\n",
		ary[i]->vl, ary[i]->wt, accum);
	}

	DPRINTF((DEBUG_BOUNDS|DEBUG_POINTS), "Bound = %f\n", bound);

	return bound;
}
