/**
 * \file histogram.h
 *
 * A histogram data structure for representing arbitrary distributions
 * of values.
 *
 * \author eaburns
 * \date 20-12-2009
 */

#if !defined(_HISTOGRAM_H_)
#define _HISTOGRAM_H_

#include <stdbool.h>

#include "debug.h"

/**
 * The type of the histogram.
 */
enum histogram_type {
	/** Not yet allocated. */
	HISTOGRAM_NONE,
	/** A bunch of points. */
	HISTOGRAM_POINTS,
	/** An array of fixed width bins. */
	HISTOGRAM_BINS,
};

/**
 * The state of the cached array in the points histogram.
 */
enum points_ary_state {
	/** The data in the array is meaningless. */
	POINTS_ARY_INVALID,
	/** The array contains the sorted points. */
	POINTS_ARY_SORTED,
	/** The array contains points but is not sorted. */
	POINTS_ARY_UNSORTED,
};

/*
 * A single point is a bin in the "hash table" of points.
 */
struct histogram_pt {
	double vl;
	double wt;
	struct histogram_pt *next;
};

/**
 * A histogram that is a bunch of points.
 *
 * The points are stored in a hash table.  When possible an array is
 * also used to also store the points.  This array allows for quick
 * access to the points in sorted order, but it may not be valid (when
 * points are removed from the histogram) and therefore can be a bit
 * expensive to rebuild.
 */
struct histogram_points {
	/* Number of elements filled in. */
	unsigned int fill;

	/* The points hash table. */
	struct histogram_pt **tbl;

	/* Cached array of the points. */
	struct histogram_pt **ary;

	/* Is the cached array currently sorted, unsorted or
	 * invalid? */
	enum points_ary_state ary_state;
};

/**
 * A histogram that is a bunch of bins of even widths.
 */
struct histogram_bins {
	/* The fixed width of each bin. */
	double width;

	/* The base value of the bins. */
	double base;

	/* Array of bins.  Each entry is the weight within the
	 * corresponding bin. */
	double *bins;
};

/**
 * The main histogram structure.
 *
 * The histogram is either a bunch of points, an array of equal width
 * bins or it is nothing yet.  When the histogram is first created its
 * 'max' field is filled in with the desired maximum number of points
 * and bins.  The histogram is then lazily created when the first
 * operation attempts to mutate it.
 */
struct histogram {
	enum histogram_type type;

	/* The maxmium number of points or bins. */
	unsigned int max;

	/* The total weight in the histogram. */
	double total;

	union {
		/* The histogram can be points or bins. */
		struct histogram_points points;
		struct histogram_bins bins;
	} as;
};

/**
 * Allocates a new histogram.
 */
void histogram_create(struct histogram *h, unsigned int max);

/**
 * Frees the memory for a histogram.
 */
void histogram_free(struct histogram *h);

/**
 * Copy the histogram 'from' to the histogram 'to'.
 */
bool histogram_copy(struct histogram *to, struct histogram *from);

/**
 * Adds the histogram in the second argument into the histogram in the
 * first argument.
 *
 * Returns true on success and false on failure.
 */
bool histogram_add(struct histogram *a, struct histogram *b);

/**
 * Adds mass to a histogram.
 */
bool histogram_add_mass(struct histogram *a, double vl, double wt);

/**
 * Compute the total mass of the histogram.  This is mostly just for
 * debugging.
 */
double histogram_compute_total_mass(struct histogram *h);

/**
 * Quickly retrieve the total mass of the histogram.
 */
static inline double histogram_total_mass(struct histogram *h)
{
	return h->total;
}

/**
 * If debugging is enabled this tests the total mass to makes sure
 * that the field matches the computed value.
 */
static inline void histogram_check_total_mass(struct histogram *h)
{
	SANITY_CHECK(effectively_equal(histogram_total_mass(h),
				       histogram_compute_total_mass(h)));
}

/**
 * Get the minimum value in the histogram.
 */
double histogram_min_value(struct histogram *h);

/**
 * Get the maximum value in the histogram.
 */
double histogram_max_value(struct histogram *h);

/**
 * Test if a histogram is empty.
 */
static inline bool histogram_is_empty(struct histogram *a)
{
	return a->type == HISTOGRAM_NONE || histogram_total_mass(a) == 0;
}

/**
 * Prunes all of the weight above the given bound.
 */
void histogram_prune_above(struct histogram *h, double bound);

/**
 * Scale the histogram's weights by a given factor.
 */
void histogram_scale(struct histogram *h, double fact);

/**
 * Normalize the histogram to contain the desired amount of weight.
 */
void histogram_normalize(struct histogram *h, double wt);

/**
 * Get the amount of weight left of the given value.
 */
double histogram_weight_left_of(struct histogram *h, double vl);

/**
 * Get the bound that will give about 'desired' weight to the left of
 * it.
 */
double histogram_val_for_weight(struct histogram *h, double wt);


/**
 * Get the bound that will give the desired weight after adding a and
 * b when b is scaled by factor.
 *
 * error is set to false on success and to true on error.  If there
 * was an error the returned bound is NAN.
 */
double histogram_bound_for_wted_combo(bool *error,
				      double desired,
				      struct histogram *a,
				      struct histogram *b,
				      double factor);

/**
 * Convolve the two histograms 'a' and 'b' into the result 'res' and
 * prune values above 'bound'.
 */
bool histogram_convolve(struct histogram *res, struct histogram *a,
			struct histogram *b, double bound);

/**
 * Print the histogram to the given file.
 */
void histogram_output(FILE *out, struct histogram *h);

#endif /* !_HISTOGRAM_H_ */
