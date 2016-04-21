/**
 * \file bvls_stubs.c
 *
 * Objective CAML stubs for BVLS.
 *
 * \author eaburns
 * \date 23-09-2010
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "f2c.h"

#if !defined(NDEBUG)
#define DPRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define DPRINTF(...)
#endif /* NDEBUG */

extern int bvls_(integer *key, integer *m, integer *n, doublereal *a,
		 doublereal *b, doublereal *bl, doublereal *bu,
		 doublereal *x, doublereal *w, doublereal *act,
		 doublereal *zz, integer *istate, integer *loopa,
		 integer *lret);

/*
 * Converts an OCaml array of floats to a C array of doubles.
 *
 * Returns the size of the output array or negative on error.
 */
int ocaml_to_c_array(value in_v, double **outp)
{
	int i;
	int nwords = Wosize_val(in_v);
	int nfloats = nwords / Double_wosize;
	double *o;

	DPRINTF("%d words, %d floats\n", nwords, nfloats);

	o = calloc(nfloats, sizeof(double));
	if (!o) {
		perror("calloc failed to allocate doubles");
		return -1;
	}

	for (i = 0; i < nfloats; i += 1) {
		double d = Double_field(in_v, i);
		o[i] = d;
	}

	*outp = o;
	return nfloats;
}


/*
 * Converts a C array of doubles to a OCaml array of floats.
 *
 * Returns 0 on success and 1 on failure.
 */
int c_to_ocaml_array(double in[], unsigned int n, value *out)
{
	CAMLlocal1(ary_v);
	unsigned int i;

	ary_v = caml_alloc(Double_wosize * n, Double_array_tag);
	for (i = 0; i < n; i += 1)
		Store_double_field(ary_v, i, in[i]);

	*out = ary_v;

	return 0;
}


/*
 * Allocate a bunch of arrays that are required as the input into
 * bvls_
 */
void alloc_arrays(int m, int n, doublereal **x, doublereal **w,
		  doublereal **zz, doublereal **act, integer **istate)
{
	*x = calloc(n, sizeof(**x));
	if (!*x) {
		perror("calloc failed to allocate the result array");
		caml_failwith("Failed to allocate result array");
	}
	*w = calloc(n, sizeof(**w));
	if (!w) {
		perror("calloc failed to allocate working array w");
		caml_failwith("Failed to allocate working array w");
	}
	*zz = calloc(m, sizeof(**zz));
	if (!zz) {
		perror("calloc failed to allocate working array zz");
		caml_failwith("Failed to allocate working array zz");
	}
	*act = calloc(m * (n + 2), sizeof(**act));
	if (!*act) {
		perror("calloc failed to allocate working array act");
		caml_failwith("Failed to allocate working array act");
	}
	*istate = calloc(n + 1, sizeof(**istate));
	if (!*istate) {
		perror("calloc failed to allocate working array istate");
		caml_failwith("Failed to allocate working array istate");
	}
}


/*
 * BVLS solves the problem:
 *
 *          min  || a.x - b ||     such that   bl <= x <= bu
 *                            2
 *
 *    where
 *
 *               x  is an unknown n-vector
 *
 *               a  is a given m by n matrix
 *
 *               b  is a given  m-vector
 *
 *               bl is a given n-vector of lower bounds on the
 *                                components of x.
 *
 *               bu is a given n-vector of upper bounds on the
 *                                components of x.
 *
 *  If key = 0, the subroutine solves the problem from scratch.
 *
 *  If key > 0 the routine initializes using the user's guess about
 *   which components of x are `active', i.e. are stricly within their
 *   bounds, which are at their lower bounds, and which are at their
 *   upper bounds.  This information is supplied through the array
 *   istate.  istate(n+1) should contain the total number of
 *   components at their bounds (the `bound variables').  The absolute
 *   values of the first nbound=istate(n+1) entries of istate are the
 *   indices of these `bound' components of x.  The sign of istate(j),
 *   j=1,..., nbound, indicates whether x(|istate(j)|) is at its upper
 *   or lower bound.  istate(j) is positive if the component is at its
 *   upper bound, negative if the component is at its lower bound.
 *   istate(j), j=nbound+1,...,n contain the indices of the components
 *   of x that are active (i.e. are expected to lie strictly within
 *   their bounds).  When key > 0, the routine initially sets the
 *   active components to the averages of their upper and lower
 *   bounds: x(j)=(bl(j)+bu(j))/2, for j in the active set.
 */
/*
 * [dims] is the tuple (m, n)
 *
 * [a] is a 1d array accessed 2-dimensionally.
 */
value bvls_stub(value dims_v, value a_v, value b_v, value bl_v, value bu_v)
{
	CAMLparam5(dims_v, a_v, b_v, bl_v, bu_v);
	CAMLlocal1(result);
	int err;
	integer key = 0, loopA, lret;
	integer m = Int_val(Field(dims_v, 0));
	integer n = Int_val(Field(dims_v, 1));
	integer *istate;
	doublereal *a, *b, *bl, *bu, *x, *w, *zz, *act;
	int a_size, b_size, bl_size, bu_size;

	DPRINTF("m=%ld, n=%ld\n", m, n);

	a_size = ocaml_to_c_array(a_v, &a);
	if (a_size < 0)
		caml_failwith("Failed to retrieve [a] array");
	DPRINTF("a_size=%d\n", a_size);
	if (a_size != m * n)
		caml_invalid_argument("[a] has the incorrect size");

	b_size = ocaml_to_c_array(b_v, &b);
	if (b_size < 0)
		caml_failwith("Failed to retrieve [b] array");
	DPRINTF("b_size=%d\n", b_size);
	if (b_size != m)
		caml_invalid_argument("[b] has the incorrect size");

	bl_size = ocaml_to_c_array(bl_v, &bl);
	if (bl_size < 0)
		caml_failwith("Failed to retrieve [bl] array");
	DPRINTF("bl_size=%d\n", bl_size);
	if (bl_size != n)
		caml_invalid_argument("[bl] has the incorrect size");

	bu_size = ocaml_to_c_array(bu_v, &bu);
	if (bu_size < 0)
		caml_failwith("Failed to retrieve [bu] array");
	DPRINTF("bu_size=%d\n", bu_size);
	if (bu_size != n)
		caml_invalid_argument("[bu] has the incorrect size");

	alloc_arrays(m, n, &x, &w, &zz, &act, &istate);
	bvls_(&key, &m, &n, a, b, bl, bu, x, w, act, zz, istate,
	      &loopA, &lret);

	DPRINTF("lret=%ld\n", lret);
	if (lret == -1)
		caml_invalid_argument("inconsistent bounds");
	if (lret == -2)
		caml_invalid_argument("no free variables, check input bounds");
	if (lret == -3)
		caml_failwith("too many active variables in starting sol");
	if (lret == -4)
		caml_failwith("too many free variables");
	if (lret == -5)
		caml_failwith("failed to converge");

	err = c_to_ocaml_array(x, n, &result);
	if (err)
		caml_failwith("Failed to copy result");

	CAMLreturn(result);
}
