#include <stdio.h>
#include <math.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_plain.h>
#include <gsl/gsl_math.h>


/* a structure for holding the state of a hovercraft */
struct state { double x;
               double y;
               double theta;
               double xdot; 
               double ydot; 
               double thetadot; };

/* a structure for holding a hovercraft motion primitive */
struct motion_prim { struct state * points; double dt; };

/* structure for holding parameters passed to gsl functions */
struct func_params { double a; double theta0; double omega0; double alpha0; };


/*---------------------------------------------------------------------------*/
/*                  Functions for Numerical Integration                      */
/*---------------------------------------------------------------------------*/

/* the double derivative of x position (x acceleration) */
double xdotdot (double t, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double a = p->a;
  double theta0 = p->theta0;
  double omega0 = p->omega0;
  double alpha0 = p->alpha0;
  double result = a * cos(theta0 + omega0 * t + 0.5 * alpha0 * t * t);
  return result;
}

/* the double derivative of y position (y acceleration) */
double ydotdot (double t, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double a = p->a;
  double theta0 = p->theta0;
  double omega0 = p->omega0;
  double alpha0 = p->alpha0;
  double result = a * sin(theta0 + omega0 * t + 0.5 * alpha0 * t * t);
  return result;
}


/*---------------------------------------------------------------------------*/
/*              Functions for Monte Carlo Double Integration                 */
/*---------------------------------------------------------------------------*/

/* the double derivative of x position (x acceleration) */
double ax (double t[], size_t dim, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double a = p->a;
  double theta0 = p->theta0;
  double omega0 = p->omega0;
  double alpha0 = p->alpha0;
  double result = a * cos(theta0 + omega0 * t[0] + 0.5 * alpha0 * t[0] * t[0]);
  return result;
}

/* the double derivative of y position (y acceleration) */
double ay (double t[], size_t dim, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double a = p->a;
  double theta0 = p->theta0;
  double omega0 = p->omega0;
  double alpha0 = p->alpha0;
  double result = a * sin(theta0 + omega0 * t[0] + 0.5 * alpha0 * t[0] * t[0]);
  return result;
}

/* 
 *
 */
double do_monte_carlo(double t, gsl_monte_function * G)
{
  double result, error;
  double xl[2] = {0, 0};
  double xu[2] = {t, t};
  const gsl_rng_type *T;
  gsl_rng *r;
  size_t calls = 500000; 
  gsl_rng_env_setup();
  T = gsl_rng_default;
  r = gsl_rng_alloc(T);
  {
    gsl_monte_plain_state *s = gsl_monte_plain_alloc(2);
    gsl_monte_plain_integrate(G, xl, xu, 2, calls, r, s, &result, &error);
    gsl_monte_plain_free(s);
  }
  return result;
}

/*
 * Calculates a hovercraft motion primitive.
 */
int main (void)
{
  /* input variables */
  double dtheta = M_PI_4; /* change heading by 45 degrees */
  double dv = 0.5;        /* change velocity by 0.5 meters per second */
  double ta = 1.0;        /* action time in seconds */

  /* motion primitive parameters */
  int mp_points = 11;               /* number of points to specify in the motion primitive */
  double dt = ta / (mp_points - 1); /* amount of time in between each point in seconds */

  /* initial state variables - x and y assumed to be zero */
  double theta0 = 0.0;
  double xdot0 = 0.0;
  double ydot0 = 0.0;
  double thetadot0 = 0.0;

  /* model properties and constraints */
  double m = 1.0;             /* mass of the hovercraft in kg */
  double r = 1.0;             /* radius of the hovercraft in meters */
  double I = m * r * r / 2.0; /* moment of inertia of the hovercraft */
  double d = 0.8;             /* distance between the two thrusters in meters */
  double Fmax = 5.0;          /* max force of a single thruster in newtons */

  /* now calculate the force required by each thruster to meet dtheta and dv */
  double F1 = ((I * (dtheta - (thetadot0 * ta))) / (d * ta * ta)) + ((m * dv) / (2.0 * ta));
  double F2 = (m * dv) - F1;

  /* calculate the angular acceleration, alpha */
  double alpha = (d * (F1 - F2)) / I;
  /* calculate the forward acceleration, a */
  double a = (F1 + F2) / m;

  printf("F1 = %lf\nF2 = %lf\nalpha=%lf\na=%lf\n", F1, F2, alpha, a);

  /* check that the forces meet the hovercraft constraints */
  if(F1 < -Fmax){ printf("error: F1 is less than -Fmax (%lf): %lf\n",  -Fmax, F1); return 1; }
  if(F1 >  Fmax){ printf("error: F1 is greater than Fmax (%lf): %lf\n", Fmax, F1); return 1; }
  if(F2 < -Fmax){ printf("error: F2 is less than -Fmax (%lf): %lf\n",  -Fmax, F2); return 1; }
  if(F2 >  Fmax){ printf("error: F2 is greater than Fmax (%lf): %lf\n", Fmax, F2); return 1; }

  /* calculate the hovercraft state at points along the motion primitive */

  /* initialize gsl stuff */
  gsl_integration_workspace * workspace = gsl_integration_workspace_alloc(1000);
  double result, error;
  struct func_params params = { a, theta0, thetadot0, alpha };
  gsl_function X;
  X.function = &xdotdot;
  X.params = &params;
  gsl_function Y;
  Y.function = &ydotdot;
  Y.params = &params;

  /* calculate each data point */
  struct state points[mp_points];
  int i;
  for(i = 0; i < mp_points; i++)
  {
    double t = i * dt;
    /* calculate thetadot */
    double thetadot = thetadot0 + alpha * t;
    /* calculate theta */
    double theta = theta0 + thetadot0 * t + 0.5 * alpha * t * t;
    /* caclulate xdot */
    gsl_integration_qags(&X, 0, t, 0, 1e-7, 1000, workspace, &result, &error);
    double xdot = xdot0 + result;
    /* calculate ydot */
    gsl_integration_qags(&Y, 0, t, 0, 1e-7, 1000, workspace, &result, &error);
    double ydot = ydot0 + result;
    /* calculate x */
    /* TODO */
    double x = 0.0;
    /* calculate y */
    /* TODO */
    double y = 0.0;
    /* assign the data point in the points array */
    points[i].x = x;
    points[i].y = y;
    points[i].theta = theta;
    points[i].xdot = xdot;
    points[i].ydot = ydot;
    points[i].thetadot = thetadot;

    printf("for point %d (t = %lf):  x = %lf, y = %lf, theta = %lf, xdot = %lf, ydot = %lf, thetadot = %lf\n", i, t, x, y, theta, xdot, ydot, thetadot);
  }

  double v0 = sqrt(xdot0 * xdot0 + ydot0 + ydot0);
  double v = sqrt(points[mp_points-1].xdot * points[mp_points-1].xdot + points[mp_points-1].ydot * points[mp_points-1].ydot);
  printf("theta should equal: %lf\n", dtheta);
  printf("v should equal: %lf\n", v0 + dv);
  printf("v equals: %lf\n", v);

  
  return 0;
}
