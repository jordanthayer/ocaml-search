#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_plain.h>
#include <gsl/gsl_math.h>


/* a structure for holding the state of a diff drive robot */
struct state { double x;
               double y;
               double theta;
               double v;
               double w; };

/* a structure for holding a hovercraft motion primitive */
struct motion_prim { struct state * points; double dt; };

/* structure for holding parameters passed to gsl functions */
struct func_params { double A; double B; double C; double D; double theta0; };


/*---------------------------------------------------------------------------*/
/*                  Functions for Numerical Integration                      */
/*---------------------------------------------------------------------------*/

/* the derivative of x position (x velocity) */
double xdot (double t, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double A = p->A;
  double B = p->B;
  double C = p->C;
  double D = p->D;
  double theta0 = p->theta0;
  double result = (A*t + B) * cos(C*t*t + D*t + theta0);
  return result;
}

/* the derivative of y position (y velocity) */
double ydot (double t, void *params)
{
  struct func_params * p = (struct func_params *) params;
  double A = p->A;
  double B = p->B;
  double C = p->C;
  double D = p->D;
  double theta0 = p->theta0;
  double result = (A*t + B) * sin(C*t*t + D*t + theta0);
  return result;
}

/*
 * converts degrees to radians
 */
double to_rad(float deg){
  return deg * M_PI / 180.0;
}

/*
 * print program usage information to stdout.
 */
void usage()
{
  printf("usage:  a.out dtheta dv t points theta0 v0 w0 b a_max v_max\n");
  printf("  dtheta = the change in heading desired (in degrees).\n");
  printf("  dv = the change in speed desired (in m/s).\n");
  printf("  t = the duration of the motion primitive (in seconds).\n");
  printf("  points = the number of points to record for the motion primitive (int).\n");
  printf("  theta0 = the initial heading of the robot (in degrees).\n");
  printf("  v0 = the initial speed of the robot (in m/s).\n");
  printf("  w0 = the initial angular velocity of the robot (in deg/s).\n");
  printf("  b = the length between the wheels of the robot (in meters).\n");
  printf("  a_max = the maximum acceleration of the robot (in m/s^2(.\n");
  printf("  v_max = the maximum speed of the robot (in m/s).\n");
  printf("\n");
}

/*
 * Calculates a hovercraft motion primitive.
 *
 * Useful equations:
 *
 *   1.)  w = (v_r - v_l) / b
 *   2.)  alpha = (a_r - a_l) / b
 *   3.)  v = (v_r + v_l) / 2
 *   4.)  a = (a_r + a_l) / 2
 *   --------------------------------------------------------------------------
 *   5.)  a_r + a_l = (2 * dv) / t
 *   6.)  a_r - a_l = (2b * (dtheta - w0 * t)) / t^2
 *   --------------------------------------------------------------------------
 *   7.)  a_l = ((2 * dv) / t)) - a_r
 *   8.)  a_r = (((2b * (dtheta - w0 * t)) / t^2) + ((2 * dv) / t)) / 2
 *   --------------------------------------------------------------------------
 *   9.)  v = sqrt(v_x^2 + v_y^2)
 *   10.) v_l = 2v - v_r
 *   11.) v_r = (b * w) + v_l
 *   12.) w = w0 + alpha * t
 */
int main (int argc, char* argv[])
{
  /* check command line args */
  if(argc != 11)
  {
    usage();
    return 1;
  }

  /* ----- get command line arguments ----- */

  /* input variables */
  double dtheta = to_rad(atof(argv[1])); /* amount to change heading (rad) */
  double dv = atof(argv[2]);             /* amount to change velocity (m/s) */
  double ta = atof(argv[3]);             /* duration of action (seconds) */

  /* motion primitive parameters */
  int mp_points = atoi(argv[4]);    /* number of points to specify in the motion primitive */
  double dt = ta / (mp_points - 1); /* amount of time in between each point in seconds */

  /* initial state variables - x and y assumed to be zero */
  double theta0 = to_rad(atof(argv[5]));  /* initial heading (rad) */
  double v0 = atof(argv[6]);              /* initial speed (m/s) */
  double w0 = to_rad(atof(argv[7]));      /* initial angular velocity (rad/s) */

  /* model properties and constraints */
  double b = atof(argv[8]);       /* axel length in meters */
  double a_max = atof(argv[9]);   /* max acceleration in m/s^2 */
  double v_max = atof(argv[10]);  /* max velocity in m/s */ 

  /* ----- begin computation ----- */

  /* calculate left and right initial wheel velocities */
  double v_r0 = (b * w0 + 2 * v0) / 2.0;
  double v_l0 = 2 * v0 - v_r0;

  /* now calculate the acceleration required by each wheel to meet dtheta and dv */
  double a_r = (((2 * b * (dtheta - w0 * ta)) / (ta * ta)) + ((2 * dv) / ta)) / 2;
  double a_l = ((2 * dv) / ta) - a_r;
  double alpha = (a_r - a_l) / b;

  /* calculate A, B, C, and D */
  double A = (a_r + a_l) / 2.0;
  double B = (v_r0 + v_l0) / 2.0;
  double C = (a_r - a_l) / (2.0 * b);
  double D = (v_r0 - v_l0) / b;

  /* check that the forces meet the hovercraft constraints */
  if(a_r < -a_max){ fprintf(stderr, "error: a_r is less than -a_max (%lf): %lf\n",  -a_max, a_r); return 1; }
  if(a_r >  a_max){ fprintf(stderr, "error: a_r is greater than a_max (%lf): %lf\n", a_max, a_r); return 1; }
  if(a_l < -a_max){ fprintf(stderr, "error: a_l is less than -a_max (%lf): %lf\n",  -a_max, a_l); return 1; }
  if(a_l >  a_max){ fprintf(stderr, "error: a_l is greater than a_max (%lf): %lf\n", a_max, a_l); return 1; }

  /* calculate the hovercraft state at points along the motion primitive */

  /* initialize gsl stuff */
  int w_size = 100000;
  double rel_err = 10000e-1; //  FIXME this is too high but it fails otherwise for some
  gsl_integration_workspace * workspace = gsl_integration_workspace_alloc(w_size);
  double result, error;
  struct func_params params = { A, B, C, D, theta0 };
  gsl_function X;
  X.function = &xdot;
  X.params = &params;
  gsl_function Y;
  Y.function = &ydot;
  Y.params = &params;

  /* calculate each data point */
  struct state points[mp_points];
  int i;
  for(i = 0; i < mp_points; i++)
  {
    double t = i * dt;
    /* calculate theta */
    double theta = theta0 + w0 * t + 0.5 * alpha * t * t;
    /* calculate v_x */
    double v_x = (A * t + B) * cos(C * t * t + D * t + theta0);
    /* calculate v_y */
    double v_y = (A * t + B) * sin(C * t * t + D * t + theta0);
    /* calculate v */
    double v = sqrt(v_x * v_x + v_y * v_y);
    /* calulate w */
    double w = w0 + alpha * t;
    /* caclulate x */
    gsl_integration_qags(&X, 0, t, 0, rel_err, w_size, workspace, &result, &error);
    double x = result;
    /* calculate y */
    gsl_integration_qags(&Y, 0, t, 0, rel_err, w_size, workspace, &result, &error);
    double y = result;

    /* check velocity constraints */
    // if(v >  v_max){ fprintf(stderr, "error: v is greater than v_max (%lf): %lf\n", v_max, v); return 1; }

    /* assign the data point in the points array */
    points[i].x = x;
    points[i].y = y;
    points[i].theta = theta;
    points[i].v = v;
    points[i].w = w;

    // fprintf(stderr, "for point %d (t = %lf):  x = %lf, y = %lf, theta = %lf, v = %lf, w = %lf\n", i, t, x, y, theta, v, w);
  }

  /* output the motion primitive to stdout */
  printf("diff_drive\n");
  printf("%lf\n", dt);
  printf("%d\n", mp_points);
  for(i = 0; i < mp_points; i++)
  {
    printf("%lf %lf %lf %lf %lf\n", points[i].x, points[i].y, points[i].theta,
        points[i].v, points[i].w);
  }

  return 0;
}
