#!/bin/bash

# model parameters
b=1
a_max=15
v_max=3

# motion prim parameters
t=0.5
points=11

w0=0

THETAS=(0 45 90 135 180 225 270 315)
DTHETAS=(-90 45 0 45 90)
#SPEEDS=('-1,-1' '-1,0' '0,-1' '0,0' '0,1.5' '1.5,0' '1.5,1.5' '1.5,3' '3,0' '3,1.5' '3,3')
SPEEDS=('-1,-1' '-1,0' '0,-1' '0,0' '0,3' '3,0' '3,3')

for theta in "${THETAS[@]}"; do
  for dtheta in "${DTHETAS[@]}"; do
    for s in "${SPEEDS[@]}"; do
      set -- `echo $s | tr , \ `
      v0=$1;
      vf=$2;
      dv=$(echo "scale=10;$vf - $v0" | bc);
      # echo "$dtheta, $dv";
      ./diff_drive $dtheta $dv $t $points $theta $v0 $w0 $b $a_max $v_max;
    done
  done
done
