# quad-precision-in-gfortran-test
quad precision in gfortran test 
Here’s a small, self-contained Fortran 2008 demo that uses true quad precision when your gfortran/platform supports it. It:

Detects whether a 128-bit (quad) real kind is available

Prints precision/range info

Does a few high-precision calculations (π, e, a tiny root via Newton)

Compares quad vs double to show the benefit

gfortran -O2 -std=f2008 -ffree-form quadtest.f08 -o quadtest
./quadtest

if necessary 
gfortran -O2 -std=f2008 -ffree-form quadtest.f90 -o quadtest -lquadmath
