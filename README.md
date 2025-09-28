# quad-precision-in-gfortran-test
quad precision in gfortran test 
Here’s a small, self-contained Fortran 2008 demo that uses true quad precision when your gfortran/platform supports it. It:

Detects whether a 128-bit (quad) real kind is available

Prints precision/range info

Does a few high-precision calculations (π, e, a tiny root via Newton)

Compares quad vs double to show the benefit
