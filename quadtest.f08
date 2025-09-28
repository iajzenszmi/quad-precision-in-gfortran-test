userland@localhost:~$ cat quadtest.f08
! Quad precision demo for gfortran
! Works when a REAL kind with ~33 decimal digits is available (binary128).
! If not available on your platform, the program will say so and exit cleanly.

program quad_demo
  use, intrinsic :: iso_fortran_env, only : real128
  implicit none

  integer, parameter :: qp_try = selected_real_kind(p=33, r=4931)  ! ~binary128
  integer            :: qp
  logical            :: have_quad

  ! Pick the best available kind >= 33 digits; confirm real128 if present
  have_quad = (qp_try > 0)
  if (have_quad) then
     qp = qp_try
  else
     ! Some platforms expose real128 even if selected_real_kind fails; try it:
     if (real128 > 0) then
        qp = real128
        have_quad = .true.
     end if
  end if

  if (.not. have_quad) then
     print *, "Quad precision (binary128) not available on this compiler/platform."
     print *, "Tip: On x86_64 with GCC, quad often works with libquadmath."
     stop 0
  end if

  call run_demo(qp)

contains

  subroutine run_demo(k)
    integer, intent(in) :: k
    real(kind=k) :: one, pi_q, e_q, eps_q, tiny_q, huge_q
    real(kind=8) :: pi_d, e_d

    one   = 1.0_k
    pi_q  = 4.0_k * atan(one)                 ! quad-precision π
    e_q   = exp(one)                          ! quad-precision e
    eps_q = epsilon(one)
    tiny_q= tiny(one)
    huge_q= huge(one)

    pi_d  = 4.0d0 * atan(1.0d0)               ! double π
    e_d   = exp(1.0d0)                        ! double e

    print *, "----------------------------------------------"
    print *, "Quad-precision kind value: ", k
    print *, "digits (base-2):           ", digits(one)
    print *, "approx decimal digits:     ", int(digits(one)*log10(2.0_k))
    print *, "epsilon:                   ", eps_q
    print *, "tiny():                    ", tiny_q
    print *, "huge():                    ", huge_q
    print *, "----------------------------------------------"
    print "(A,ES42.35)", "pi (quad): ", pi_q
    print "(A,ES42.35)", "pi (double): ", real(pi_d,kind=k)
    print "(A,ES42.35)", "  |pi_q - pi_d|: ", abs(pi_q - real(pi_d,kind=k))
    print *, "----------------------------------------------"
    print "(A,ES42.35)", "e (quad):  ", e_q
    print "(A,ES42.35)", "e (double):", real(e_d,kind=k)
    print "(A,ES42.35)", "  |e_q - e_d|:  ", abs(e_q - real(e_d,kind=k))
    print *, "----------------------------------------------"

    call newton_small_root_demo(k)
  end subroutine run_demo

  ! Solve f(x) = cos(x) - x = 0 near 0.739085... with Newton's method
  subroutine newton_small_root_demo(k)
    integer, intent(in) :: k
    real(kind=k) :: x, fx, dfx, dx
    integer :: it

    x = 0.7_k
    do it = 1, 20
       fx  = cos(x) - x
       dfx = -sin(x) - 1.0_k
       dx  = -fx/dfx
       x   = x + dx
       if (abs(dx) < 1.0e-33_k) exit
    end do
    print *, "Newton root for cos(x)-x=0 (quad):"
    print "(A,ES42.35)", "  x ≈ ", x
    print "(A,ES42.35)", "  residual |cos(x)-x|: ", abs(cos(x)-x)
  end subroutine newton_small_root_demo

end program quad_demo
userland@localhost:~$ p
