!==============================================================
! rng.f90
! Random number utilities for the demo.
! Provides:
!   - seed_with_time : seed PRNG from system clock
!   - seed_with      : seed PRNG with user-provided integers
!   - uniform01      : Uniform(0,1) variate
!   - normal01       : Standard Normal N(0,1) variate
!==============================================================

module rng
  implicit none
  private
  public :: seed_with_time, seed_with, uniform01, normal01

contains

  !------------------------------------------------------------
  ! Seed RNG using system clock. Single official entry point.
  !------------------------------------------------------------
  subroutine seed_with_time()
    integer :: n, t, i
    integer, allocatable :: seed(:)

    call random_seed(size=n)
    allocate(seed(n))
    call system_clock(count=t)

    do i = 1, n
      seed(i) = iand(2147483647, t + 104729*i)
    end do

    call random_seed(put=seed)
  end subroutine seed_with_time

  !------------------------------------------------------------
  ! Seed RNG with caller-provided integers.
  !------------------------------------------------------------
  subroutine seed_with(vals)
    integer, intent(in) :: vals(:)
    integer :: n, m
    integer, allocatable :: seed(:)

    call random_seed(size=n)
    allocate(seed(n))
    seed = 0
    m = min(n, size(vals))
    if (m > 0) seed(1:m) = vals(1:m)
    if (all(seed == 0)) seed(1) = 123456789
    call random_seed(put=seed)
  end subroutine seed_with

  !------------------------------------------------------------
  ! Uniform(0,1) variate.
  !------------------------------------------------------------
  real function uniform01()
    call random_number(uniform01)
  end function uniform01

  !------------------------------------------------------------
  ! Standard Normal(0,1) via Box–Muller.
  !------------------------------------------------------------
  real function normal01()
    real :: u1, u2, pi
    pi = acos(-1.0)
    call random_number(u1); if (u1 <= 0.0) u1 = 1.0e-6
    call random_number(u2)
    normal01 = sqrt(-2.0*log(u1)) * cos(2.0*pi*u2)
  end function normal01

end module rng
