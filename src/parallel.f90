!==============================================================
! parallel.f90
! OpenMP-based parallel utilities.
!
! Backward-compatible export (used by main):
!   - parallel_sum(x): sum of vector x using reduction
!
! Additional demo routine:
!   - mc_pi(n)       : Monte Carlo estimate of pi using RNG (single-threaded core,
!                      but the sampling loop is parallelized)
!     NOTE: this USEs module rng; make sure rng.f90 is compiled before this file.
!==============================================================
module parallel
  use omp_lib
  use rng, only: seed_with_time, uniform01
  implicit none
  private
  public :: parallel_sum, mc_pi

contains

  !------------------------------------------------------------
  ! Parallel reduction sum over a vector.
  !------------------------------------------------------------
  real function parallel_sum(x)
    real, intent(in) :: x(:)
    integer :: i
    parallel_sum = 0.0
!$omp parallel do reduction(+:parallel_sum)
    do i = 1, size(x)
      parallel_sum = parallel_sum + x(i)
    end do
!$omp end parallel do
  end function parallel_sum

  !------------------------------------------------------------
  ! Monte Carlo estimate of pi by sampling points in the unit square.
  ! Each thread keeps its own hit count; reduction at the end.
  ! Seeding once per call; RNG calls are thread-safe with intrinsic random_number,
  ! but sequences overlap between threads on some compilers. For a demo, OK.
  !------------------------------------------------------------
  real function mc_pi(n)
    integer, intent(in) :: n
    integer :: i, hits
    real :: x, y
    hits = 0
    call seed_with_time()
!$omp parallel private(x,y,i) reduction(+:hits)
!$omp do
    do i = 1, n
      call random_number(x)
      call random_number(y)
      if (x*x + y*y <= 1.0) hits = hits + 1
    end do
!$omp end do
!$omp end parallel
    mc_pi = 4.0 * real(hits) / real(n)
  end function mc_pi

end module parallel
