! ---------------------------------------------------------------------------
! kinds.f90
! Purpose: Centralize numeric kinds and commonly used I/O units.
! Why: Keeps numeric precision consistent across the project.
! ---------------------------------------------------------------------------
module kinds
  use iso_fortran_env, only: real32, real64, int32, int64, output_unit, error_unit
  implicit none
  integer, parameter :: rk = real64   ! default real kind for numerics
  integer, parameter :: ik = int64    ! default integer kind for counters/products
end module kinds

