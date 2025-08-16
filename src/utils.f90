! =============================================================================
! utils.f90
! Purpose: General utilities (math helpers, sequences, generics, operators).
! Highlights:
!   - elemental function (clamp)
!   - recursive function (factorial)
!   - generic interface (swap)
!   - user-defined operator (.dot.) for dot product
! =============================================================================
module utils
  use kinds
  implicit none
  private
  public :: clamp, linspace, factorial, mean, swap, operator(.dot.)

  ! Generic interfaces let us overload by argument types.
  interface swap
    module procedure swap_real, swap_int
  end interface
  interface operator(.dot.)
    module procedure dot_r
  end interface

contains
  elemental real(rk) function clamp(x, lo, hi) result(y)
    ! Clamp x into [lo,hi]. "elemental" => works on scalars and arrays.
    real(rk), intent(in) :: x, lo, hi
    if (x < lo) then
      y = lo
    else if (x > hi) then
      y = hi
    else
      y = x
    end if
  end function

  function linspace(a, b, n) result(v)
    ! Returns n linearly spaced points from a to b (inclusive).
    real(rk), intent(in) :: a, b
    integer,  intent(in) :: n
    real(rk), allocatable :: v(:)
    integer :: i
    if (n < 2) then
      allocate(v(1)); v = a; return
    end if
    allocate(v(n))
    do i = 1, n
      v(i) = a + (b - a) * real(i-1, rk) / real(n-1, rk)
    end do
  end function

  recursive function factorial(n) result(f)
    ! Classic recursive factorial (64-bit to reduce overflow).
    integer, intent(in) :: n
    integer(ik) :: f
    if (n < 2) then
      f = 1_ik
    else
      f = int(n, ik) * factorial(n-1)
    end if
  end function

  real(rk) function mean(x) result(m)
    ! Arithmetic mean; returns 0.0 for empty vectors.
    real(rk), intent(in) :: x(:)
    if (size(x) == 0) then
      m = 0.0_rk
    else
      m = sum(x) / real(size(x), rk)
    end if
  end function

  subroutine swap_real(a, b)
    ! Swap two reals (first specific for generic "swap").
    real(rk), intent(inout) :: a, b
    real(rk) :: t
    t = a; a = b; b = t
  end subroutine

  subroutine swap_int(a, b)
    ! Swap two default integers (second specific for generic "swap").
    integer, intent(inout) :: a, b
    integer :: t
    t = a; a = b; b = t
  end subroutine

  real(rk) function dot_r(a, b) result(d)
    ! Dot product via SUM of elementwise products.
    real(rk), intent(in) :: a(:), b(:)
    d = sum(a*b)
  end function
end module utils
