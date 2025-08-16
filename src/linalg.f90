!==============================================================
! linalg.f90
! Linear algebra helpers used by the demos.
!
! Exports:
!   - vnorm(x)   : Euclidean norm of a real vector (renamed to avoid
!                  shadowing the Fortran intrinsic NORM2)
!   - dot(x,y)   : dot product
!   - unit(x)    : normalized copy of x
!   - axpy(a,x,y): y := a*x + y
!   - scal(a,x)  : x := a*x
!   - proj(u,v)  : projection of v onto u
!   - matvec(A,x): matrix-vector product
!==============================================================
module linalg
  implicit none
  private
  public :: vnorm, dot, unit, axpy, scal, proj, matvec

contains

  ! Euclidean 2-norm (name chosen to avoid intrinsic NORM2)
  real function vnorm(x)
    real, intent(in) :: x(:)
    vnorm = sqrt(sum(x*x))
  end function vnorm

  real function dot(x,y)
    real, intent(in) :: x(:), y(:)
    if (size(x) /= size(y)) then
      print *, "dot(): size mismatch: ", size(x), size(y)
      dot = 0.0
      return
    end if
    dot = sum(x*y)
  end function dot

  function unit(x) result(u)
    real, intent(in) :: x(:)
    real, allocatable :: u(:)
    real :: n
    allocate(u(size(x)))
    n = vnorm(x)
    if (n > 0.0) then
      u = x / n
    else
      u = 0.0
    end if
  end function unit

  subroutine axpy(a, x, y)
    real, intent(in)    :: a
    real, intent(in)    :: x(:)
    real, intent(inout) :: y(:)
    if (size(x) /= size(y)) then
      print *, "axpy(): size mismatch: ", size(x), size(y)
      return
    end if
    y = a*x + y
  end subroutine axpy

  subroutine scal(a, x)
    real, intent(in)    :: a
    real, intent(inout) :: x(:)
    x = a*x
  end subroutine scal

  function proj(u, v) result(p)
    real, intent(in) :: u(:), v(:)
    real, allocatable :: p(:)
    real :: uu
    allocate(p(size(u)))
    if (size(u) /= size(v)) then
      print *, "proj(): size mismatch: ", size(u), size(v)
      p = 0.0
      return
    end if
    uu = dot(u,u)
    if (uu > 0.0) then
      p = (dot(u,v)/uu) * u
    else
      p = 0.0
    end if
  end function proj

  function matvec(A, x) result(y)
    real, intent(in) :: A(:,:), x(:)
    real, allocatable :: y(:)
    integer :: m, n
    m = size(A,1); n = size(A,2)
    if (size(x) /= n) then
      print *, "matvec(): size mismatch: A is (", m, ",", n, "), x is ", size(x)
      allocate(y(0))
      return
    end if
    allocate(y(m))
    y = matmul(A, x)
  end function matvec

end module linalg
