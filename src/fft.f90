! =============================================================================
! fft.f90
! Purpose: Recursive radix-2 Cooley–Tukey FFT (educational, not optimized).
! Notes: Length must be a power of 2. Set inverse=.true. for inverse FFT.
! =============================================================================
module fft
  use kinds
  implicit none
  private
  public :: fft_radix2

contains
  recursive subroutine fft_radix2(x, inverse)
    ! In-place radix-2 FFT. Splits into even/odd, recurses, then combines.
    complex(rk), intent(inout) :: x(:)
    logical,     intent(in)    :: inverse
    integer :: n, k
    complex(rk), allocatable :: xe(:), xo(:)
    complex(rk) :: wk, t
    real(rk) :: ang, sgn

    n = size(x)
    if (n == 1) return
    if (iand(n, n-1) /= 0) stop 'fft_radix2: n must be power of 2'

    ! Even/odd split and recurse.
    allocate(xe(n/2), xo(n/2))
    xe = x(1:n:2)
    xo = x(2:n:2)
    call fft_radix2(xe, inverse)
    call fft_radix2(xo, inverse)

    ! Combine using twiddle factors.
    sgn = merge(-1.0_rk, 1.0_rk, inverse)  ! -1 for inverse, +1 for forward
    do k = 0, n/2 - 1
      ang = sgn * 2.0_rk * acos(-1.0_rk) * real(k, rk) / real(n, rk)
      wk = cmplx(cos(ang), sin(ang), kind=rk)
      t = wk * xo(k+1)
      x(k+1)       = xe(k+1) + t
      x(k+1+n/2)   = xe(k+1) - t
    end do

    ! Normalize on inverse (this version divides by 2 each merge).
    if (inverse) x = x / 2.0_rk
  end subroutine
end module fft
