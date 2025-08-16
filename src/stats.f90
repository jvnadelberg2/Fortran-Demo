! =============================================================================
! stats.f90
! Purpose: Basic statistics utilities (median, percentile, histogram).
! Highlights: PURE helper (sort), copy-sort approach, simple interpolation.
! =============================================================================
module stats
  use kinds
  use utils, only: mean
  implicit none
  private
  public :: median, percentile, histogram

contains
  pure subroutine sort_real(a)
    ! Educational insertion sort (O(n^2)). PURE for illustration.
    real(rk), intent(inout) :: a(:)
    integer :: i, j
    real(rk) :: key
    do i = 2, size(a)
      key = a(i); j = i - 1
      do while (j >= 1 .and. a(j) > key)
        a(j+1) = a(j); j = j - 1
      end do
      a(j+1) = key
    end do
  end subroutine

  real(rk) function median(x) result(m)
    ! Median via copy + sort.
    real(rk), intent(in) :: x(:)
    real(rk), allocatable :: t(:)
    integer :: n
    n = size(x)
    if (n == 0) then
      m = 0.0_rk; return
    end if
    t = x
    call sort_real(t)
    if (mod(n,2) == 1) then
      m = t((n+1)/2)
    else
      m = 0.5_rk*(t(n/2) + t(n/2+1))
    end if
  end function

  real(rk) function percentile(x, p) result(v)
    ! p-th percentile (0..100). Linear interpolation between neighbors.
    real(rk), intent(in) :: x(:)
    real(rk), intent(in) :: p
    real(rk), allocatable :: t(:)
    real(rk) :: pos
    integer :: n, i0, i1
    if (size(x) == 0) then
      v = 0.0_rk; return
    end if
    t = x
    call sort_real(t)
    n = size(t)
    pos = (p/100.0_rk) * real(n-1, rk) + 1.0_rk    ! 1-based position
    i0 = max(1, min(n, floor(pos)))
    i1 = max(1, min(n, ceiling(pos)))
    v = t(i0) + (pos - real(i0, rk))*(t(i1)-t(i0))
  end function

  subroutine histogram(x, nbins, lo, hi, counts, edges)
    ! Histogram counts within [lo, hi) into nbins equally spaced bins.
    real(rk), intent(in)  :: x(:)
    integer,  intent(in)  :: nbins
    real(rk), intent(in)  :: lo, hi
    integer,   allocatable, intent(out) :: counts(:)
    real(rk),  allocatable, intent(out) :: edges(:)
    integer :: i, b
    real(rk) :: w, xx

    allocate(counts(nbins)); counts = 0
    allocate(edges(nbins+1))

    w = (hi - lo) / real(nbins, rk)

    ! Bin edges (inclusive lo, exclusive hi convention).
    do i=0,nbins
      edges(i+1) = lo + w*real(i, rk)
    end do

    ! Count only values inside [lo,hi).
    do i=1,size(x)
      xx = x(i)
      if (xx < lo .or. xx >= hi) cycle
      b = int((xx - lo)/w) + 1
      if (b < 1) b = 1
      if (b > nbins) b = nbins
      counts(b) = counts(b) + 1
    end do
  end subroutine
end module stats
