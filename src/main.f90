! =============================================================================
! main.f90
! Purpose: Program entry & interactive demo driver.
! Features:
!   - Simple command loop: show menu, read command, dispatch, repeat
!   - Demos: rng, norm, psum, adder, fft (stub), fileio (intrinsic), shapes, stats
!   - Uses REAL(8) only where utils.mean expects it; otherwise default REAL
! =============================================================================
program app
  use rng,       only: seed_with_time, uniform01, normal01
  use linalg,    only: vnorm
  use parallel,  only: parallel_sum, mc_pi
  use utils,     only: mean
  use kinds,     only: rk            ! for shapes kinds
  use shapes,    only: circle, rectangle, area_of_any
  implicit none

  integer, parameter :: dp = kind(0.0d0)   ! for utils.mean (REAL(8))
  character(len=64) :: cmd

  call seed_with_time()
  call show_menu()

  do
    write(*,'(A)',advance='no') 'demo> '
    read(*,'(A)') cmd
    call lowercase_inplace(cmd)
    cmd = trim(adjustl(cmd))

    select case (cmd)
    case ('rng');     call demo_rng()
    case ('norm');    call demo_norm()
    case ('psum');    call demo_psum()
    case ('adder');   call demo_adder()
    case ('fft');     call demo_fft()
    case ('fileio');  call demo_fileio()
    case ('shapes');  call demo_shapes()
    case ('stats');   call demo_stats()
    case ('help','menu','?'); call show_menu()
    case ('exit');    exit
    case default
      write(*,*) 'Unknown command. Type "help" for menu.'
    end select
  end do

contains

  ! =============================================================================
  ! Menu
  ! =============================================================================
  subroutine show_menu()
    implicit none
    write(*,*)
    write(*,*) ' Available demos:'
    write(*,*) '   rng     - random numbers'
    write(*,*) '   norm    - vector norm'
    write(*,*) '   psum    - parallel sum + Monte Carlo pi'
    write(*,*) '   adder   - simple addition'
    write(*,*) '   fft     - placeholder transform (reverse)'
    write(*,*) '   fileio  - write/read vector (intrinsic I/O, REAL(8))'
    write(*,*) '   shapes  - OOP shapes (area/perimeter, polymorphism)'
    write(*,*) '   stats   - statistics (mean, REAL(8))'
    write(*,*)
    write(*,*) ' Other commands:'
    write(*,*) '   help/menu/? - show this menu'
    write(*,*) '   exit        - quit'
    write(*,*)
  end subroutine show_menu

  ! =============================================================================
  ! Demo: RNG — Uniform(0,1) and Normal(0,1)
  ! =============================================================================
  subroutine demo_rng()
    implicit none
    real :: u, z
    u = uniform01()
    z = normal01()
    write(*,*) 'Uniform U(0,1):';  write(*,'(1X,ES14.6)') u
    write(*,*) 'Normal  N(0,1):';  write(*,'(1X,ES14.6)') z
  end subroutine demo_rng

  ! =============================================================================
  ! Demo: Linalg — Euclidean norm of [1..5]
  ! =============================================================================
  subroutine demo_norm()
    implicit none
    real, allocatable :: v(:)
    integer :: i
    allocate(v(5)); v = [( real(i), i=1,5 )]
    write(*,*) 'Vector [1,2,3,4,5]; norm ='
    write(*,'(1X,ES14.6)') vnorm(v)
    deallocate(v)
  end subroutine demo_norm

  ! =============================================================================
  ! Demo: Parallel — reduction + Monte Carlo π
  ! =============================================================================
  subroutine demo_psum()
    implicit none
    real, allocatable :: v(:)
    integer :: i
    real :: pi_est
    allocate(v(5)); v = [( real(i), i=1,5 )]
    write(*,*) 'Parallel sum of [1..5] ='
    write(*,'(1X,ES14.6)') parallel_sum(v)
    deallocate(v)
    write(*,*) 'Monte Carlo pi (1e6 samples) ='
    pi_est = mc_pi(1000000)
    write(*,'(1X,ES14.6)') pi_est
  end subroutine demo_psum

  ! =============================================================================
  ! Demo: Adder — trivial arithmetic (self-contained)
  ! =============================================================================
  subroutine demo_adder()
    implicit none
    integer :: a, b
    a = 2; b = 3
    write(*,*) 'Adder demo: 2 + 3 =', a + b
  end subroutine demo_adder

  ! =============================================================================
  ! Demo: FFT (stub) — reverse a complex vector to mimic a transform
  ! =============================================================================
  subroutine demo_fft()
    implicit none
    complex, allocatable :: x(:)
    integer :: n, i, j
    complex :: t
    n = 8
    allocate(x(n))
    do i = 1, n
      x(i) = cmplx(real(i), 0.0)
    end do
    i = 1; j = n
    do while (i < j)
      t = x(i); x(i) = x(j); x(j) = t
      i = i + 1; j = j - 1
    end do
    write(*,*) 'FFT stub (reverse):'
    write(*,'(1X,"Reversed:",1X,*(F6.1,1X))') real(x)
    deallocate(x)
  end subroutine demo_fft

  ! =============================================================================
  ! Demo: File I/O — write/read REAL(8) vector using intrinsic I/O
  ! =============================================================================
  subroutine demo_fileio()
    implicit none
    real(dp), allocatable :: v(:), w(:)
    integer :: i, u, ios, n
    character(len=*), parameter :: fname = 'demo_vector.txt'

    allocate(v(5))
    do i = 1, 5
      v(i) = real(i, dp)
    end do

    open(newunit=u, file=fname, status='replace', action='write', form='formatted')
    write(u,'(I0)') size(v)
    do i = 1, size(v)
      write(u,'(ES24.16)') v(i)
    end do
    close(u)

    open(newunit=u, file=fname, status='old', action='read', form='formatted', iostat=ios)
    if (ios /= 0) then
      write(*,*) "Failed to open '", fname, "' for read"
      deallocate(v); return
    end if
    read(u,*,iostat=ios) n
    if (ios /= 0 .or. n < 0) then
      write(*,*) "Invalid header in '", fname, "'"
      close(u); deallocate(v); return
    end if
    allocate(w(n))
    do i = 1, n
      read(u,*,iostat=ios) w(i)
      if (ios /= 0) then
        write(*,*) 'Read error at index ', i
        w = 0.0d0
        exit
      end if
    end do
    close(u)

    write(*,*) "Wrote then read dp vector from '", fname, "':"
    write(*,'(1X,*(ES20.12,1X))') w
    deallocate(v, w)
  end subroutine demo_fileio

  ! =============================================================================
  ! Demo: Shapes — OOP demo using your shapes.f90 (area/perimeter, polymorphism)
  ! =============================================================================
  subroutine demo_shapes()
    implicit none
    type(circle)    :: c
    type(rectangle) :: r
    real(rk)        :: a_c, p_c, a_r, p_r

    ! sample dimensions
    c%r = 3.0_rk
    r%w = 4.0_rk;  r%h = 2.5_rk

    ! type-bound calls
    a_c = c%area(); p_c = c%perimeter()
    a_r = r%area(); p_r = r%perimeter()

    write(*,*) 'Circle:    r=', c%r, '  area=', a_c, '  perim=', p_c
    write(*,*) 'Rectangle: w=', r%w, '  h=', r%h, '  area=', a_r, '  perim=', p_r

    ! polymorphic helper
    write(*,*) 'area_of_any(circle)=',    area_of_any(c)
    write(*,*) 'area_of_any(rectangle)=', area_of_any(r)
  end subroutine demo_shapes

  ! =============================================================================
  ! Demo: Stats — REAL(8) mean using utils.mean
  ! =============================================================================
  subroutine demo_stats()
    implicit none
    real(dp), allocatable :: v(:)
    integer :: i
    allocate(v(6))
    do i = 1, 6
      v(i) = 2.0d0 * real(i, dp)
    end do
    write(*,*) 'Vector(dp): [2,4,6,8,10,12]; mean ='
    write(*,'(1X,ES20.12)') mean(v)
    deallocate(v)
  end subroutine demo_stats

  ! =============================================================================
  ! Utils
  ! =============================================================================
  subroutine lowercase_inplace(s)
    character(len=*), intent(inout) :: s
    integer :: i, c
    do i = 1, len_trim(s)
      c = iachar(s(i:i))
      if (c >= 65 .and. c <= 90) s(i:i) = achar(c + 32)
    end do
  end subroutine lowercase_inplace

end program app
