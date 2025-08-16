! =============================================================================
! fileio.f90
! Purpose: NAMELIST read/write of a simple config; minimal CSV I/O for reals.
! Notes: CSV writer emits comma-separated values to match the reader.
! =============================================================================
module fileio
  use kinds
  use console
  implicit none
  private
  public :: config_t, read_config, write_config, read_csv_real, write_csv_real

  ! Simple config record to show NAMELIST I/O.
  type :: config_t
    integer :: n = 8
    real(rk) :: lo = -1.0_rk, hi = 1.0_rk
    character(len=64) :: name = 'default'
  end type

contains
  logical function read_config(path, cfg)
    ! Reads config via NAMELIST /app/.
    character(*), intent(in)  :: path
    type(config_t), intent(out) :: cfg
    integer :: u, ios
    namelist /app/ cfg
    read_config = .false.
    open(newunit=u, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      call log_error('open failed: '//trim(path))
      return
    end if
    read(u, nml=app, iostat=ios)
    close(u)
    if (ios /= 0) then
      call log_error('namelist read failed')
      return
    end if
    read_config = .true.
  end function

  logical function write_config(path, cfg)
    ! Writes config via NAMELIST /app/.
    character(*), intent(in) :: path
    type(config_t), intent(in) :: cfg
    integer :: u, ios
    namelist /app/ cfg
    write_config = .false.
    open(newunit=u, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      call log_error('open failed for write: '//trim(path))
      return
    end if
    write(u, nml=app, iostat=ios)
    close(u)
    if (ios /= 0) then
      call log_error('namelist write failed')
      return
    end if
    write_config = .true.
  end function

  logical function read_csv_real(path, m, n, a)
    ! Reads a numeric CSV into a 2D real array a(m,n).
    ! Assumes: one row per line, values separated by commas.
    character(*), intent(in) :: path
    integer,     intent(out) :: m, n
    real(rk), allocatable, intent(out) :: a(:,:)
    integer :: u, ios, i, j
    character(len=4096) :: buf
    integer :: count, pos, start, lenb
    real(rk), allocatable :: tmp(:)

    read_csv_real = .false.

    ! First pass: count rows (m) and infer columns (n) from first non-empty line.
    open(newunit=u, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) return
    m = 0; n = -1
    do
      read(u,'(A)',iostat=ios) buf
      if (ios /= 0) exit
      lenb = len_trim(buf)
      if (lenb == 0) cycle
      count = 1
      do pos=1,lenb
        if (buf(pos:pos) == ',') count = count + 1
      end do
      if (n < 0) n = count
      m = m + 1
    end do

    ! Second pass: actually parse.
    rewind(u)
    allocate(a(m,n))
    do i=1,m
      read(u,'(A)',iostat=ios) buf
      if (ios /= 0) exit
      allocate(tmp(n))
      start = 1; j = 0
      do pos=1,len_trim(buf)+1
        if (pos > len_trim(buf) .or. buf(pos:pos) == ',') then
          j = j + 1
          read(buf(start:pos-1),*,iostat=ios) tmp(j)
          start = pos + 1
        end if
      end do
      a(i,:) = tmp
      deallocate(tmp)
    end do
    close(u)
    read_csv_real = .true.
  end function

  logical function write_csv_real(path, a)
    ! Writes a 2D real array as comma-separated values to match the reader.
    character(*), intent(in) :: path
    real(rk), intent(in) :: a(:,:)
    integer :: u, ios, i, j, ncol
    write_csv_real = .false.
    open(newunit=u, file=trim(path), status='replace', action='write', iostat=ios)
    if (ios /= 0) return
    ncol = size(a,2)
    do i=1,size(a,1)
      do j=1,ncol
        write(u,'(G0)', advance='no') a(i,j)     ! value
        if (j < ncol) write(u,'(A)', advance='no') ','  ! comma between values
      end do
      write(u,*)                                 ! newline
    end do
    close(u)
    write_csv_real = .true.
  end function
end module fileio
