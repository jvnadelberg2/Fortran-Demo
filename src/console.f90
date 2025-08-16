! =============================================================================
! console.f90
! Purpose: Small console helpers for consistent stdout/stderr formatting.
! Why: Keep I/O formatting logic in one place and keep demos tidy.
! =============================================================================
module console
  use iso_fortran_env, only: output_unit, error_unit
  implicit none
contains
  subroutine print_header(title)
    ! Prints a banner header like:
    !   ============
    !     Title
    !   ============
    character(*), intent(in) :: title
    write(output_unit, '(a)') repeat('=', len_trim(title)+4)
    write(output_unit, '(a)') '  ' // trim(title)
    write(output_unit, '(a)') repeat('=', len_trim(title)+4)
  end subroutine

  subroutine print_kv(key, val)
    ! Prints "key: value"
    character(*), intent(in) :: key, val
    write(output_unit, '(a, a)') adjustl(key)//': ', trim(val)
  end subroutine

  subroutine log_error(msg)
    ! Prints an error to stderr with a prefix.
    character(*), intent(in) :: msg
    write(error_unit, '(a)') 'ERROR: ' // trim(msg)
  end subroutine
end module console
