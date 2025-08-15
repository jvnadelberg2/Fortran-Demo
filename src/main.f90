program main
  use adder
  implicit none
  integer :: argc, a, b, ios
  character(len=32) :: s1, s2

  argc = command_argument_count()
  if (argc >= 2) then
    call get_command_argument(1, s1)
    call get_command_argument(2, s2)
    read(s1, *, iostat=ios) a
    if (ios /= 0) then
      print *, "Invalid first integer:", trim(s1)
      stop 1
    end if
    read(s2, *, iostat=ios) b
    if (ios /= 0) then
      print *, "Invalid second integer:", trim(s2)
      stop 1
    end if
  else
    a = 2
    b = 3
  end if

  print *, "Hello, Fortran 2008."
  print *, a, "+", b, "=", add(a,b)
end program main
