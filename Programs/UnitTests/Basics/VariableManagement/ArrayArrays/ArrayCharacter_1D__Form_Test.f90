program ArrayCharacter_1D__Form_Test

  use Specifiers
  use ArrayCharacter_1D__Form

  implicit none

  integer ( KDI ), parameter :: &
    SIZE = 4
  integer ( KDI ) :: &
    i
  type ( ArrayCharacter_1D_Form ) :: &
    AC

  call AC % Initialize ( SIZE, iLowerBoundOption = -1 )
  AC % Value ( -1 ) = "These are the times"
  AC % Value ( 0 ) = "Bad to the bone"
  AC % Value ( 1 ) = "Mutha f***a!"
  AC % Value ( 2 ) = "Are you kidding me?"
  print *
  print *, &
    'lbound ( AC % Value ) = ', lbound ( AC % Value )
  print *, 'AC % Value = ', AC % Value

end program ArrayCharacter_1D__Form_Test
