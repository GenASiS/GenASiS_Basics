program ArrayReal_1D__Form_Test

  use Specifiers
  use ArrayReal_1D__Form

  implicit none

  integer ( KDI ), parameter :: &
    SIZE = 4
  integer ( KDI ) :: &
    i
  type ( ArrayReal_1D_Form ), dimension ( 3 ) :: &
    AR_1D

  call AR_1D ( 1 ) % Initialize ( SIZE, ClearOption = .true. )
  print *
  print *, 'AR_1D ( 1 ) % Value = ', AR_1D ( 1 ) % Value 

  call AR_1D ( 2 ) % Initialize ( SIZE, iLowerBoundOption = -1 )
  associate ( PI => CONSTANT % PI )
  AR_1D ( 2 ) % Value = [ ( i * PI, i = 1, SIZE ) ]
  end associate
  print *
  print *, &
    'lbound ( AR_1D ( 2 ) % Value ) = ', lbound ( AR_1D ( 2 ) % Value )
  print *, 'AR_1D ( 2 ) % Value = ', AR_1D ( 2 ) % Value

  call AR_1D ( 3 ) % Initialize ( AR_1D ( 2 ) )
  print *
  print *, &
    'lbound ( AR_1D ( 3 ) % Value ) = ', lbound ( AR_1D ( 3 ) % Value )
  print *, 'AR_1D ( 3 ) % Value = ', AR_1D ( 3 ) % Value

end program ArrayReal_1D__Form_Test
