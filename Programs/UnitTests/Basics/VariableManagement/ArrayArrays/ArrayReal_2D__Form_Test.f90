program ArrayReal_2D__Form_Test

  use Specifiers
  use ArrayReal_2D__Form

  implicit none
  
  integer ( KDI ), dimension ( 2 ), parameter :: &
    SHAPE = [ 4, 3 ]
  integer ( KDI ) :: &
    i
  type ( ArrayReal_2D_Form ), dimension ( 3 ) :: &
    AR_2D

  call AR_2D ( 1 ) % Initialize ( SHAPE, ClearOption = .true. )
  print *
  print *, 'AR_2D ( 1 ) % Value = ', AR_2D ( 1 ) % Value 

  call AR_2D ( 2 ) % Initialize ( SHAPE, iaLowerBoundOption = [ -1, -1 ] )
  associate ( PI => CONSTANT % PI )
  AR_2D ( 2 ) % Value &
    = reshape ( [ ( i * PI, i = 1, product ( SHAPE ) ) ], SHAPE )
  end associate
  print *
  print *, &
    'lbound ( AR_2D ( 2 ) % Value ) = ', lbound ( AR_2D ( 2 ) % Value )
  print *, 'AR_2D ( 2 ) % Value = ', AR_2D ( 2 ) % Value

  call AR_2D ( 3 ) % Initialize ( AR_2D ( 2 ) )
  print *
  print *, &
    'lbound ( AR_2D ( 3 ) % Value ) = ', lbound ( AR_2D ( 3 ) % Value )
  print *, 'AR_2D ( 3 ) % Value = ', AR_2D ( 3 ) % Value

end program ArrayReal_2D__Form_Test
