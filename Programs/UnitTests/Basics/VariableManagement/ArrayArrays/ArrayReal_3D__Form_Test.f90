program ArrayReal_3D__Form_Test

  use Specifiers
  use ArrayReal_3D__Form

  implicit none
  
  integer ( KDI ), dimension ( 3 ), parameter :: &
    SHAPE = [ 4, 3, 2 ]
  integer ( KDI ) :: &
    i
  type ( ArrayReal_3D_Form ), dimension ( 3 ) :: &
    AR_3D

  call AR_3D ( 1 ) % Initialize ( SHAPE, ClearOption = .true. )
  print *
  print *, 'AR_3D ( 1 ) % Value = ', AR_3D ( 1 ) % Value 

  call AR_3D ( 2 ) % Initialize ( SHAPE, iaLowerBoundOption = [ -1, -1, -1 ] )
  associate ( PI => CONSTANT % PI )
  AR_3D ( 2 ) % Value &
    = reshape ( [ ( i * PI, i = 1, product ( SHAPE ) ) ], SHAPE )
  end associate
  print *
  print *, &
    'lbound ( AR_3D ( 2 ) % Value ) = ', lbound ( AR_3D ( 2 ) % Value )
  print *, 'AR_3D ( 2 ) % Value = ', AR_3D ( 2 ) % Value

  call AR_3D ( 3 ) % Initialize ( AR_3D ( 2 ) )
  print *
  print *, &
    'lbound ( AR_3D ( 3 ) % Value ) = ', lbound ( AR_3D ( 3 ) % Value )
  print *, 'AR_3D ( 3 ) % Value = ', AR_3D ( 3 ) % Value

end program ArrayReal_3D__Form_Test
