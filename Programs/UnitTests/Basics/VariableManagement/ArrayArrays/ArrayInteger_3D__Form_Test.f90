program ArrayInteger_3D__Form_Test

  use Specifiers
  use ArrayInteger_3D__Form

  implicit none
  
  integer ( KDI ), dimension ( 3 ), parameter :: &
    SHAPE = [ 4, 3, 2 ]
  integer ( KDI ) :: &
    i
  type ( ArrayInteger_3D_Form ), dimension ( 3 ) :: &
    AI_3D

  call AI_3D ( 1 ) % Initialize ( SHAPE, ClearOption = .true. )
  print *
  print *, 'AI_3D ( 1 ) % Value = ', AI_3D ( 1 ) % Value 

  call AI_3D ( 2 ) % Initialize ( SHAPE, iaLowerBoundOption = [ -1, -1, -1 ] )
  AI_3D ( 2 ) % Value = reshape ( [ ( i, i = 1, product ( SHAPE ) ) ], SHAPE )
  print *
  print *, &
    'lbound ( AI_3D ( 2 ) % Value ) = ', lbound ( AI_3D ( 2 ) % Value )
  print *, 'AI_3D ( 2 ) % Value = ', AI_3D ( 2 ) % Value

  call AI_3D ( 3 ) % Initialize ( AI_3D ( 2 ) )
  print *
  print *, &
    'lbound ( AI_3D ( 3 ) % Value ) = ', lbound ( AI_3D ( 3 ) % Value )
  print *, 'AI_3D ( 3 ) % Value = ', AI_3D ( 3 ) % Value

end program ArrayInteger_3D__Form_Test
