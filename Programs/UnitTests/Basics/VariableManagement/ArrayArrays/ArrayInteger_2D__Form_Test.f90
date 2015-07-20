program ArrayInteger_2D__Form_Test

  use Specifiers
  use ArrayInteger_2D__Form

  implicit none
  
  integer ( KDI ), dimension ( 2 ), parameter :: &
    SHAPE = [ 4, 3 ]
  integer ( KDI ) :: &
    i
  type ( ArrayInteger_2D_Form ), dimension ( 3 ) :: &
    AI_2D

  call AI_2D ( 1 ) % Initialize ( SHAPE, ClearOption = .true. )
  print *
  print *, 'AI_2D ( 1 ) % Value = ', AI_2D ( 1 ) % Value 

  call AI_2D ( 2 ) % Initialize ( SHAPE, iaLowerBoundOption = [ -1, -1 ] )
  AI_2D ( 2 ) % Value = reshape ( [ ( i, i = 1, product ( SHAPE ) ) ], SHAPE )
  print *
  print *, &
    'lbound ( AI_2D ( 2 ) % Value ) = ', lbound ( AI_2D ( 2 ) % Value )
  print *, 'AI_2D ( 2 ) % Value = ', AI_2D ( 2 ) % Value

  call AI_2D ( 3 ) % Initialize ( AI_2D ( 2 ) )
  print *
  print *, &
    'lbound ( AI_2D ( 3 ) % Value ) = ', lbound ( AI_2D ( 3 ) % Value )
  print *, 'AI_2D ( 3 ) % Value = ', AI_2D ( 3 ) % Value

end program ArrayInteger_2D__Form_Test
