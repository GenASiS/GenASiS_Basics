program ArrayInteger_1D__Form_Test

  use Specifiers
  use ArrayInteger_1D__Form

  implicit none

  integer ( KDI ), parameter :: &
    SIZE = 4
  integer ( KDI ) :: &
    i
  type ( ArrayInteger_1D_Form ), dimension ( 3 ) :: &
    AI_1D

  call AI_1D ( 1 ) % Initialize ( SIZE, ClearOption = .true. )
  print *
  print *, 'AI_1D ( 1 ) % Value = ', AI_1D ( 1 ) % Value 

  call AI_1D ( 2 ) % Initialize ( SIZE, iLowerBoundOption = -1 )
  AI_1D ( 2 ) % Value = [ ( i, i = 1, SIZE ) ]
  print *
  print *, &
    'lbound ( AI_1D ( 2 ) % Value ) = ', lbound ( AI_1D ( 2 ) % Value )
  print *, 'AI_1D ( 2 ) % Value = ', AI_1D ( 2 ) % Value

  call AI_1D ( 3 ) % Initialize ( AI_1D ( 2 ) )
  print *
  print *, &
    'lbound ( AI_1D ( 3 ) % Value ) = ', lbound ( AI_1D ( 3 ) % Value )
  print *, 'AI_1D ( 3 ) % Value = ', AI_1D ( 3 ) % Value

end program ArrayInteger_1D__Form_Test
