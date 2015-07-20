program CurveImage_ASCII__Form_Test

  use VariableManagement
  use Display
  use MessagePassing
  use GridImageStream_ASCII__Form
  use CurveImage_ASCII__Form
  
  implicit none
  
  integer ( KDI ) :: &
    iC, &
    oC_1, &
    DisplayRank = 0
  real ( KDR ), dimension ( 20 ) :: &
    NodeCoordinate
  character ( LDF ) :: &
    Name = 'CurveImage_ASCII__Form_Test'
  character ( LDL ), dimension ( 6 ) :: &
    VariableName
  type ( VariableGroupForm ) :: &
    VG
  type ( CommunicatorForm ), allocatable :: &
    C
  type ( GridImageStream_ASCII_Form ) :: &
    GIS
  type ( CurveImage_ASCII_Form ) :: & 
    CI
  
  allocate ( C )
  call C % Initialize ( )
  
  call CONSOLE % Initialize ( C % Rank )
  call CONSOLE % SetDisplayRank ( DisplayRank )

  NodeCoordinate =  [ ( iC * 0.5_KDR, iC = 1, 20 ) ]
  
  oC_1 = C % Rank * maxval ( NodeCoordinate )
  NodeCoordinate = NodeCoordinate  + oC_1
  
  VariableName ( 1 ) = 'VariableName_1'
  VariableName ( 2 ) = 'VariableName_2'
  VariableName ( 3 ) = 'VariableName_3'
  VariableName ( 4 ) = 'VariableName_4'
  VariableName ( 5 ) = 'VariableName_5'
  VariableName ( 6 ) = 'VariableName_6'
  
  call VG % Initialize &
         ( [ 20, 6 ], VariableOption = VariableName, &
           NameOption = 'VariableGroup' )
  VG % Value ( :, 1 ) &
    = [ ( ( iC + 20 * C % Rank ) * 2.0_KDR, iC = 1, 20 ) ] 
  call random_number ( VG % Value ( :, 2 : 6 ) )
    
  call Show ( NodeCoordinate, 'NodeCoordinate' )
  call Show ( VG % Value ( :, 1 ), 'Variable' )

  call GIS % Initialize ( Name, CommunicatorOption = C )
  
  call GIS % Open ( GIS % ACCESS_CREATE )
  
  call CI % Initialize ( GIS )
  
  call CI % AddVariableGroup ( VG )

  call CI % SetGrid  &
         ( Directory = 'Curves', NodeCoordinate = NodeCoordinate, &
           nProperCells = 20, oValue = 0 )

  call CI % Write ()

  call GIS % Close ( ) 

  deallocate ( C )
  
end program CurveImage_ASCII__Form_Test
