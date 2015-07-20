program GridImageStreamGnuplot_Form_Test

  use VariableManagement
  use Display
  use MessagePassing
  use GridImageStreamGnuplot_Form

  implicit none
  
  integer ( KDI ) :: &
    iStep
  character ( LDF ) :: &
    Name = 'StreamGnuplot_Form_Test'
  type ( CommunicatorForm ), allocatable :: &
    C
  type ( GridImageStreamGnuplotForm ), allocatable :: &
    GIS
  
  allocate ( C )
  call C % Initialize ( )
  call CONSOLE % Initialize ( C % Rank )
  call CONSOLE % SetVerbosity ( 'INFO_7' )

  allocate ( GIS )
  call GIS % Initialize ( Name, CommunicatorOption = C )
  
  do iStep = 1, 5

    call GIS % Open ( GIS % ACCESS_CREATE )
    
    call Show ( GIS % IsWritable ( ), 'Is Stream Writable ?' )
    call Show ( GIS % IsReadable ( ), 'Is Stream Readable ?' )
    
    call GIS % Close ( )
    
    call Show ( GIS % IsWritable ( ), 'Is Stream Writable ?' )
    call Show ( GIS % IsReadable ( ), 'Is Stream Readable ?' )
    
    call GIS % Open ( GIS % ACCESS_WRITE )
    
    call Show ( GIS % IsWritable ( ), 'Is Stream Writable ?' )
    call Show ( GIS % IsReadable ( ), 'Is Stream Readable ?' )
    
    call GIS % Close ( )
    
    call GIS % Open ( GIS % ACCESS_READ, NumberOption = iStep - 1 )
    
    call Show ( GIS % IsWritable ( ), 'Is Stream Writable ?' )
    call Show ( GIS % IsReadable ( ), 'Is Stream Readable ?' )
    
    call GIS % Close ( )
    
  end do
  
  call GIS % Open ( GIS % ACCESS_WRITE, NumberOption = 0 )
  
  call GIS % Close ( ) 
  
  call GIS % Open ( GIS % ACCESS_CREATE ) 
  
  call GIS % Close ( ) 

  deallocate ( GIS )
  deallocate ( C )

end program GridImageStreamGnuplot_Form_Test
