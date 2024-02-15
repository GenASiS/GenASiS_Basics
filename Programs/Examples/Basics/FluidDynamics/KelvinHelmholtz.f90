program KelvinHelmholtz

  use Basics
  use KelvinHelmholtz_Form

  implicit none

  type ( KelvinHelmholtzForm ) :: &
   KH 

  allocate ( PROGRAM_HEADER )
  call PROGRAM_HEADER % Initialize ( 'KelvinHelmholtz' )

  call KH % Initialize ( )
  call KH % Evolve ( )

  deallocate ( PROGRAM_HEADER )

end program KelvinHelmholtz 
