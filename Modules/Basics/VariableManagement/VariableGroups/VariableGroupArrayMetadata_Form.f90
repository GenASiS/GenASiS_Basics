!-- VariableGroupArrayMetadataForm allows bundling of an array of 
!   VariableGroup and stores information about each element of the array.

module VariableGroupArrayMetadata_Form

  use Specifiers
  use VariableGroup_Form
  
  implicit none
  private

  type, public :: VariableGroupArrayMetadataForm
    integer ( KDI ) :: &
      nGroups         = 0, &
      nVariablesTotal = 0
    integer ( KDI ), dimension ( : ), allocatable :: &
      nVariables
    type ( VariableGroupForm ), dimension ( : ), allocatable :: &
      VariableGroup
  contains
    procedure, public, pass :: &
      Initialize
    final :: &
      Finalize
  end type VariableGroupArrayMetadataForm

contains

  
  subroutine Initialize ( VGAM, VG )

    class ( VariableGroupArrayMetadataForm ), intent ( inout ) :: &
      VGAM
    class ( VariableGroupForm ), dimension ( : ), intent ( in ), target :: &
      VG

    integer ( KDI ) :: &
      iG  !-- iGroup

    VGAM % nGroups = size ( VG )

    allocate ( VGAM % nVariables ( VGAM % nGroups ) )
    VGAM % nVariables &
      = [ ( VG ( iG ) % nVariables, iG = 1, VGAM % nGroups ) ]           

    VGAM % nVariablesTotal = sum ( VGAM % nVariables )
    
    allocate ( VGAM % VariableGroup ( VGAM % nGroups ) )
    do iG = 1, VGAM % nGroups
      call VGAM % VariableGroup ( iG ) % Initialize ( VG ( iG ) )
    end do

  end subroutine Initialize


  elemental subroutine Finalize ( VGAM )

    type ( VariableGroupArrayMetadataForm ), intent ( inout ) :: &
      VGAM
    
    if ( allocated ( VGAM % VariableGroup ) ) &
      deallocate ( VGAM % VariableGroup )
    
    if ( allocated ( VGAM % nVariables ) ) deallocate ( VGAM % nVariables )

  end subroutine Finalize


end module VariableGroupArrayMetadata_Form
