!-- PackedVariableGroupForm loads and stores selected rows and columns of a
!   VariableGroup data array into a contiguous data array. 

module PackedVariableGroup_Form

  use Specifiers
  use VariableGroup_Form
  
  implicit none
  private
  
  type, public :: PackedVariableGroupForm
    integer ( KDI ) :: &
      nValues    = 0, &
      nVariables = 0
    integer ( KDI ), dimension ( : ), pointer :: &
      iaUnpacked => null ( )
    real ( KDR ), dimension ( :, : ), allocatable :: &
      Value
    type ( VariableGroupForm ), allocatable :: &
      VariableGroup
  contains
    procedure, public, pass :: &
      Initialize
    procedure, public, pass :: &
      SetIndexArray
    procedure, public, pass :: &
      Load
    procedure, public, pass :: &
      Store
    final :: &
      Finalize
  end type PackedVariableGroupForm

    private :: &
      LoadVariable, &
      StoreVariable

contains


  subroutine Initialize ( PVG, VG, nMaxValues )

    class ( PackedVariableGroupForm ), intent ( inout ) :: &
      PVG
    class ( VariableGroupForm ), intent ( in ) :: &
      VG
    integer ( KDI ), intent ( in ) :: &
      nMaxValues
      
    PVG % nVariables = VG % nVariables
    
    allocate ( PVG % Value ( nMaxValues, PVG % nVariables ) )

    allocate ( PVG % VariableGroup )
    call PVG % VariableGroup % Initialize ( VG )

  end subroutine Initialize
  
  
  subroutine SetIndexArray ( PVG, iaUnpacked )
    
    class ( PackedVariableGroupForm ), intent ( inout ) :: &
      PVG
    integer ( KDI ), dimension ( : ), intent ( in ), target :: &
      iaUnpacked

    PVG % nValues = min ( size ( iaUnpacked ), size ( PVG % Value, dim = 1 ) )
    if ( size ( iaUnpacked ) > 0 ) PVG % iaUnpacked => iaUnpacked
  
  end subroutine SetIndexArray


  subroutine Load ( PVG )
    
    class ( PackedVariableGroupForm ), intent ( inout ), target :: &
      PVG
      
    integer ( KDI ) :: &
      iV  !-- iVariable
    
    if ( .not. associated ( PVG % iaUnpacked ) ) return
    
    associate ( VG => PVG % VariableGroup )

    do iV = 1, PVG % nVariables
      call LoadVariable (  &
             PVG % Value ( :, iV ), VG % Value ( :, VG % Selected ( iV ) ), &
             PVG % iaUnpacked, PVG % nValues )
    end do

    end associate

  end subroutine Load
  
    
  subroutine Store ( PVG )
    
    class ( PackedVariableGroupForm ), intent ( inout ), target :: &
      PVG
    
    integer ( KDI ) :: &
      iV  !-- iVariable
    
    if ( .not. associated ( PVG % iaUnpacked ) ) return
    
    associate ( VG => PVG % VariableGroup )

    do iV = 1, PVG % nVariables
      call StoreVariable &
             ( VG % Value ( :, VG % Selected ( iV ) ), PVG % Value ( :, iV ), &
               PVG % iaUnpacked, PVG % nValues )
    end do
    
    end associate
  
  end subroutine Store


  elemental subroutine Finalize ( PVG )

    type ( PackedVariableGroupForm ), intent ( inout ) :: &
      PVG

    if ( allocated ( PVG % VariableGroup ) ) deallocate ( PVG % VariableGroup )
   
    if ( allocated ( PVG % Value ) ) deallocate ( PVG % Value )

    nullify ( PVG % iaUnpacked )

  end subroutine Finalize
  

  subroutine LoadVariable &
               ( PackedValue, UnpackedValue, iaUnpacked, nValues )
      
    real ( KDR ), dimension ( : ), intent ( inout ) :: &
      PackedValue
    real ( KDR ), dimension ( : ), intent ( in ) :: &
      UnpackedValue
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      iaUnpacked
    integer ( KDI ), intent ( in ) :: &
      nValues
    
    integer ( KDI ) :: &
      iV  !-- iValue
    
    !$OMP parallel do default ( shared ) private ( iV ) schedule ( static )
    
    do iV = 1, nValues
      PackedValue ( iV ) = UnpackedValue ( iaUnpacked ( iV ) )
    end do
    
    !$OMP end parallel do

  end subroutine LoadVariable


  subroutine StoreVariable &
               ( UnpackedValue, PackedValue, iaUnpacked, nValues )
      
    real ( KDR ), dimension ( : ), intent ( inout ) :: &
      UnpackedValue
    real ( KDR ), dimension ( : ), intent ( in ) :: &
      PackedValue
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      iaUnpacked
    integer ( KDI ), intent ( in ) :: &
      nValues
    
    integer ( KDI ) :: &
      iV  !-- iValue
    
    !$OMP parallel do default ( shared ) private ( iV ) schedule ( static )

    do iV = 1, nValues
      UnpackedValue ( iaUnpacked ( iV ) ) = PackedValue ( iV )
    end do
    
    !$OMP end parallel do

  end subroutine StoreVariable
  

end module PackedVariableGroup_Form
