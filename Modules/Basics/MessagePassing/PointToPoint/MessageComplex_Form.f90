!-- MessageIntegerForm provides the concrete type for Message object for
!   complex datatype

module MessageComplex_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template

  implicit none
  private

  type, public, extends ( MessageTemplate ) :: MessageComplexForm 
    complex ( KDC ), dimension ( : ), pointer :: &
      Value => null ( )
  contains
    procedure, public, pass :: &
      InitializeAllocate_C
    procedure, public, pass :: &
      InitializeAssociate_C
    generic :: &
      Initialize => InitializeAllocate_C, InitializeAssociate_C
    final :: &
      Finalize_MC
  end type MessageComplexForm

contains


  subroutine InitializeAllocate_C ( M, C, Tag, Rank, nValues )

    class ( MessageComplexForm ), intent ( inout ), target :: &
      M
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), intent ( in ) :: &
      Tag, &
      Rank, &
      nValues

    if ( M % Initialized ) return
    
    call M % InitializeTemplate ( C, Tag, Rank )
    
    allocate ( M % Value ( nValues ) )
  
  end subroutine InitializeAllocate_C
  
  
  subroutine InitializeAssociate_C ( M, C, Value, Tag, Rank )

    class ( MessageComplexForm ), intent ( inout ), target :: &
      M
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    complex ( KDC ), dimension ( : ), intent ( in ), target :: &
      Value
    integer ( KDI ), intent ( in ) :: &
      Tag, &
      Rank

    if ( M % Initialized ) return
    
    call M % InitializeTemplate ( C, Tag, Rank, AllocatedOption = .false. )

    M % Value => Value
  
  end subroutine InitializeAssociate_C
  
  
  elemental subroutine Finalize_MC ( M )

    type ( MessageComplexForm ), intent ( inout ) :: &
      M 
      
    if ( M % AllocatedValue ) then
      if ( associated ( M % Value ) ) deallocate ( M % Value )
    end if
    nullify ( M % Value )

    nullify ( M % Communicator )

  end subroutine Finalize_MC
  

end module MessageComplex_Form
