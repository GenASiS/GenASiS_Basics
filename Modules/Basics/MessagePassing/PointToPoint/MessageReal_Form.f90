!-- MessageIntegerForm provides the concrete type for Message object for
!   real datatype

module MessageReal_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template

  implicit none
  private

  type, public, extends ( MessageTemplate ) :: MessageRealForm 
    real ( KDR ), dimension ( : ), pointer :: &
      Value => null ( )
  contains
    procedure, public, pass :: &
      InitializeAllocate_R
    procedure, public, pass :: &
      InitializeAssociate_R
    generic :: &
      Initialize => InitializeAllocate_R, InitializeAssociate_R
    final :: &
      Finalize_MR
  end type MessageRealForm

contains


  subroutine InitializeAllocate_R ( M, C, Tag, Rank, nValues )

    class ( MessageRealForm ), intent ( inout ), target :: &
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
  
  end subroutine InitializeAllocate_R
  
  
  subroutine InitializeAssociate_R ( M, C, Value, Tag, Rank )

    class ( MessageRealForm ), intent ( inout ), target :: &
      M
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    real ( KDR ), dimension ( : ), intent ( in ), target :: &
      Value
    integer ( KDI ), intent ( in ) :: &
      Tag, &
      Rank

    if ( M % Initialized ) return
    
    call M % InitializeTemplate ( C, Tag, Rank, AllocatedOption = .false. )

    M % Value => Value
  
  end subroutine InitializeAssociate_R
  
  
  elemental subroutine Finalize_MR ( M )

    type ( MessageRealForm ), intent ( inout ) :: &
      M 
      
    if ( M % AllocatedValue ) then
      if ( associated ( M % Value ) ) deallocate ( M % Value )
    end if
    nullify ( M % Value )

    nullify ( M % Communicator )
    
  end subroutine Finalize_MR
  
  
end module MessageReal_Form
