!-- MessageIntegerForm provides the concrete type for Message object for
!   long (big) integer datatype

module MessageBigInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template

  implicit none
  private

  type, public, extends ( MessageTemplate ) :: MessageBigIntegerForm 
    integer ( KBI ), dimension ( : ), pointer :: &
      Value => null ( )
  contains
    procedure, public, pass :: &
      InitializeAllocate_BI
    procedure, public, pass :: &
      InitializeAssociate_BI
    generic :: &
      Initialize => InitializeAllocate_BI, InitializeAssociate_BI
    final :: &
      Finalize_MBI
  end type MessageBigIntegerForm

contains


  subroutine InitializeAllocate_BI ( M, C, Tag, Rank, nValues )

    class ( MessageBigIntegerForm ), intent ( inout ), target :: &
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
  
  end subroutine InitializeAllocate_BI
  
  
  subroutine InitializeAssociate_BI ( M, C, Value, Tag, Rank )

    class ( MessageBigIntegerForm ), intent ( inout ), target :: &
      M
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KBI ), dimension ( : ), intent ( in ), target :: &
      Value
    integer ( KDI ), intent ( in ) :: &
      Tag, &
      Rank

    if ( M % Initialized ) return
    
    call M % InitializeTemplate ( C, Tag, Rank, AllocatedOption = .false. )
    
    M % Value => Value
  
  end subroutine InitializeAssociate_BI
  
  
  elemental subroutine Finalize_MBI ( M )

    type ( MessageBigIntegerForm ), intent ( inout ) :: &
      M 
      
    if ( M % AllocatedValue ) then
      if ( associated ( M % Value ) ) deallocate ( M % Value )
    end if
    nullify ( M % Value )
    
    nullify ( M % Communicator )

  end subroutine Finalize_MBI
  

end module MessageBigInteger_Form
