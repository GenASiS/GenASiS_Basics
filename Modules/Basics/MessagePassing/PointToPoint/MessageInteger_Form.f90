!-- MessageIntegerForm provides the concrete type for Message object for
!   integer datatype

module MessageInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template

  implicit none
  private

  type, public, extends ( MessageTemplate ) :: MessageIntegerForm 
    integer ( KDI ), dimension ( : ), pointer :: &
      Value => null ( )
  contains
    procedure, public, pass :: &
      InitializeAllocate_I
    procedure, public, pass :: &
      InitializeAssociate_I
    generic :: &
      Initialize => InitializeAllocate_I, InitializeAssociate_I
    final :: &
      Finalize_MI
  end type MessageIntegerForm
  
contains


  subroutine InitializeAllocate_I ( M, C, Tag, Rank, nValues )

    class ( MessageIntegerForm ), intent ( inout ), target :: &
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
  
  end subroutine InitializeAllocate_I
  
  
  subroutine InitializeAssociate_I ( M, C, Value, Tag, Rank )

    class ( MessageIntegerForm ), intent ( inout ), target :: &
      M
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ), target :: &
      Value
    integer ( KDI ), intent ( in ) :: &
      Tag, &
      Rank

    if ( M % Initialized ) return
    
    call M % InitializeTemplate ( C, Tag, Rank, AllocatedOption = .false. )
    
    M % Value => Value
  
  end subroutine InitializeAssociate_I
  
  
  elemental subroutine Finalize_MI ( M )

    type ( MessageIntegerForm ), intent ( inout ) :: &
      M 
      
    if ( M % AllocatedValue ) then
      if ( associated ( M % Value ) ) deallocate ( M % Value )
    end if
    nullify ( M % Value )

    nullify ( M % Communicator )

  end subroutine Finalize_MI
  

end module MessageInteger_Form
