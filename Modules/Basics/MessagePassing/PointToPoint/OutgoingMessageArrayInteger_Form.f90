!-- OutgoingMessageArrayIntegerForm provides the concrete extension of 
!   MessageArrayTemplate for integer datatype to handle sending array of 
!   messages.

module OutgoingMessageArrayInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use OutgoingMessageInteger_Form
!-- See FIXME below
!  use MessageArrayInteger_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayRealInteger to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayIntegerForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    OutgoingMessageArrayIntegerForm
      type ( OutgoingMessageIntegerForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_I
    procedure, public, pass :: &
      SendOne_I
    procedure, public, pass :: &
      SendAll_I
    generic :: &
      Send => SendOne_I, SendAll_I
    final :: &
      Finalize_OMA_I
  end type OutgoingMessageArrayIntegerForm

contains


  subroutine InitializeAllocate_I ( MA, C, Tag, Rank, nElements )

    class ( OutgoingMessageArrayIntegerForm ), intent ( inout ), target :: &
      MA
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Tag, &
      Rank, &
      nElements

    integer ( KDI ) :: &
      nMessages
!-- See FIXME above
    integer ( KDI ) :: &
      iM    !-- iMessage
    type ( OutgoingMessageIntegerForm ), pointer :: &
      M
     
    nMessages = size ( Tag )

!-- See FIXME above
!     allocate ( OutgoingMessageIntegerForm :: MA % Message ( nMessages ) )

!     call MA % MessageArrayIntegerForm % Initialize &
!            ( C, Tag, Rank, nElements )

    allocate ( MA % Message ( nMessages ) )

    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
    end do

    nullify ( M )

  end subroutine InitializeAllocate_I


  subroutine SendAll_I ( OMA )

    class ( OutgoingMessageArrayIntegerForm ), intent ( inout ), target :: &
      OMA

    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), dimension ( : ), pointer :: &
      M

    !-- FIXME: The select type only works here on the first iteration,
    !          while a comparable structure in 
    !          MessageArrayForm % InitializeAllocate seems to work fine
!     do iM = 1, OMA % nMessages
!       select type ( Message => OMA % Message ( iM ) )
!         type is ( OutgoingMessageForm )
!           call Message % Send ( )
!       end select
!     end do
    
    M => OMA % Message
    select type ( M )
    type is ( OutgoingMessageIntegerForm )
      do iM = 1, OMA % nMessages
        call M ( iM ) % Send ( )
      end do
    end select

    nullify ( M )

  end subroutine SendAll_I
  
  
  subroutine SendOne_I ( OMA, iM )

    class ( OutgoingMessageArrayIntegerForm ), intent ( inout ), target :: &
      OMA
    integer ( KDI ), intent ( in ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), pointer :: &
      M
        
    M => OMA % Message ( iM )
    select type ( M )
    type is ( OutgoingMessageIntegerForm )
      call M % Send ( )
    end select

    nullify ( M )

  end subroutine SendOne_I

 
  elemental subroutine Finalize_OMA_I ( OMA )
  
    type ( OutgoingMessageArrayIntegerForm ), intent ( inout ) :: &
      OMA

    !-- Triggers finalization of parent type
  
!-- See FIXME above
    if ( allocated ( OMA % Message ) ) deallocate ( OMA % Message )

  end subroutine Finalize_OMA_I


end module OutgoingMessageArrayInteger_Form
