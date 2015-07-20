!-- IncomingMessageArrayIntegerForm provides the concrete extension of 
!   MessageArrayTemplate for integer datatype to handle receiving array of 
!   messages.

module IncomingMessageArrayInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use IncomingMessageInteger_Form
!-- See FIXME below
!  use MessageArrayInteger_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayIntegerForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayIntegerForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    IncomingMessageArrayIntegerForm
      type ( IncomingMessageIntegerForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_I
    procedure, public, pass :: &
      Receive => Receive_I
    final :: &
      Finalize_IMA_I
  end type IncomingMessageArrayIntegerForm

contains


  subroutine InitializeAllocate_I ( MA, C, Tag, Rank, nElements )

    class ( IncomingMessageArrayIntegerForm ), intent ( inout ), target :: &
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
    type ( IncomingMessageIntegerForm ), pointer :: &
      M      

    nMessages = size ( Tag )

!-- See FIXME above
!     allocate ( IncomingMessageIntegerForm :: MA % Message ( nMessages ) )

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


  subroutine Receive_I ( IMA )

    class ( IncomingMessageArrayIntegerForm ), intent ( inout ), target :: &
      IMA

    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), dimension ( : ), pointer :: &
      M

    !-- FIXME: The select type only works here on the first iteration,
    !          while a comparable structure in 
    !          MessageArrayForm % InitializeAllocate seems to work fine
!     do iM = 1, IMA % nMessages
!       select type ( Message => IMA % Message ( iM ) )
!         type is ( IncomingMessageForm )
!           call Message % Receive ( )
!       end select
!     end do

    M => IMA % Message 
    select type ( M )
    type is ( IncomingMessageIntegerForm )
      do iM = 1, IMA % nMessages
        call M ( iM ) % Receive ( )
      end do
    end select
    
    nullify ( M )

  end subroutine Receive_I


  elemental subroutine Finalize_IMA_I ( IMA )

    type ( IncomingMessageArrayIntegerForm ), intent ( inout ) :: &
      IMA

    !-- Trigger finalization of parent type

!-- See FIXME above
    if ( allocated ( IMA % Message ) ) deallocate ( IMA % Message )

  end subroutine Finalize_IMA_I


end module IncomingMessageArrayInteger_Form
