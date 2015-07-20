!-- IncomingMessageArrayRealForm provides the concrete extension of 
!   MessageArrayTemplate for real datatype to handle receiving array of 
!   messages.

module IncomingMessageArrayReal_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use IncomingMessageReal_Form
!-- See FIXME below
!  use MessageArrayReal_Form
  use MessageArray_Template 

  implicit none
  private

!-- FIXME: Bypass MessageArrayRealForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayRealForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    IncomingMessageArrayRealForm
      type ( IncomingMessageRealForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_R
    procedure, public, pass :: &
      Receive => Receive_R
    final :: &
      Finalize_IMA_R
  end type IncomingMessageArrayRealForm

contains


  subroutine InitializeAllocate_R ( MA, C, Tag, Rank, nElements )

    class ( IncomingMessageArrayRealForm ), intent ( inout ), target :: &
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
    type ( IncomingMessageRealForm ), pointer :: &
      M      

    nMessages = size ( Tag )

!-- See FIXME above
!    allocate ( IncomingMessageRealForm :: MA % Message ( nMessages ) )
!
!    call MA % MessageArrayRealForm % Initialize &
!           ( C, Tag, Rank, nElements )

    allocate ( MA % Message ( nMessages ) )

    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
    end do

    nullify ( M )

  end subroutine InitializeAllocate_R


  subroutine Receive_R ( IMA )

    class ( IncomingMessageArrayRealForm ), intent ( inout ), target :: &
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
    type is ( IncomingMessageRealForm )
      do iM = 1, IMA % nMessages
        call M ( iM ) % Receive ( )
      end do
    end select

    nullify ( M )

  end subroutine Receive_R


  elemental subroutine Finalize_IMA_R ( IMA )

    type ( IncomingMessageArrayRealForm ), intent ( inout ) :: &
      IMA

    !-- Trigger finalization of parent type

!-- See FIXME above
    if ( allocated ( IMA % Message ) ) deallocate ( IMA % Message )

  end subroutine Finalize_IMA_R


end module IncomingMessageArrayReal_Form
