!-- IncomingMessageArrayComplexForm provides the concrete extension of 
!   MessageArrayTemplate for complex datatype to handle receiving array of 
!   messages.

module IncomingMessageArrayComplex_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use IncomingMessageComplex_Form
!-- See FIXME below
!  use MessageArrayComplex_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayComplexForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayComplexForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    IncomingMessageArrayComplexForm
      type ( IncomingMessageComplexForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_C
    procedure, public, pass :: &
      Receive => Receive_C
    final :: &
      Finalize_IMA_C
  end type IncomingMessageArrayComplexForm

contains


  subroutine InitializeAllocate_C ( MA, C, Tag, Rank, nElements )

    class ( IncomingMessageArrayComplexForm ), intent ( inout ), target :: &
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
    type ( IncomingMessageComplexForm ), pointer :: &
      M      

    nMessages = size ( Tag )

!-- See FIXME above
!     allocate ( IncomingMessageComplexForm :: MA % Message ( nMessages ) )

!     call MA % MessageArrayComplexForm % Initialize &
!            ( C, Tag, Rank, nElements )

    allocate ( MA % Message ( nMessages ) )

    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
    end do

    nullify ( M )

  end subroutine InitializeAllocate_C


  subroutine Receive_C ( IMA )

    class ( IncomingMessageArrayComplexForm ), intent ( inout ), target :: &
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
    type is ( IncomingMessageComplexForm )
      do iM = 1, IMA % nMessages
        call M ( iM ) % Receive ( )
      end do
    end select

    nullify ( M )

  end subroutine Receive_C


  elemental subroutine Finalize_IMA_C ( IMA )

    type ( IncomingMessageArrayComplexForm ), intent ( inout ) :: &
      IMA

    !-- Trigger finalization of parent type

!-- See FIXME above
    if ( allocated ( IMA % Message ) ) deallocate ( IMA % Message )

  end subroutine Finalize_IMA_C


end module IncomingMessageArrayComplex_Form
