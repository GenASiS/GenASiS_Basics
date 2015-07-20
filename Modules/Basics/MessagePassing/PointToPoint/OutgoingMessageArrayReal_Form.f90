!-- OutgoingMessageArrayRealForm provides the concrete extension of 
!   MessageArrayTemplate for real datatype to handle sending array of 
!   messages.

module OutgoingMessageArrayReal_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use OutgoingMessageReal_Form
!-- See FIXME below
!  use MessageArrayReal_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayRealForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayRealForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    OutgoingMessageArrayRealForm
      type ( OutgoingMessageRealForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_R
    procedure, public, pass :: &
      SendOne_R
    procedure, public, pass :: &
      SendAll_R
    generic :: &
      Send => SendOne_R, SendAll_R
    final :: &
      Finalize_OMA_R
  end type OutgoingMessageArrayRealForm

contains


  subroutine InitializeAllocate_R ( MA, C, Tag, Rank, nElements )

    class ( OutgoingMessageArrayRealForm ), intent ( inout ), target :: &
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
    type ( OutgoingMessageRealForm ), pointer :: &
      M
     
    nMessages = size ( Tag )

!-- See FIXME above
!    allocate ( OutgoingMessageRealForm :: MA % Message ( nMessages ) )
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


  subroutine SendAll_R ( OMA )

    class ( OutgoingMessageArrayRealForm ), intent ( inout ), target :: &
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
    type is ( OutgoingMessageRealForm )
      do iM = 1, OMA % nMessages
        call M ( iM ) % Send ( )
      end do
    end select

    nullify ( M )

  end subroutine SendAll_R
  
  
  subroutine SendOne_R ( OMA, iM )

    class ( OutgoingMessageArrayRealForm ), intent ( inout ), target :: &
      OMA
    integer ( KDI ), intent ( in ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), pointer :: &
      M
        
    M => OMA % Message ( iM )
    select type ( M )
    type is ( OutgoingMessageRealForm )
      call M % Send ( )
    end select

    nullify ( M )

  end subroutine SendOne_R

 
  elemental subroutine Finalize_OMA_R ( OMA )
  
    type ( OutgoingMessageArrayRealForm ), intent ( inout ) :: &
      OMA

    !-- Triggers finalization of parent type
  
!-- See FIXME above
    if ( allocated ( OMA % Message ) ) deallocate ( OMA % Message )

  end subroutine Finalize_OMA_R


end module OutgoingMessageArrayReal_Form
