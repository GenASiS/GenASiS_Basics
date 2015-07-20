!-- OutgoingMessageArrayBigIntegerForm provides the concrete extension of 
!   MessageArrayTemplate for big (long) integer datatype to handle sending 
!   array of messages.

module OutgoingMessageArrayBigInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use OutgoingMessageBigInteger_Form
!-- See FIXME below
!  use MessageArrayBigInteger_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayRealForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayBigIntegerForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    OutgoingMessageArrayBigIntegerForm
      type ( OutgoingMessageBigIntegerForm ), dimension ( : ), allocatable :: &
        Message
 contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_BI
    procedure, public, pass :: &
      SendOne_BI
    procedure, public, pass :: &
      SendAll_BI
    generic :: &
      Send => SendOne_BI, SendAll_BI
    final :: &
      Finalize_OMA_BI
  end type OutgoingMessageArrayBigIntegerForm

contains


  subroutine InitializeAllocate_BI ( MA, C, Tag, Rank, nElements )

    class ( OutgoingMessageArrayBigIntegerForm ), intent ( inout ), target :: &
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
    type ( OutgoingMessageBigIntegerForm ), pointer :: &
      M
     
    nMessages = size ( Tag )

!-- See FIXME above
!     allocate ( OutgoingMessageBigIntegerForm :: MA % Message ( nMessages ) )

!     call MA % MessageArrayBigIntegerForm % Initialize &
!            ( C, Tag, Rank, nElements )

    allocate ( MA % Message ( nMessages ) )

    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
    end do

    nullify ( M )

  end subroutine InitializeAllocate_BI


  subroutine SendAll_BI ( OMA )

    class ( OutgoingMessageArrayBigIntegerForm ), intent ( inout ), target :: &
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
    type is ( OutgoingMessageBigIntegerForm )
      do iM = 1, OMA % nMessages
        call M ( iM ) % Send ( )
      end do
    end select

    nullify ( M )

  end subroutine SendAll_BI
  
  
  subroutine SendOne_BI ( OMA, iM )

    class ( OutgoingMessageArrayBigIntegerForm ), intent ( inout ), target :: &
      OMA
    integer ( KDI ), intent ( in ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), pointer :: &
      M
        
    M => OMA % Message ( iM )
    select type ( M )
    type is ( OutgoingMessageBigIntegerForm )
      call M % Send ( )
    end select

    nullify ( M )

  end subroutine SendOne_BI

 
  elemental subroutine Finalize_OMA_BI ( OMA )
  
    type ( OutgoingMessageArrayBigIntegerForm ), intent ( inout ) :: &
      OMA

    !-- Triggers finalization of parent type
  
!-- See FIXME above
    if ( allocated ( OMA % Message ) ) deallocate ( OMA % Message )

  end subroutine Finalize_OMA_BI


end module OutgoingMessageArrayBigInteger_Form
