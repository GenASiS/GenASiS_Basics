!-- OutgoingMessageArrayComplexForm provides the concrete extension of 
!   MessageArrayTemplate for complex datatype to handle sending array of 
!   messages.

module OutgoingMessageArrayComplex_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use Message_Template
  use OutgoingMessageComplex_Form
!-- See FIXME below
!  use MessageArrayComplex_Form
  use MessageArray_Template 
 
  implicit none
  private

!-- FIXME: Bypass MessageArrayRealForm to avoid failed Finalization 
!   of polymorphic arrays with the Cray compiler
!  type, public, extends ( MessageArrayComplexForm ) :: &
  type, public, extends ( MessageArrayTemplate ) :: &
    OutgoingMessageArrayComplexForm
      type ( OutgoingMessageComplexForm ), dimension ( : ), allocatable :: &
        Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_C
    procedure, public, pass :: &
      SendOne_C
    procedure, public, pass :: &
      SendAll_C
    generic :: &
      Send => SendOne_C, SendAll_C
    final :: &
      Finalize_OMA_C
  end type OutgoingMessageArrayComplexForm

contains


  subroutine InitializeAllocate_C ( MA, C, Tag, Rank, nElements )

    class ( OutgoingMessageArrayComplexForm ), intent ( inout ), target :: &
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
    type ( OutgoingMessageComplexForm ), pointer :: &
      M
     
    nMessages = size ( Tag )

!-- See FIXME above
!     allocate ( OutgoingMessageComplexForm :: MA % Message ( nMessages ) )

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


  subroutine SendAll_C ( OMA )

    class ( OutgoingMessageArrayComplexForm ), intent ( inout ), target :: &
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
    type is ( OutgoingMessageComplexForm )
      do iM = 1, OMA % nMessages
        call M ( iM ) % Send ( )
      end do
    end select

    nullify ( M )

  end subroutine SendAll_C
  
  
  subroutine SendOne_C ( OMA, iM )

    class ( OutgoingMessageArrayComplexForm ), intent ( inout ), target :: &
      OMA
    integer ( KDI ), intent ( in ) :: &
      iM    !-- iMessage
    class ( MessageTemplate ), pointer :: &
      M
        
    M => OMA % Message ( iM )
    select type ( M )
    type is ( OutgoingMessageComplexForm )
      call M % Send ( )
    end select

    nullify ( M )

  end subroutine SendOne_C

 
  elemental subroutine Finalize_OMA_C ( OMA )
  
    type ( OutgoingMessageArrayComplexForm ), intent ( inout ) :: &
      OMA

    !-- Triggers finalization of parent type
  
!-- See FIXME above
    if ( allocated ( OMA % Message ) ) deallocate ( OMA % Message )

  end subroutine Finalize_OMA_C


end module OutgoingMessageArrayComplex_Form
