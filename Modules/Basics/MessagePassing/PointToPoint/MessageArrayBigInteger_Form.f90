module MessageArrayBigInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use MessageBigInteger_Form
  use IncomingMessageBigInteger_Form
  use OutgoingMessageBigInteger_Form
  use MessageArray_Template
  
  implicit none
  private

  type, public, extends ( MessageArrayTemplate ) :: MessageArrayBigIntegerForm
    class ( MessageBigIntegerForm ), dimension ( : ), allocatable :: &
      Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_BI
    final :: &
      Finalize_MA_BI
  end type MessageArrayBigIntegerForm
  
contains


  subroutine InitializeAllocate_BI ( MA, C, Tag, Rank, nElements )

    class ( MessageArrayBigIntegerForm ), intent ( inout ), target :: &
      MA
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Tag, &
      Rank, &
      nElements
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageBigIntegerForm ), pointer :: &
      M
      
    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      select type ( M )
      type is ( IncomingMessageBigIntegerForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      type is ( OutgoingMessageBigIntegerForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      end select
    end do

    nullify ( M )

  end subroutine InitializeAllocate_BI

  
  elemental subroutine Finalize_MA_BI ( MA )
  
    type ( MessageArrayBigIntegerForm ), intent ( inout ) :: &
      MA

    if ( allocated ( MA % Message ) ) deallocate ( MA % Message )
    
    nullify ( MA % MessageTemplate )

  end subroutine Finalize_MA_BI


end module MessageArrayBigInteger_Form
