module MessageArrayInteger_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use MessageInteger_Form
  use IncomingMessageInteger_Form
  use OutgoingMessageInteger_Form
  use MessageArray_Template
  
  implicit none
  private

  type, public, extends ( MessageArrayTemplate ) :: MessageArrayIntegerForm
    class ( MessageIntegerForm ), dimension ( : ), allocatable :: &
      Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_I
    final :: &
      Finalize_MA_I
  end type MessageArrayIntegerForm
  
contains


  subroutine InitializeAllocate_I ( MA, C, Tag, Rank, nElements )

    class ( MessageArrayIntegerForm ), intent ( inout ), target :: &
      MA
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Tag, &
      Rank, &
      nElements
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageIntegerForm ), pointer :: &
      M
      
    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message

    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      select type ( M )
      type is ( IncomingMessageIntegerForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      type is ( OutgoingMessageIntegerForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      end select
    end do
    
    nullify ( M )

  end subroutine InitializeAllocate_I

  
  elemental subroutine Finalize_MA_I ( MA )
  
    type ( MessageArrayIntegerForm ), intent ( inout ) :: &
      MA

    if ( allocated ( MA % Message ) ) deallocate ( MA % Message )
    
    nullify ( MA % MessageTemplate )

  end subroutine Finalize_MA_I


end module MessageArrayInteger_Form
