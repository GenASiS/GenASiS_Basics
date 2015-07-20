module MessageArrayReal_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use MessageReal_Form
  use IncomingMessageReal_Form
  use OutgoingMessageReal_Form
  use MessageArray_Template
  
  implicit none
  private

  type, public, extends ( MessageArrayTemplate ) :: MessageArrayRealForm
    class ( MessageRealForm ), dimension ( : ), allocatable :: &
      Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_R
    final :: &
      Finalize_MA_R
  end type MessageArrayRealForm
  
contains


  subroutine InitializeAllocate_R ( MA, C, Tag, Rank, nElements )

    class ( MessageArrayRealForm ), intent ( inout ), target :: &
      MA
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Tag, &
      Rank, &
      nElements
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageRealForm ), pointer :: &
      M
      
    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      select type ( M )
      type is ( IncomingMessageRealForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      type is ( OutgoingMessageRealForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      end select
    end do

    nullify ( M )

  end subroutine InitializeAllocate_R

  
  elemental subroutine Finalize_MA_R ( MA )
  
    type ( MessageArrayRealForm ), intent ( inout ) :: &
      MA

    if ( allocated ( MA % Message ) ) deallocate ( MA % Message )
    
    nullify ( MA % MessageTemplate )

  end subroutine Finalize_MA_R


end module MessageArrayReal_Form
