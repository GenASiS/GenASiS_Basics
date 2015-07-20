module MessageArrayComplex_Form

  use MPI
  use VariableManagement
  use MessagePassingBasics
  use MessageComplex_Form
  use IncomingMessageComplex_Form
  use OutgoingMessageComplex_Form
  use MessageArray_Template
  
  implicit none
  private

  type, public, extends ( MessageArrayTemplate ) :: MessageArrayComplexForm
    class ( MessageComplexForm ), dimension ( : ), allocatable :: &
      Message
  contains
    procedure, public, pass :: &
      Initialize => InitializeAllocate_C
    final :: &
      Finalize_MA_C
  end type MessageArrayComplexForm
  
contains


  subroutine InitializeAllocate_C ( MA, C, Tag, Rank, nElements )

    class ( MessageArrayComplexForm ), intent ( inout ), target :: &
      MA
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Tag, &
      Rank, &
      nElements
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    class ( MessageComplexForm ), pointer :: &
      M
      
    MA % nMessages = size ( Tag )
    MA % MessageTemplate => MA % Message
    
    do iM = 1, MA % nMessages
      M => MA % Message ( iM )
      select type ( M )
      type is ( IncomingMessageComplexForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      type is ( OutgoingMessageComplexForm )
        call M % Initialize ( C, Tag ( iM ), Rank ( iM ), nElements ( iM ) )
      end select
    end do

    nullify ( M )

  end subroutine InitializeAllocate_C

  
  elemental subroutine Finalize_MA_C ( MA )
  
    type ( MessageArrayComplexForm ), intent ( inout ), target :: &
      MA

    if ( allocated ( MA % Message ) ) deallocate ( MA % Message )
    
    nullify ( MA % MessageTemplate )

  end subroutine Finalize_MA_C


end module MessageArrayComplex_Form
