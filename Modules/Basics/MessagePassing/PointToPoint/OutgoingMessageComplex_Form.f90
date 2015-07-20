!-- OutgoingMessageComplexForm is inherited from MessageComplexForm to 
!   provide specific methods for sending messages.

module OutgoingMessageComplex_Form

  use MPI
  use VariableManagement
  use MessageComplex_Form

  implicit none
  private

  type, public, extends ( MessageComplexForm ) :: OutgoingMessageComplexForm
  contains
    procedure, public, pass :: &
      Send => Send_C
    final :: &
      Finalize_OMC
  end type OutgoingMessageComplexForm

contains


  subroutine Send_C ( OM )

    class ( OutgoingMessageComplexForm ), intent ( inout ) :: &
      OM

    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount
    complex :: &
      PlainComplex

    inquire ( iolength = ThisValueSize ) OM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OM % Value ) * SizeRatio
        
    call MPI_ISEND &
           ( OM % Value, SendCount, MPI_COMPLEX, OM % Rank, OM % Tag, &
             OM % Communicator % Handle, OM % Handle, OM % Error )

  end subroutine Send_C


  elemental subroutine Finalize_OMC ( OM )

    type ( OutgoingMessageComplexForm ), intent ( inout ) :: &
      OM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_OMC
  
  
end module OutgoingMessageComplex_Form
