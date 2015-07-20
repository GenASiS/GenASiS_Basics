!-- OutgoingMessageRealForm is inherited from MessageRealForm to 
!   provide specific methods for sending messages.

module OutgoingMessageReal_Form

  use MPI
  use VariableManagement
  use MessageReal_Form

  implicit none
  private

  type, public, extends ( MessageRealForm ) :: OutgoingMessageRealForm
  contains
    procedure, public, pass :: &
      Send => Send_R
    final :: &
      Finalize_OMR
  end type OutgoingMessageRealForm

contains


  subroutine Send_R ( OM )

    class ( OutgoingMessageRealForm ), intent ( inout ) :: &
      OM

    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount
    real :: &
      PlainReal
  
    inquire ( iolength = ThisValueSize ) OM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OM % Value ) * SizeRatio

    call MPI_ISEND &
           ( OM % Value, SendCount, MPI_REAL, OM % Rank, OM % Tag, &
             OM % Communicator % Handle, OM % Handle, OM % Error )
    
  end subroutine Send_R


  elemental subroutine Finalize_OMR ( OM )

    type ( OutgoingMessageRealForm ), intent ( inout ) :: &
      OM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_OMR
  
  
end module OutgoingMessageReal_Form
