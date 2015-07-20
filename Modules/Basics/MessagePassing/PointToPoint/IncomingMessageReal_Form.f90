!-- IncomingMessageRealForm is inherited from MessageRealForm to 
!   provide specific methods for receiving messages.

module IncomingMessageReal_Form

  use MPI
  use VariableManagement
  use MessageReal_Form

  implicit none
  private

  type, public, extends ( MessageRealForm ) :: IncomingMessageRealForm
  contains
    procedure, public, pass :: &
      Receive => Receive_R
    final :: &
      Finalize_IMR
  end type IncomingMessageRealForm

contains


  subroutine Receive_R ( IM )

    class ( IncomingMessageRealForm ), intent ( inout ) :: &
      IM

    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      ReceiveCount
    real :: &
      PlainReal
  
    inquire ( iolength = ThisValueSize ) IM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    ReceiveCount = size ( IM % Value ) * SizeRatio

    call MPI_IRECV &
           ( IM % Value, ReceiveCount, MPI_REAL, IM % Rank, IM % Tag, &
             IM % Communicator % Handle, IM % Handle, IM % Error )
    
  end subroutine Receive_R


  elemental subroutine Finalize_IMR ( IM )

    type ( IncomingMessageRealForm ), intent ( inout ) :: &
      IM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_IMR
  
  
end module IncomingMessageReal_Form
