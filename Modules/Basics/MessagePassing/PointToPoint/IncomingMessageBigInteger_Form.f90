!-- IncomingMessageBigIntegerForm is inherited from MessageBigIntegerForm to 
!   provide specific methods for receiving messages.

module IncomingMessageBigInteger_Form

  use MPI
  use VariableManagement
  use MessageBigInteger_Form

  implicit none
  private

  type, public, extends ( MessageBigIntegerForm ) :: &
    IncomingMessageBigIntegerForm
  contains
    procedure, public, pass :: &
      Receive => Receive_BI
    final :: &
      Finalize_IMBI
  end type IncomingMessageBigIntegerForm

contains


  subroutine Receive_BI ( IM )

    class ( IncomingMessageBigIntegerForm ), intent ( inout ) :: &
      IM

    integer :: &
      PlainInteger
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      ReceiveCount

    inquire ( iolength = ThisValueSize ) IM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    ReceiveCount = size ( IM % Value ) * SizeRatio

    call MPI_IRECV &
           ( IM % Value, ReceiveCount, MPI_INTEGER, IM % Rank, IM % Tag, &
             IM % Communicator % Handle, IM % Handle, IM % Error )
    
  end subroutine Receive_BI


  elemental subroutine Finalize_IMBI ( IM )

    type ( IncomingMessageBigIntegerForm ), intent ( inout ) :: &
      IM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_IMBI
  
  
end module IncomingMessageBigInteger_Form
