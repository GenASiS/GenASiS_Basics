!-- IncomingMessageIntegerForm is inherited from MessageIntegerForm to 
!   provide specific methods for receiving messages.

module IncomingMessageInteger_Form

  use MPI
  use VariableManagement
  use MessageInteger_Form

  implicit none
  private

  type, public, extends ( MessageIntegerForm ) :: IncomingMessageIntegerForm
  contains
    procedure, public, pass :: &
      Receive => Receive_I
    final :: &
      Finalize_IMI
  end type IncomingMessageIntegerForm
  
contains


  subroutine Receive_I ( IM )

    class ( IncomingMessageIntegerForm ), intent ( inout ) :: &
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

  end subroutine Receive_I


  elemental subroutine Finalize_IMI ( IM )

    type ( IncomingMessageIntegerForm ), intent ( inout ) :: &
      IM 

    !-- Trigger finalization of parent type
    
  end subroutine Finalize_IMI
  
  
end module IncomingMessageInteger_Form

