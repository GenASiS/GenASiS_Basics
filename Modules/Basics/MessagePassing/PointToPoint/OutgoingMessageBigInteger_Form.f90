!-- OutgoingMessageBigIntegerForm is inherited from MessageBigIntegerForm to 
!   provide specific methods for sending messages.

module OutgoingMessageBigInteger_Form

  use MPI
  use VariableManagement
  use MessageBigInteger_Form

  implicit none
  private

  type, public, extends ( MessageBigIntegerForm ) :: OutgoingMessageBigIntegerForm
  contains
    procedure, public, pass :: &
      Send => Send_BI
    final :: &
      Finalize_OMBI
  end type OutgoingMessageBigIntegerForm

contains


  subroutine Send_BI ( OM )

    class ( OutgoingMessageBigIntegerForm ), intent ( inout ) :: &
      OM

    integer :: &
      PlainInteger
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    inquire ( iolength = ThisValueSize ) OM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OM % Value ) * SizeRatio

    call MPI_ISEND &
           ( OM % Value, SendCount, MPI_INTEGER, OM % Rank, OM % Tag, &
             OM % Communicator % Handle, OM % Handle, OM % Error )
    
  end subroutine Send_BI


  elemental subroutine Finalize_OMBI ( OM )

    type ( OutgoingMessageBigIntegerForm ), intent ( inout ) :: &
      OM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_OMBI
  
  
end module OutgoingMessageBigInteger_Form
