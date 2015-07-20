!-- OutgoingMessageIntegerForm is inherited from MessageIntegerForm to 
!   provide specific methods for sending messages.

module OutgoingMessageInteger_Form

  use MPI
  use VariableManagement
  use MessageInteger_Form

  implicit none
  private

  type, public, extends ( MessageIntegerForm ) :: OutgoingMessageIntegerForm
  contains
    procedure, public, pass :: &
      Send => Send_I
    final :: &
      Finalize_OMI
  end type OutgoingMessageIntegerForm

contains


  subroutine Send_I ( OM )

    class ( OutgoingMessageIntegerForm ), intent ( inout ) :: &
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

  end subroutine Send_I


  elemental subroutine Finalize_OMI ( OM )

    type ( OutgoingMessageIntegerForm ), intent ( inout ) :: &
      OM 

    !-- Trigger finalization of parent type
    
  end subroutine Finalize_OMI
  
  
end module OutgoingMessageInteger_Form
