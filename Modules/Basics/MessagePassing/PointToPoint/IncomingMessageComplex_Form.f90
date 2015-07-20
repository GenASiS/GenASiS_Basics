!-- IncomingMessageComplexForm is inherited from MessageComplexForm to 
!   provide specific methods for receiving messages.

module IncomingMessageComplex_Form

  use MPI
  use VariableManagement
  use MessageComplex_Form

  implicit none
  private

  type, public, extends ( MessageComplexForm ) :: IncomingMessageComplexForm
  contains
    procedure, public, pass :: &
      Receive => Receive_C
   final :: &
     Finalize_IMC
  end type IncomingMessageComplexForm

contains


  subroutine Receive_C ( IM )

    class ( IncomingMessageComplexForm ), intent ( inout ) :: &
      IM

    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      ReceiveCount
    complex :: &
      PlainComplex

    inquire ( iolength = ThisValueSize ) IM % Value ( 1 )
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    ReceiveCount = size ( IM % Value ) * SizeRatio
        
    call MPI_IRECV &
           ( IM % Value, ReceiveCount, MPI_COMPLEX, IM % Rank, IM % Tag, &
             IM % Communicator % Handle, IM % Handle, IM % Error )

  end subroutine Receive_C


  elemental subroutine Finalize_IMC ( IM )

    type ( IncomingMessageComplexForm ), intent ( inout ) :: &
      IM 
      
    !-- Trigger finalization of parent type
    
  end subroutine Finalize_IMC
  
  
end module IncomingMessageComplex_Form
