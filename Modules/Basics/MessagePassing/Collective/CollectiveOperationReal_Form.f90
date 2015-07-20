!-- CollectiveOperationRealForm provides a concrete extension of 
!   CollectiveOperationTemplate for real datatype for handling collective
!   operations.

module CollectiveOperationReal_Form

  use MPI
  use VariableManagement
  use Display
  use MessagePassingBasics
  use PointToPoint
  use CollectiveOperation_Template

  implicit none
  private

  type, public, extends ( CollectiveOperationTemplate ) :: &
    CollectiveOperationRealForm
      type ( IncomingMessageRealForm ), allocatable :: &
        Incoming
      type ( OutgoingMessageRealForm ), allocatable :: &
        Outgoing
  contains
    procedure, public, pass :: &
      InitializeAllocate_R
    procedure, public, pass :: &
      InitializeAssociate_R
    generic :: &
      Initialize => InitializeAllocate_R, InitializeAssociate_R
    procedure, public, pass :: &
      Broadcast => Broadcast_R
    procedure, public, pass :: &
      Gather => Gather_R
    procedure, public, pass :: &
      AllToAll => AllToAll_R
    procedure, public, pass :: &
      Reduce => Reduce_R
    final :: &
      Finalize_CO_R
  end type CollectiveOperationRealForm
  
contains


  subroutine InitializeAllocate_R &
               ( CO, C, nOutgoing, nIncoming, RootOption )

    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KDI ), intent ( in ), dimension ( : ) :: &
      nOutgoing, &
      nIncoming
    integer ( KDI ), intent ( in ), optional :: &
      RootOption

    call CO % InitializeTemplate ( C, nOutgoing, nIncoming, RootOption )

    allocate ( CO % Incoming )
    call CO % Incoming % Initialize ( C, 0, C % Rank, sum ( nIncoming ) )

    allocate ( CO % Outgoing )
    call CO % Outgoing % Initialize ( C, 0, C % Rank, sum ( nOutgoing ) )
      
  end subroutine InitializeAllocate_R
  
  
  subroutine InitializeAssociate_R &
               ( CO, C, OutgoingValue, IncomingValue, RootOption )

    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    real ( KDR ), dimension ( : ), intent ( in ), target :: &
      OutgoingValue, &
      IncomingValue
    integer ( KDI ), intent ( in ), optional :: &
      RootOption

    call CO % InitializeTemplate &
           ( C, shape ( OutgoingValue ), shape ( IncomingValue ), RootOption )
    
    allocate ( CO % Incoming )
    call CO % Incoming % Initialize ( C, IncomingValue, 0, C % Rank )
    
    allocate ( CO % Outgoing )
    call CO % Outgoing % Initialize ( C, OutgoingValue, 0, C % Rank )

  end subroutine InitializeAssociate_R
  
  
  subroutine Broadcast_R ( CO )

    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO

    real :: &
      PlainReal
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
    
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
    
    call MPI_BCAST &
           ( OV, SendCount, MPI_REAL, &
             CO % Root, CO % Communicator % Handle, CO % Error)
    call Copy ( OV, IV )
            
    end associate

  end subroutine Broadcast_R
      

  subroutine Gather_R ( CO )
  
    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO
      
    real :: &
      PlainReal
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
    
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
    
    if ( CO % Root /= UNSET ) then
      call MPI_GATHER &
             ( OV, SendCount, MPI_REAL, &
               IV, SendCount, MPI_REAL, &
               CO % Root, CO % Communicator % Handle, CO % Error)  
    else
      call MPI_ALLGATHER &
             ( OV, SendCount, MPI_REAL, &
               IV, SendCount, MPI_REAL, &
               CO % Communicator % Handle, CO % Error)  
    end if

    end associate

  end subroutine Gather_R


  subroutine AllToAll_R ( CO )
  
    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO

    real :: &
      PlainReal
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
    
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = ( size ( OV ) / CO % Communicator % Size ) * SizeRatio
    
    call MPI_ALLTOALL &
           ( OV, SendCount, MPI_REAL, &
             IV, SendCount, MPI_REAL, &
             CO % Communicator % Handle, CO % Error)  
      
    end associate
    
  end subroutine AllToAll_R


  subroutine Reduce_R ( CO, Operation )
  
    class ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO
    integer ( KDI ), intent ( in ) :: &
      Operation

    real :: &
      PlainReal
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount, &
      MPI_Datatype
    
  associate &
    ( OV => CO % Outgoing % Value ( : ), &
      IV => CO % Incoming % Value ( : ) )
    
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainReal
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV )
    
    select case ( SizeRatio )
    case ( 1 )
      MPI_Datatype = MPI_REAL
    case ( 2 ) 
      MPI_Datatype = MPI_DOUBLE_PRECISION
    case default
      call Show &
             ( 'MPI datatype error in CollectiveOperation_Form % Reduce', &
               CONSOLE % ERROR )
    end select
      
    if ( CO % Root /= UNSET ) then
      call MPI_REDUCE &
             ( OV, IV, SendCount, MPI_Datatype, Operation, &
               CO % Root, CO % Communicator % Handle, CO % Error )
    else
      call MPI_ALLREDUCE &
             ( OV, IV, SendCount, MPI_Datatype, Operation, &
               CO % Communicator % Handle, CO % Error )
    end if
      
    end associate

  end subroutine Reduce_R


  elemental subroutine Finalize_CO_R ( CO )

    type ( CollectiveOperationRealForm ), intent ( inout ) :: &
      CO

    if ( allocated ( CO % Outgoing ) ) deallocate ( CO % Outgoing )
    if ( allocated ( CO % Incoming ) ) deallocate ( CO % Incoming )
    
    if ( allocated ( CO % nOutgoing ) ) deallocate ( CO % nOutgoing )
    if ( allocated ( CO % nIncoming ) ) deallocate ( CO % nIncoming )
    
    nullify ( CO % Communicator )

  end subroutine Finalize_CO_R

  
end module CollectiveOperationReal_Form
