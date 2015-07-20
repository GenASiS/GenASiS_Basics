!-- CollectiveOperationComplexForm provides a concrete extension of 
!   CollectiveOperationTemplate for complex datatype for handling collective
!   operations.

module CollectiveOperationComplex_Form

  use MPI
  use VariableManagement
  use Display
  use MessagePassingBasics
  use PointToPoint
  use CollectiveOperation_Template

  implicit none
  private

  type, public, extends ( CollectiveOperationTemplate ) :: &
    CollectiveOperationComplexForm
      type ( IncomingMessageComplexForm ), allocatable :: &
        Incoming
      type ( OutgoingMessageComplexForm ), allocatable :: &
        Outgoing
  contains
    procedure, public, pass :: &
      InitializeAllocate_C
    procedure, public, pass :: &
      InitializeAssociate_C
    generic :: &
      Initialize => InitializeAllocate_C, InitializeAssociate_C
    procedure, public, pass :: &
      Broadcast => Broadcast_C
    procedure, public, pass :: &
      Gather => Gather_C
    procedure, public, pass :: &
      AllToAll => AllToAll_C
    procedure, public, pass :: &
      Reduce => Reduce_C
    final :: &
      Finalize_CO_C
  end type CollectiveOperationComplexForm
  
contains


  subroutine InitializeAllocate_C &
               ( CO, C, nOutgoing, nIncoming, RootOption )

    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
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
      
  end subroutine InitializeAllocate_C
  

  subroutine InitializeAssociate_C &
               ( CO, C, OutgoingValue, IncomingValue, RootOption )

    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    complex ( KDC ), dimension ( : ), intent ( in ), target :: &
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

  end subroutine InitializeAssociate_C
  
  
  subroutine Broadcast_C ( CO )

    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO

    complex :: &
      PlainComplex
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
      
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
    
    call MPI_BCAST &
           ( OV, SendCount, MPI_COMPLEX, &
             CO % Root, CO % Communicator % Handle, CO % Error)
    call Copy ( OV, IV )
      
    end associate

  end subroutine Broadcast_C


  subroutine Gather_C ( CO )
  
    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO
      
    complex :: &
      PlainComplex
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
      
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
    
    if ( CO % Root /= UNSET ) then
      call MPI_GATHER &
             ( OV, SendCount, MPI_COMPLEX, &
               IV, SendCount, MPI_COMPLEX, &
               CO % Root, CO % Communicator % Handle, CO % Error)  
    else
      call MPI_ALLGATHER &
             ( OV, SendCount, MPI_COMPLEX, &
               IV, SendCount, MPI_COMPLEX, &
               CO % Communicator % Handle, CO % Error)  
    end if

    end associate

  end subroutine Gather_C
    
 
  subroutine AllToAll_C ( CO )
  
    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO

    complex :: &
      PlainComplex
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )
      
    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = ( size ( OV ) / CO % Communicator % Size ) * SizeRatio
    
    call MPI_ALLTOALL &
           ( OV, SendCount, MPI_COMPLEX, &
             IV, SendCount, MPI_COMPLEX, &
             CO % Communicator % Handle, CO % Error)  
      
    end associate

  end subroutine AllToAll_C


  subroutine Reduce_C ( CO, Operation )
  
    class ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO
    integer ( KDI ), intent ( in ) :: &
      Operation

    complex :: &
      PlainComplex
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
    inquire ( iolength = PlainValueSize ) PlainComplex
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV )
    
    select case ( SizeRatio )
    case ( 1 )
      MPI_Datatype = MPI_COMPLEX
    case ( 2 ) 
      MPI_Datatype = MPI_DOUBLE_COMPLEX
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

  end subroutine Reduce_C


  elemental subroutine Finalize_CO_C ( CO )

    type ( CollectiveOperationComplexForm ), intent ( inout ) :: &
      CO

    if ( allocated ( CO % Outgoing ) ) deallocate ( CO % Outgoing )
    if ( allocated ( CO % Incoming ) ) deallocate ( CO % Incoming )
    
    if ( allocated ( CO % nOutgoing ) ) deallocate ( CO % nOutgoing )
    if ( allocated ( CO % nIncoming ) ) deallocate ( CO % nIncoming )
    
    nullify ( CO % Communicator )

  end subroutine Finalize_CO_C

  
end module CollectiveOperationComplex_Form
