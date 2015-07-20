module CollectiveOperationBigInteger_Form

  use MPI
  use VariableManagement
  use Display
  use MessagePassingBasics
  use PointToPoint
  use CollectiveOperation_Template

  implicit none
  private

  type, public, extends ( CollectiveOperationTemplate ) :: &
    CollectiveOperationBigIntegerForm
      type ( IncomingMessageBigIntegerForm ), allocatable :: &
        Incoming
      type ( OutgoingMessageBigIntegerForm ), allocatable :: &
        Outgoing
  contains
    procedure, public, pass :: &
      InitializeAllocate_BI
    procedure, public, pass :: &
      InitializeAssociate_BI
    generic :: &
      Initialize => InitializeAllocate_BI, InitializeAssociate_BI
    procedure, public, pass :: &
      Broadcast => Broadcast_BI
    procedure, public, pass :: &
      Gather => Gather_BI
    procedure, public, pass :: &
      AllToAll => AllToAll_BI
    final :: &
      Finalize_CO_BI
  end type CollectiveOperationBigIntegerForm
  
contains


  subroutine InitializeAllocate_BI &
               ( CO, C, nOutgoing, nIncoming, RootOption )

    class ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
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
      
  end subroutine InitializeAllocate_BI
  
  
  subroutine InitializeAssociate_BI &
               ( CO, C, OutgoingValue, IncomingValue, RootOption )

    class ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
      CO
    type ( CommunicatorForm ), intent ( in ), target :: &
      C
    integer ( KBI ), dimension ( : ), intent ( in ), target :: &
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

  end subroutine InitializeAssociate_BI
  
  
  subroutine Broadcast_BI ( CO )

    class ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
      CO

    integer :: &
      PlainInteger
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )

    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
      
    call MPI_BCAST &
           ( OV, SendCount, MPI_INTEGER, &
             CO % Root, CO % Communicator % Handle, CO % Error)
    call Copy ( OV, IV )
      
    end associate
  
  end subroutine Broadcast_BI


  subroutine Gather_BI ( CO )
  
    class ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
      CO
      
    integer :: &
      PlainInteger
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount
    
    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )

    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV ) * SizeRatio
      
    if ( CO % Root /= UNSET ) then
      call MPI_GATHER &
             ( OV, SendCount, MPI_INTEGER, &
               IV, SendCount, MPI_INTEGER, &
               CO % Root, CO % Communicator % Handle, CO % Error)
    else
      call MPI_ALLGATHER &
             ( OV, SendCount, MPI_INTEGER, &
               IV, SendCount, MPI_INTEGER, &
               CO % Communicator % Handle, CO % Error)
    end if
      
    end associate
  
  end subroutine Gather_BI


  subroutine AllToAll_BI ( CO )
  
    class ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
      CO

    integer :: &
      PlainInteger
    integer ( KDI ) :: &
      PlainValueSize, &
      ThisValueSize, &
      SizeRatio, &
      SendCount

    associate &
      ( OV => CO % Outgoing % Value ( : ), &
        IV => CO % Incoming % Value ( : ) )

    inquire ( iolength = ThisValueSize ) OV ( 1 )
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = ( size ( OV ) / CO % Communicator % Size ) * SizeRatio
      
    call MPI_ALLTOALL &
           ( OV, SendCount, MPI_INTEGER, &
             IV, SendCount, MPI_INTEGER, &
             CO % Communicator % Handle, CO % Error)  
      
    end associate
    
  end subroutine AllToAll_BI


  elemental subroutine Finalize_CO_BI ( CO )

    type ( CollectiveOperationBigIntegerForm ), intent ( inout ) :: &
      CO

    if ( allocated ( CO % Outgoing ) ) deallocate ( CO % Outgoing )
    if ( allocated ( CO % Incoming ) ) deallocate ( CO % Incoming )
    
    if ( allocated ( CO % nOutgoing ) ) deallocate ( CO % nOutgoing )
    if ( allocated ( CO % nIncoming ) ) deallocate ( CO % nIncoming )
    
    nullify ( CO % Communicator )

  end subroutine Finalize_CO_BI

  
end module CollectiveOperationBigInteger_Form
