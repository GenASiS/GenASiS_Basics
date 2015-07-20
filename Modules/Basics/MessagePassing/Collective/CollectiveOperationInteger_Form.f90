!-- CollectiveOperationIntegerForm provides a concrete extension of 
!   CollectiveOperationTemplate for integer datatype for handling collective
!   operations.

module CollectiveOperationInteger_Form

  use MPI
  use VariableManagement
  use Display
  use MessagePassingBasics
  use PointToPoint
  use CollectiveOperation_Template

  implicit none
  private

  type, public, extends ( CollectiveOperationTemplate ) :: &
    CollectiveOperationIntegerForm
      type ( IncomingMessageIntegerForm ), allocatable :: &
        Incoming
      type ( OutgoingMessageIntegerForm ), allocatable :: &
        Outgoing
  contains
    procedure, public, pass :: &
      InitializeAllocate_I
!-- FIXME: ambiguous interface with InitializeAllocate_I so just leave it out  
!    procedure, public, pass :: &
!      InitializeAssociate_I
    generic :: &
      Initialize => InitializeAllocate_I!, InitializeAssociate_I
    procedure, public, pass :: &
      Broadcast => Broadcast_I
    procedure, public, pass :: &
      Gather => Gather_I
    procedure, public, pass :: &
      AllToAll => AllToAll_I
    procedure, public, pass :: &
      Reduce => Reduce_I
    final :: &
      Finalize_CO_I
  end type CollectiveOperationIntegerForm
  
contains


  subroutine InitializeAllocate_I &
               ( CO, C, nOutgoing, nIncoming, RootOption )

    class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
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
      
  end subroutine InitializeAllocate_I
  
  
!-- FIXME: ambiguous interface with InitializeAllocate_I so just leave it out  
!   subroutine InitializeAssociate_I &
!                ( CO, C, OutgoingValue, IncomingValue, RootOption )

!     class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
!       CO
!     type ( CommunicatorForm ), intent ( in ), target :: &
!       C
!     integer ( KDI ), dimension ( : ), intent ( in ), target :: &
!       OutgoingValue, &
!       IncomingValue
!     integer ( KDI ), intent ( in ), optional :: &
!       RootOption

!     CO % Communicator => C

!     if ( present ( RootOption ) ) CO % Root = RootOption
    
!     allocate ( CO % nOutgoing, source = shape ( OutgoingValue ) )    
!     allocate ( CO % nIncoming, source = shape ( IncomingValue ) )
    
!     allocate ( CO % Incoming )
!     call CO % Incoming % Initialize ( C, IncomingValue, 0, C % Rank )
    
!     allocate ( CO % Outgoing )
!     call CO % Outgoing % Initialize ( C, OutgoingValue, 0, C % Rank )
    
!   end subroutine InitializeAssociate_I
  
  
  subroutine Broadcast_I ( CO )

    class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
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

  end subroutine Broadcast_I


  subroutine Gather_I ( CO )
  
    class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
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

  end subroutine Gather_I


  subroutine AllToAll_I ( CO )
  
    class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
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
    
  end subroutine AllToAll_I


  subroutine Reduce_I ( CO, Operation )
  
    class ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
      CO
    integer ( KDI ), intent ( in ) :: &
      Operation

    integer :: &
      PlainInteger
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
    inquire ( iolength = PlainValueSize ) PlainInteger
    SizeRatio = max ( 1, ThisValueSize / PlainValueSize )
    SendCount = size ( OV )

    select case ( SizeRatio )
    case ( 1 )
      MPI_Datatype = MPI_INTEGER
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

  end subroutine Reduce_I


  elemental subroutine Finalize_CO_I ( CO )

    type ( CollectiveOperationIntegerForm ), intent ( inout ) :: &
      CO

    if ( allocated ( CO % Outgoing ) ) deallocate ( CO % Outgoing )
    if ( allocated ( CO % Incoming ) ) deallocate ( CO % Incoming )
    
    if ( allocated ( CO % nOutgoing ) ) deallocate ( CO % nOutgoing )
    if ( allocated ( CO % nIncoming ) ) deallocate ( CO % nIncoming )
    
    nullify ( CO % Communicator )

  end subroutine Finalize_CO_I

  
end module CollectiveOperationInteger_Form
