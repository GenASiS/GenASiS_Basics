!-- ArrayInteger_1D_Form allows the construction of an array of 1D integer
!   arrays to form ragged arrays.

module ArrayInteger_1D__Form

  use Specifiers
  use ArrayOperations

  implicit none
  private

  type, public :: ArrayInteger_1D_Form
    integer ( KDI ), dimension ( : ), allocatable :: &
      Value
  contains
    procedure, private, pass :: &
      Initialize_AI_1D
    procedure, private, pass :: &
      Initialize_AI_1D_FromValue
    procedure, private, pass :: &
      Initialize_AI_1D_Copy
    generic :: &
      Initialize &
        => Initialize_AI_1D, Initialize_AI_1D_FromValue, Initialize_AI_1D_Copy
    final :: &
      Finalize_AI_1D
  end type ArrayInteger_1D_Form
  
contains


  subroutine Initialize_AI_1D ( A, nValues, ClearOption, iLowerBoundOption )
    
    class ( ArrayInteger_1D_Form ), intent ( inout ) :: &
      A
    integer ( KDI ), intent ( in ) :: &
      nValues
    logical ( KDL ), intent ( in ), optional :: &
      ClearOption
    integer ( KDI ), intent ( in ), optional :: &
      iLowerBoundOption

    integer ( KDI ) :: &
      iLB
    logical ( KDL ) :: &
      ClearRequested

    if ( nValues < 0 ) return
    
    if ( nValues == 0 ) then
      allocate ( A % Value ( 0 ) )
      return
    end if 
    
    ClearRequested = .false.
    if ( present ( ClearOption ) ) ClearRequested = ClearOption

    iLB = 1
    if ( present ( iLowerBoundOption ) ) iLB = iLowerBoundOption
    
    allocate ( A % Value ( iLB : iLB + nValues - 1 ) )
    
    if ( ClearRequested ) call Clear ( A % Value )

  end subroutine Initialize_AI_1D
  
  
  subroutine Initialize_AI_1D_FromValue ( A, Value, iLowerBoundOption )
    
    class ( ArrayInteger_1D_Form ), intent ( inout ) :: &
      A
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      Value
    integer ( KDI ), intent ( in ), optional :: &
      iLowerBoundOption

    call A % Initialize_AI_1D &
           ( size ( Value ), iLowerBoundOption = iLowerBoundOption )
    A % Value = Value 

  end subroutine Initialize_AI_1D_FromValue
  
  
  subroutine Initialize_AI_1D_Copy ( A, B, iLowerBoundOption )
    
    class ( ArrayInteger_1D_Form ), intent ( inout ) :: &
      A
    type (  ArrayInteger_1D_Form ), intent ( in ) :: &
      B
    integer ( KDI ), intent ( in ), optional :: &
      iLowerBoundOption
      
    integer ( KDI ) :: &
      iLB
    
    iLB = lbound ( B % Value, dim = 1 ) 
    if ( present ( iLowerBoundOption ) ) iLB = iLowerBoundOption

    call A % Initialize_AI_1D_FromValue ( B % Value, iLowerBoundOption = iLB )
  
  end subroutine Initialize_AI_1D_Copy 
  
  
  elemental subroutine Finalize_AI_1D ( A )

    type ( ArrayInteger_1D_Form ), intent ( inout ) :: &
      A

    if ( allocated ( A % Value ) ) deallocate ( A % Value )

  end subroutine Finalize_AI_1D
  
  
end module ArrayInteger_1D__Form
