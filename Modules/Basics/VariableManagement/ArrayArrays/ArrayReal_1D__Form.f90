!-- ArrayReal_1D_Form allows the construction of an array of 1D real
!   arrays to form ragged arrays.

module ArrayReal_1D__Form

  use Specifiers
  use ArrayOperations

  implicit none
  private

  type, public :: ArrayReal_1D_Form
    real ( KDR ), dimension ( : ), allocatable :: &
      Value
  contains
    procedure, private, pass :: &
      Initialize_AR_1D
    procedure, private, pass :: &
      Initialize_AR_1D_FromValue
    procedure, private, pass :: &
      Initialize_AR_1D_Copy
    generic :: &
      Initialize &
        => Initialize_AR_1D, Initialize_AR_1D_FromValue, Initialize_AR_1D_Copy
    final :: &
      Finalize_AR_1D
  end type ArrayReal_1D_Form
  
contains


  subroutine Initialize_AR_1D ( A, nValues, ClearOption, iLowerBoundOption )
    
    class ( ArrayReal_1D_Form ), intent ( inout ) :: &
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

  end subroutine Initialize_AR_1D
  
  
  subroutine Initialize_AR_1D_FromValue ( A, Value, iLowerBoundOption )
    
    class ( ArrayReal_1D_Form ), intent ( inout ) :: &
      A
    real ( KDR ), dimension ( : ), intent ( in ) :: &
      Value
    integer ( KDI ), intent ( in ), optional :: &
      iLowerBoundOption

    call A % Initialize_AR_1D &
           ( size ( Value ), iLowerBoundOption = iLowerBoundOption )
    A % Value = Value 

  end subroutine Initialize_AR_1D_FromValue
  
  
  subroutine Initialize_AR_1D_Copy ( A, B, iLowerBoundOption )
    
    class ( ArrayReal_1D_Form ), intent ( inout ) :: &
      A
    type (  ArrayReal_1D_Form ), intent ( in ) :: &
      B
    integer ( KDI ), intent ( in ), optional :: &
      iLowerBoundOption
      
    integer ( KDI ) :: &
      iLB
    
    iLB = lbound ( B % Value, dim = 1 ) 
    if ( present ( iLowerBoundOption ) ) iLB = iLowerBoundOption

    call A % Initialize_AR_1D_FromValue ( B % Value, iLowerBoundOption = iLB )
  
  end subroutine Initialize_AR_1D_Copy 
  
  
  elemental subroutine Finalize_AR_1D ( A )

    type ( ArrayReal_1D_Form ), intent ( inout ) :: &
      A

    if ( allocated ( A % Value ) ) deallocate ( A % Value )

  end subroutine Finalize_AR_1D
  
  
end module ArrayReal_1D__Form
