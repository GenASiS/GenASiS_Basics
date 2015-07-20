!-- Search_Command implements an overloaded "search" subroutine to find the 
!   index of a sorted array (of intrinsic types) corresponding to an input 
!   value

module Search_Command

  !-- based on Numerical Recipes, Fortran (1992), Fortran 90 (1996)  
  !   routine "locate", but with modification
  
  !-- Given a sorted array A and a value Value, returns iValue such that 
  !   A ( iValue ) <= Value < A ( iValue + 1 )

  use Specifiers

  implicit none
  private

  public :: &
    Search

contains


  pure subroutine Search ( A, Value, iValue ) 

    class ( * ), dimension ( : ), intent ( in )  :: &
      A
    class ( * ), intent ( in )  :: &
      Value
    integer ( KDI ), intent ( out )  :: &
      iValue
    
    integer ( KDI )  :: &
      nValues, &
      iLow, &
      iMiddle, &
      iHigh
    logical ( KDL )  :: &
      Ascending
      
    select type ( A )
    
    type is ( integer ( KDI ) )
      select type ( Value )
      type is ( integer ( KDI ) )
        include 'Search_Generic.inc'
      end select
    
    type is ( real ( KDR ) )
      select type ( Value )
      type is ( real ( KDR ) )
        include 'Search_Generic.inc'
      end select
    end select
    
  end subroutine Search


end module Search_Command
