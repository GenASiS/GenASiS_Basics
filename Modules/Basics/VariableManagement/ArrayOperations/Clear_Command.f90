!-- Clear_Command provides overloaded "clear" routines with intent(out)
!   arguments of intrinsic data types, so that the compiler will use fast
!   operations to set arrays to zero (or to .false. in the case of logical 
!   arrays)

module Clear_Command

  use Specifiers

  implicit none
  private

  public :: &
    Clear

  interface Clear
    module procedure ClearInteger_1D
    module procedure ClearInteger_2D
    module procedure ClearInteger_3D
    module procedure ClearInteger_4D
    module procedure ClearBigInteger_1D
    module procedure ClearBigInteger_2D
    module procedure ClearBigInteger_3D
    module procedure ClearBigInteger_4D
    module procedure ClearReal_1D
    module procedure ClearReal_2D
    module procedure ClearReal_3D
    module procedure ClearReal_4D
    module procedure ClearComplex_1D
    module procedure ClearComplex_2D
    module procedure ClearComplex_3D
    module procedure ClearComplex_4D
    module procedure ClearLogical_1D
    module procedure ClearLogical_2D
    module procedure ClearLogical_3D
    module procedure ClearLogical_4D
  end interface Clear

contains


  pure subroutine ClearInteger_1D ( A )

    integer ( KDI ), dimension ( : ), intent ( out ) :: &
      A
    
    A = 0_KDI

  end subroutine ClearInteger_1D

  
  pure subroutine ClearInteger_2D ( A )

    integer ( KDI ), dimension ( :, : ), intent ( out ) :: &
      A

    A = 0_KDI

  end subroutine ClearInteger_2D


  pure subroutine ClearInteger_3D ( A )

    integer ( KDI ), dimension ( :, :, : ), intent ( out ) :: &
      A

    A = 0_KDI

  end subroutine ClearInteger_3D


  pure subroutine ClearInteger_4D ( A )

    integer ( KDI ), dimension ( :, :, :, : ), intent ( out ) :: &
      A

    A = 0_KDI

  end subroutine ClearInteger_4D


  pure subroutine ClearBigInteger_1D ( A )

    integer ( KBI ), dimension ( : ), intent ( out ) :: &
      A
    
    A = 0_KBI

  end subroutine ClearBigInteger_1D

  
  pure subroutine ClearBigInteger_2D ( A )

    integer ( KBI ), dimension ( :, : ), intent ( out ) :: &
      A

    A = 0_KBI

  end subroutine ClearBigInteger_2D


  pure subroutine ClearBigInteger_3D ( A )

    integer ( KBI ), dimension ( :, :, : ), intent ( out ) :: &
      A

    A = 0_KBI

  end subroutine ClearBigInteger_3D


  pure subroutine ClearBigInteger_4D ( A )

    integer ( KBI ), dimension ( :, :, :, : ), intent ( out ) :: &
      A

    A = 0_KBI

  end subroutine ClearBigInteger_4D


  pure subroutine ClearReal_1D ( A )

    real ( KDR ), dimension ( : ), intent ( out ) :: &
      A

    A = 0.0_KDR

  end subroutine ClearReal_1D
  
  
  pure subroutine ClearReal_2D ( A )

    real ( KDR ), dimension ( :, : ), intent ( out ) :: &
      A

    A = 0.0_KDR

  end subroutine ClearReal_2D
  
  
  pure subroutine ClearReal_3D ( A )

    real ( KDR ), dimension ( :, :, : ), intent ( out ) :: &
      A

    A = 0.0_KDR

  end subroutine ClearReal_3D
  
  
  pure subroutine ClearReal_4D ( A )

    real ( KDR ), dimension ( :, :, :, : ), intent ( out ) :: &
      A

    A = 0.0_KDR

  end subroutine ClearReal_4D
  
  
  pure subroutine ClearComplex_1D ( A )

    complex ( KDC ), dimension ( : ), intent ( out ) :: &
       A

     A = 0.0_KDC

  end subroutine ClearComplex_1D


  pure subroutine ClearComplex_2D ( A )

    complex ( KDC ), dimension ( :, : ), intent ( out ) :: &
       A

     A = 0.0_KDC

  end subroutine ClearComplex_2D


  pure subroutine ClearComplex_3D ( A )

    complex ( KDC ), dimension ( :, :, : ), intent ( out ) :: &
       A

     A = 0.0_KDC

  end subroutine ClearComplex_3D


  pure subroutine ClearComplex_4D ( A )

    complex ( KDC ), dimension ( :, :, :, : ), intent ( out ) :: &
       A

     A = 0.0_KDC

   end subroutine ClearComplex_4D


  pure subroutine ClearLogical_1D ( A )

    logical ( KDL ), dimension ( : ), intent ( out ) :: &
      A

    A = .false.

  end subroutine ClearLogical_1D
  
  
  pure subroutine ClearLogical_2D ( A )

    logical ( KDL ), dimension ( :, : ), intent ( out ) :: &
      A

    A = .false.

  end subroutine ClearLogical_2D
  
  
  pure subroutine ClearLogical_3D ( A )

    logical ( KDL ), dimension ( :, :, : ), intent ( out ) :: &
      A

    A = .false.

  end subroutine ClearLogical_3D
  
  
  pure subroutine ClearLogical_4D ( A )

    logical ( KDL ), dimension ( :, :, :, : ), intent ( out ) :: &
      A

    A = .false.

  end subroutine ClearLogical_4D
  
  
end module Clear_Command
