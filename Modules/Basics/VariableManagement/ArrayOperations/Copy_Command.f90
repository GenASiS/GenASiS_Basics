!-- Copy_Command provides overloaded "copy" routines with intent(in) and
!   intent(out) arguments of intrinsic data types, so that the compiler will
!   use fast copy methods. 

module Copy_Command

  use iso_c_binding
  use Specifiers

  implicit none
  private

  public :: &
    Copy

  interface Copy
    module procedure CopyInteger_1D
    module procedure CopyInteger_2D
    module procedure CopyInteger_3D
    module procedure CopyInteger_1D_Section
    module procedure CopyInteger_2D_Section
    module procedure CopyInteger_3D_Section
    module procedure CopyBigInteger_1D
    module procedure CopyBigInteger_2D
    module procedure CopyBigInteger_3D
    module procedure CopyBigInteger_1D_Section
    module procedure CopyBigInteger_2D_Section
    module procedure CopyBigInteger_3D_Section
    module procedure CopyReal_1D
    module procedure CopyReal_2D
    module procedure CopyReal_3D
    module procedure CopyReal_1D_Section
    module procedure CopyReal_2D_Section
    module procedure CopyReal_3D_Section
!-- NOTE: This may be needed if KDR is not double precision
!    module procedure Copy_C_Double_Real_1D
    module procedure CopyReal_3D_1D
    module procedure CopyReal_1D_3D
    module procedure CopyComplex_1D
    module procedure CopyComplex_2D
    module procedure CopyComplex_3D
    module procedure CopyComplex_1D_Section
    module procedure CopyComplex_2D_Section
    module procedure CopyComplex_3D_Section
    module procedure Copy_Character_C_String
  end interface Copy

contains


  pure subroutine CopyInteger_1D ( A, B )

    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyInteger_1D
  
  
  pure subroutine CopyInteger_2D ( A, B )

    integer ( KDI ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyInteger_2D
  
  
  pure subroutine CopyInteger_3D ( A, B )

    integer ( KDI ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( :, :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyInteger_3D
  
  
  pure subroutine CopyInteger_1D_Section ( A, oSource, oTarget, nValues, B )
    
    integer ( KDI ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KDI ), dimension ( : ), intent ( inout ) :: &
      B

    call Copy ( A ( oSource + 1 : oSource + nValues ), &
                B ( oTarget + 1 : oTarget + nValues ) )

  end subroutine CopyInteger_1D_Section
  

  pure subroutine CopyInteger_2D_Section ( A, oSource, oTarget, nValues, B )

    integer ( KDI ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 2 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KDI ), dimension ( :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2  !-- iValue_2

    do iV_2 = 1, nValues ( 2 )
      call Copy (  &
             A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                 oSource ( 2 ) + iV_2 ), &
             B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                 oTarget ( 2 ) + iV_2 ) )
    end do

  end subroutine CopyInteger_2D_Section
  

  pure subroutine CopyInteger_3D_Section ( A, oSource, oTarget, nValues, B )

    integer ( KDI ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KDI ), dimension ( :, :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2, iV_3  !-- iValue_2, iValue_3

    do iV_3 = 1, nValues ( 3 )
      do iV_2 = 1, nValues ( 2 )
        call Copy (  &
               A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2, oSource ( 3 ) + iV_3 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2, oTarget ( 3 ) + iV_3 ) )
      end do
    end do

  end subroutine CopyInteger_3D_Section
  

  pure subroutine CopyBigInteger_1D ( A, B )

    integer ( KBI ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KBI ), dimension ( : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyBigInteger_1D
  
  
  pure subroutine CopyBigInteger_2D ( A, B )

    integer ( KBI ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KBI ), dimension ( :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyBigInteger_2D
  
  
  pure subroutine CopyBigInteger_3D ( A, B )

    integer ( KBI ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KBI ), dimension ( :, :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyBigInteger_3D
  
  
  pure subroutine CopyBigInteger_1D_Section ( A, oSource, oTarget, nValues, B )
    
    integer ( KBI ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KBI ), dimension ( : ), intent ( inout ) :: &
      B

    call Copy ( A ( oSource + 1 : oSource + nValues ), &
                B ( oTarget + 1 : oTarget + nValues ) )

  end subroutine CopyBigInteger_1D_Section
  

  pure subroutine CopyBigInteger_2D_Section ( A, oSource, oTarget, nValues, B )

    integer ( KBI ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 2 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KBI ), dimension ( :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2  !-- iValue_2

    do iV_2 = 1, nValues ( 2 )
      call Copy (  &
             A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                 oSource ( 2 ) + iV_2 ), &
             B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                 oTarget ( 2 ) + iV_2 ) )
    end do

  end subroutine CopyBigInteger_2D_Section
  

  pure subroutine CopyBigInteger_3D_Section ( A, oSource, oTarget, nValues, B )

    integer ( KBI ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    integer ( KBI ), dimension ( :, :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2, iV_3  !-- iValue_2, iValue_3

    do iV_3 = 1, nValues ( 3 )
      do iV_2 = 1, nValues ( 2 )
        call Copy (  &
               A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2, oSource ( 3 ) + iV_3 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2, oTarget ( 3 ) + iV_3 ) )
      end do
    end do

  end subroutine CopyBigInteger_3D_Section
  

  pure subroutine CopyReal_1D ( A, B )

    real ( KDR ), dimension ( : ), intent ( in ) :: &
      A
    real ( KDR ), dimension ( : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyReal_1D
  
  
  pure subroutine CopyReal_2D ( A, B )

    real ( KDR ), dimension ( :, : ), intent ( in ) :: &
      A
    real ( KDR ), dimension ( :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyReal_2D
  
  
  pure subroutine CopyReal_3D ( A, B )

    real ( KDR ), dimension ( :, :, : ), intent ( in ) :: &
      A
    real ( KDR ), dimension ( :, :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyReal_3D
  
  
  pure subroutine CopyReal_1D_Section ( A, oSource, oTarget, nValues, B )
    
    real ( KDR ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    real ( KDR ), dimension ( : ), intent ( inout ) :: &
      B

    call Copy &
           ( A ( oSource + 1 : oSource + nValues ), &
             B ( oTarget + 1 : oTarget + nValues ) )
  
  end subroutine CopyReal_1D_Section
  

  pure subroutine CopyReal_2D_Section ( A, oSource, oTarget, nValues, B )

    real ( KDR ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 2 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    real ( KDR ), dimension ( :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2  !-- iValue_2

    do iV_2 = 1, nValues ( 2 )
      call Copy &
             ( A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2 ) )
    end do

  end subroutine CopyReal_2D_Section


  pure subroutine CopyReal_3D_Section ( A, oSource, oTarget, nValues, B )

    real ( KDR ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    real ( KDR ), dimension ( :, :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2, &  !-- iValue_2
      iV_3     !-- iValue_3

    do iV_3 = 1, nValues ( 3 )
      do iV_2 = 1, nValues ( 2 )
      call Copy &
             ( A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2, oSource ( 3 ) + iV_3 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2, oTarget ( 3 ) + iV_3 ) )
      end do
    end do

  end subroutine CopyReal_3D_Section


  ! pure subroutine Copy_C_Double_Real_1D ( A, B )
    
  !   real ( c_double ), dimension ( : ), intent ( in ) :: &
  !     A
  !   real ( KDR ), dimension ( : ), intent ( out ) :: &
  !     B
      
  !   B = A
  
  ! end subroutine Copy_C_Double_Real_1D

  
  pure subroutine CopyReal_3D_1D ( A, nSource, oSource, oTarget, B )

    real ( KDR ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      nSource, &
      oSource
    integer ( KDI ), intent ( in ) :: &
      oTarget
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    real ( KDR ), dimension ( : ), intent ( inout ) :: &
      B

    B ( oTarget + 1 : oTarget + product ( nSource ) ) &
      = reshape ( A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nSource ( 1 ), &
                      oSource ( 2 ) + 1 : oSource ( 2 ) + nSource ( 2 ), &
                      oSource ( 3 ) + 1 : oSource ( 3 ) + nSource ( 3 ) ), &
                  [ product ( nSource ) ] )

  end subroutine CopyReal_3D_1D


  pure subroutine CopyReal_1D_3D ( A, nTarget, oTarget, oSource, B )

    real ( KDR ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      nTarget, &
      oTarget
    integer ( KDI ), intent ( in ) :: &
      oSource
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    real ( KDR ), dimension ( :, :, : ), intent ( inout ) :: &
      B

    B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nTarget ( 1 ), &
        oTarget ( 2 ) + 1 : oTarget ( 2 ) + nTarget ( 2 ), &
        oTarget ( 3 ) + 1 : oTarget ( 3 ) + nTarget ( 3 ) ) &
      = reshape ( A ( oSource + 1 : oSource + product ( nTarget ) ), &
                  nTarget ) 

  end subroutine CopyReal_1D_3D


  pure subroutine CopyComplex_1D ( A, B )

    complex ( KDC ), dimension ( : ), intent ( in ) :: &
      A
    complex ( KDC ), dimension ( : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyComplex_1D
  
  
  pure subroutine CopyComplex_2D ( A, B )

    complex ( KDC ), dimension ( :, : ), intent ( in ) :: &
      A
    complex ( KDC ), dimension ( :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyComplex_2D
  
  
  pure subroutine CopyComplex_3D ( A, B )

    complex ( KDC ), dimension ( :, :, : ), intent ( in ) :: &
      A
    complex ( KDC ), dimension ( :, :, : ), intent ( out ) :: &
      B

    B = A

  end subroutine CopyComplex_3D
  
  
  pure subroutine CopyComplex_1D_Section ( A, oSource, oTarget, nValues, B )
    
    complex ( KDC ), dimension ( : ), intent ( in ) :: &
      A
    integer ( KDI ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    complex ( KDC ), dimension ( : ), intent ( inout ) :: &
      B

    call Copy &
           ( A ( oSource + 1 : oSource + nValues ), &
             B ( oTarget + 1 : oTarget + nValues ) )
  
  end subroutine CopyComplex_1D_Section
  

  pure subroutine CopyComplex_2D_Section ( A, oSource, oTarget, nValues, B )

    complex ( KDC ), dimension ( :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 2 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    complex ( KDC ), dimension ( :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2  !-- iValue_2

    do iV_2 = 1, nValues ( 2 )
      call Copy &
             ( A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2 ) )
    end do

  end subroutine CopyComplex_2D_Section


  pure subroutine CopyComplex_3D_Section ( A, oSource, oTarget, nValues, B )

    complex ( KDC ), dimension ( :, :, : ), intent ( in ) :: &
      A
    integer ( KDI ), dimension ( 3 ), intent ( in ) :: &
      oSource, &
      oTarget, &
      nValues
    !-- This argument is last in the spirit of intent ( out ), but should
    !   remain intent ( inout ) so existing values outside the section
    !   are not corrupted
    complex ( KDC ), dimension ( :, :, : ), intent ( inout ) :: &
      B
      
    integer ( KDI ) :: &
      iV_2, &  !-- iValue_2
      iV_3     !-- iValue_3

    do iV_3 = 1, nValues ( 3 )
      do iV_2 = 1, nValues ( 2 )
      call Copy &
             ( A ( oSource ( 1 ) + 1 : oSource ( 1 ) + nValues ( 1 ), &
                   oSource ( 2 ) + iV_2, oSource ( 3 ) + iV_3 ), &
               B ( oTarget ( 1 ) + 1 : oTarget ( 1 ) + nValues ( 1 ), &
                   oTarget ( 2 ) + iV_2, oTarget ( 3 ) + iV_3 ) )
      end do
    end do

  end subroutine CopyComplex_3D_Section


  pure subroutine Copy_Character_C_String ( A, B )
    
    character ( * ), intent ( in ) :: &
      A
    character ( c_char ), dimension ( : ), intent ( out ) :: &
      B
      
    integer ( KDI ) :: &
      iC
      
    do iC = 1, len_trim ( A ) 
      B ( iC ) = A ( iC : iC )
    end do 
    B ( iC ) = c_null_char
  
  end subroutine Copy_Character_C_String
  

end module Copy_Command
