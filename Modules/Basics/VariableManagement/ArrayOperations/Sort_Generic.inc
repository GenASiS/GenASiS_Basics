!-- This file contains the 'generic' part of Sort and is included by
!   Sort_Command.f90

    Pivot = A ( 1 ) 
    iLow  = 0
    iHigh = size ( A ) + 1
    
    do

      iHigh = iHigh - 1
      do while ( A ( iHigh ) > Pivot ) 
        iHigh = iHigh - 1
      end do

      iLow = iLow + 1
      do while ( A ( iLow ) < Pivot ) 
        iLow = iLow + 1
      end do

      if ( iLow < iHigh ) then
        Swap        = A ( iLow ) 
        A ( iLow )  = A ( iHigh ) 
        A ( iHigh ) = Swap
      else if ( iLow == iHigh ) then
        iPartition = iLow + 1
        return
      else
        iPartition = iLow
        return
      end if

    end do
