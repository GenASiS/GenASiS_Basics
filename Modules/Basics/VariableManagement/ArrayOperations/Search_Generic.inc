!-- This file contains the 'generic' part of Search and is included by
!   Search_Command.f90

nValues = size ( A ) 
Ascending = ( A ( nValues )  >= A ( 1 )  ) 
iLow = 0
iHigh = nValues + 1
do while ( iHigh - iLow > 1 ) 
  iMiddle =  ( iHigh + iLow )  / 2
  if ( Ascending .and. ( Value >= A ( iMiddle )  )  ) then
    iLow = iMiddle
  else
    iHigh = iMiddle
  end if
end do
if ( Value == A ( 1 ) ) then
  iValue = 1
else if ( Value == A ( nValues ) ) then
  iValue = nValues  !-- modification from Numerical Recipes
else
  iValue = iLow
end if
 