!-- MessageArrayTemplate provides an abstraction commonly shared by 
!   higher-level, concrete type object with specified datatype to array of 
!   messages.

module MessageArray_Template

  use MPI
  use VariableManagement
  use Message_Template
 
  implicit none
  private

  type, public, abstract :: MessageArrayTemplate
    integer ( KDI ) :: &
      nMessages, &
      Error
    class ( MessageTemplate ), dimension ( : ), pointer :: &
      MessageTemplate => null ( )
  contains
    procedure, public, pass :: &
      WaitOne
    procedure, public, pass :: &
      WaitAny
    procedure, public, pass :: &
      WaitAll
    generic :: &
      Wait => WaitOne, WaitAny, WaitAll
  end type MessageArrayTemplate
  
contains


  subroutine WaitOne ( MA, Index )

    class ( MessageArrayTemplate ), intent ( inout ) :: &
      MA
    integer ( KDI ), intent ( in ) :: &
      Index

    call MA % MessageTemplate ( Index ) % Wait ( )

  end subroutine WaitOne


  subroutine WaitAny ( MA, AllFinished, Index )
  
    class ( MessageArrayTemplate ), intent ( inout ) :: &
      MA
    logical ( KDL ), intent ( out ) :: &
      AllFinished
    integer ( KDI ), intent ( out ) :: &
      Index
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    integer ( KDI ), dimension ( MA % nMessages ) :: &
      Handle
      
    if ( MA % nMessages <= 0 ) then
      AllFinished = .true.
      return
    end if
    
    do iM = 1, MA % nMessages
      Handle ( iM ) = MA % MessageTemplate ( iM ) % Handle
    end do
    
    call MPI_WAITANY &
           ( MA % nMessages, Handle, Index, MPI_STATUSES_IGNORE, &
             MA % Error )
    
    AllFinished = .false.
    if ( Index == MPI_UNDEFINED ) then
      AllFinished = .true.
      return
    else 
      MA % MessageTemplate ( Index ) % Handle = Handle ( Index )
    end if
  
  end subroutine WaitAny
  
  
  subroutine WaitAll ( MA )
    
    class ( MessageArrayTemplate ), intent ( inout ) :: &
      MA
    
    integer ( KDI ) :: &
      iM    !-- iMessage
    integer ( KDI ), dimension ( MA % nMessages ) :: &
      Handle
    
    do iM = 1, MA % nMessages
      Handle ( iM ) = MA % MessageTemplate ( iM ) % Handle
    end do
          
    if ( MA % nMessages <= 0 ) return
    
    call MPI_WAITALL &
           ( MA % nMessages, Handle, MPI_STATUSES_IGNORE, MA % Error )
    
    if ( MA % Error == MPI_SUCCESS ) then
      do iM = 1, MA % nMessages
        MA % MessageTemplate ( iM ) % Handle = Handle ( iM )
      end do
    end if
    
  end subroutine WaitAll
 

end module MessageArray_Template
