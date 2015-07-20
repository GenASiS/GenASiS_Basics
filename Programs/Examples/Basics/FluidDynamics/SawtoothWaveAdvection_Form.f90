module SawtoothWaveAdvection_Form

  use Basics
  use PlaneWaveAdvection_Template

  implicit none
  private

  type, public, extends ( PlaneWaveAdvectionTemplate ) :: SawtoothWaveAdvectionForm
    type ( MeasuredValueForm ) :: &
      Offset, &
      Amplitude
  contains
    procedure, private, pass :: &
      Initialize_SWA
    generic, public :: &
      Initialize => Initialize_SWA
    procedure, public, pass :: &
      Waveform
    final :: &
      Finalize
  end type SawtoothWaveAdvectionForm

contains


  subroutine Initialize_SWA ( SWA )
    
    class ( SawtoothWaveAdvectionForm ), intent ( inout ) :: &
      SWA

    type ( MeasuredValueForm ) :: &
      OffsetUnit, &
      AmplitudeUnit

    if ( SWA % Type == '' ) SWA % Type = 'a SawtoothWaveAdvection' 
    
    OffsetUnit      = UNIT % IDENTITY
    AmplitudeUnit   = UNIT % IDENTITY
    SWA % Offset    = 2.0_KDR * OffsetUnit
    SWA % Amplitude = 1.0_KDR * AmplitudeUnit
    call PROGRAM_HEADER % GetParameter &
           ( SWA % Offset, 'Offset', InputUnitOption = OffsetUnit )
    call PROGRAM_HEADER % GetParameter &
           ( SWA % Amplitude, 'Amplitude', InputUnitOption = AmplitudeUnit )

    if ( OffsetUnit /= AmplitudeUnit ) then
      call Show ( 'Please use the same units for Offset and Amplitude', &
                  CONSOLE % ERROR )
      call PROGRAM_HEADER % Abort ( )
    end if

    call SWA % Initialize ( AmplitudeUnit )

  end subroutine Initialize_SWA


  elemental function Waveform ( PWA, X ) result ( W )

    !-- Waveform with a full period in the range 0 < X < 1

    class ( SawtoothWaveAdvectionForm ), intent ( in ) :: &
      PWA
    real ( KDR ), intent ( in ) :: &
      X
    real ( KDR ) :: &
      W
    
    associate ( Pi => CONSTANT % PI )

    W = PWA % Offset &
        - 2.0_KDR * PWA % Amplitude / Pi &
          * atan ( 1.0_KDR / tan ( Pi * max ( X, tiny ( 1.0_KDR ) ) ) )

    end associate !-- Pi

  end function Waveform


  subroutine Finalize ( SWA )

    type ( SawtoothWaveAdvectionForm ), intent ( inout ) :: &
      SWA

    if ( allocated ( SWA % ConservedFields ) ) &
      deallocate ( SWA % ConservedFields )

  end subroutine Finalize


end module SawtoothWaveAdvection_Form
