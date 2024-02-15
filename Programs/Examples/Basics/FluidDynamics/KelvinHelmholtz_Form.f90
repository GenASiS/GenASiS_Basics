module KelvinHelmholtz_Form

  use Basics
  use PolytropicFluid_Form
  use ConservationLawEvolution_Template

  implicit none
  private

  type, public, extends ( ConservationLawEvolutionTemplate ) :: &
    KelvinHelmholtzForm
  contains
    procedure, private, pass :: &
      Initialize_KH
    generic, public :: &
      Initialize => Initialize_KH
    final :: &
      Finalize
  end type KelvinHelmholtzForm

contains


  subroutine Initialize_KH ( KH )

    class ( KelvinHelmholtzForm ), intent ( inout ) :: &
      KH

    integer ( KDI ) :: &
      iV
    real ( KDR ) :: &
      Density_A, Density_B, &  !-- Region A and B
      Pressure, &
      Speed_XA, Speed_XB, Speed_YZ, &
      AdiabaticIndex
    real ( KDR ), dimension ( : ), allocatable :: &
      Random  !-- Array of Random values      ! -- AJE 06/25
    type ( QuantityForm ) :: &
      DensityUnit, &
      EnergyUnit, &
      SpeedUnit

    KH % Type = 'a KelvinHelmholtz' 

    call KH % Initialize &
           ( PROGRAM_HEADER % Communicator, &
             BoundaryConditionOption = 'PERIODIC'  )

    associate ( DM => KH % DistributedMesh )

    allocate ( PolytropicFluidForm :: KH % ConservedFields )
    select type ( PF => KH % ConservedFields )
    type is ( PolytropicFluidForm )

    call PF % Initialize ( DM, NameOption = 'PolytropicFluid' )
    call PF % AllocateDevice ( )
    
    call DM % SetGhostExchange ( PF )

    !-- States in the Regions

    Pressure       =  2.5_KDR
    Density_A      =  2.0_KDR
    Speed_XA       =  0.5_KDR
    Density_B      =  1.0_KDR
    Speed_XB       = -0.5_KDR
    Speed_YZ       =  0.0_KDR
    AdiabaticIndex =  1.4_KDR

    DensityUnit    = UNIT % IDENTITY
    EnergyUnit     = UNIT % IDENTITY
    SpeedUnit      = DM % CoordinateUnit ( 1 ) / KH % TimeUnit
 
    call PROGRAM_HEADER % GetParameter &
           ( Pressure, 'Pressure', InputUnitOption = EnergyUnit )
    call PROGRAM_HEADER % GetParameter &
           ( Density_A, 'Density_A', InputUnitOption = DensityUnit )
    call PROGRAM_HEADER % GetParameter &
           ( Speed_XA, 'Speed_XA', InputUnitOption = SpeedUnit )
    call PROGRAM_HEADER % GetParameter &
           ( Density_B, 'Density_B', InputUnitOption = DensityUnit )
    call PROGRAM_HEADER % GetParameter &
           ( Speed_XB, 'Speed_XB', InputUnitOption = SpeedUnit )
    call PROGRAM_HEADER % GetParameter &
           ( Speed_YZ, 'Speed_YZ', InputUnitOption = SpeedUnit )
    call PROGRAM_HEADER % GetParameter &
           ( AdiabaticIndex, 'AdiabaticIndex' )
    
   !-- Translate to origin, rotate normal to xz plane and then to x axis

    associate &
      ( X     => DM % Position % Value ( :, 1 ), &
        Y     => DM % Position % Value ( :, 2 ), &
        Z     => DM % Position % Value ( :, 3 ), &
        VX    => PF % Value ( :, PF % VELOCITY ( 1 ) ), &
        VY    => PF % Value ( :, PF % VELOCITY ( 2 ) ), &
        VZ    => PF % Value ( :, PF % VELOCITY ( 3 ) ), &
        N     => PF % Value ( :, PF % COMOVING_DENSITY ), &
        P     => PF % Value ( :, PF % PRESSURE ), &
        Gamma => PF % Value ( :, PF % ADIABATIC_INDEX ) )

    allocate ( Random ( PF % nValues ) )                           ! -- AJE 06/25
    call InitializeRandomSeed ( PROGRAM_HEADER % Communicator )    ! -- AJE 06/25
    call Random_Number ( Random )                                  ! -- AJE 06/25

   do iV = 1, PF % nValues
      P  ( iV ) = Pressure
      VY ( iV ) = Speed_YZ + 0.02_KDR * Random ( iV ) + 0.01_KDR ! -- AJE 06/25
      VZ ( iV ) = Speed_YZ
      if ( ABS ( Y ( iV ) - 0.5_KDR ) < 0.25_KDR ) &
      then
        N  ( iV ) = Density_A
        VX ( iV ) = Speed_XA + 0.02_KDR * Random ( iV ) - 0.01_KDR ! -- AJE 06/25
      else
        N  ( iV ) = Density_B    
        VX ( iV ) = Speed_XB + 0.02_KDR * Random ( iV ) - 0.01_KDR ! -- AJE 06/25
      end if 
    end do
    Gamma = AdiabaticIndex

    call PF % ComputeAuxiliaryFromPressure ( PF % Value )
    call PF % ComputeConserved ( PF % Value )

    call PF % SetOutputPolytropic &
           ( VelocityUnitOption = spread ( SpeedUnit, 1, 3 ), &
             DensityUnitOption = DensityUnit, EnergyUnitOption = EnergyUnit )

    call PF % UpdateDevice ( )
    
    end associate !-- X, etc.
    end select !-- PF
    end associate !-- DM

  end subroutine Initialize_KH


  subroutine Finalize ( KH )

    type ( KelvinHelmholtzForm ), intent ( inout ) :: &
      KH

    if ( allocated ( KH % ConservedFields ) ) &
      deallocate ( KH % ConservedFields )

  end subroutine Finalize


end module KelvinHelmholtz_Form
