!-- PROGRAM_HEADER provides functionalities commonly required by all programs
!   (drivers), including initialization of parallel environment, obtaining
!   program parameters, and displaying basic runtime statistics.

module PROGRAM_HEADER_Singleton
  
  use VariableManagement
  use Display
  use MessagePassing
  use FileSystem
  use WallTime_Function
  use GetMemoryUsage_Command
  use CommandLineOptions_Form
!  use petsc

  implicit none
  private
  
  type, public :: ProgramHeaderSingleton
    integer ( KDI ) :: &
      nThreads = 1
    type ( MeasuredValueForm )  :: &
      WallTime, &
      WallTimeStart, &
      InputOutputWallTime, &
      ComputationalWallTime, &
      HighWaterMark, &
      MaxHighWaterMark, &
      MinHighWaterMark, &
      MeanHighWaterMark, &
      ResidentSetSize, &
      MaxResidentSetSize, &
      MinResidentSetSize, &
      MeanResidentSetSize
    type ( MeasuredValueForm ), private :: &
      OldHighWaterMark, &
      OldMeanHighWaterMark, &
      OldResidentSetSize, &
      OldMeanResidentSetSize
    character ( LDL ) :: &
      Dimensionality = ''
    character ( LDF ) :: &
      Name
    type ( CommunicatorForm ), allocatable :: &
      Communicator 
    type ( ParametersStreamForm ), allocatable :: &
      ParametersStream 
    type ( CommandLineOptionsForm ), allocatable :: &
      CommandLineOptions
  contains
    procedure, public, nopass :: &
      Initialize
    procedure, private, nopass :: &
      GetParameter_0D_Integer
    procedure, private, nopass :: &
      GetParameter_0D_Real
    procedure, private, nopass :: &
      GetParameter_0D_MeasuredValue
    procedure, private, nopass :: &
      GetParameter_0D_Logical
    procedure, private, nopass :: &
      GetParameter_0D_Character
    procedure, private, nopass :: &
      GetParameter_1D_Integer
    procedure, private, nopass :: &
      GetParameter_1D_Real
    procedure, private, nopass :: &
      GetParameter_1D_MeasuredValue
    procedure, private, nopass :: &
      GetParameter_1D_Logical
    procedure, private, nopass :: &
      GetParameter_1D_Character
    generic :: &
      GetParameter &
        => GetParameter_0D_Integer, GetParameter_0D_Real, &
           GetParameter_0D_MeasuredValue, GetParameter_0D_Logical, &
           GetParameter_0D_Character, &
           GetParameter_1D_Integer, GetParameter_1D_Real, &
           GetParameter_1D_MeasuredValue, GetParameter_1D_Logical, &
           GetParameter_1D_Character
    procedure, public, nopass :: &
      ShowStatistics
    procedure, public, nopass :: &
      Abort => Abort_PH  !-- avoids conflict with intrinsic "abort"
    final :: &
      Finalize
  end type ProgramHeaderSingleton
  
  type ( ProgramHeaderSingleton ), public, target, allocatable :: &
    PROGRAM_HEADER
    
contains

 
  subroutine Initialize ( Name, AppendDimensionalityOption ) 

    character ( * ), intent ( in )  :: &
      Name
    logical ( KDL ), intent ( in ), optional :: &
      AppendDimensionalityOption
      
    integer ( KDI )  :: &
      Error, &
      DisplayRank
    character ( LDL )  :: &
      Verbosity
    character ( LDF ) :: &
      Filename
    logical ( KDL ) :: &
      AppendDimensionality, &
      DimensionalityFound
    type ( ProgramHeaderSingleton ), pointer :: &
      PH
    procedure ( ), pointer :: &
      Abort
      
    AppendDimensionality = .true.
    if ( present ( AppendDimensionalityOption ) ) &
      AppendDimensionality = AppendDimensionalityOption

    PH => PROGRAM_HEADER 
      
!    PH % nThreads = OMP_GET_MAX_THREADS ( )
      
    call PH % InputOutputWallTime % Initialize ( 's', 0.0_KDR )
    call PH % ComputationalWallTime % Initialize ( 's', 0.0_KDR )

    allocate ( PH % Communicator )
    call PH % Communicator % Initialize ( )
    
    PH % WallTimeStart = WallTime ( ) 
    
    call UNIT % Initialize ( )
    
    Abort => Abort_PH
    call CONSOLE % Initialize ( PH % Communicator % Rank, AbortOption = Abort )

    allocate ( PH % ParametersStream )
    allocate ( PH % CommandLineOptions )
    associate &
      ( PS   => PH % ParametersStream, &
        CLO => PH % CommandLineOptions )

    call CLO % Initialize ( )

    if ( AppendDimensionality ) then
      call CLO % Read &
             ( PH % Dimensionality, 'Dimensionality', &
               IgnorabilityOption = CONSOLE % INFO_1, &
               SuccessOption = DimensionalityFound )
      if ( .not. DimensionalityFound ) then
        PH % Dimensionality = '3D'
        call Show &
               ( 'Dimensionality not specified, defaulting to 3D', &
                 CONSOLE % WARNING )
        call Show ( PH % Dimensionality, 'Dimensionality' )
      end if
      PH % Name = trim ( Name ) // '_' // trim ( PH % Dimensionality )
    else
      PH % Name = Name
    end if

    Filename = trim ( PH % Name ) // '_Program_Parameters'
    call PH % ParametersStream % Initialize &
           ( Filename, PH % Communicator % Rank )

    DisplayRank  = CONSOLE % DisplayRank
    call PH % GetParameter ( DisplayRank, 'DisplayRank' )
    call PH % Communicator % Synchronize ( )
    call CONSOLE % SetDisplayRank ( DisplayRank )

    Verbosity = CONSOLE % LABEL ( CONSOLE % Verbosity ) 
    call PH % GetParameter ( Verbosity, 'Verbosity' )
    call CONSOLE % SetVerbosity ( Verbosity )

    call GetMemoryUsage &
           ( PH % Communicator, PH % OldHighWaterMark, &
             PH % OldResidentSetSize, &
             Mean_HWM_Option = PH % OldMeanHighWaterMark, &
             Mean_RSS_Option = PH % OldMeanResidentSetSize )
             
!    call Show ( 'Initializing PETSc', CONSOLE % INFO_1)
!    call PETSCINITIALIZE ( PETSC_NULL_CHARACTER, Error )

    call Show ( 'Starting the Program', CONSOLE % INFO_1 ) 
    call Show ( PH % Name, 'Name', CONSOLE % INFO_1 ) 
!    call Show ( PH % nThreads, 'nThreads', CONSOLE % INFO_1 )

    end associate  !-- P, CLO 

    nullify ( Abort )
    nullify ( PH )

  end subroutine Initialize
  
  
  subroutine GetParameter_0D_Integer &
               ( Value, Name, ParametersStreamOption, IgnorabilityOption, &
                 SuccessOption )

    integer ( KDI ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_0D_Integer


  subroutine GetParameter_0D_Real &
               ( Value, Name, InputUnitOption, ParametersStreamOption, &
                 IgnorabilityOption, SuccessOption )

    real ( KDR ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( MeasuredValueForm ), intent ( inout ), optional :: &
      InputUnitOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS )
    call CLO % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_0D_Real


  subroutine GetParameter_0D_MeasuredValue &
               ( Value, Name, InputUnitOption, ParametersStreamOption, &
                 IgnorabilityOption, ConvertOption, SuccessOption )

    type ( MeasuredValueForm ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( MeasuredValueForm ), intent ( inout ), optional :: &
      InputUnitOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( in ), optional :: &
      ConvertOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             ConvertOption = ConvertOption, SuccessOption = Success_PS )
    call CLO % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             ConvertOption = ConvertOption, SuccessOption = Success_CLO )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_0D_MeasuredValue


  subroutine GetParameter_0D_Logical &
               ( Value, Name, ParametersStreamOption, IgnorabilityOption, &
                 SuccessOption )

    logical ( KDL ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_0D_Logical


  subroutine GetParameter_0D_Character &
               ( Value, Name, ParametersStreamOption, IgnorabilityOption, &
                 SuccessOption )

    character ( * ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_0D_Character


  subroutine GetParameter_1D_Integer &
               ( Value, Name, nValuesOption, ParametersStreamOption, &
                 IgnorabilityOption, SuccessOption )

    integer ( KDI ), dimension ( : ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    integer ( KDI ), intent ( inout ), optional :: &
      nValuesOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS, nValuesOption = nValuesOption )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO, nValuesOption = nValuesOption )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_1D_Integer


  subroutine GetParameter_1D_Real &
               ( Value, Name, InputUnitOption, nValuesOption, &
                 ParametersStreamOption, IgnorabilityOption, SuccessOption )

    real ( KDR ), dimension ( : ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( MeasuredValueForm ), dimension ( : ), intent ( inout ), optional :: &
      InputUnitOption
    integer ( KDI ), intent ( inout ), optional :: &
      nValuesOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS, nValuesOption = nValuesOption )
    call CLO % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO, nValuesOption = nValuesOption )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_1D_Real


  subroutine GetParameter_1D_MeasuredValue &
               ( Value, Name, InputUnitOption, nValuesOption, &
                 ParametersStreamOption, IgnorabilityOption, SuccessOption )

    type ( MeasuredValueForm ), dimension ( : ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    type ( MeasuredValueForm ), dimension ( : ), intent ( inout ), optional :: &
      InputUnitOption
    integer ( KDI ), intent ( inout ), optional :: &
      nValuesOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS, nValuesOption = nValuesOption )
    call CLO % Read &
           ( Value, Name, InputUnitOption = InputUnitOption, &
             IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO, nValuesOption = nValuesOption )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_1D_MeasuredValue


  subroutine GetParameter_1D_Logical &
               ( Value, Name, nValuesOption, ParametersStreamOption, &
                 IgnorabilityOption, SuccessOption )

    logical ( KDL ), dimension ( : ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    integer ( KDI ), intent ( inout ), optional :: &
      nValuesOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS, nValuesOption = nValuesOption )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO, nValuesOption = nValuesOption )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_1D_Logical


  subroutine GetParameter_1D_Character &
               ( Value, Name, nValuesOption, ParametersStreamOption, &
                 IgnorabilityOption, SuccessOption )

    character ( * ), dimension ( : ), intent ( inout ) :: &
      Value
    character ( * ), intent ( in ) :: &
      Name
    integer ( KDI ), intent ( inout ), optional :: &
      nValuesOption
    type ( ParametersStreamForm ), intent ( in ), target, optional :: &
      ParametersStreamOption
    integer ( KDI ), intent ( in ), optional :: &
      IgnorabilityOption
    logical ( KDL ), intent ( out ), optional :: &
      SuccessOption

    integer ( KDI ) :: &
      Ignorability
    logical ( KDL ) :: &
      Success_PS, &
      Success_CLO
    type ( ParametersStreamForm ), pointer :: &
      PS

    Ignorability = CONSOLE % INFO_3
    if ( present ( IgnorabilityOption ) ) Ignorability = IgnorabilityOption

    if ( present ( ParametersStreamOption ) ) then
      PS => ParametersStreamOption
    else
      PS => PROGRAM_HEADER % ParametersStream
    end if

    associate ( CLO => PROGRAM_HEADER % CommandLineOptions )

    call Show &
           ( 'Parameter ' // trim ( Name ) // ' default value', &
             Ignorability )
    call Show ( Value, Name, Ignorability )
    call PS % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_PS, nValuesOption = nValuesOption )
    call CLO % Read &
           ( Value, Name, IgnorabilityOption = IgnorabilityOption, &
             SuccessOption = Success_CLO, nValuesOption = nValuesOption )
    if ( present ( SuccessOption ) ) &
      SuccessOption = Success_PS .or. Success_CLO

    end associate !-- CLO

    nullify ( PS )

  end subroutine GetParameter_1D_Character


  subroutine ShowStatistics ( Verbosity, AcrossProcessesOption )
  
    integer ( KDI ), intent ( in ) :: &
      Verbosity
    logical ( KDL ), intent ( in ), optional :: &
      AcrossProcessesOption
      
    logical ( KDL ) :: &
      AcrossProcesses
    type ( ProgramHeaderSingleton ), pointer :: &
      PH
      
    PH => PROGRAM_HEADER 
      
    AcrossProcesses = .false.
    if ( present ( AcrossProcessesOption ) ) &
      AcrossProcesses = AcrossProcessesOption
      
    PH % WallTime = WallTime ( )
    
    if ( AcrossProcesses ) then
      call GetMemoryUsage &
             ( PH % Communicator, PH % HighWaterMark, PH % ResidentSetSize, &
               Max_HWM_Option = PH % MaxHighWaterMark, &
               Min_HWM_Option = PH % MinHighWaterMark, &
               Mean_HWM_Option = PH % MeanHighWaterMark, &
               Max_RSS_Option = PH % MaxResidentSetSize, &
               Min_RSS_Option = PH % MinResidentSetSize, &
               Mean_RSS_Option = PH % MeanResidentSetSize )
    else
      call GetMemoryUsage &
             ( PH % Communicator, PH % HighWaterMark, PH % ResidentSetSize )
    end if
    
    call Show ( 'Runtime statistic', Verbosity )
    
    call Show &
           ( PH % WallTime - PH % WallTimeStart, 'Elapsed wall time', &
             Verbosity )
    call Show &
           ( PH % ComputationalWallTime, 'Computational wall time', &
             Verbosity )
    call Show &
           ( PH % InputOutputWallTime, 'Input/output wall time', &
             Verbosity )
    
    call Show ( PH % HighWaterMark, 'This process HWM', Verbosity )
    call Show &
           ( PH % HighWaterMark - PH % OldHighWaterMark, &
             'This process HWM increase' , Verbosity )
    call Show ( PH % ResidentSetSize, 'This process RSS', Verbosity )
    call Show &
           ( PH % ResidentSetSize - PH % OldResidentSetSize, &
             'This process RSS increase', Verbosity )
    
    if ( AcrossProcesses ) then
      
      call Show &
             ( PH % MaxHighWaterMark, 'Across processes max HWM', Verbosity )
      call Show &
             ( PH % MinHighWaterMark, 'Across processes min HWM', Verbosity )
      call Show &
             ( PH % MeanHighWaterMark, 'Across processes mean HWM', Verbosity )
      call Show &
             ( PH % MeanHighWaterMark - PH % OldMeanHighWaterMark, &
               'Across processes mean HWM increase', Verbosity )
      
      call Show &
             ( PH % MaxResidentSetSize, 'Across processes max RSS', Verbosity )
      call Show &
             ( PH % MinResidentSetSize, 'Across processes min RSS', Verbosity )
      call Show &
             ( PH % MeanResidentSetSize, 'Across processes mean RSS', &
               Verbosity )
      call Show &
             ( PH % MeanResidentSetSize - PH % OldMeanResidentSetSize, &
               'Across processes mean RSS increase', Verbosity )
    
    end if
    
    PH % OldHighWaterMark   = PH % HighWaterMark
    PH % OldResidentSetSize = PH % ResidentSetSize
    
    if ( AcrossProcesses ) then
      PH % OldMeanHighWaterMark   = PH % MeanHighWaterMark
      PH % OldMeanResidentSetSize = PH % MeanResidentSetSize
    end if
  
    nullify ( PH )

  end subroutine ShowStatistics 
  
  
  subroutine Abort_PH ( )
  
    if ( PROGRAM_HEADER % Communicator % Initialized ) then
      call PROGRAM_HEADER % Communicator % Abort ( )
    else
      stop 1
    end if
  
  end subroutine Abort_PH


  subroutine Finalize ( PH ) 

    type ( ProgramHeaderSingleton ), intent ( inout ) :: &
      PH
    
    integer ( KDI )  :: &
      Error
    
    call Show ( 'Finishing the Program', CONSOLE % INFO_1 ) 
    call Show ( PH % Name, 'Name', CONSOLE % INFO_1 )
    
!    call Show ( 'Finalizing PETSc', CONSOLE % INFO_1)
!    call PETSCFINALIZE ( Error )
    
    call PH % ShowStatistics &
           ( CONSOLE % INFO_1, AcrossProcessesOption = .true. )

    if ( allocated ( PH % CommandLineOptions ) ) &
      deallocate ( PH % CommandLineOptions ) 

    if ( allocated ( PH % ParametersStream ) ) &
      deallocate ( PH % ParametersStream )

    if ( allocated ( PH % Communicator ) ) &
      deallocate ( PH % Communicator )

  end subroutine Finalize
  
  
end module PROGRAM_HEADER_Singleton
