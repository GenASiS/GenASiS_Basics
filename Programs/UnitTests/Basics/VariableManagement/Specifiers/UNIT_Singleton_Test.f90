program UNIT_Singleton_Test

  use UNIT_Singleton
  
  implicit none
  
  call UNIT % Initialize ( )

  print*
  print*, 'Identity Unit'
  print*, 'IDENTITY            ', UNIT % IDENTITY 

  print*
  print*, 'Length Units'
  print*, 'METER               ', UNIT % METER      
  print*, 'CENTIMETER          ', UNIT % CENTIMETER 
  print*, 'FEMTOMETER          ', UNIT % FEMTOMETER 
  print*, 'KILOMETER           ', UNIT % KILOMETER  
  print*, 'PARSEC              ', UNIT % PARSEC     
  print*, 'GIGAPARSEC          ', UNIT % GIGAPARSEC

  print*
  print*, 'Time Units' 
  print*, 'SECOND              ', UNIT % SECOND     
  print*, 'MILLISECOND         ', UNIT % MILLISECOND

  print*
  print*, 'Frequency Units'
  print*, 'HERTZ               ', UNIT % HERTZ      
  print*, 'KILOHERTZ           ', UNIT % KILOHERTZ

  print*
  print*, 'Mass Units'  
  print*, 'KILOGRAM            ', UNIT % KILOGRAM   
  print*, 'GRAM                ', UNIT % GRAM       
  print*, 'ATOMIC_MASS_UNIT    ', UNIT % ATOMIC_MASS_UNIT
  print*, 'SOLAR_MASS          ', UNIT % SOLAR_MASS

  print*
  print*, 'Speed Units'
  print*, 'SPEED_OF_LIGHT      ', UNIT % SPEED_OF_LIGHT

  print*
  print*, 'Energy Units'
  print*, 'JOULE               ', UNIT % JOULE
  print*, 'ERG                 ', UNIT % ERG
  print*, 'BETHE               ', UNIT % BETHE
  print*, 'ELECTRON_VOLT       ', UNIT % ELECTRON_VOLT
  print*, 'MEV                 ', UNIT % MEV

  print*
  print*, 'Force Units'
  print*, 'NEWTON              ', UNIT % NEWTON

  print*
  print*, 'Pressure Units'
  print*, 'PASCAL              ', UNIT % PASCAL

  print*
  print*, 'Temperature Units'
  print*, 'KELVIN              ', UNIT % KELVIN

  print*, 'Magnetic Current Units'
  print*, 'AMPERE              ', UNIT % AMPERE

  print*
  print*, 'Magnetic Field Units'
  print*, 'TESLA               ', UNIT % TESLA
  print*, 'GAUSS               ', UNIT % GAUSS

  print*, 'Mass Density'
  print*, 'MASS_DENSITY_CGS    ', UNIT % MASS_DENSITY_CGS

end program UNIT_Singleton_Test
