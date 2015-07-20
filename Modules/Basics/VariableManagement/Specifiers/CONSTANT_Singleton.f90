!-- CONSTANT_Singleton defines physical and astrophysical constants

module CONSTANT_Singleton

  use KIND_DEFAULT_Singleton
  
  implicit none
  private
  
  type, public :: ConstantSingleton
    real ( KDR ) :: &
      !-- Mathematical
      PI = acos ( -1.0_KDR ), &
      !-- Physical 
      !   http://pdg.lbl.gov/2013/reviews/rpp2013-rev-phys-constants.pdf
      SPEED_OF_LIGHT_MKS = 2.99792458e+8_KDR, &
      PERMEABILITY_MKS   = 4.0e-7_KDR * acos ( -1.0_KDR ), & 
      GRAVITATIONAL_MKS  = 6.67384e-11_KDR, &
      AVOGADRO_MKS       = 6.02214129e+23_KDR, &
      BOLTZMANN_MKS      = 1.3806488e-23_KDR, &
      ELECTRON_VOLT_MKS  = 1.602176565e-19_KDR, &
      !-- Astrophysical 
      !   http://pdg.lbl.gov/2013/reviews/rpp2013-rev-astrophysical-constants.pdf
      ASTRONOMICAL_UNIT_MKS = 1.49597870700e+11_KDR, &
      SOLAR_MASS_MKS        = 1.9885e+30_KDR, &
      !-- GenASiS
      SPEED_OF_LIGHT = 1.0_KDR, &
      PERMEABILITY   = 1.0_KDR, &
      GRAVITATIONAL  = 1.0_KDR, &
      BOLTZMANN      = 1.0_KDR
  end type ConstantSingleton
  
  type ( ConstantSingleton ), public, parameter :: &
    CONSTANT = ConstantSingleton ( )

end module CONSTANT_Singleton
