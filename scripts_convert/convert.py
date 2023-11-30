from Config import Config

default = Config("ARMCU", "EXS", None)
default.write('default_namelist.nam')

config = Config("ARMCU", "EXS", "LES")
config.write('default_LES_namelist.nam')

config.set_warm_microphysics()
config.activate_radiation()
config.modify("NAM_PARAM_ECRAD", "NSWSOLVER", "1")
config.write('cumulus_with_radiation_LES_namelist.nam')
