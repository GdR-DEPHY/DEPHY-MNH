from Config import Config

default = Config("ARMCU", "EXSEG01", None)
default.write('default_namelist.nam')
print(default)

config = Config("ARMCU", "EXS", "LES")
config.write('default_LES_namelist.nam')

config.set_warm_microphysics()
config.activate_radiation()
config.modify("NAM_PARAM_ECRAD", "NSWSOLVER", "1")
config.write('cumulus_with_radiation_LES_namelist.nam')

exit()

## Scénario du script de conversion au format commun

# Créer une namelist par défaut en mode LES|CRM|SCM

# Lire les variables du fichier format commun
# Modifier la namelist en fonction du format commun
# => on obtient preidea et exseg mère

# Lire le fichier de config spécifique du cas
# contenant les temps clés et durée du spinup
# En déduire le nombre de EXSEG à créer,
# Copier autant de fois la config exseg mère
# Modifier les configs filles en fonction
#   fichier init, restart, durée ...
#   flux sur le segment simulé...
#   fréquence des xout ...
