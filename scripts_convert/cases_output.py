# config outputs 

# spinup = durée du spin up en heures
# hourhf = liste des heures à sortir à haute fréquence (1h à chaque fois)

base_cas = {"spinup":4, "hourhf":[]}

CasesOutputs = {
  "BOTANY":{"spinup":0, "hourhf" : []},
  "CASS":{"spinup":4, "hourhf" : [4, 8]},
  "ARMCU":{"spinup":4, "hourhf" : [4, 8]},
  "BOMEX":{"spinup":4, "hourhf" : [5, 11]},
  "RICO" :{"spinup":4, "hourhf" : [11, 23]}, #, 35, 47, 59, 71
  "SANDU":base_cas,
  "SCMS":{"spinup":4, "hourhf" : [4, 8]},
  "FIRE":{"spinup":4, "hourhf" : [4, 16, 23]},
  #"FIRE":{"spinup":0, "hourhf" : []},
  "LBA":{"spinup":4, "hourhf" : [4, 6]},
  "AMMA":{"spinup":4, "hourhf" : [4, 7, 8, 9, 10, 11]},
  "KB2006":{"spinup":4, "hourhf" : [6, 12, 36, 60, 75, 90, 105, 119]},
  "EUROCS":base_cas,
  "GABLS1":{"spinup":4, "hourhf" : [8]},
  "GABLS4":{"spinup":4, "hourhf" : [5,10,13,15]},
  "AYOTTE":{"spinup":4, "hourhf" : [6]},
  "IHOP":{"spinup":4, "hourhf" : [6]},
  "BLLAST":{"spinup":4, "hourhf" : [6]},
  "MOSAI":base_cas,
}
