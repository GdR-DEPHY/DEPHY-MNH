# config outputs 

# spinup = durée du spin up en heures
# hourhf = liste des heures à sortir à haute fréquence (1h à chaque fois)

base_cas = {"spinup":4, "hourhf":[]}
CasesOutputs = {
  "ARMCU":{"spinup":4, "hourhf" : [4, 8]},
  "BOMEX":base_cas,
  "RICO" :base_cas,
  "SANDU":base_cas,
  "SCMS":base_cas,
  "FIRE":base_cas,
  "LBA":base_cas,
  "AMMA":{"spinup":4, "hourhf" : [6, 10, 16]},
  "KB2006":base_cas,
  "EUROCS":base_cas,
  "GABLS1":base_cas,
  "GABLS4":base_cas,
  "AYOTTE":base_cas,
  "IHOP":base_cas,
  "BLLAST":base_cas,
}
