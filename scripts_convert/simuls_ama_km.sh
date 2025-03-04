# Script pour générer les namelists des simulations Meso-NH que l'on fait tourner pour les AMA 2025

# On a trois cas ARMCU, RICO et FIRE (3)
# en SCM, CRM 1km, CRM 2.5km         (x3)
# avec EDKF, sans EDKF, sans pluie, avec les modifs d'Adrien   (x4)

# pour FIRE, on fait des tests de sensibilité en plus 
# grille verticale proche de la surface 
# schéma de rayonnement
# forçages ?

# dans les CRM, on veut les bilans 3D

set -e

out=../simuls_ama_km_namelists/
mkdir -p $out

rename() {
  ccas=${cas:0:3}
  mv conf_PRE_IDEA_${ccas}*_${mode}.nam $out/conf_PRE_IDEA_${cas}${1}${MNH}_${mode}${2}.nam
  mv conf_EXSEG00_${ccas}*_${mode}.nam $out/conf_EXSEG00_${cas}${1}${MNH}_${mode}${2}.nam 
}

for cas in FIRE RICO ARMCU
do
  scas=MESONH
  case $cas in 
    FIRE) grille=_top1200;;
    *) grille="";;
  esac
  cmd="python convert.py -c $cas -i ../../dephy-scm -s $scas -B"
  for MNH in "571-LIMA" "ADR-ICE3" "ADR-NOAC" #"571-ICE3" "ADR-LIMA" "ADR-ICE3"
  do
    case $MNH in
      "571-LIMA") opt=;;
      "571-ICE3") opt="-I";;
      "ADR-NOAC") opt="-a 1 -I";;
      "ADR-ICE3") opt="-a 2 -I";;
      "ADR-LIMA") opt="-a 2";;
    esac
    for modd in SCM CRM1 CRM2
    do
      case $modd in
        SCM)  mode=SCM; ext=""     ; opts=$opt" -x 1300" ;;
        CRM1) mode=CRM; ext=""     ; opts=$opt ;;
        CRM2) mode=CRM; ext=_2.5km ; opts=$opt" -x 2500 -L 40" ;;
      esac

      echo $cas $mode $MNH

      # with edkf, grille AROME 25m en bas
      $cmd -m $mode  -g ../grilles/grille_AROME_CL_25m$grille.txt $opts
      rename _edkf_25m_en_bas_ $ext

      # with edkf, grille AROME (5 m en bas)
      $cmd -m ${mode}  -g ../grilles/grille_AROME_CL$grille.txt $opts
      rename _edkf_ $ext

      # no edkf, grille AROME (5 m en bas)
      $cmd -m ${mode}  -g ../grilles/grille_AROME_CL$grille.txt -e $opts
      rename _noedkf_ $ext

      # no rain, grille AROME (5 m en bas)
      $cmd -m ${mode}  -g ../grilles/grille_AROME_CL$grille.txt -R $opts
      rename _norain_ $ext

      ### TEST RADIATION SCHEME ###
      case $cas in 
      FIRE)
        # with edkf rad ECMWF, grille AROME (5 m en bas)
        $cmd -m ${mode}  -g ../grilles/grille_AROME_CL$grille.txt -r $opts
        rename _edkf_ecmwf_ $ext
        ;;
      esac
    done # mode
  done # MNH
done # cas
