set -e

out=../simuls_ama_km_namelists_FIRE_MESONH
mkdir -p $out

rename() {
  ccas=${cas:0:3}
  mv conf_PRE_IDEA_${ccas}*_${mode}.nam $out/conf_PRE_IDEA_${cas}${1}${MNH}_${mode}${2}.nam
  mv conf_EXSEG00_${ccas}*_${mode}.nam $out/conf_EXSEG00_${cas}${1}${MNH}_${mode}${2}.nam 
}

for cas in FIRE #RICO ARMCU 
do
  case $cas in 
    ARMCU|RICO) scas=MESONH;;
    *) scas=MESONH;;
  esac
  cmd="python convert.py -c $cas -i ../../dephy-scm -s $scas"
  for MNH in "571-LIMA" #"571-ICE3" "ADR-LIMA" "ADR-ICE3"
  do
    case $MNH in
      "571-LIMA") opt=;;
      "571-ICE3") opt="-I";;
      "ADR-LIMA") opt="-a";;
      "ADR-ICE3") opt="-a -I";;
    esac
    for mode in SCM #CRM
    do
      echo $cas $mode $MNH

      ## with edkf, grille AROME 25m en bas
      #$cmd -m $mode  -g ../grilles/grille_AROME_CL_25m.txt $opt
      #rename _edkf_25m_en_bas_

      ## with edkf, grille tout 25m
      #$cmd -m $mode $opt
      #rename _edkf_25m_partout_

      # with edkf, grille AROME (5 m en bas)
      $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt $opt
      rename _edkf_5m_en_bas_

      # no edkf, grille AROME (5 m en bas)
      $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -e $opt
      rename _noedkf_5m_en_bas_

      if [ $mode == CRM ] ; then
        # with edkf, grille AROME (5 m en bas), dx = 2.5 km, domain 100 km (40 points)
        $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -x 2500 -L 40 $opt
        rename _edkf_5m_en_bas_ _2.5km

        # no edkf, grille AROME (5 m en bas), dx = 2.5 km, domain 100 km (40 points)
        $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -e -x 2500 -L 40 $opt
        rename _noedkf_5m_en_bas_ _2.5km
      fi

      ### TEST RADIATION SCHEME ###
      case $cas in 
      FIRE)
        # with edkf rad ECMWF, grille AROME (5 m en bas)
        $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -r $opt
        rename _ecmwf_edkf_5m_en_bas_

        # no edkf rad ECMWF, grille AROME (5 m en bas)
        $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -r -e $opt
        rename _ecmwf_noedkf_5m_en_bas_

        if [ $mode == CRM ]; then
          # with edkf rad ECMWF, grille AROME dx = 2.5km, domain 100 km
          $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -r -x 2500 -L 40 $opt
          rename _ecmwf_edkf_5m_en_bas_ _2.5km

          # no edkf, grille AROME (5 m en bas), dx = 2.5 km, domain 100 km (40 points)
          $cmd -m ${mode}  -g ../grilles/grille_AROME_CL.txt -r -e -x 2500 -L 40 $opt
          rename _ecmwf_noedkf_5m_en_bas_ _2.5km
        fi
        ;;
      esac
    done # mode
  done # MNH
done # cas
