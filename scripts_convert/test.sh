DIR_DEPHY_SCM=~/work/dephy/dephy-scm

set -e
for cas in `ls -lrt -d $DIR_DEPHY_SCM/*/|awk -F"m/" '{print $NF}'|sed -e "s/\///g"`
do
  case $cas in 
    RICO|ARMCU) add="-s MESONH";;
    GABLS4) add="-s STAGE3";;
    AYOTTE) add="-s 00SC";;
    MOSAI) add="-s MAIZE";;
    EUROCS) add="-z" ;;
    dephycf|GABLS1|ARPEGE|DYNAMO|ISDAC|MAGIC|MPACE|ASTEX|SCMS) echo "$cas -- skip"; continue ;;
    *) add="";;
  esac
  echo "$cas"
  for mode in SCM LES 
  do
    run="/usr/bin/python3 convert.py -c $cas -i $DIR_DEPHY_SCM -v 3 -g ../grilles/ -o ../output_namelists/ -m $mode $add >> ../logs/log_cas_$cas 2> ../logs/err_cas_$cas "
    echo $run
    eval $run
  done
done 

## BOMEX : ok interpolation des forçages sur grille commune à partir des profils du papier
## cas pas interpolés en grilles de forçages :
#  BOMEX
#     vent géostrophique définit sur 301 niveaux ? dans le papier de ref c'est
#     donné comme une fonction linéaire de z donc on doit pouvoir fixer ug à 5000m
#     et laisser MNH refaire l'interpolation qui va bien au lieu de définir le cas
#     sur 301 niveaux ?
# SCMS

# cas pas défini dans les listes moistshcv / dcv / dryshcv / stable
# ARPEGE|DYNAMO|ISDAC|MAGIC|MPACE|ASTEX

exit 
python convert.py -c MOSAI -i ../../dephy-scm -v 3 -o . -m SCM -z -s MAIZE_ADV
python convert.py -c MOSAI -i ../../dephy-scm -v 3 -o . -m SCM -z -s MAIZE
exit

