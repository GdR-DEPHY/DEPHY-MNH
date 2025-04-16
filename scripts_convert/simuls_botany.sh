set -e

sim=SHORT
sim=RADFRC
listcas="006b " #006 037 040 045 066 084"

MNH=
out=../namelists_botany/
mkdir -p $out

rename() {
  ccas=${cas:0:3}
  mv conf_PRE_IDEA_${ccas}*_${mode}.nam $out/conf_PRE_IDEA_${cas}${1}${MNH}_${mode}${2}.nam
  mv conf_EXSEG00_${ccas}*_${mode}.nam $out/conf_EXSEG00_${cas}${1}${MNH}_${mode}${2}.nam 
}

cas=BOTANY

for scas in $listcas 
do
  cmd="python convert.py -c $cas -S -i ../../dephy-scm -s ${sim}$scas -g ../grilles/grille_BOTANY.txt"
  for mode in SCM 
  do
    echo $cas ${sim}$scas $mode 
    $cmd -m ${mode}
    rename -${sim}$scas
  done
done
