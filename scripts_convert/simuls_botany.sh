set -e

sim=SHORT
sim=RADFRC
sim=ALLFRC
sim=SFCFRC
sim=REF
sim=NONUDGING

listsim="REF SHORT RADFRC ALLFRC SFCFRC NONUDGING"
listsim="MNH"

listcas="006b 006 037 040 045 066 084"
listcas="006 040 045 066 084"
listcas="006 037 040 045 066 084"

MNH=
out=../namelists_botany/
mkdir -p $out

rename() {
  ccas=${cas:0:3}
  #mv conf_PRE_IDEA_${ccas}*_${mode}.nam $out/conf_PRE_IDEA_${cas}${1}${MNH}_${mode}${2}.nam
  #mv conf_EXSEG00_${ccas}*_${mode}.nam $out/conf_EXSEG00_${cas}${1}${MNH}_${mode}${2}.nam 
}

cas=BOTANY
grille="grille_BOTANY_4km"

for sim in $listsim ; do
  for scas in $listcas  ; do
    cmd="echo python convert.py -v -c $cas -i ../../dephy-scm -s ${sim}$scas -g ../grilles/$grille.txt"
    for mode in SCM 
    do
      echo $cas ${sim}$scas $mode 
      $cmd -m ${mode} -S ECUME6
      rename -ECUM6${sim}$scas
      
      $cmd -m ${mode} -S ECUME
      rename -ECUM${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 1
      rename -ADRI1${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 2
      rename -ADRI2${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 3
      rename -ADRI3${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 4
      rename -ADRI4${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 1 -x 1300
      rename -ADRI1DX1300${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 3 -x 1300
      rename -ADRI3DX1300${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -a 4 -x 1300
      rename -ADRI4DX1300${sim}$scas
      
      $cmd -m ${mode} -S ECUME6 -g ../grilles/grille_BOTANY.txt
      rename -ZTOP7${sim}$scas
      
      $cmd -m ${mode} -I -S ECUME6
      rename -ICE3${sim}${scas}
      
      $cmd -m ${mode} -I -p -S ECUME6
      rename -PLUIE${sim}${scas}

      $cmd -m ${mode} -r -S ECUME6
      rename -RRTM${sim}${scas}

      $cmd -m ${mode} -S DIRECT
      rename -NOECUM${sim}${scas}
    done
  done
done
