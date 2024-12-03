# le script convert_mnh_000 doit être dans le répertoire courant ou dans $HOME/.local/bin
export PATH=$HOME/.local/bin/:$(pwd):$PATH

source ../../conf/profile_mesonh
# source ../../conf/profile_mesonh-LXgfortran-R8I4-MNH-V5-7-1-ECRAD140-MPIAUTO-DEBUG

DIR_NAMELISTS=$HOME/work/dephy/dephy-mnh/output_namelists/

set -e
for conf in `ls $DIR_NAMELISTS/conf*IDEA*SCM.nam`
do
  repo=$(echo $conf|sed -e "s/^.*IDEA_//g" -e "s/_SCM.nam//g")
  echo $repo

  set +e
  ls $repo/*flat*nc 
  if [ $? -eq 0 ] ; then echo "already done" ; continue ; fi
  set -e

  mkdir -p $repo
  cp $DIR_NAMELISTS/conf*$repo*SCM.nam $repo
  cd $repo
  ln -sf conf*IDEA* PRE_IDEA1.nam
  ln -sf conf*EXSEG* EXSEG1.nam
  time ${MONORUN} PREP_IDEAL_CASE${XYZ} > log_PREP_IDEAL 2>&1
  time ${MONORUN} MESONH${XYZ} > log_MESONH 2>&1
  res=$(ls *SEG00.000.nc)
  convert_mnh_000 $res
  cd ..
done
