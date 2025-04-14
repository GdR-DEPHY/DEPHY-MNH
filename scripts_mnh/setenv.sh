# util functions 
clean_env(){
  mv $HOME/.local/bin/convert_mnh_000 /tmp
  mv $HOME/.local/bin/convert_MNH571_to_DEPHY /tmp
  mv $HOME/.local/lib/mypy/dephy_variables.py /tmp
  mv $HOME/.local/lib/mypy/mesonh2dephy_variables.py /tmp
}

change_shebang(){
  nam=$(hostname)
  case $nam in 
    belenos*) shb="#!/opt/softs/anaconda3/bin/python" ;;
    *) shb="#!/bin/python" ;;
  esac
  sed -i "s,^#!.*,$shb,g" convert_MNH571_to_DEPHY
}

install_env(){
  set -e 
  pwd=$PWD
  mkdir -p $HOME/.local/bin 
  mkdir -p $HOME/.local/lib/mypy
  cd $HOME/.local/bin
  ln -s $pwd/convert_MNH571_to_DEPHY $pwd/convert_mnh_000 .
  change_shebang

  cd $HOME/.local/lib/mypy
  ln -s $pwd/mesonh2dephy_variables.py $pwd/dephy_variables.py .
}



### MAIN COMMANDS
if [ $# -ge 1 ] ; then
  if [[ "$1" == "clean" ]]; then
    ( clean_env ) # do this in child shell
  else
    echo "unknown option $@"
  fi
else 
  ( install_env ) # do this in child shell

  cmd="export PYTHONPATH="$HOME/.local/lib/mypy/:$PYTHONPATH""
  eval $cmd

  # add this line to bashrc for future uses (only if not already there)
  set -o pipefail
  if ! cat ~/.bashrc|grep PYTHON|grep mypy > /dev/null ; then echo $cmd >> ~/.bashrc ; fi
  if cat ~/.bashrc|grep PYTHON|grep mypy|grep "^#" > /dev/null ; then echo $cmd >> ~/.bashrc ; fi
fi
