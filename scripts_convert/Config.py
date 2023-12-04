# Copyright (C) 2023 Météo-France
# Copyright (C) 2023 Centre National de la Recherche Scientifique
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

"""
Created on November 2023 
@author: F. Couvreux & N. Villefranque
"""


import os
import logging
logging.basicConfig(format='%(asctime)s - %(name)20s - %(levelname)s - %(message)s', level=logging.INFO)
logger = logging.getLogger(__name__)

import time
import copy
import netCDF4 as nc
import numpy as np
from datetime import datetime

from mesonh import default_preidea, default_exseg
from mesonh import preidea_LES, exseg_LES
from mesonh import preidea_CRM, exseg_CRM
from mesonh import preidea_SCM, exseg_SCM

#------------------------------------------#
#------------ HELPER FUNCTIONS ------------#
#------------------------------------------#
def copy_config(copy_to, copy_from, namelist, key):
  """ either full copy or only one namelist or only one key of one namelist 
  # copy_to   intent inout
  # copy_from intent in
  # namelist  intent in
  # key       intent in
  """
  if namelist=="all": # copy all
    copy_to = copy.deepcopy(copy_from)
  else:
    if key=="all":
      copy_to[namelist] = copy_from[namelist].copy()
    else:
      copy_to[namelist][key] = copy_from[namelist][key]
  return copy_to

def replace_config(copy_to, copy_from, namelist, key):
  """ either full config or only one namelist or only one key of one namelist 
  # copy_to   intent inout
  # copy_from intent in
  # namelist  intent in
  # key       intent in
  """
  if namelist=="all": # copy all
    for nam in copy_from:
      for k in copy_from[nam]:
        copy_to[nam][k] = copy_from[nam][k]
  else:
    if key=="all":
      for k in copy_from[namelist]:
        copy_to[namelist][k] = copy_from[namelist][k]
    else:
      copy_to[namelist][key] = copy_from[namelist][key]
  return copy_to

def write_namelist(to_write, outfile):
  """ write config to namelist file 
  input: to_write is dict of dicts config 
         outfile  is str name of file to write
  """
  str=""
  for k in to_write:
    str+="&%s\n"%k
    for kk in to_write[k]:
      str+="  {:s} = {:s},\n".format(kk, to_write[k][kk])
    str+="/ \n\n"
  with open(outfile, "w") as f:
    f.write(str)

#------------------------------------------#
#----------------- CLASS ------------------#
#------------------------------------------#

class Config:
  
  def __init__(self, case, fic, mode="LES"):
    """ construct Config object 
    ## inputs:
    # case = ARMCU, RICO, AYOTTE, ...
    # fic  = PREIDEA, EXSEG01, ...
    # mode = None (default MNH), LES, CRM, SCM
    #
    ## fields of Config object:
    # .case = case
    # .name = fic
    # .mode = mode [None|LES|CRM|SCM]
    # .is_exseg = 0 if preidea, 1 if exseg
    # .config = preidea or exseg dicts of dicts of the form:
    # {"NAM_name":{"key1":"val1", "key2":"val2}, "NAM_other":{"key":"val"}}
    """
    self.case = case # ARMCU...
    self.name = fic  # PREIDEA OR EXSEG ...
    self.mode = mode # None|LES|CRM|SCM

    if "PRE" in fic:
      self.is_exseg=0
      self.config = copy.deepcopy(default_preidea)
    elif "EXS" in fic:
      self.is_exseg=1
      self.config = copy.deepcopy(default_exseg)
    else: print("error: unknown file format %s"%fic); exit()

    if mode is None: return # default Meso-NH config
    elif mode == "LES" :    # adapted to LES
      config_ = exseg_LES if self.is_exseg else preidea_LES
    elif mode == "CRM" : 
      config_ = exseg_CRM if self.is_exseg else preidea_CRM
    elif mode == "SCM" :
      config_ = exseg_SCM if self.is_exseg else preidea_SCM
    else : print("error: unknown constructor mode %s"%mode); exit()

    self.config = replace_config(self.config, config_, "all", "all")

  def __str__(self):
    """ print namelist to screen """
    str="############\n %s \n############\n"%(self.name)
    for k in self.config:
        str+="===\nNAMELIST : %s\n>> KEYS : %s\n"%(k, self.config[k])
    return str

  def write(self, outfile):
    write_namelist(self.config, outfile)

  def duplicate_config(self, name=None):
    new_conf = Config(self.case, self.name if name is None else name, self.mode)
    new_conf.config = copy_config(new_conf.config, self.config, "all", "all")
    return new_conf

  def copy_config_from(self, copy_from, namelist="all", key="all"):
    self.config = copy_config(self.config, copy_from.config, namelist, key)

  def modify(self, namelist, key, value):
    if namelist not in self.config:
        print("cannot modify namelist %s"%namelist); exit()
    if key not in self.config[namelist]: 
        print("cannot modify key %s in namelist %s"%(key, namelist)); exit()
    self.config[namelist][key] = value

  def set_warm_microphysics(self):
    self.modify("NAM_PARAM_LIMA", "NMOM_I", "0")
    self.modify("NAM_PARAM_LIMA", "NMOM_S", "0")
    self.modify("NAM_PARAM_LIMA", "NMOM_G", "0")

  def activate_radiation(self):
    self.modify("NAM_PARAMn", "CRAD", "'ECRA'")
