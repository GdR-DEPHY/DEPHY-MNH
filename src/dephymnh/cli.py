"""
dephymnh command line client
"""

import argparse
import os

def namelist_create():
  """
  Create MNH namelists from DMNH file
  """
  from dephymnh.namelists import create
  parser = argparse.ArgumentParser("Namelist Create")
  create(parser)

def output_convert_000():
  """
  Converts 1D outputs from MNH571 to dephy format
  """
  from dephymnh.outputs import init_convert_000
  from dephymnh.outputs import convert_000
  parser = argparse.ArgumentParser("Output Convert")
  args = init_convert_000(parser)
  convert_000(*args)

def output_coarse_grain():
  """
  Performs coarse graining on 3D MNH file
  """
  from dephymnh.outputs import init_coarse_grain
  from dephymnh.outputs import coarse_grain
  parser=argparse.ArgumentParser(description="Output Coarse Grain")
  args = init_coarse_grain(parser)
  coarse_grain(*args)
