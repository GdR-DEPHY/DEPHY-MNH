"""
dephy mnh outputs
"""

import os

def init_convert_000(parser):
  parser.add_argument("-i", help="input", metavar="input_file", required=True)
  parser.add_argument("-o", help="output file", metavar="output_file")
  parser.add_argument("-f", help="force overwrite", action="store_true")
  args=parser.parse_args()

  # copy args fields to local variables
  input_file = args.i             # input file (mandatory)
  overwrite = args.f              # force overwrite (default = False)
  output_file = input_file.split(".nc")[0]+"_dephy.nc" # default output file name
  if args.o: output_file = args.o # overwrite default output file name

  # check arguments validity
  if (not os.path.isfile(input_file)):
    print("error: %s does not exist."%input_file)
    exit(1)
  if os.path.isfile(output_file) and not overwrite:
    print("error: %s exists.\nForce overwriting with option -f"%output_file)
    exit(1)

  return [input_file, output_file]

def convert_000(input_file, output_file):
  """
  convert 000 outputs from MNH571 to dephy format
  """
  import netCDF4 as nc
  import numpy as np
  import sys
  from datetime import datetime
  from dephymnh.outputs.dephy_variables import Dict_attr
  from dephymnh.outputs.output_variables import variables as dephy_outvars
  from dephymnh.outputs.mesonh2dephy_variables import Dict_new_varnames_all
  
  list_bil = ["UU", "VV", "TH", "RV", "RC"]     # in MesoNH file
  list_cs  = ["cart", "neb", "core", "cs1"]     # in MesoNH file
  list_cs_name = ["cld", "core", "sam"]         # in var short name
  list_cs_longname = ["cloud sampling ", "core sampling ", "tracer sampling "]
  list_variables_convert = ["E0", "Q0", "INST_PREC"]
  
  dataIn = nc.Dataset(input_file,'r')
  dataOut = nc.Dataset(output_file,'w')
  
  #Variable pour recuperer les dimensions (= doit toujours etre present)
  varDate = dataIn.variables['time_les'][:]
  try: varDateB = dataIn.variables['time_budget'][:]
  except: varDateB=varDate
  varLevel_h = dataIn.variables['level_w'][1:-1]
  varLevel_f = dataIn.variables['level'][1:-1]
  
  #### Gestion du Temps
  varTime = varDate.flatten() 
  varTimeB = varDateB.flatten()
  
  #Creation des Dimensions
  dataOut.createDimension('time', None) # unlimited
  dataOut.createDimension('time_budget', None) # unlimited
  dataOut.createDimension('levf',size=varLevel_f.size)
  dataOut.createDimension('levh',size=varLevel_h.size)
  
  #Variables correspondantes aux dimensions
  levf = dataOut.createVariable('levf', float, ('levf'))
  levf.axis="Z"
  levf[:] = varLevel_f[:]
  
  levh = dataOut.createVariable('levh', float, ('levh'))
  levh.axis="Z"
  levh[:] = varLevel_h[:]
  
  time = dataOut.createVariable('time', float, ('time'))
  time.axis="T"
  time[:] = varTime[:]
  
  time_budget = dataOut.createVariable('time_budget', float, ('time_budget'))
  time_budget.axis="T"
  time_budget[:] = varTimeB[:]

  #Attributs des variables dimensions
  def attin(var, att):
    return dataIn.variables[var].getncattr(att)
  def dephyatt(var, att):
    return dephy_outvars[var][att]

  levf.setncattr('standard_name', dephyatt("levf", "standard_name"))
  levf.setncattr('units',         dephyatt("levf", "units"))
  levf.setncattr('long_name',     attin("level", "long_name"))

  levh.setncattr('standard_name', dephyatt("levh", "standard_name"))
  levh.setncattr('units',         dephyatt("levh", "units"))
  levh.setncattr('long_name',     attin("level_w", "long_name"))
  
  time.setncattr('standard_name', dephyatt("time", "standard_name"))
  time.setncattr('units',         dephyatt("time", "units"))
  time.setncattr('calendar',      attin("time_les", "calendar"))
  time.setncattr('long_name',     attin("time_les", "long_name"))
  
  time_budget.setncattr('standard_name', dephyatt("time_budget", "standard_name"))
  time_budget.setncattr('units',         dephyatt("time_budget", "units"))
  time_budget.setncattr('calendar',      attin("time_les", "calendar"))
  if "time_budget" in dataIn.variables:
    time_budget.setncattr('long_name',     attin("time_budget", "long_name"))
  
  def lnm(n):
    if n in dephy_outvars:
      return dephy_outvars[n]["standard_name"]
    elif n in Dict_attr:
      return Dict_attr[n]
    else: return None

  def get_longname(new_var_name):
    longname = lnm(new_var_name)
    if longname is None:
      for cs,nam in zip(list_cs_name, list_cs_longname):
        if cs in new_var_name:
          cart_name = new_var_name.split("_"+cs)[0]
          longname = nam+lnm(cart_name)
    try: longname
    except NameError : print(new_var_name); raise
    return longname
  
  def create_var(new_var_name, old_var, vardims, data=None, units=None):
    vartype = old_var.dtype
    varunits = old_var.units if units is None else units
    longname = get_longname(new_var_name)
    new_var = dataOut.createVariable(new_var_name, vartype, vardims, fill_value=999)
    if data is None : new_var[:] = old_var[:]
    else: new_var[:] = data[:]
    new_var.long_name = longname
    new_var.units = varunits #setncattr('units', old_var.getncattr('units'))
    return new_var
  
  def convert(var, old_var, new_var):
    if(var == 'Q0')or(var == 'hfss'):
      #Flux de chaleur sensible surface m K s-1 ==> W/m2
      new_var[:] = new_var[:] * Dict_new_var['rho'][:,0] * 1004.9
      new_var.units = 'W m-2'
    
    if(var == 'E0')or(var == 'hfls'):
      #Flux de chaleur latente surface kg kg-1 ms-1 ==> W/m2
      new_var[:] = new_var[:] * Dict_new_var['rho'][:,0] * 2500000.0
      new_var.units = 'W m-2'

    if(var == 'INST_PREC')or(var == 'prl'):
      # Flux de precip surface mm day-1 -> kg m-2 s-1
      rho_w = 1000.
      mm_to_m = 1e-3
      perday_to_persec = 1./86400
      new_var[:] = new_var[:]*mm_to_m*rho_w*perday_to_persec
  
    return new_var
  
  vardims4D = ('time', 'levf', 'S_N_direction','W_E_direction')
  vardims2D = ('time', 'levf')
  vardims1D = ('time',)
  buddims4D = ('time_budget', 'levf', 'S_N_direction','W_E_direction')
  buddims2D = ('time_budget', 'levf')
  buddims1D = ('time_budget',)
  
  def extract_group(groupe, bilan=False):
    try:
      for var in dataIn[groupe].variables:
        if var in Dict_new_varnames.keys():
          old_var = dataIn[groupe].variables[var]

          # new var name 
          new_varname = Dict_new_varnames[var]

          # new var dimensions 
          new_vardims = vardims2D if len(old_var.shape)==2 else vardims1D
          if bilan: new_vardims=buddims2D

          # new var units
          if new_varname in dephy_outvars:
            new_varunits = dephy_outvars[new_varname]["units"]
          else: new_varunits = None

          # create new var
          new_var = create_var(new_varname, old_var, new_vardims, units=new_varunits)

          # convert new var values if necessary
          if var in list_variables_convert:
            new_var = convert(var, old_var, new_var)

          # add new var to dict
          Dict_new_var[Dict_new_varnames[var]] = new_var

    except (KeyError,IndexError):
      return
  
  Dict_new_var = {}
  
  # Copying existing variables 
  
  for cs in list_cs:
    Dict_new_varnames = Dict_new_varnames_all[cs]
    for sgroup in ["Mean", "Resolved", "Subgrid", "Surface", "Radiation", "Miscellaneous"]:
      groupe="/LES_budgets/%s/Cartesian/Not_time_averaged/Not_normalized/%s"%(sgroup,cs)
      extract_group(groupe)
  
  for bil in list_bil:
    Dict_new_varnames = Dict_new_varnames_all[bil]
    groupe="/Budgets/%s/"%bil
    extract_group(groupe, bilan=True)
  
  # Processing variables
  list_var_tot  = ['wrt', 'wthl', 'wrv', 'wth']
  list_var_tot += ['uu', 'vv', 'ww', 'tke', 'uw', 'vw']
  list_var_tot += ['thl2', 'rt2', 'th2', 'rv2']
  for var_tot in list_var_tot:
    try:
      old_var = Dict_new_var[var_tot+"_res"]
      dat_tot = Dict_new_var[var_tot+"_res"][:,:]+Dict_new_var[var_tot+"_sbg"][:,:]
      create_var(var_tot, old_var, vardims2D, data=dat_tot)
    except (KeyError, IndexError):
      continue

  try:
    dat = Dict_new_var['rv'][:,:] / (1+Dict_new_var['rt'][:,:])
    new_var = create_var("qv", Dict_new_var['rv'], vardims2D, data=dat)
  except KeyError:
    print("warning: Missing key variable for computation of specific humidity")
      
  try:
    dat = Dict_new_var['theta'][:,:] * (Dict_new_var['pfull'][:,:] / 100000.0)**0.286
    new_var = create_var("temp", Dict_new_var['theta'], vardims2D, data=dat)
  except KeyError:
    print("warning: Missing key variable for computation of temperature")

  # each list includes terms from both LIMA and ICE3, the program will
  # automatically sum over variables that are present in the file
  list_warm = ['accr', 'auto', 'ceds', 'r2c1', 'sedi', 'acc', 'reva', 'adju']
  list_cold = ['berfi', 'depg', 'depi', 'deps', 'dryg', 'honc', 'honr', 'hmlt', 'imlt', 'gmlt', 'cfrz', 'rim', 'wetg', 'weth', 'depo']
  for vv in ['theta', 'rv', 'rl']:
    try:
      for tendname, list_tends in zip(["warm", "cold"], [list_warm, list_cold]): 
        list_vars = ['tn%s_%s'%(vv, tend) for tend in list_tends]
        dat = np.sum(np.array([Dict_new_var[var][:,:] for var in list_vars if var in Dict_new_var]), axis=0)
        new_var = create_var("tn%s_micro_%s"%(vv, tendname), Dict_new_var['tn%s_adv'%vv], buddims2D, data=dat)
    except:
      print("warning: Missing key variable for computation of microphysics budgets for var %s"%vv)
      
  dataOut.case = output_file
  dataOut.version = "Created on " + str(datetime.now())
  dataOut.format_version = "0"
  dataOut.title = "Output from MesoNH v5-7-1"
  dataOut.script = sys.argv[0]
  
  dataOut.close()
  dataIn.close()


##########################
# COARSE GRAIN 3D FIELDS #
##########################

def init_coarse_grain(parser):
  parser.add_argument("-i", help="input netCDF file", metavar="input_file", required=True)
  parser.add_argument("-o", help="output netCDF file", metavar="output_file", required=True)
  parser.add_argument("-n", help="# of averaged cells", metavar="n_cells", required=True)
  parser.add_argument("-b", help="boundary treatment", metavar="trim|pad", default="trim")
  parser.add_argument("-f", help="force overwrite", action="store_true")
  args=parser.parse_args()
  
  # copy args fields to local variables
  input_file  = args.i        # input file,  mandatory
  output_file = args.o        # output file, mandatory
  n_cells     = int(args.n)   # coarse graining factor, mandatory
  boundary    = args.b        # boundary treatment: trim|pad
  overwrite   = args.f        # force overwrite (default = False)
  
  # check arguments validity
  if (os.path.isfile(output_file) and not overwrite):
    print("error: %s exists.\nForce overwriting with option -f"%output_file)
    exit(1)
  if (not os.path.isfile(input_file)):
    print("error: %s does not exist."%input_file)
    exit(1)
  if (n_cells <= 0):
    print("error: invalid n_cells value %i (must be > 0)."%n_cells)
    exit(1)

  return [input_file, output_file, n_cells, boundary]

def coarse_grain(input_file, output_file, n_cells, boundary):
  """
  Coarsen MNH 3D output to another resolution
  """
  import xarray as xr

  # read input file
  input_ds = xr.open_dataset(input_file, engine="netcdf4")
  
  # coarsen input along horizontal dimensions
  if "ni" in input_ds.dims:
    output_ds = input_ds.coarsen(ni=n_cells, nj=n_cells, 
                                 ni_u=n_cells, nj_u=n_cells, 
                                 ni_v=n_cells, nj_v=n_cells, 
                                 boundary=boundary).mean()

  elif "W_E_direction" in input_ds.dims:
    output_ds = input_ds.coarsen(W_E_direction=n_cells, S_N_direction=n_cells, 
                                 boundary=boundary).mean()
  
  # write output to file
  output_ds.to_netcdf(output_file)
  
  # close files
  input_ds.close()
  output_ds.close()
