## Default grids for LES|CRM|SCM modes
# Nhoriz == minimum pour avoir des circulations résolues
# deltav :: pour l'instant on garde la même résolution verticale qu'en LES

LES_Nhoriz = 1024   # npoints
LES_Nverti = 160    # npoints
LES_deltah = 25.    # horizontal resolution in m
LES_deltav = 25.    # vertical   resolution in m

CRM_Nhoriz = 100    # npoints
CRM_Nverti = 160    # npoints
CRM_deltah = 2500.  # horizontal resolution in m
CRM_deltav = 25     # vertical   resolution in m

SCM_Nhoriz = 1      # npoints
SCM_Nverti = 160    # npoints
SCM_deltah = 50000. # horizontal resolution in m
SCM_deltav = 25.    # vertical   resolution in m

# specific adaptations of default preidea and exseg to LES mode
preidea_LES = {
  "NAM_DIMn_PRE" : { "NIMAX" : "%i"%LES_Nhoriz, 
                     "NJMAX" : "%i"%LES_Nhoriz  },
  "NAM_VER_GRID" : { "NKMAX" : "%i"%LES_Nverti  }, 
  "NAM_GRIDH_PRE": { "XDELTAX"  : "%g"%LES_deltah, 
                     "XDELTAY"  : "%g"%LES_deltah   },
  "NAM_CONF_PRE" : { "LPERTURB" : ".TRUE.",     },
  "NAM_CONFZ"    : { "MPI_BUFFER_SIZE" : "400", },
}
exseg_LES = {
  "NAM_DYNn"   : { "XTSTEP"     : "1.",
                   "XT4DIFU"    : "300.",
                   "CPRESOPT"   : "'ZRESI'",     }, # parallel CRESI
  "NAM_TURBn"  : { "XIMPL"      : "0.",
                   "CTURBLEN"   : "'DEAR'",
                   "CTURBDIM"   : "'3DIM'",
                   "LSIGMAS"    : ".FALSE.",     },
  "NAM_LES" :   { "LLES_NEB_MASK"  : ".TRUE.", 
                  "LLES_CORE_MASK" : ".TRUE.", 
                  "LLES_CS_MASK"   : ".TRUE.",
                  "XLES_TEMP_SAMPLING"   : "300.",
                  "XLES_TEMP_MEAN_START" : "0.",
                  "XLES_TEMP_MEAN_STEP"  : "3600."},
  "NAM_CONDSAMP": { "LCONDSAMP"    : ".TRUE."     },
  "NAM_CONFZ"   : { "MPI_BUFFER_SIZE" :"400"      },
  "NAM_BUDGET"  : { "NBUIH"        : "%i"%LES_Nhoriz,
                    "NBUJH"        : "%i"%LES_Nhoriz, 
                    "NBUKH"        : "%i"%LES_Nverti,},
# no need for now: the SVT variables will be output by default
#  "NAM_OUTPUT"  : { "COUT_VAR(1,13)" : "'SVT001'",
#                    "COUT_VAR(1,14)" : "'SVT002'",
#                    "COUT_VAR(1,15)" : "'SVT003'", },
}

# specific adaptations of default preidea and exseg to CRM mode
preidea_CRM = {
  "NAM_DIMn_PRE" : { "NIMAX" : "%i"%CRM_Nhoriz, 
                     "NJMAX" : "%i"%CRM_Nhoriz      },
  "NAM_VER_GRID" : { "NKMAX" : "%i"%CRM_Nverti,     },
  "NAM_GRIDH_PRE": { "XDELTAX"  : "%g"%CRM_deltah, 
                     "XDELTAY"  : "%g"%CRM_deltah   },
  "NAM_CONF_PRE" : { "LPERTURB" : ".TRUE.",         },
}
exseg_CRM = {
  "NAM_PARAMn" : { "CSCONV"     : "'EDKF'",      }, # activate shallow conv param
  "NAM_DYNn"   : { "XTSTEP"     : "10.",
                   "XT4DIFU"    : "3000.",          # convention 300xtime_step
                   "CPRESOPT"   : "'ZRESI'",     }, # parallel CRESI
  "NAM_TURBn"  : { "LSUBG_COND" : ".TRUE.",      }, # activate subrid condensation
  "NAM_LES"    : { "XLES_TEMP_SAMPLING"   : "300.",
                   "XLES_TEMP_MEAN_START" : "0.",
                   "XLES_TEMP_MEAN_STEP"  : "3600."},
  "NAM_CONDSAMP": { "LCONDSAMP"    : ".TRUE."    },
  "NAM_BUDGET"  : { "NBUIH"        : "%i"%CRM_Nhoriz,
                    "NBUJH"        : "%i"%CRM_Nhoriz, 
                    "NBUKH"        : "%i"%CRM_Nverti,},
}

# specific adaptations of default exseg to SCM mode
preidea_SCM = {
  "NAM_DIMn_PRE" : { "NIMAX" : "%i"%SCM_Nhoriz, 
                     "NJMAX" : "%i"%SCM_Nhoriz    },
  "NAM_VER_GRID" : { "NKMAX" : "%i"%SCM_Nverti,   },
  "NAM_GRIDH_PRE": { "XDELTAX"  : "%g"%SCM_deltah, 
                     "XDELTAY"  : "%g"%SCM_deltah },
}
exseg_SCM = {
  "NAM_PARAMn" : { "CSCONV"     : "'EDKF'",
                   "CDCONV"     : "'KAFR'"      },
  "NAM_DYNn"   : { "XTSTEP"     : "30.",
                   "XT4DIFU"    : "9000."        },# convention 300xtime_step
  "NAM_TURBn"  : { "LSUBG_COND" : ".TRUE.",     },
  "NAM_PARAM_MFSHALLn" : {"CMF_UPDRAFT"    :"EDKF",
                          "CMF_CLOUD"      :"DIRE", 
                          "LMIXUV"         :".TRUE.", 
                          "LMF_FLX"        : ".TRUE."},
  "NAM_PARAM_KAFRn" : {"XDTCONV"    :"10.",
                          "LDAIGCONV"      :"TRUE"}, 
  "NAM_LES" :   { "XLES_TEMP_SAMPLING"   : "300.",
                  "XLES_TEMP_MEAN_START" : "0.",
                  "XLES_TEMP_MEAN_STEP"  : "3600."},
  "NAM_CONDSAMP": { "LCONDSAMP"    : ".TRUE."    },
  "NAM_BUDGET"  : { "NBUIH"        : "%i"%SCM_Nhoriz,
                    "NBUJH"        : "%i"%SCM_Nhoriz, 
                    "NBUKH"        : "%i"%SCM_Nverti},
}

######################################
## DEFAULT CONFIGS IN MESO-NH 5.6.2 ##
# modified to match default config  ##
# in DEPHY project                  ##
######################################

MODD_CONFIO__ = { # I/O file type
    "LCDF4"           : ".TRUE.",
    "LLFIOUT"         : ".FALSE.",
    "LLFIREAD"        : ".FALSE.",
}

MODD_LUNITn__ = { # Init file names
    "CINIFILE"        : "'init'",
    "CINIFILEPGD"     : "'init_pgd'", 
}

MODD_CONFZ__  = { # parallelism
  "NZ_VERB"         : "0",
  "NZ_PROC"         : "0",
  "NB_PROCIO_R"     : "1",
  "NB_PROCIO_W"     : "1",
  "MPI_BUFFER_SIZE" : "40",
  "LMNH_MPI_BSEND"  : ".TRUE.",
  "LMNH_MPI_ALLTOALLV_REMAP" : ".FALSE.",
  "NZ_SPLITTING"    : "10",
}

MODD_BACKUP__ = { # backup files for restarts
  "XBAK_TIME_FREQ(1)" : "3600.0",
}

SURF_IDEAL_FLUX__ = { # surfex default namelist
  "NFORCF"            : "2",
  "NFORCT"            : "2",
  "XTIMEF"            : "0",
  "XTIMET"            : "0",
  "XSFTH"             : "0.",
  "CSFTQ"             : "'W/m2'", #'kg/m2/s'",
  "XSFTQ"             : "0.",
  "CUSTARTYPE"        : "Z0",
  "XUSTAR"            : "0.",
  "XZ0"               : "0.01",
  "XTSRAD"            : "273.15K",
}

MODD_CONF__   = { # general config
  "CCONF"             :"'START'",
  "L2D"               : ".FALSE.",
  "L1D"               : ".FALSE.",
  "LFLAT"             : ".TRUE.", #".FALSE.",
  "NMODEL"            : "1",
  "CEQNSYS"           : "'DUR'",
  "NVERB"             : "6",      # "5", 
  "CEXP"              : "'EXP01'",
  "CSEG"              : "'SEG01'",
  "LFORCING"          : ".TRUE",  #".FALSE.",
  "L2D_ADV_FRC"       : ".FALSE.",
  "L2D_REL_FRC"       : ".FALSE. ",
  "XRELAX_HEIGHT_BOT" : "0.",
  "XRELAX_HEIGHT_TOP" : "30000.",
  "XRELAX_TIME"       : "864000.",
  "LPACK"             : ".TRUE.",
  "NHALO"             : "1",
  "CSPLIT"            :"'BSPLITTING'",
  "NZ_PROC"           : "0",
  "NZ_SPLITTING"      : "10",
  "LLG"               : ".FALSE.",
  "LINIT_LG"          : ".FALSE.",
  "CINIT_LG"          : "'FMOUT'",
  "LNOMIXLG"          : ".FALSE.",
  "LCHECK"            : ".FALSE.",
}

MODD_CONFn__  = { # config for model n (nesting)
  "LUSERV"            : ".TRUE.",
  "LUSERC"            : ".FALSE.",
  "LUSERR"            : ".FALSE.",
  "LUSERI"            : ".FALSE.",
  "LUSERS"            : ".FALSE.",
  "LUSERG"            : ".FALSE.",
  "LUSERH"            : ".FALSE.",
  "LOCEAN"            : ".FALSE.",
  "LUSECI"            : ".FALSE.",
}

MODD_FRC__    = {   # forcings
  "LGEOST_UV_FRC"     : ".FALSE.",
  "LGEOST_TH_FRC"     : ".FALSE.",
  "LTEND_THRV_FRC"    : ".FALSE.",
  "LTEND_UV_FRC"      : ".FALSE.", 
  "LVERT_MOTION_FRC"  : ".FALSE.",
  "LRELAX_THRV_FRC"   : ".FALSE.",
  "LRELAX_UV_FRC"     : ".FALSE.",
  "LRELAX_UVMEAN_FRC" : ".FALSE.",
  "XRELAX_TIME_FRC"   : "10800.",
  "XRELAX_HEIGHT_FRC" : "0.",
  "CRELAX_HEIGHT_TYPE": "'FIXE'",
  "LTRANS"            : ".FALSE.",
  "XUTRANS"           : "0.0",
  "XVTRANS"           : "0.0",
  "LPGROUND_FRC"      : ".FALSE.",
  "LDEEPOC"           : ".FALSE.",
  "XCENTX_OC"         : "16000.",
  "XCENTY_OC"         : "16000.",
  "XRADX_OC"          :  "8000.",  
  "XRADY_OC"          :  "8000.",
}

MODD_LBCn__   = {  # boundary conditions for model n
  "CLBCX(1)"          :"'CYCL'",
  "CLBCX(2)"          :"'CYCL'",
  "CLBCY(1)"          :"'CYCL'",
  "CLBCY(2)"          :"'CYCL'",
  "NLBLX(:)"          : "1",
  "NLBLY(:)"          : "1",
  "XCPHASE"           : "20.",
  "XCPHASE_PBL"       : "0.",
  "XPOND"             : "1.0",
}

MODD_DYN__    = { # general dynamics
  "XSEGLEN"           : "43200.",
  "XASSELIN"          : "0.2",
  "XASSELIN_SV"       : "0.02",
  "LCORIO"            : ".TRUE.",
  "LNUMDIFU"          : ".TRUE.",
  "LNUMDIFTH"         : ".FALSE.",
  "LNUMDIFSV"         : ".FALSE.",
  "XALZBOT"           : "4000.",
  "XALKTOP"           : "0.01",
  "XALKGRD"           : "0.01",
  "XALZBAS"           : "0.01",
}

MODD_DYNn__   = { # dynamics for model n (nesting)
  "XTSTEP"            : "60.",
  "CPRESOPT"          : "'CRESI'",
  "NITR"              : "4",
  "LITRADJ"           : ".TRUE.",
  "LRES"              : ".FALSE.",
  "XRES"              : "1.E-07",
  "XRELAX"            : "1.",
  "LVE_RELAX"         : ".FALSE.",
  "LVE_RELAX_GRD"     : ".FALSE.",
  "XT4DIFU"           : "1800.",
  "XT4DIFTH"          : "1800.",
  "XT4DIFSV"          : "1800.",
}

MODD_ADVn__   = { # advection for model n
  "CUVW_ADV_SCHEME"   : "'CEN4TH'",
  "CMET_ADV_SCHEME"   : "'PPM_01'",
  "CSV_ADV_SCHEME"    : "'PPM_01'",
  "CTEMP_SCHEME"      : "'RKC4'",
  "NWENO_ORDER"       : "3",
  "NSPLIT"            : "1",
  "LSPLIT_CFL"        : ".TRUE.",
  "LSPLIT_WENO"       : ".TRUE.",
  "XSPLIT_CFL"        : "0.8",
  "LCFL_WRIT"         : ".FALSE.",
}

MODD_PARAMn__ = { # activate params for model n
  "CTURB"             : "'TKEL'", # "'NONE'"
  "CRAD"              : "'NONE'",
  "CCLOUD"            : "'LIMA'", # "'NONE'"
  "CDCONV"            : "'NONE'",
  "CSCONV"            : "'NONE'",
  "CELEC"             : "'NONE'",
  "CACTCCN"           : "'NONE'",
}

MODD_TURBn__  = { # config turbulence
  "XIMPL"             : "1.",
  "XKEMIN"            : "1E-10",
  "XCEDIS"            : "0.84",
  "XCADAP"            : "0.5",
  "CTURBLEN"          : "'BL89'",
  "CTURBDIM"          : "'1DIM'",
  "LTURB_FLX"         : ".TRUE",    #".FALSE.",
  "LTURB_DIAG"        : ".TRUE.",   #".FALSE.",
  "LSUBG_COND"        : ".FALSE.",
  "CSUBG_AUCV"        : "'NONE'", 
  "CSUBG_AUCV_RI"     : "'NONE'",
  "LSIGMAS"           : ".TRUE.",
  "LSIG_CONV"         : ".FALSE.",
  "LRMC01"            : ".TRUE.",   #".FALSE.",
  "CTOM"              : "'NONE'",
  "VSIGQSAT"          :  "0.02",
  "CCONDENS"          : "'CB02'",
  "CLAMBDA3"          : "'CB'",
  "CSUBG_MF_PDF"      : "'TRIANGLE'",
  "LLEONARD"          : ".FALSE.",
  "XCOEFHGRADTHL"     : "1.0",
  "XCOEFHGRADRM"      : "1.0",
  "XALTHGRAD"         : "2000.0",
  "XCLDTHOLD"         : "-1.0",
}

MODD_RADn__   = { # config radiation
  "XDTRAD"            : "XTSTEP",
  "XDTRAD_CLONLY"     : "XTSTEP",
  "LCLEAR_SKY"        :".FALSE.",
  "NRAD_COLNBR"       : "1000",
  "NRAD_DIAG"         : "0",
  "CLW"               :"'RRTM'",
  "CAER"              :"'SURF'",
  "CAOP"              :"'CLIM'",
  "CEFRADL"           :"'MART'",
  "CEFRADI"           :"'LIOU'",
  "COPWSW"            : "'FOUQ'",
  "COPISW"            : "'EBCU'",
  "COPWLW"            : "'SMSH'",
  "COPILW"            : "'EBCU'",
  "XFUDG"             : "1.",
  "LAERO_FT"          :".FALSE.",
  "LFIX_DAT"          :".FALSE.",
}

MODD_PARAM_MFSHALLn__ = { # config shallow mass flux scheme
  "CMF_UPDRAFT"       : "EDKF",
  "CMF_CLOUD"         : "DIRE", 
  "LMIXUV"            : ".TRUE.", 
  "LMF_FLX"           : ".TRUE.",   #".FALSE."
}

MODD_PARAM_ECRAD__ = { # config radiation scheme
  "NSWSOLVER"         : "0",
  "NLWSOLVER"         : "0",
  "LSPEC_ALB"         : ".FALSE.",
  "LSPEC_EMISS"       : ".FALSE.",
  "SURF_TYPE"         : "'SNOW'",
  "NREG"              : "3",
  "NLWSCATTERING"     : "2",
  "NAERMACC"          : "0",
  "NOVLP"             : "1",
  "NLIQOPT"           : "3",
  "NICEOPT"           : "3",
  "NRADLP"            : "1",
  "NRADIP"            : "1",
  "XCLOUD_FRAC_STD"   : "1.0",
}

MODD_PARAM_LIMA__ = {  # config microphysics scheme
   "LPTSPLIT"         : ".TRUE.",
   "L_LFEEDBACKT"     : ".TRUE.",
   "L_NMAXITER"       : "5",
   "L_XMRSTEP"        : "0.005",
   "L_XTSTEP_TS"      : "20.",
   "YNUC"             : "1.0",
   "YALPHAC"          : "3.0",
   "YNUR"             : "2.0",
   "YALPHAR"          : "1.0",
   "LACTI"            : ".FALSE.",#".TRUE.",
   "OSEDC"            : ".TRUE.",
   "OACTIT"           : ".FALSE.",
   "LADJ"             : ".TRUE.",
   "LSPRO"            : ".FALSE.",
   "LKHKO"            : ".FALSE.",
   "ODEPOC"           : ".TRUE.",
   "LBOUND"           : ".FALSE.",
   "OACTTKE"          : ".TRUE.",
   "LKESSLERAC"       : ".TRUE.", #".FALSE.",
   "NMOM_C"           : "1",      #"2",
   "NMOM_R"           : "1",      #"2",
   "OVDEPOC"          : "0.02",
   "CINI_CCN"         : "'AER'",
   "CTYPE_CCN(:)"     : "'M'",
   "YAERDIFF"         : "0.0",
   "YAERHEIGHT"       : "2000.",
   "YFSOLUB_CCN"      : "1.0",
   "YACTEMP_CCN"      : "280.",
   "NMOD_CCN"         : "0",      #"1",
   "LSCAV"            : ".FALSE.",
   "LAERO_MASS"       : ".FALSE.",
   "LCCN_HOM"         : ".TRUE.",
   "CCCN_MODES"       : "'COPT'",
   "XCCN_CONC(:)"     : "300.",
   "LHHONI"           : ".FALSE.",
   "LNUCL"            : ".TRUE.",
   "LSEDI"            : ".TRUE.",
   "YSNOW_T"          : ".FALSE.",
   "LMURAKAMI"        : ".TRUE.",
   "CPRISTINE_ICE_LIMA": "'PLAT'",
   "CHEVRIMED_ICE_LIMA": "'GRAU'",
   "XFACTNUC_DEP"     : "1.0",  
   "XFACTNUC_CON"     : "1.0",
   "NMOM_I"           : "1",      #"2",
   "NMOM_S"           : "1",
   "NMOM_G"           : "1",
   "NMOM_H"           : "0",
   "NMOD_IFN"         : "0",      #"1",
   "NIND_SPECIE"      : "1",
   "LMEYERS"          : ".FALSE.",
   "LIFN_HOM"         : ".TRUE.",
   "CIFN_SPECIES"     : "'PHILLIPS'",
   "CINT_MIXING"      : "'DM2'",
   "XIFN_CONC(:)"     : "100.",
   "NMOD_IMM"         : "0",
   "NPHILLIPS"        : "8",
   "LCIBU"            : ".FALSE.",
   "XNDEBRIS_CIBU"    : "50.0",
   "LRDSF"            : ".FALSE.",
}

MODD_PARAM_C2R2__ = {               # default MNH
    "HPARAM_CCN"      : "'CPB'",    #"'XXX'",
    "HINI_CCN"        : "'CCN'",    #"'XXX'",
    "XCHEN"           : "0.173E+09",#"0.0",
    "XKHEN"           : "1.403",    #"0.0",
    "XMUHEN"          : "0.834",    #"0.0",
    "XBETAHEN"        : "25.499",   #"0.0",
    "LRAIN"           : ".FALSE.",  #".TRUE.",
    "LSEDC"           : ".FALSE.",  #".TRUE.",
}

MODD_BUDGET__  = { # compute budgets
  "CBUTYPE"           : "'CART'",   #"'NONE'",
  "LBU_KCP"           : ".FALSE.",  #".TRUE",
  "XBUWRI"            : "3600.",    #MODD_DYN__["XSEGLEN"],
  "XBULEN"            : "3600.",    #MODD_DYN__["XSEGLEN"],
  "NBUMOD"            : "1",
  "NBUKL"             : "1",
  "NBUKH"             : "0",        
  "NBUIL"             : "1",
  "NBUIH"             : "0",      
  "NBUJL"             : "1",
  "NBUJH"             : "0",          
  "LBU_ICP"           : ".TRUE.",
  "LBU_JCP"           : ".TRUE.",
  "NBUMASK"           : "1",
}

MODD_LES__     = { # compute horizontal statistics 
  "LLES_MEAN"           : ".TRUE.",  #".FALSE.",
  "LLES_RESOLVED"       : ".TRUE.",  #".FALSE.",
  "LLES_SUBGRID"        : ".TRUE.",  #".FALSE.",
  "LLES_UPDRAFT"        : ".FALSE.",
  "LLES_DOWNDRAFT"      : ".FALSE.",
  "LLES_SPECTRA"        : ".FALSE.",
  "CLES_NORM_TYPE"      : "'NONE'",
  "CBL_HEIGHT_DEF"      : "'KE'",
  "XLES_TEMP_MEAN_STEP" : "3600.",
  "LLES_CART_MASK"      : ".FALSE.",
  "LLES_NEB_MASK"       : ".FALSE.",
  "LLES_CORE_MASK"      : ".FALSE.",
  "LLES_MY_MASK"        : ".FALSE.",
  "LLES_CS_MASK"        : ".FALSE.",
}

MODD_CONDSAMP__ = { # conditional sampling
  "LCONDSAMP"         : ".FALSE.",
  "NCONDSAMP"         : "3",
  "XRADIO(:)"         : "900.",
  "XSCAL(:)"          : "1.",
  "XHEIGHT_BASE"      : "100.",
  "XDEPTH_BASE"       : "100.",
  "XHEIGHT_TOP"       : "100.",
  "XDEPTH_TOP"        : "100.",
  "NFINDTOP"          : "0",
  "XTHVP"             : "0.25",
  "LTPLUS"            : ".TRUE.",
}

MODD_OUTPUT__  = {
  #"XOUT_TIME"              : "%i"%8*999*-999.,
  #"NOUT_STEP"              : "%i"%8*999*-999 ,
  "XOUT_TIME_FREQ(1)"      : "3600", # "-999.",
  "XOUT_TIME_FREQ_FIRST(1)": "3600", # "0.",
  #"NOUT_STEP_FREQ"         : "-999",
  #"NOUT_STEP_FREQ_FIRST"   : "1",
  "LOUT_BEG"               : ".FALSE.",
  "LOUT_END"               : ".FALSE.",
  "LOUT_REDUCE_FLOAT_PRECISION(1)" : ".FALSE.", 
  "LOUT_COMPRESS(1)"       : ".FALSE.",
  "NOUT_COMPRESS_LEVEL(1)" : "4",
  "COUT_DIR"               : "''", 
  #"COUT_VAR"             : "''",
  "COUT_VAR(1,1)"          : "'UT'",
  "COUT_VAR(1,2)"          : "'VT'",
  "COUT_VAR(1,3)"          : "'WT'",
  "COUT_VAR(1,4)"          : "'THT'",
  "COUT_VAR(1,5)"          : "'RVT'", 
  "COUT_VAR(1,6)"          : "'RCT'",
  "COUT_VAR(1,7)"          : "'RRT'",
  "COUT_VAR(1,8)"          : "'TKET'",
  "COUT_VAR(1,9)"          : "'PABST'",
  "COUT_VAR(1,10)"         : "'RIT'",
  "COUT_VAR(1,11)"         : "'RST'",
  "COUT_VAR(1,12)"         : "'RGT'",
}

MODD_BU_RTH__ = { 
  "LBU_RTH"        : ".TRUE.",
  "CBULIST_RTH(1)" : "'FRC'",
  "CBULIST_RTH(2)" : "'ADV'",
  "CBULIST_RTH(3)" : "'VTURB+HTURB'",
  "CBULIST_RTH(4)" : "'ADJU'", 
  "CBULIST_RTH(5)" : "'REVA+HENU+HON+SFR+DEPS+DEPG+IMLT+BERFI+RIM+ACC+CFRZ+WETG+DRYG+GMLT+DEPI+CORR+NECON+NEADV+NETUR+NEGA'",
  "CBULIST_RTH(6)" : "'INIF'",
  "CBULIST_RTH(7)" : "'ENDF'",
  "CBULIST_RTH(8)" : "'AVEF'",
}

MODD_BU_RRV__ = {
  "LBU_RRV"         : ".TRUE.",
  "CBULIST_RRV(1)"  : "'FRC'",
  "CBULIST_RRV(2)"  : "'ADV'",
  "CBULIST_RRV(3)"  : "'VTURB+HTURB'",
  "CBULIST_RRV(4)"  : "'ADJU'",
  "CBULIST_RRV(5)"  : "'REVA+DEPS+DEPG+DEPI+HENU+CORR+NECON+NEADV+NETUR+NEGA'",
  "CBULIST_RRV(6)"  : "'INIF'",
  "CBULIST_RRV(7)"  : "'ENDF'",
  "CBULIST_RRV(8)"  : "'AVEF'",
}

MODD_BU_RU__ = {
  "LBU_RU"          : ".TRUE.",
  "CBULIST_RU(1)"   : "'FRC'",
  "CBULIST_RU(2)"   : "'COR'",
  "CBULIST_RU(3)"   : "'ADV'",
  "CBULIST_RU(4)"   : "'VTURB+HTURB'",
  "CBULIST_RU(5)"   : "'PRES'", 
  "CBULIST_RU(6)"   : "'INIF'",
  "CBULIST_RU(7)"   : "'ENDF'",
  "CBULIST_RU(8)"   : "'AVEF'",
}

MODD_BU_RV__ = {
  "LBU_RV"          : ".TRUE.",
  "CBULIST_RV(1)"   : "'FRC'",
  "CBULIST_RV(2)"   : "'COR'",
  "CBULIST_RV(3)"   : "'ADV'",
  "CBULIST_RV(4)"   : "'VTURB+HTURB'",
  "CBULIST_RV(5)"   : "'PRES'", 
  "CBULIST_RV(6)"   : "'INIF'",
  "CBULIST_RV(7)"   : "'ENDF'",
  "CBULIST_RV(8)"   : "'AVEF'",
}

MODD_DIMn_PRE__ = { # pre_idea dimensions
  "NIMAX" : "10", 
  "NJMAX" : "10" 
}

MODD_DYNn_PRE__ = { # pressure solver
  "CPRESOPT"          : "'CRESI'",
  "NITR"              : "4",
  "XRELAX"            : "1.",
  "LRES"              : ".FALSE.",
  "XRES"              : "1.E-07",
}

MODD_GRID_PRE__ = {
  "XLON0"   : "0.",
  "XLAT0"   : "60.",
  "XBETA"   : "0.",
  "XRPK"    : "1.",
  "XLONORI" : "350.",
  "XLATORI" : "37.",
}

MODD_VER_GRID__ = { # pre_idea vertical grid
  "LTHINSHELL" : ".FALSE.",
  "NKMAX"      : "10",
  "YZGRID_TYPE": "'FUNCTN'",
  "ZDZGRD"     : "%g"%LES_deltav, #"300.",
  "ZDZTOP"     : "%g"%LES_deltav, #"300.",
  "ZZMAX_STRGRD" : "0.",
  "ZSTRGRD"    : "0.",
  "ZSTRTOP"    : "0.",
  "LSLEVE"     : ".FALSE.",
  "XLEN1"      : "7500.",
  "XLEN2"      : "2500.",
}

MODD_CONF_PRE__ = { # pre_idea config
  "LCARTESIAN" : ".TRUE.", 
  "LPACK"      : ".TRUE.", 
  "CEQNSYS"    : "'DUR'",
  "NVERB"      : "10",       #"5", 
  "CIDEAL"     : "'RSOU'",   #"'CSTN'",
  "CZS"        : "'FLAT'", 
  "LBOUSS"     : ".FALSE.", 
  "LPERTURB"   : ".FALSE.",
  "LFORCING"   : ".TRUE.",   # ".FALSE."
  "LSHIFT"     : ".FALSE.",
  "L2D_ADV_FRC": ".FALSE.",
  "L2D_REL_FRC": ".FALSE.",
  "NHALO"      : "1",
  "JPHEXT"     : "1",
  "LOCEAN"     : ".FALSE.",
}

MODD_PERT_PRE__ = {
  "CPERT_KIND" : "'WH'",
  "XAMPLITH"   : "1.5",
  "XAMPLIRV"   : "0.0",
  "XAMPLIUV"   : "1.0834",
  "XAMPLIWH"   : "0.1",
  "NKWH"       : "2",
  "LSET_RHU"   : ".TRUE.",
  "XCENTERZ"   : "2000.",
  "XRADX"      : "10000.",
  "XRADY"      : "10000.",
  "XRADZ"      : "2000.",
  "LWH_LBXU"   : ".FALSE.",
  "LWH_LBYV"   : ".FALSE.",
}

MODD_GRIDH_PRE__ = { 
  "XDELTAX"    : "5000.",
  "XDELTAY"    : "5000.",
  "XHMAX"      : "300.",
  "NEXPX"      : "3",
  "NEXPY"      : "1",
  "XAX"        : "10000.",
  "XAY"        : "10000.",
  "NIZS"       : "5",
  "NJZS"       : "5",
}

MODD_GRn_PRE__ =  {
  "CSURF" : "'EXTE'", #"NONE",
}

MODD_PGD_SCHEMES__ =  {
  "CSEA"    : "'FLUX'", #"'SEAFLUX'"
  "CNATURE" : "'TSZ0'",
}

MODD_COVER__ =  {
  "XUNIF_COVER(1)"    : "0.", # (fraction of ocean)
  "XUNIF_COVER(6)"    : "1.", # (fraction of land)
}

MODD_SEABATHY__ =  {
  "XUNIF_SEABATHY"    : "5.",
}

MODD_PREP_SEAFLUX__ =  {
  "XSST_UNIF"         : "280.", # random default
}
MODD_DATA_SEAFLUX__ =  {
  "LSST_DATA"         : ".FALSE.", # 
  "NTIME_SST"         : "0",         #
  "NYEAR_SST(1)"      : "1980", # random default
  "NMONTH_SST(1)"     : "1",    # random default
  "NDAY_SST(1)"       : "1",    # random default
  "XTIME_SST(1)"      : "0.",   # random default
  "XUNIF_SST(1)"      : "280.", # random default
}
MODD_FRAC__ =  {
  "LECOCLIMAP"        : ".TRUE.",
  "XUNIF_NATURE"      : "1.",
}
MODD_DATA_TSZ0__ =  {
  "NTIME"             : "0",
  "XUNIF_DTS(1)"      : "-0.250", # SFX default 
}
MODD_DATA_ISBA__ =  {
  "NTIME"             : "0",
  "XUNIF_Z0(1,1)"     : "0", # depends on the veg type and time
}
MODD_ISBA__ =  {
  "XUNIF_CLAY"        : "1.",
  "XUNIF_SAND"        : "0.",
  "XUNIF_RUNOFFB"     : "0.5",
  "CISBA"             : "'2-L'",
  "CPHOTO"            : "'NON'",
  "NPATCH"            : "1",
  "NGROUND_LAYER"     : "2",
}
MODD_PREP_SURF_ATM__ =  {
  "NYEAR"             : "1980",
  "NMONTH"            : "1",
  "NDAY"              : "1",
  "XTIME"             : "0.",
}
MODD_PREP_ISBA__ =  {
  "XHUG_SURF"         : "0.",
  "XHUG_ROOT"         : "0.",
  "XHUG_DEEP"         : "0.",
  "XHUGI_SURF"        : "1.",
  "XHUGI_ROOT"        : "1.",
  "XHUGI_DEEP"        : "1.",
  "XTG_SURF"          : "280", # random default
  "XTG_ROOT"          : "280", # random default 
  "XTG_DEEP"          : "280", # random default 
  "LISBA_CANOPY"      : ".FALSE.",
  "NYEAR"             : "1980",
  "NMONTH"            : "1",
  "NDAY"              : "1",
  "XTIME"             : "0.",
}
MODD_DEEPSOIL__ =  {
  "LPHYSDOMC"         : ".FALSE.",
  "LDEEPSOIL"         : ".FALSE.",
}

default_preidea = {
  "NAM_CONFIO": MODD_CONFIO__,
  "NAM_CONFZ": MODD_CONFZ__,
  "NAM_LUNITn": MODD_LUNITn__,
  "NAM_DIMn_PRE": MODD_DIMn_PRE__,
  "NAM_DYNn_PRE": MODD_DYNn_PRE__,
  "NAM_GRID_PRE": MODD_GRID_PRE__,
  "NAM_VER_GRID": MODD_VER_GRID__,
  "NAM_CONF_PRE": MODD_CONF_PRE__,
  "NAM_PERT_PRE": MODD_PERT_PRE__,
  "NAM_GRIDH_PRE": MODD_GRIDH_PRE__,
  "NAM_GRn_PRE": MODD_GRn_PRE__,
  "NAM_CONFn": MODD_CONFn__,
  "NAM_LBCn" : MODD_LBCn__,
  "NAM_PGD_SCHEMES": MODD_PGD_SCHEMES__,
  "NAM_COVER": MODD_COVER__,
  "NAM_SEABATHY": MODD_SEABATHY__,
  "NAM_PREP_SEAFLUX": MODD_PREP_SEAFLUX__,
  "NAM_DATA_SEAFLUX": MODD_DATA_SEAFLUX__,
  "NAM_FRAC": MODD_FRAC__,
  "NAM_DATA_TSZ0": MODD_DATA_TSZ0__,
  "NAM_DATA_ISBA": MODD_DATA_ISBA__,
  "NAM_ISBA": MODD_ISBA__,
  "NAM_PREP_SURF_ATM": MODD_PREP_SURF_ATM__,
  "NAM_PREP_ISBA": MODD_PREP_ISBA__,
  "NAM_DEEPSOIL": MODD_DEEPSOIL__,
  "freeformat": {},
}

default_exseg = {
  "NAM_CONFIO": MODD_CONFIO__,
  "NAM_CONFZ": MODD_CONFZ__,
  "NAM_BACKUP": MODD_BACKUP__,
  "NAM_IDEAL_FLUX": SURF_IDEAL_FLUX__,
  "NAM_CONF": MODD_CONF__,
  "NAM_CONFn": MODD_CONFn__,
  "NAM_FRC" : MODD_FRC__,
  "NAM_LBCn" : MODD_LBCn__,
  "NAM_DYN" : MODD_DYN__,
  "NAM_DYNn" : MODD_DYNn__,
  "NAM_ADVn" : MODD_ADVn__,
  "NAM_PARAMn" : MODD_PARAMn__,
  "NAM_TURBn" : MODD_TURBn__,
  "NAM_RADn" : MODD_RADn__,
  "NAM_PARAM_MFSHALLn" : MODD_PARAM_MFSHALLn__,
  "NAM_PARAM_ECRAD" : MODD_PARAM_ECRAD__,
  "NAM_PARAM_LIMA" : MODD_PARAM_LIMA__,
  "NAM_PARAM_C2R2" : MODD_PARAM_C2R2__,
  "NAM_BUDGET" : MODD_BUDGET__,
  "NAM_LES" : MODD_LES__,
  "NAM_CONDSAMP" : MODD_CONDSAMP__,
  "NAM_OUTPUT" : MODD_OUTPUT__,
  "NAM_BU_RTH" : MODD_BU_RTH__,
  "NAM_BU_RRV" : MODD_BU_RRV__,
  "NAM_BU_RU" : MODD_BU_RU__,
  "NAM_BU_RV" : MODD_BU_RV__,
}
