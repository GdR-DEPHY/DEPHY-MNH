## Default grids for LES|CRM|SCM modes
# Nhoriz == minimum pour avoir des circulations résolues
# deltav :: pour l'instant on garde la même résolution verticale qu'en LES

LES_Nhoriz = 1024   # npoints
LES_Nverti = 160    # npoints
LES_deltah = 25.    # horizontal resolution in m
LES_deltav = 25.    # vertical   resolution in m
#pour les cas de transition on va adapter la grille horizontale => 200m
# et adapter la grille verticale

CRM_Nhoriz = 100    # npoints
CRM_Nverti = 160    # npoints
CRM_deltah = 2500.  # horizontal resolution in m
CRM_deltav = 25     # vertical   resolution in m
# pour les cas de transition on veut adapter la grille verticale

SCM_Nhoriz = 1      # npoints
SCM_Nverti = 160    # npoints
SCM_deltah = 50000. # horizontal resolution in m
SCM_deltav = 25.    # vertical   resolution in m
# pour les cas de transition on veut adapter la grille verticale

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
  "NAM_TURBn"  : { "XIMPL"      : "1.",
                   "CTURBLEN"   : "'DEAR'",
                   "CTURBDIM"   : "'3DIM'",     },
  "NAM_PARAM_RADn"  : { "XDTRAD"        : "1.",
                        "XDTRAD_CLONLY" : "1.",},
  "NAM_LES" :   { "LLES_NEB_MASK"  : ".TRUE.", 
                  "LLES_CORE_MASK" : ".TRUE.", 
                  "LLES_CS_MASK"   : ".TRUE.",
                  "XLES_TEMP_SAMPLING"   : "300.",
                  "XLES_TEMP_MEAN_STEP"  : "3600."},
  "NAM_CONDSAMP": { "LCONDSAMP"    : ".TRUE."     },
  "NAM_CONFZ"   : { "MPI_BUFFER_SIZE" :"400"      },
  "NAM_BUDGET"  : { "NBUIH"        : "%i"%LES_Nhoriz,
                    "NBUJH"        : "%i"%LES_Nhoriz, 
                    "NBUKH"        : "%i"%LES_Nverti,},
# no need for now: the SVT variables will be output by default
#  "NAM_OUTPUT"  : { "COUT_VAR(1,15)" : "'SVT002'",
#                    "COUT_VAR(1,16)" : "'SVT003'", },
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
  "NAM_PARAM_RADn"  : { "XDTRAD"        : "10.",
                        "XDTRAD_CLONLY" : "10.",},
  "NAM_NEBn"  : { "LSUBG_COND" : ".TRUE.",      },# activate subrid condensation
  "NAM_LES"    : { "XLES_TEMP_SAMPLING"   : "300.",
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
  "NAM_TURBn"  : { "CTURBLEN"   :"'BL89'",
                   "CTURBDIM"   :"'1DIM'"     },
  "NAM_NEBn"  : { "LSUBG_COND" : ".TRUE.",      },# activate subrid condensation
  "NAM_PARAM_RADn"  : { "XDTRAD"        : "30.",
                        "XDTRAD_CLONLY" : "30.",},
  "NAM_PARAM_MFSHALLn" : {"CMF_UPDRAFT"    :"'EDKF'",
                          "CMF_CLOUD"      :"'DIRE'", 
                          "LMIXUV"         :".TRUE.", 
                          "LMF_FLX"        : ".TRUE."},
  "NAM_PARAM_KAFRn" : {"XDTCONV"    :"30.", # doit etre au moins  nXTSTEP
                          "LDIAGCONV"      :".TRUE."}, 
  "NAM_LES" :   { "LLES_MEAN"           : ".TRUE.",
                  "LLES_RESOLVED"       : ".TRUE.",
                  "LLES_SUBGRID"        : ".TRUE.",
                  "LLES_NEB_MASK"       : ".FALSE.",
                  "LLES_CORE_MASK"      : ".FALSE.",
                  "LLES_CS_MASK"        : ".FALSE.",
                },
  "NAM_CONDSAMP": { "LCONDSAMP"    : ".FALSE."    },
  "NAM_BUDGET"  : { "CBUTYPE"      : "'NONE'",
                    "NBUIH"        : "%i"%SCM_Nhoriz,
                    "NBUJH"        : "%i"%SCM_Nhoriz, 
                    "NBUKH"        : "%i"%SCM_Nverti},
}

######################################
## DEFAULT CONFIGS IN MESO-NH 5.7.0 ##
# modified to match default config  ##
# in DEPHY project                  ##
######################################

NAM_CONFIO = { # I/O file type
    "LCDF4"           : ".TRUE.",
    "LLFIOUT"         : ".FALSE.",
    "LLFIREAD"        : ".FALSE.",
}

NAM_LUNITn = { # Init file names
    "CINIFILE"        : "'init'",
    "CINIFILEPGD"     : "'init_pgd'", 
}

NAM_CONFZ  = { # parallelism
  "NZ_VERB"         : "0",
  "NZ_PROC"         : "0",
  "NB_PROCIO_R"     : "1",
  "NB_PROCIO_W"     : "1",
  "MPI_BUFFER_SIZE" : "40",
  "LMNH_MPI_BSEND"  : ".TRUE.",
  "LMNH_MPI_ALLTOALLV_REMAP" : ".FALSE.",
  "NZ_SPLITTING"    : "10",
}

NAM_BACKUP = { # backup files for restarts
  "XBAK_TIME_FREQ(1)" : "3600.0",
}

SURF_IDEAL_FLUX = { # surfex default namelist
  "NFORCF"            : "2", # default number of flux forcings
  "NFORCT"            : "2", # default number of temp forcings
  "CSFTQ"             : "'W/m2'", #'kg/m2/s'",
  "CUSTARTYPE"        : "'Z0'",
}

NAM_CONF   = { # general config
  "CCONF"             :"'START'",
  "LFLAT"             : ".TRUE.", #".FALSE.",
  "NMODEL"            : "1",
  "CEQNSYS"           : "'DUR'",
  "NVERB"             : "6",      # "5", 
  "CEXP"              : "'EXP01'",
  "CSEG"              : "'SEG01'",
  "LFORCING"          : ".TRUE.",  #".FALSE.",
  "NHALO"             : "1",
  "JPHEXT"            : "1",
  "CSPLIT"            :"'BSPLITTING'",
  "LLG"               : ".FALSE.",
  "LINIT_LG"          : ".FALSE.",
  "CINIT_LG"          : "'FMOUT'",
  "LNOMIXLG"          : ".FALSE.",
  "LCHECK"            : ".FALSE.",
}

NAM_CONFn  = { # config for model n (nesting)
  "LUSERV"            : ".TRUE.",
  "LUSERC"            : ".FALSE.",
  "LUSERR"            : ".FALSE.",
  "LUSERI"            : ".FALSE.",
  "LUSERS"            : ".FALSE.",
  "LUSERG"            : ".FALSE.",
  "LUSERH"            : ".FALSE.",
  "LUSECI"            : ".FALSE.",
}

NAM_FRC    = {   # forcings
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
  "LDEEPOC"           : ".FALSE.",
  "XCENTX_OC"         : "16000.",
  "XCENTY_OC"         : "16000.",
  "XRADX_OC"          :  "8000.",  
  "XRADY_OC"          :  "8000.",
}

NAM_LBCn   = {  # boundary conditions for model n
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

NAM_DYN    = { # general dynamics
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

NAM_DYNn   = { # dynamics for model n (nesting)
  "XTSTEP"            : "60.",
  "CPRESOPT"          : "'CRESI'",
  "NITR"              : "4",
  "LITRADJ"           : ".TRUE.",
  "LRES"              : ".FALSE.",
  "XRES"              : "1.E-07",
  "XRELAX"            : "1.",
  "LVE_RELAX"         : ".TRUE.",
  "LVE_RELAX_GRD"     : ".FALSE.",
  "XT4DIFU"           : "1800.",
  "XT4DIFTH"          : "1800.",
  "XT4DIFSV"          : "1800.",
}

NAM_ADVn   = { # advection for model n
  "CUVW_ADV_SCHEME"   : "'CEN4TH'",
  "CMET_ADV_SCHEME"   : "'PPM_01'",
  "CSV_ADV_SCHEME"    : "'PPM_01'",
  "CTEMP_SCHEME"      : "'RKC4'",
  "NWENO_ORDER"       : "3",
  "LSPLIT_CFL"        : ".TRUE.",
  "LSPLIT_WENO"       : ".TRUE.",
  "XSPLIT_CFL"        : "0.8",
  "LCFL_WRIT"         : ".FALSE.",
}

NAM_PARAMn = { # activate params for model n
  "CTURB"             : "'TKEL'", # "'NONE'"
  "CRAD"              : "'NONE'",
  "CCLOUD"            : "'LIMA'", # "'NONE'"
  "CDCONV"            : "'NONE'",
  "CSCONV"            : "'NONE'",
  "CELEC"             : "'NONE'",
  "CACTCCN"           : "'NONE'",
}

NAM_TURBn  = { # config turbulence
  "XIMPL"             : "1.",
  "XTKEMIN"           : "1E-10",
  "XCED"              : "0.84",
  "XCTP"              : "4.65",
  "XCEI_MIN"          : "0.001E-6",
  "XCEI_MAX"          : "0.01E-6",
  "XMINSIGS"          : "1.E-12",
  "XLINI"             : "0.1",
  "XCADAP"            : "0.5",
  "CTURBLEN"          : "'BL89'",
  "CTURBDIM"          : "'1DIM'",
  "LTURB_FLX"         : ".TRUE.",    #".FALSE.",
  "LTURB_DIAG"        : ".TRUE.",   #".FALSE.",
  "LSIG_CONV"         : ".FALSE.",
  "LRMC01"            : ".TRUE.",   #".FALSE.",
  "CTOM"              : "'NONE'",
  "LLEONARD"          : ".FALSE.",
  "XCOEFHGRADTHL"     : "1.0",
  "XCOEFHGRADRM"      : "1.0",
  "XALTHGRAD"         : "2000.0",
  "XCLDTHOLD"         : "-1.0",
  "LCLOUDMODIFLM"     : ".FALSE.",
  "CTURBLEN_CLOUD"    : "'DELT'",
  "XCOEF_AMPL_SAT"    : "5.",
  "LHARAT"            : ".FALSE.",
  "LPROJQITURB"       : ".TRUE.",
  "LSMOOTH_PRANDTL"   : ".TRUE.",
  "NTURBSPLIT"        : "1",
  "LTURB_PRECIP"      : ".FALSE.",
}

NAM_PARAM_RADn   = { # config radiation
  "XDTRAD"            : "60.",
  "XDTRAD_CLONLY"     : "60.",
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


NAM_PARAM_KAFRn = {  # config deep convection scheme
        "NICE"         :"1",
        "LREFRESH_ALL" :".TRUE.",
        "LCHTRANS"     :".FALSE.",
        "LDOWN"        :".TRUE.",
        "LSETTADJ"     :".TRUE.",
        "XTADJD"       :"3600",
        "XTADJS"       :"10800",
        "LDIAGCONV"    :".TRUE.",
        "NENSM"        :"0",
        }
NAM_PARAM_MFSHALLn = { # config shallow mass flux scheme
  "CMF_UPDRAFT"       : "'EDKF'",   # choix de l'updraft
  "CMF_CLOUD"         : "'DIRE'",   # choix de la param pour prévoir la fraction nuageuse et le contenu en hydrom
  "LMIXUV"            : ".TRUE.",   # activation du melange du vent par le thermique
  "LMF_FLX"           : ".TRUE.",   #diagnostiques des flux de masse
  "LVERLIMUP"         : ".TRUE.",   #decroissance lente de l'intensité de l'updraft sur les derniers 1000m de l'upd
}

NAM_PARAM_ECRAD = { # config radiation scheme
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

NAM_PARAM_LIMA = {  # config microphysics scheme
   "LPTSPLIT"         : ".TRUE.",
   "LFEEDBACKT"       : ".TRUE.",
   "NMAXITER"         : "5",
   "XMRSTEP"          : "0.005",
   "XTSTEP_TS"      : "20.",
   "XNUC"             : "1.0",
   "XALPHAC"          : "3.0",
   "XNUR"             : "2.0",
   "XALPHAR"          : "1.0",
   "LACTI"            : ".FALSE.",#".TRUE.",
   "LSEDC"            : ".TRUE.",
   "LACTIT"           : ".FALSE.",
   "LADJ"             : ".TRUE.",
   "LSPRO"            : ".FALSE.",
   "LKHKO"            : ".FALSE.",
   "LDEPOC"           : ".TRUE.",
   "LACTTKE"          : ".TRUE.",
   "LKESSLERAC"       : ".TRUE.", #".FALSE.",
   "NMOM_C"           : "1",      #"2",
   "NMOM_R"           : "1",      #"2",
   "XVDEPOC"          : "0.02",
   "XFSOLUB_CCN"      : "1.0",
   "XACTEMP_CCN"      : "280.",
   "NMOD_CCN"         : "0",      #"1",
   "LSCAV"            : ".FALSE.",
   "LAERO_MASS"       : ".FALSE.",
   "LCCN_HOM"         : ".TRUE.",
   "CCCN_MODES"       : "'COPT'",
   "XCCN_CONC(:)"     : "300.",
   "HINI_CCN"         : "'AER'",
   "HTYPE_CCN"         : "'M'",
   "LHHONI"           : ".FALSE.",
   "LNUCL"            : ".TRUE.",
   "LSEDI"            : ".TRUE.",
   "LSNOW_T"          : ".FALSE.",
   "LMURAKAMI"        : ".TRUE.",
   "CPRISTINE_ICE_LIMA": "'PLAT'",
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
NAM_NEBn = { # default
   "LHGT_QS"      : ".FALSE.",
   "LSTATNW"      : ".FALSE.",
   "XTMINMIX"     : "253.16",
   "XTMAXMIX"     : "273.16",
   "LSUBG_COND"   : ".FALSE.",
   "CCONDENS"     : "'CB02'",
   "CLAMBDA3"     : "'CB'",
   "LSIGMAS"      : ".TRUE.",
   "VSIGQSAT"     : "0.02",
   "CFRAC_ICE_ADJUST" : "'S'",
   "CFRAC_ICE_SHALLOW_MF" : "'S'",
}
NAM_PARAM_C2R2 = {               # default MNH
    "HPARAM_CCN"      : "'CPB'",    #"'XXX'",
    "HINI_CCN"        : "'CCN'",    #"'XXX'",
    "XCHEN"           : "0.173E+09",#"0.0",
    "XKHEN"           : "1.403",    #"0.0",
    "XMUHEN"          : "0.834",    #"0.0",
    "XBETAHEN"        : "25.499",   #"0.0",
    "LRAIN"           : ".FALSE.",  #".TRUE.",
    "LSEDC"           : ".FALSE.",  #".TRUE.",
}

NAM_PARAM_ICE = {               # default MNH
    "LWARM"           : ".TRUE.",    #"'XXX'",
    "LSEDIC"          : ".TRUE.",    #"'XXX'",
    "LRED"            : ".TRUE.",    #"'XXX'", 
    "CSUBG_AUCV_RC"   : "'NONE'",#"0.0",
    "CSUBG_AUCV_RI"   : "'NONE'",    #"0.0",
    "CSUBG_MF_PDF"    : "'NONE'",    #"0.0",
    "CSUBG_RC_RR_ACCR": "'NONE'",   #"0.0",
    "CSUBG_RR_EVAP"   : "'NONE'",  #".TRUE.",
    "CSUBG_PR_PDF"    : "'SIGM'",  #".TRUE.",
}
NAM_DIAG_SURFn = {   #default SURFEX
        "N2M"             : "2",
}
NAM_DIAG_SURF_ATMn = { #default SURFEX
        "LDIAG_GRID"      : ".TRUE.", 
}

NAM_BUDGET  = { # compute budgets
  "CBUTYPE"           : "'CART'",   #"'NONE'",
  "LBU_KCP"           : ".FALSE.",  #".TRUE",
  "XBUWRI"            : "3600.",    #NAM_DYN["XSEGLEN"],
  "XBULEN"            : "3600.",    #NAM_DYN["XSEGLEN"],
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

NAM_LES     = { # compute horizontal statistics 
  "LLES_MEAN"           : ".TRUE.",  #".FALSE.",
  "LLES_RESOLVED"       : ".TRUE.",  #".FALSE.",
  "LLES_SUBGRID"        : ".TRUE.",  #".FALSE.",
  "LLES_UPDRAFT"        : ".FALSE.",
  "LLES_DOWNDRAFT"      : ".FALSE.",
  "LLES_SPECTRA"        : ".FALSE.",
  "CLES_NORM_TYPE"      : "'NONE'",
  "CBL_HEIGHT_DEF"      : "'KE'",
  "XLES_TEMP_SAMPLING"  : "300.",
  "XLES_TEMP_MEAN_START": "0.",
  "XLES_TEMP_MEAN_STEP" : "3600.",
  "LLES_CART_MASK"      : ".FALSE.",
  "LLES_NEB_MASK"       : ".TRUE.",
  "LLES_CORE_MASK"      : ".TRUE.",
  "LLES_CS_MASK"        : ".TRUE.",
  "LLES_MY_MASK"        : ".FALSE.",
}

NAM_CONDSAMP = { # conditional sampling
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

NAM_NEBn = { # schéma de nuages
  "LHGT_QS"             : ".FALSE.",
  "LSTATNW"             : ".FALSE.",
  "LSUBG_COND"          : ".FALSE.",
  "LSIGMAS"             : ".TRUE.",
  "XTMINMIX"            : "253.16",
  "XTMAXMIX"            : "273.16",
  "CCONDENS"            : "'CB02'",
  "CLAMBDA3"            : "'CB'",
  "VSIGQSAT"            : "0.02",
  "CFRAC_ICE_ADJUST"    : "'S'",
  "CFRAC_ICE_SHALLOW_MF" : "'S'",
}

NAM_OUTPUT  = {
  "XOUT_TIME_FREQ(1)"      : "3600", # "-999.",
  "XOUT_TIME_FREQ_FIRST(1)": "3600", # "0.",
  "LOUT_BEG"               : ".FALSE.",
  "LOUT_END"               : ".FALSE.",
  "LOUT_REDUCE_FLOAT_PRECISION(1)" : ".TRUE.", 
  "LOUT_COMPRESS(1)"       : ".FALSE.",
  "NOUT_COMPRESS_LEVEL(1)" : "4",
  "COUT_VAR(1,1)"          : "'UT'",
  "COUT_VAR(1,2)"          : "'VT'",
  "COUT_VAR(1,3)"          : "'WT'",
  "COUT_VAR(1,4)"          : "'THT'",
  "COUT_VAR(1,5)"          : "'TKET'",
  "COUT_VAR(1,6)"          : "'PABST'",
  "COUT_VAR(1,7)"          : "'SVT001'", 
  "COUT_VAR(1,8)"          : "'RVT'", 
  "COUT_VAR(1,9)"          : "'RCT'",
  "COUT_VAR(1,10)"          : "'RRT'",
  "COUT_VAR(1,11)"         : "'INPRR'",
  "COUT_VAR(1,12)"         : "'RIT'",
  "COUT_VAR(1,13)"         : "'RST'",
  "COUT_VAR(1,14)"         : "'RGT'",
}

NAM_BU_RTH = { 
  "LBU_RTH"        : ".TRUE.",
  "CBULIST_RTH(1)" : "'ALL'",
}

NAM_BU_RRV = {
  "LBU_RRV"         : ".TRUE.",
  "CBULIST_RRV(1)"  : "'ALL'",
}

NAM_BU_RRC = {
  "LBU_RRC"         : ".TRUE.",
  "CBULIST_RRC(1)"  : "'ALL'",
}

NAM_BU_RRR = {
  "LBU_RRR"         : ".TRUE.",
  "CBULIST_RRR(1)"  : "'ALL'",
}

NAM_BU_RRI = {
  "LBU_RRI"         : ".TRUE.",
  "CBULIST_RRI(1)"  : "'ALL'",
}

NAM_BU_RRS = {
  "LBU_RRS"         : ".TRUE.",
  "CBULIST_RRS(1)"  : "'ALL'",
}

NAM_BU_RRG = {
  "LBU_RRG"         : ".TRUE.",
  "CBULIST_RRG(1)"  : "'ALL'",
}

NAM_BU_RU = {
  "LBU_RU"          : ".TRUE.",
  "CBULIST_RU(1)"   : "'ALL'",
}

NAM_BU_RV = {
  "LBU_RV"          : ".TRUE.",
  "CBULIST_RV(1)"   : "'ALL'",
}

NAM_DIMn_PRE = { # pre_idea dimensions
  "NIMAX" : "10", 
  "NJMAX" : "10" 
}

NAM_DYNn_PRE = { # pressure solver
  "CPRESOPT"          : "'CRESI'",
  "NITR"              : "4",
  "XRELAX"            : "1.",
  "LRES"              : ".FALSE.",
  "XRES"              : "1.E-07",
}

NAM_GRID_PRE = {
  "XLON0"   : "0.",
  "XLAT0"   : "60.",
  "XBETA"   : "0.",
  "XRPK"    : "1.",
  "XLONORI" : "350.",
  "XLATORI" : "37.",
}

NAM_VER_GRID = { # pre_idea vertical grid
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

NAM_CONF_PRE = { # pre_idea config
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
}

NAM_PERT_PRE = {
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

NAM_GRIDH_PRE = { 
  "XDELTAX"    : "5000.",
  "XDELTAY"    : "5000.",
  "XHMAX"      : "0.",
  "NEXPX"      : "3",
  "NEXPY"      : "1",
  "XAX"        : "10000.",
  "XAY"        : "10000.",
  "NIZS"       : "5",
  "NJZS"       : "5",
}

NAM_GRn_PRE =  {
  "CSURF" : "'EXTE'", #"NONE",
}

NAM_PGD_SCHEMES =  {
  "CSEA"    : "'FLUX'", #"'SEAFLUX'"
}
#"CNATURE" : "'TSZ0'",

NAM_COVER =  {
  "XUNIF_COVER(1)"    : "0.", # (fraction of ocean)
  "XUNIF_COVER(6)"    : "1.", # (fraction of land)
}

NAM_SEABATHY =  {
  "XUNIF_SEABATHY"    : "5.",
}

NAM_PREP_SEAFLUX =  {
  "XSST_UNIF"         : "280.", # random default
}
NAM_DATA_SEAFLUX =  {
  "LSST_DATA"         : ".FALSE.", # 
  "NTIME_SST"         : "0",         #
  "NYEAR_SST(1)"      : "1980", # random default
  "NMONTH_SST(1)"     : "1",    # random default
  "NDAY_SST(1)"       : "1",    # random default
  "XTIME_SST(1)"      : "0.",   # random default
  "XUNIF_SST(1)"      : "280.", # random default
}
NAM_FRAC =  {
  "LECOCLIMAP"        : ".TRUE.",
  "XUNIF_NATURE"      : "1.",
}
NAM_DATA_TSZ0 =  {
  "NTIME"             : "0",
  "XUNIF_DTS(1)"      : "-0.250", # SFX default 
}
NAM_DATA_ISBA =  {
  "NTIME"             : "0",
  "XUNIF_Z0(1,1)"     : "0", # depends on the veg type and time
}
NAM_ISBA =  {
  "XUNIF_CLAY"        : "1.",
  "XUNIF_SAND"        : "0.",
  "XUNIF_RUNOFFB"     : "0.5",
  "CISBA"             : "'2-L'",
  "CPHOTO"            : "'NON'",
  "NPATCH"            : "1",
  "NGROUND_LAYER"     : "2",
}
NAM_PREP_SURF_ATM =  {
  "NYEAR"             : "1980",
  "NMONTH"            : "1",
  "NDAY"              : "1",
  "XTIME"             : "0.",
}
NAM_PREP_ISBA =  {
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
NAM_DEEPSOIL =  {
  "LPHYSDOMC"         : ".FALSE.",
  "LDEEPSOIL"         : ".FALSE.",
}

default_preidea = {
  "NAM_CONFIO": NAM_CONFIO,
  "NAM_CONFZ": NAM_CONFZ,
  "NAM_LUNITn": NAM_LUNITn,
  "NAM_DIMn_PRE": NAM_DIMn_PRE,
  "NAM_DYNn_PRE": NAM_DYNn_PRE,
  "NAM_GRID_PRE": NAM_GRID_PRE,
  "NAM_VER_GRID": NAM_VER_GRID,
  "NAM_CONF_PRE": NAM_CONF_PRE,
  "NAM_PERT_PRE": NAM_PERT_PRE,
  "NAM_GRIDH_PRE": NAM_GRIDH_PRE,
  "NAM_GRn_PRE": NAM_GRn_PRE,
  "NAM_CONFn": NAM_CONFn,
  "NAM_LBCn" : NAM_LBCn,
  "NAM_PGD_SCHEMES": NAM_PGD_SCHEMES,
  "NAM_COVER": NAM_COVER,
  "NAM_SEABATHY": NAM_SEABATHY,
  "NAM_PREP_SEAFLUX": NAM_PREP_SEAFLUX,
  "NAM_DATA_SEAFLUX": NAM_DATA_SEAFLUX,
  "NAM_FRAC": NAM_FRAC,
  "NAM_DATA_TSZ0": NAM_DATA_TSZ0,
  "NAM_DATA_ISBA": NAM_DATA_ISBA,
  "NAM_ISBA": NAM_ISBA,
  "NAM_PREP_SURF_ATM": NAM_PREP_SURF_ATM,
  "NAM_PREP_ISBA": NAM_PREP_ISBA,
  "NAM_DEEPSOIL": NAM_DEEPSOIL,
  "freeformat": {},
}
default_exseg = {
  "NAM_CONFIO": NAM_CONFIO,
  "NAM_CONFZ": NAM_CONFZ,
  "NAM_LUNITn": NAM_LUNITn,
  "NAM_BACKUP": NAM_BACKUP,
  "NAM_IDEAL_FLUX": SURF_IDEAL_FLUX,
  "NAM_CONF": NAM_CONF,
  "NAM_CONFn": NAM_CONFn,
  "NAM_FRC" : NAM_FRC,
  "NAM_LBCn" : NAM_LBCn,
  "NAM_DYN" : NAM_DYN,
  "NAM_DYNn" : NAM_DYNn,
  "NAM_ADVn" : NAM_ADVn,
  "NAM_PARAMn" : NAM_PARAMn,
  "NAM_TURBn" : NAM_TURBn,
  "NAM_PARAM_RADn" : NAM_PARAM_RADn,
  "NAM_NEBn": NAM_NEBn,
  "NAM_PARAM_KAFRn" : NAM_PARAM_KAFRn,
  "NAM_PARAM_MFSHALLn" : NAM_PARAM_MFSHALLn,
  "NAM_PARAM_ECRAD" : NAM_PARAM_ECRAD,
  "NAM_PARAM_LIMA" : NAM_PARAM_LIMA,
  "NAM_PARAM_C2R2" : NAM_PARAM_C2R2,
  "NAM_PARAM_ICEn" : NAM_PARAM_ICE,
  "NAM_DIAG_SURFn" : NAM_DIAG_SURFn,
  "NAM_DIAG_SURF_ATMn" : NAM_DIAG_SURF_ATMn,
  "NAM_BUDGET" : NAM_BUDGET,
  "NAM_LES" : NAM_LES,
  "NAM_CONDSAMP" : NAM_CONDSAMP,
  "NAM_OUTPUT" : NAM_OUTPUT,
  "NAM_BU_RTH" : NAM_BU_RTH,
  "NAM_BU_RRV" : NAM_BU_RRV,
  "NAM_BU_RRC" : NAM_BU_RRC,
  "NAM_BU_RRR" : NAM_BU_RRR,
  "NAM_BU_RRI" : NAM_BU_RRI,
  "NAM_BU_RRS" : NAM_BU_RRS,
  "NAM_BU_RRG" : NAM_BU_RRG,
  "NAM_BU_RU" : NAM_BU_RU,
  "NAM_BU_RV" : NAM_BU_RV,
}
