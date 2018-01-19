#######################################################################################
#
# Put the correct modules in your .bashrc (alternatively, put them in a .sh script and
# source it each time you work with NEMO-Elmer/Ice
# 
# Exemple on occigen:
#
# ## GET REQUIRED MODULES
# module purge
# module load intel/17.0
# module load openmpi/intel/2.0.1
# module load hdf5/1.8.17
# module load netcdf/4.4.0_fortran-4.4.2
# module load cmake
#
# ## Elmer's paths :
# export ELMER_HOME=/home/njourd/models/Elmer/install
# export PATH=.:$ELMER_HOME/bin:$PATH
#
# Install all the libraries with this environment



#######################################################################################
## 1- Install libraries required for Elmer/Ice
#######################################################################################

mkdir ~/models
mkdir ~/bin
mkdir ~/lib
mkdir ~/util
mkdir ~/include

#######################################################################################
# VTK (Visualisation Tool Kit)
# see http://www.vtk.org
# and http://www.vtk.org/Wiki/VTK/Configure_and_Build

cd ~/util
# the wget command may not work in your cluster, if so copy paste it from elsewhere
wget http://www.vtk.org/files/release/8.0/VTK-8.0.0.tar.gz  ## 29 Mo
tar xzvf VTK-8.0.0.tar.gz
cd VTK-8.0.0
mkdir build
cd build
ccmake ~/util/VTK-8.0.0
# This will open an interactive window.
# Press c to configure, then fill some of the fields, for example:
# CMAKE_INSTALL_PREFIX             /home/njourd/util/VTK-8.0.0/build
# EXECUTABLE_OUTPUT_PATH           /home/njourd/lib
# LIBRARY_OUTPUT_PATH              /home/njourd/lib
# Then press c again. Then press g to generate the makefile.
make  ## or make -j4 to go faster on parallel machines
cd ~/lib
for file in ~/util/VTK-8.0.0/build/lib/lib*; do ln -s -v $file; done
#NB: to save space (if quota), it is recommended to remove all directories 
#    in ~/util/VTK-8.0.0 except build/bin and build/lib

#######################################################################################
# MUMPS (MUltifrontal Massively Parallel sparse direct Solver)
# see http://mumps.enseeiht.fr
cd ~/util
wget http://mumps.enseeiht.fr/MUMPS_5.1.1.tar.gz
tar xzvf MUMPS_5.1.1.tar.gz
cd MUMPS_5.1.1
cp -p Make.inc/Makefile.xxxxx Makefile.inc # take xxxxx=INTEL.PAR then adapt...
#=============
# On Occigen, the Makefile.inc should be:
# NB: $MKLROOT is already defined if mkl lirary is installed.
PLAT    =
LIBEXT  = .a
OUTC    = -o
OUTF    = -o
RM = /bin/rm -f
CC = mpicc -openmp -mkl=parallel -fPIC
FC = mpifort -openmp -mkl=parallel -fPIC
FL = mpifort -openmp -mkl=parallel -fPIC
AR = ar vr
RANLIB  = echo
INCPAR = -I$(MKLROOT)/include/lp64
LIBBLAS = $(MKLROOT)/lib/intel64/libmkl_blas95_lp64.a -lmkl_scalapack_lp64 -lmkl_blacs_openmpi_lp64 -lpthread -lm
INCSEQ = -I$(topdir)/libseq
LIBSEQ  =  -L$(topdir)/libseq -lmpiseq
LIBOTHERS = -lpthread
CDEFS   = -DAdd_
OPTF    = -O -DALLOW_NON_INIT -nofor_main -openmp # or -qopenmp for most recent compilers
OPTL    = -O -nofor_main -openmp
OPTC    = -O -openmp
INCS = $(INCPAR)
LIBS = $(LIBPAR)
LIBSEQNEEDED =
#=============
# Then, do:
make all
make
cd ~/lib
for file in ~/util/MUMPS_5.1.1/lib/lib*; do ln -s -v $file; done
cd ~/include
for file in ~/util/MUMPS_5.1.1/include/*; do ln -s -v $file; done
# NB: if quota, you can remove PORD/ SCILAB/ MATLAB/ doc/ src/ examples/

#######################################################################################
# CSA (Bivariate Cubic Spline approximation library)
# https://github.com/hetland/pygridgen/tree/master/external/csa

cd ~/util
git clone https://github.com/hetland/pygridgen.git
cd pygridgen/external/csa
./configure 
make
cd ~/lib/
ln -s -v ~/util/pygridgen/external/csa/libcsa.a
cd ~/bin
ln -s -v ~/util/pygridgen/external/csa/csabathy 
cd ~/include
mkdir csa
cd csa
for file in ~/util/pygridgen/external/csa/*.h; do ln -s -v $file; done
cd ~/util
ln -s -v ~/util/pygridgen/external/csa

#######################################################################################
# NN (Natural Neighbours interpolation library)
# https://github.com/hetland/pygridgen/tree/master/external/nn

cd ~/util/pygridgen/external/nn
./configure
make
cd ~/lib
ln -s -v ~/util/pygridgen/external/nn/libnn.a 
cd ~/bin
ln -s -v ~/util/pygridgen/external/nn/minell
ln -s -v ~/util/pygridgen/external/nn/nnbathy
cd ~/include
mkdir nn
cd nn
for file in ~/util/pygridgen/external/nn/*.h; do ln -s -v $file; done
cd ~/util
ln -s -v ~/util/pygridgen/external/nn

#######################################################################################
## 2- Compile Elmer/Ice
## Version 6be9699fd6d9b15082f5bfad04776aabfa742489 works (21/12/2017)
#######################################################################################
# see https://groupes.renater.fr/wiki/elmerice/elmergit

cd models
mkdir Elmer
cd Elmer
git clone git://www.github.com/ElmerCSC/elmerfem -b elmerice elmerfem
mkdir build install
cd build
export LANG=C
ccmake ../elmerfem
#================
# Press c
# First, adjust the following option if needed
# It not appearing type 't' and change it NOW because it affects the further other options
#  CMAKE_Fortran_COMPILER           /opt/software/common/intel/compilers_and_libraries_2017.0.098/linux/bin/intel64/ifort
# Then press c again. Then adjust the main options, in particular:
#  CMAKE_BUILD_TYPE                RelWithDebInfo
#  CMAKE_Fortran_MODULE_DIRECTORY  /home/njourd/models/Elmer/build/fmodules
#  CMAKE_INSTALL_PREFIX            /home/njourd/models/Elmer/install
#  CPACK_PACKAGE_FILE_NAME         elmerfem-8.2-ddb8140-20170712_Linux-x86_64
#  ELMER_SOLVER_HOME               /home/njourd/bin/elmersolver
#  WITH_CONTRIB                     OFF
#  WITH_ELMERGUI                    OFF
#  WITH_ELMERGUILOGGER              OFF
#  WITH_ELMERGUITESTER              OFF
#  WITH_ELMERPOST                   OFF
#  WITH_ElmerIce                    ON
#  WITH_FETI4I                      OFF
#  WITH_Hypre                       OFF
#  WITH_MKL                         ON
#  WITH_MPI                         ON
#  WITH_Mumps                       ON
#  WITH_OpenMP                      ON
#  WITH_Trilinos                    OFF
# Then press c again
# Press t to have more options, e.g.:
#  OpenMP_Fortran_FLAGS             -qopenmp
#  WITH_GridDataReader              ON
#  WITH_ScatteredDataInterpolator   ON
# Press c several times as new options appear each time.
#  NN_INCLUDE_DIR                   /home/njourd/include/nn
#  NN_LIB                           /home/njourd/lib/libnn.a
#  CSA_INCLUDE_DIR                  /home/njourd/include/csa
#  CSA_LIB                          /home/njourd/lib/libcsa.a
# Adjust iteratively, then use option g (generate) if available (if not keep adjusting)
# NB1: Pay attention not to remove lines (with 'd'). To restart the process from scratch,
#      remove everything in the build directory and relaunch the ccmake command.
# NB2: CMAKE_Fortran_MODULE_DIRECTORY must be an absolute path !!
# NB3: If not working, try with WITH_GridDataReader and WITH_ScatteredDataInterpolator OFF
#      And later on if you need them, recompile elmer with these lines in a Makefile:
#
#    Scattered2DDataInterpolator: $(ELMER_Scatter)/Scattered2DDataInterpolator.F90 $(ExecPath)/csa_interpolate_points.o $(ExecPath)/Scattered2D_FInterface.o
#            elmerf90 $(NETCDFINC) $^ $(NNLIB) $(CSLIB) -o $(ExecPath)/$@ $(NETCDFLIBS)
#  
#    $(ExecPath)/csa_interpolate_points.o : $(ELMER_Scatter)/csa_interpolate_points.c
#            $(CC) $(CCFLAG) -I$(CS_HOME) -c $^ -o $@
#
#    $(ExecPath)/Scattered2D_FInterface.o : $(ELMER_Scatter)/Scattered2D_FInterface.F90
#            elmerf90 -c $< -o $@
#===============
nproc        # to know how many available procs
make -j8     # or make install if only one proc
make install
# to check compilation:
ctest -L elmerice-fast   # should pass all the tests

#######################################################################################
## 3- Install Elmer/Ice-NEMO Coupling interface
#######################################################################################

#######################################################################
# First, install NetCDF-c++ library if not already installed :
# Choose your version on http://www.unidata.ucar.edu/downloads/netcdf

cd ~/util
wget https://github.com/Unidata/netcdf-cxx4/archive/v4.2.1.tar.gz
tar xzvf v4.2.1.tar.gz
cd netcdf-cxx4-4.2.1
mkdir BUILD
./configure --prefix=${HOME}/util/netcdf-cxx4-4.2.1/BUILD
make
make install
make check # should pass 7/7
# NB: if needed (quota), you can remove cxx4/ and examples/ once compiled

##############################################################
# Then install the coupling tools to transform VTK to netcdf:

cd ~/util
git clone https://github.com/nicojourdain/From_VTK_to_NetCDF.git
cd From_VTK_to_NetCDF
vi CMakeLists.txt
# adjust:
#     set(VTK_DIR "/home/njourd/util/VTK-8.0.0/build")
# and:
#     include_directories("/home/njourd/util/netcdf-cxx4-4.2.1/BUILD/include/")    
mkdir build
cd build
ccmake ..
# adjust:
#  CMAKE_INSTALL_PREFIX             /home/njourd/util/From_VTK_to_NetCDF/build
#  NETCDF_LIB                       /home/njourd/util/netcdf-cxx4-4.2.1/BUILD/lib/libnetcdf_c++4.so
make

##############################################################
# Get the Elmer Solver to feed Elmer with NEMO's melt rates in the context of MISOMIP:
# (and other modified solvers for MISOMIP)
cd ~/util
git clone https://github.com/nicojourdain/MISOMIP_Melt.git

# NB: if SSAStar.F90 is not on the official elmerice web site yet, you can download it
#     from the "LGGE wiki" https://groupes.renater.fr/wiki/elmerice/elmericegit
#     [if so you need to upload your public key on https://sourcesup.cru.fr (connect, then "Mon Compte")]
#     If so, mkdir ~/util/ElmerSolver_IGE
#     Then, put the specific solvers there.

#######################################################################################
## 4- Prepare Elmer/Ice-NEMO run using the Config Manager
#######################################################################################

# Prepare working directories:
export WORK=/scratch/shared/egige60    ## to adapt, typically $SCRATCHDIR
mkdir $WORK/NEMO_MISOMIP
mkdir $WORK/ELMER_MISOMIP

# Prepare NEMO
mkdir $WORK/NEMO_MISOMIP/FILES  ## here put NEMO's xml, namelist, f90, rebuild, etc
mkdir $WORK/NEMO_MISOMIP/input  ## here put NEMO's inputs (bathy, dta, resto, ...)
mkdir $WORK/NEMO_MISOMIP/output
mkdir $WORK/NEMO_MISOMIP/restart
mkdir $WORK/NEMO_MISOMIP/run

# Get and use Nacho's Config Manager:
cd ~
git clone https://github.com/nicojourdain/CM_MISOMIP.git
cd CM_MISOMIP
vi Makefile_G    ## (_G -> Generic) Adapt libraries and path to your case
vi createRUN.sh  ## Adapt to the run you want to initialize

#############
# IMPORTANT #
#############
vi $WORK/NEMO_MISOMIP/FILES/namelist_nemo_GENERIC_ISOMIP         ## Choose NEMO's options
vi ~/CM_MISOMIP/Templates/Sif/scketchIce1r_SSAStar_fromNEMO.sif  ## Choose Elmer/Ice's options 
                                                                 ## (including MISOMIP_Melt_Consv vs MISOMIP_Melt_Consv_Evolv)
                                                                 ## WARNING : the sif are copied from this file at each submission,
                                                                 ##   so not possible to run 2 runs with different sif simultaneously !!
                                                                 ##   [ or change variable scketch in WORK_ELMER/scriptIce1rExecute.sh
                                                                 ##     once the run is prepared ].
vi ~/CM_MISOMIP/Scripts/run_nemo_ISOMIP.sh                       ## Adapth paths and executable directories
vi ~/CM_MISOMIP/Scripts/scriptIce1rExecute.sh                    ## Adapt sif name, Elmer cstes, SBATCH walltime, etc
vi ~/CM_MISOMIP/Scripts/scriptIce1aExecute.sh                    ## Adapt sif name, Elmer cstes, SBATCH walltime, etc

#######################################################################################
## 5- Run MISOMIP
#######################################################################################

export CASE="CPL06_hmin20"  ## CASE name (should be different for each simulation)

cd ~/CM_MISOMIP ## from https://github.com/neichin/CM_MISOMIP.git
# - edit createRUN.sh and indicate pathways, cpl frequency, etc
# - you may also have to change things in Scripts/run_nemo_ISOMIP.sh
./createRUN.sh ${CASE}

## Compile Elmer/Ice
cd RUNS/${CASE}/WORK_ELMER
make all

## check values in scriptIce1rExecute.sh :
##      - sbatch parameters (run duration, etc)
##      - sif parameters

cd ../WORK_NEMO
## check namelist parameters, xml files, batch parameters 
#  (if not correct, adapt scripts in $WORK/NEMO_MISOMIP/FILES)

cd ..
./script_RUN_MISOMIP.sh
# - le suivi est dans COUPLED_Run.db
# - si on veut aller au dela du NRUN_MAX initialement defini, executer :
#           ./script_Exec_MISOMIP.sh ${NEW_RUN_MAX}
# - si on veut que NEMO parte d'un restart plutot qu'au repos, executer :
#           ./script_Start_From_Restart.sh

# the outputs are on ...

# to restart a job that failed :
cd ~/CM_MISOMIP/CM_MISOMIP/RUNS/WORK_NEMO/${CASE}
# check the last run_nemo.eXXXX
# you should find something like: ./scriptIce1rExecute.sh 20 3247833 /scratch/cnt0021/gge6066/njourdain/NEMO_MISOMIP//output/nemo_ISOMIP_CPLFREQ1yr_hmin30/0019
# re-execute this script with the same line.
# if the path are not correct, clean by re-executing the ./createRUN.sh ${CASE} in the config manager.

# Si NEMO plante:
vi prod_nemo.db  ## virer la derniere ligne
#source ~/modulesMISOMIP.sh
qsub run_nemo_ISOMIP.sh
