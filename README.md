# User Guide for running coupled NEMO - Elmer/Ice simulations

Package to couple the [NEMO ocean model](https://www.nemo-ocean.eu) and the [Elmer/Ice ice sheet model](elmerice.elmerfem.org).

[![DOI](https://zenodo.org/badge/114695996.svg)](https://zenodo.org/badge/latestdoi/114695996) https://doi.org/10.5281/zenodo.2562837

All the files in this repository are under the GNU GENERAL PUBLIC LICENSE.

## Known caveats:
* For the moment, limited to the MISOMIP case (Asay-Davis et al. 2016).

## History:
* 07/2017: First version (Nacho Merino)
* 12/2017: Add README files and multiple EXP cases (Nicolas Jourdain)
* 01/2018: Cleaning and further comments (Lionel Favier & Nicolas Jourdain)

## Download

```shell
cd ~
git clone https://github.com/nicojourdain/CPL_NEMO_ELMER.git
```

Or, if you have a github account with a registered public key:
```shell
git clone git@github.com:nicojourdain/CPL_NEMO_ELMER.git
```

----------------------------

## Installation

### Prerequisite

Everything written for $USER, to be defined with :
```shell
 export USER=`whoami`
```

Put the correct modules and environment variables in your .bashrc (alternatively, put them in a .sh script and source it each time you work with NEMO-Elmer/Ice).
 
Here is an exemple for occigen at CINES:
```shell
## Load required modules :
  module purge
  module load intel/17.0
  module load openmpi/intel/2.0.1
  module load hdf5/1.8.17
  module load netcdf/4.4.0_fortran-4.4.2
  module load cmake/3.7.2
## Elmer's paths :
  export ELMER_HOME=/home/`whoami`/models/Elmer/install
  export PATH=.:$ELMER_HOME/bin:$PATH
```

**NB:** Install all the libraries with this environment.


### 1- Install libraries required for Elmer/Ice

```shell
mkdir ~/models
mkdir ~/bin
mkdir ~/lib
mkdir ~/util
mkdir ~/include
```

#### VTK (Visualisation Tool Kit)

See [http://www.vtk.org](http://www.vtk.org) and [http://www.vtk.org/Wiki/VTK/Configure_and_Build](http://www.vtk.org/Wiki/VTK/Configure_and_Build)

```shell
cd ~/util
# the wget command may not work in your cluster, if so copy paste it from elsewhere
wget http://www.vtk.org/files/release/8.0/VTK-8.0.0.tar.gz  ## 29 Mo
tar xzvf VTK-8.0.0.tar.gz
cd VTK-8.0.0
mkdir build
cd build
ccmake ~/util/VTK-8.0.0
```

This will open an interactive window. Press c to configure, then fill some of the fields, for example:
```console
CMAKE_INSTALL_PREFIX             /home/${USER}/util/VTK-8.0.0/build
EXECUTABLE_OUTPUT_PATH           /home/${USER}/lib
LIBRARY_OUTPUT_PATH              /home/${USER}/lib
```
**NB:** It works with cmake version 3.7.2 but not 3.5.2...

Then press c again. Then press g to generate the makefile.
```shell
make  ## or make -j4 to go faster on parallel machines
cd ~/lib
for file in ~/util/VTK-8.0.0/build/lib/lib*; do ln -s -v $file; done
```

**NB:** to save space (if quota), it is recommended to remove all directories in ~/util/VTK-8.0.0 except build/bin and build/lib

#### MUMPS (MUltifrontal Massively Parallel sparse direct Solver)

To install in not available in module environment (note for occigen users: module load mumps/5.1.1 doesn't work so far).

See [http://mumps.enseeiht.fr](http://mumps.enseeiht.fr)

```shell
cd ~/util
wget http://mumps.enseeiht.fr/MUMPS_5.1.1.tar.gz
tar xzvf MUMPS_5.1.1.tar.gz
cd MUMPS_5.1.1
cp -p Make.inc/Makefile.xxxxx Makefile.inc # take xxxxx=INTEL.PAR then adapt...
```

On Occigen, the Makefile.inc should be (**NB:** $MKLROOT is already defined if mkl lirary is installed) :
```console
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
```

Then, do:
```shell
make all
make
cd ~/lib
for file in ~/util/MUMPS_5.1.1/lib/lib*; do ln -s -v $file; done
cd ~/include
for file in ~/util/MUMPS_5.1.1/include/*; do ln -s -v $file; done
```

**NB:** if quota issues, you can remove ```PORD/``` ```SCILAB/``` ```MATLAB/``` ```doc/``` ```src/``` ```examples/```.

#### CSA (Bivariate Cubic Spline approximation library)

See https://github.com/hetland/pygridgen/tree/master/external/csa

```shell
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
```

#### NN (Natural Neighbours interpolation library)

See https://github.com/hetland/pygridgen/tree/master/external/nn

```shell
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
```

### Compile Elmer/Ice

Tested with Elmer/Ice git version 6be9699fd6d9b15082f5bfad04776aabfa742489 (21/12/2017)

See https://groupes.renater.fr/wiki/elmerice/elmergit

```shell
cd models
mkdir Elmer
cd Elmer
git clone git://www.github.com/ElmerCSC/elmerfem -b elmerice elmerfem
mkdir build install
cd build
export LANG=C
ccmake ../elmerfem
```
Press c ; adjust the following option if needed. If not appearing type 't' **IMMEDIATELY** because it affects the other options.
```console
#  CMAKE_Fortran_COMPILER           /opt/software/common/intel/compilers_and_libraries_2017.0.098/linux/bin/intel64/ifort
```
Then press c again. Then adjust the main options, in particular:
```console
 CMAKE_BUILD_TYPE                RelWithDebInfo
 CMAKE_Fortran_MODULE_DIRECTORY  /home/${USER}/models/Elmer/build/fmodules
 CMAKE_INSTALL_PREFIX            /home/${USER}/models/Elmer/install
 CPACK_PACKAGE_FILE_NAME         elmerfem-8.2-ddb8140-20170712_Linux-x86_64
 ELMER_SOLVER_HOME               /home/${USER}/bin/elmersolver
 WITH_CONTRIB                     OFF
 WITH_ELMERGUI                    OFF
 WITH_ELMERGUILOGGER              OFF
 WITH_ELMERGUITESTER              OFF
 WITH_ELMERPOST                   OFF
 WITH_ElmerIce                    ON
 WITH_FETI4I                      OFF
 WITH_Hypre                       OFF
 WITH_MKL                         ON
 WITH_MPI                         ON
 WITH_Mumps                       ON
 WITH_OpenMP                      ON
 WITH_Trilinos                    OFF
```

Then press c again.

Press t to have more options, e.g.:
```shell
 OpenMP_Fortran_FLAGS             -qopenmp
 WITH_GridDataReader              ON
 WITH_ScatteredDataInterpolator   ON
```

Press c several times as new options appear each time.
```shell
 NN_INCLUDE_DIR                   /home/${USER}/include/nn
 NN_LIB                           /home/${USER}/lib/libnn.a
 CSA_INCLUDE_DIR                  /home/${USER}/include/csa
 CSA_LIB                          /home/${USER}/lib/libcsa.a
```
**NB:**
* Adjust iteratively, then use option g (generate) if available (if not keep adjusting).
* Pay attention not to remove lines (with 'd'). To restart the process from scratch, remove everything in the build directory and relaunch the ccmake command.
* ```CMAKE_Fortran_MODULE_DIRECTORY``` must be an absolute path !!
* If not working, try with ```WITH_GridDataReader``` and ```WITH_ScatteredDataInterpolator``` ```OFF```

And later on if you need them, recompile elmer with these lines in a Makefile:

```makefile
    Scattered2DDataInterpolator: $(ELMER_Scatter)/Scattered2DDataInterpolator.F90 $(ExecPath)/csa_interpolate_points.o $(ExecPath)/Scattered2D_FInterface.o
            elmerf90 $(NETCDFINC) $^ $(NNLIB) $(CSLIB) -o $(ExecPath)/$@ $(NETCDFLIBS)
  
    $(ExecPath)/csa_interpolate_points.o : $(ELMER_Scatter)/csa_interpolate_points.c
            $(CC) $(CCFLAG) -I$(CS_HOME) -c $^ -o $@

    $(ExecPath)/Scattered2D_FInterface.o : $(ELMER_Scatter)/Scattered2D_FInterface.F90
            elmerf90 -c $< -o $@
```

```shell
nproc        # to know how many available procs
make -j8     # or make install if only one proc
make install
```

Then, to check compilation:
```shell
ctest -L elmerice-fast   # should pass all the tests
```

### Install the Elmer/Ice-NEMO Coupling interface

First, install NetCDF-c++ library if not already installed. Choose your version on http://www.unidata.ucar.edu/downloads/netcdf

```shell
cd ~/util
wget https://github.com/Unidata/netcdf-cxx4/archive/v4.2.1.tar.gz
tar xzvf v4.2.1.tar.gz
cd netcdf-cxx4-4.2.1
mkdir BUILD
./configure --prefix=${HOME}/util/netcdf-cxx4-4.2.1/BUILD
make
make install
make check # should pass 7/7
```

**NB:** if needed (quota), you can remove cxx4/ and examples/ once compiled

### Install the coupling tools to transform VTK to netcdf

This is used to write Elmer/Ice's ice draft in NEMO's netcdf format.

```shell
cd ~/CPL_NEMO_ELMER/From_VTK_to_NetCDF
vi CMakeLists.txt
```
Adapt these lines according to previous steps :
```console
 set(VTK_DIR "/home/${USER}/util/VTK-8.0.0/build")
```
and:
```console
 include_directories("/home/${USER}/util/netcdf-cxx4-4.2.1/BUILD/include/")    
```

Then:
```shell
mkdir build
cd build
ccmake ..
```

Adjust:
```console
 CMAKE_INSTALL_PREFIX             /home/${USER}/CPL_NEMO_ELMER/From_VTK_to_NetCDF/build
 NETCDF_LIB                       /home/${USER}/util/netcdf-cxx4-4.2.1/BUILD/lib/libnetcdf_c++4.so
```

Then:
```shell
make
```

### Get the Elmer Solver to feed Elmer with NEMO's melt rates in the context of MISOMIP (and other modified solvers for MISOMIP)

The routines used by Elmer/Ice to read NEMO's melt rates are here:
```shell
ls -al ~/CPL_NEMO_ELMER/MISOMIP_Melt
```

There are specific routines (not yet in standard Elmer/Ice release) in the following folder (check on the "LGGE wiki" https://groupes.renater.fr/wiki/elmerice/elmericegit and if needed put your own solvers in it):
```shell
ls -al ~/CPL_NEMO_ELMER/My_ElmerSolver
```

------------------------------

## Prepare Elmer/Ice-NEMO run using the Config Manager (CM)

Prepare working directories:
```shell
export WORK=/scratch/shared/egige60    ## to adapt, typically $SCRATCHDIR
mkdir $WORK/NEMO_MISOMIP
mkdir $WORK/ELMER_MISOMIP
```

Prepare NEMO:
```shell
cd $WORK/NEMO_MISOMIP
ln -s -v ~/CPL_NEMO_ELMER/NEMO_FILES FILES   ## (if needed, adapt NEMO's xml, namelist, f90, rebuild, etc)
mkdir input   ## here put NEMO's inputs (bathy, dta, resto, ...)
              ## NB: you can use fortran scripts in ~/CPL_NEMO_ELMER/BUILD_NEMO_INPUT to build the netcdf input files.
mkdir output  ## where NEMO's netcdf output files will go.
mkdir restart ## where NEMO's netcdf restart files will go.
mkdir run     ## will be filled by the config manager.
```

Get and use the Configuration Manager:
```shell
cd ~/CPL_NEMO_ELMER/CM_MISOMIP
vi Makefile_G    ## (_G -> Generic) Adapt libraries and path to your case if needed
vi createRUN.sh  ## Adapt to the run you want to initialize (see built-in comments)
```
Examples of createRUN.sh can be found in the Template directory.

You will also need to edit the following files :
```shell
vi $WORK/NEMO_MISOMIP/FILES/namelist_nemo_GENERIC_ISOMIP         ## Choose NEMO's options
vi ~/CPL_NEMO_ELMER/CM_MISOMIP/Templates/Sif/scketchIce1r_SSAStar_fromNEMO.sif  
             ## Choose Elmer/Ice's options (including MISOMIP_Melt_Consv vs MISOMIP_Melt_Consv_Evolv)
             ## WARNING : the sif are copied from this file at each submission,
             ##   so not possible to run 2 runs with different sif simultaneously !!
             ##   [ or change variable scketch in WORK_ELMER/scriptIce1rExecute.sh once the run is prepared ].
vi ~/CPL_NEMO_ELMER/CM_MISOMIP/Scripts/run_nemo_ISOMIP.sh        ## Adapth paths and executable directories
vi ~/CPL_NEMO_ELMER/CM_MISOMIP/Scripts/scriptIce1rExecute.sh     ## Adapt sif name, Elmer cstes, SBATCH walltime, etc
vi ~/CPL_NEMO_ELMER/CM_MISOMIP/Scripts/scriptIce1aExecute.sh     ## Adapt sif name, Elmer cstes, SBATCH walltime, etc
```

---------------------------------

## Run a coupled simulation

For each case (i.e. set of parameters, boundary conditions, etc), choose a case name, e.g. :

```shell
export CASE="CPL06_hmin20"  ## CASE name (should be different for each simulation)
cd ~/CPL_NEMO_ELMER/CM_MISOMIP
vi createRUN.sh # indicate pathways, coupling frequency, etc
vi Scripts/run_nemo_ISOMIP.sh # change if needed
./createRUN.sh ${CASE}
```

**NB:** when you execute createRUN.sh, a copy of createRUN.sh is saved under the ```RUNS/createRUN``` directory so that you can easily duplicate a run or be sure to change only one parameter.

Compile Elmer/Ice:
```shell
cd RUNS/${CASE}/WORK_ELMER
make all
```

Check values in scriptIce1rExecute.sh :
* sbatch parameters (run duration, etc)
* sif parameters

```shell
cd ../WORK_NEMO
```
Check namelist parameters, xml files, batch parameters (if not correct, adapt scripts in ```$WORK/NEMO_MISOMIP/FILES```)
```shell
cd ..
```

Then, you have two options:
1. To start with an ocean at rest: 
```shell
./script_RUN_MISOMIP.sh
```
2. To start from an ocean restart (e.g. from a spin up with imposed geometry) :
```shell
./script_Start_From_Restart.sh
```
You can follow soma basic diagnostics in COUPLED_Run.db

**NB:** to increase NRUN_MAX during the simulation, you can do:
```shell
./script_Exec_MISOMIP.sh ${NEW_RUN_MAX}
```

**NB:** To restart a job that failed :
```shell
cd ~/CPL_NEMO_ELMER/CM_MISOMIP/RUNS/WORK_NEMO/${CASE}
```
Check the last standard error output, e.g. run_nemo.eXXXX. You should find something like: 
```console
./scriptIce1rExecute.sh 20 3247833 /scratch/cnt0021/gge6066/njourdain/NEMO_MISOMIP//output/nemo_ISOMIP_CPLFREQ1yr_hmin30/0019
```
Re-execute this script with the same line. If the path are not correct, clean by re-executing the ```./createRUN.sh ${CASE}``` in the config manager.

**NB:** If NEMO crashes:
```shell
vi prod_nemo.db  ## remove last line
qsub run_nemo_ISOMIP.sh 1 <restart_file>
```
