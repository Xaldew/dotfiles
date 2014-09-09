source /arm/tools/setup/init/bash

module load eda
module load swdev
module load util
module load arm/cluster
# Only load workflow for the EUHPC cluster.
if [[ `hostname -d` = "euhpc.arm.com" ]]; then
  module load workflow 
  module load workflow/mp/regress-AUDA-mti
fi



# Cleanup
module unload tigris/subversion
module unload apache/subversion
module unload mentor/modelsim
module unload mentor/questasim
module unload synopsys/vcs
module unload synopsys/synthesis
module unload arm/rascdevkit
module unload gnu/make
module unload gnu/gcc
module unload gnu/gcc-arm
module unload gnu/valgrind
module unload tortall/yasm
module unload python/python
module unload swig/swig
module unload synopsys/synplify
module unload xilinx/ise
module unload synopsys/formality
module unload atrenta/spyglass
module unload synopsys/icc
module unload synopsys/primetime
module unload synopsys/starrc
module unload synopsys/tetramax
module unload synopsys/mvtools
module unload novas/verdi
module unload sun/jdk
module unload arm/vpe301
module unload python/matplotlib_py2.7
module unload python/numpy2.7
module unload python/scipy_py2.7
module unload jasper/jaspergold
module unload novas/verdi
module unload novas/verdi3


# Load tools
module load apache/subversion/1.7.3
#module load mentor/modelsim/10.0a
module load mentor/questasim/10.2b
module load synopsys/vcs/2011.12
module load synopsys/synthesis/2012.06-SP5
module load cadence/incisive/12.10.011
module load arm/rascdevkit/1.1.4
module load gnu/make/3.81
module load gnu/gcc/4.1.2
module load gnu/gcc-arm/4.2.0
module load gnu/valgrind/3.8.1
module load tortall/yasm/0.8.0
module load python/python/2.7.8
module load python/matplotlib_py2.7/1.2.1 
module load python/numpy2.7/1.6.2 
module load python/scipy_py2.7/0.12.0
module load swig/swig/1.3.40
module load synopsys/synplify/2011.03_Patched
module load xilinx/ise/12.4
module load synopsys/formality/2012.06-SP5
module load atrenta/spyglass/5.0.0_EngBuild

module load synopsys/icc/2012.06-SP5
module load synopsys/primetime/2010.12-SP3
module load synopsys/starrc/2010.12-SP3-2
module load synopsys/tetramax/2010.12-SP4
module load synopsys/mvtools/2011.03
#module load novas/verdi/201201v3
#module load novas/verdi/2012.10
module load novas/verdi3/2013.09
# Loading sun/jdk for now, this should be removed when vpe301 has been confirmed to work without JDK.
# Please also remove the module unload sun/jdk when removing module load sun/jdk.
module load sun/jdk
module load arm/vpe301/r0p1-00rel0
module load jasper/jaspergold/2013.12p001

# Set SVN
export SVNROOT=http://lun-svn1.lund.arm.com/svn/mpd/video

# Avoid warnings from vcs about unsupported system
export VCS_ARCH_OVERRIDE=linux

export MTI_VCO_MODE=32 ; # force modelsim to use 32-bit mode (required for RCPU etc)
#export MTI=${MODELTECH_HOME}
# When we switch to Questasim the one below should be used instead:
export MTI=${QUESTASIM_HOME}

# Only define DESIGNKIT on the Lund cluster.
if [[ `hostname -d` = "lund.arm.com" ]]; then
  export DESIGNKIT=/projects/mpd/designkit
fi

this_dir=$(dirname $BASH_SOURCE)
full_path=$(cd $this_dir; pwd)
export MODELSIM=${full_path}/../../modelsim.ini
