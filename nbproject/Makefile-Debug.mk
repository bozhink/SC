#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=GNU-Linux-x86
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/findEsEf.o \
	${OBJECTDIR}/src/superconductivity/fermi.o \
	${OBJECTDIR}/src/superconductivity/blockdat.o \
	${OBJECTDIR}/src/superconductivity/esef.o \
	${OBJECTDIR}/src/integ2d/twodq/xerrwv.o \
	${OBJECTDIR}/src/integ2d/twodq/greatr.o \
	${OBJECTDIR}/src/tcef.o \
	${OBJECTDIR}/src/surf.o \
	${OBJECTDIR}/src/integ2d/integ2d.o \
	${OBJECTDIR}/src/integ2d/twodq/twodq.o \
	${OBJECTDIR}/src/integ2d/triangulation/triangulation.o \
	${OBJECTDIR}/src/integ2d/twodq/basout.o \
	${OBJECTDIR}/src/vanHove.o \
	${OBJECTDIR}/src/scdos.o \
	${OBJECTDIR}/src/superconductivity/tcritical.o \
	${OBJECTDIR}/src/integ2d/twodq/hpdel.o \
	${OBJECTDIR}/main.o \
	${OBJECTDIR}/src/superconductivity/spec.o \
	${OBJECTDIR}/src/integ2d/twodq/hpins.o \
	${OBJECTDIR}/src/superconductivity/cec.o \
	${OBJECTDIR}/src/integ2d/triangulation/setriang.o


# C Compiler Flags
CFLAGS=-lblas -llapack

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=-lblas -llapack

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/sc

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/sc: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/sc ${OBJECTFILES} ${LDLIBSOPTIONS} 

${OBJECTDIR}/src/findEsEf.o: src/findEsEf.f 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/findEsEf.o src/findEsEf.f

${OBJECTDIR}/src/superconductivity/fermi.o: src/superconductivity/fermi.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/fermi.o src/superconductivity/fermi.f

${OBJECTDIR}/src/superconductivity/blockdat.o: src/superconductivity/blockdat.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/blockdat.o src/superconductivity/blockdat.f

${OBJECTDIR}/src/superconductivity/esef.o: src/superconductivity/esef.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/esef.o src/superconductivity/esef.f

${OBJECTDIR}/src/integ2d/twodq/xerrwv.o: src/integ2d/twodq/xerrwv.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/xerrwv.o src/integ2d/twodq/xerrwv.f

${OBJECTDIR}/src/integ2d/twodq/greatr.o: src/integ2d/twodq/greatr.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/greatr.o src/integ2d/twodq/greatr.f

${OBJECTDIR}/src/tcef.o: src/tcef.f 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/tcef.o src/tcef.f

${OBJECTDIR}/src/surf.o: src/surf.f 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/surf.o src/surf.f

${OBJECTDIR}/src/integ2d/integ2d.o: src/integ2d/integ2d.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/integ2d.o src/integ2d/integ2d.f

${OBJECTDIR}/src/integ2d/twodq/twodq.o: src/integ2d/twodq/twodq.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/twodq.o src/integ2d/twodq/twodq.f

${OBJECTDIR}/src/integ2d/triangulation/triangulation.o: src/integ2d/triangulation/triangulation.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/triangulation
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/triangulation/triangulation.o src/integ2d/triangulation/triangulation.f

${OBJECTDIR}/src/integ2d/twodq/basout.o: src/integ2d/twodq/basout.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/basout.o src/integ2d/twodq/basout.f

${OBJECTDIR}/src/vanHove.o: src/vanHove.f 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/vanHove.o src/vanHove.f

${OBJECTDIR}/src/scdos.o: src/scdos.f 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/scdos.o src/scdos.f

${OBJECTDIR}/src/superconductivity/tcritical.o: src/superconductivity/tcritical.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/tcritical.o src/superconductivity/tcritical.f

${OBJECTDIR}/src/integ2d/twodq/hpdel.o: src/integ2d/twodq/hpdel.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/hpdel.o src/integ2d/twodq/hpdel.f

${OBJECTDIR}/main.o: main.c 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.c) -g -MMD -MP -MF $@.d -o ${OBJECTDIR}/main.o main.c

${OBJECTDIR}/src/superconductivity/spec.o: src/superconductivity/spec.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/spec.o src/superconductivity/spec.f

${OBJECTDIR}/src/integ2d/twodq/hpins.o: src/integ2d/twodq/hpins.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/twodq
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/twodq/hpins.o src/integ2d/twodq/hpins.f

${OBJECTDIR}/src/superconductivity/cec.o: src/superconductivity/cec.f 
	${MKDIR} -p ${OBJECTDIR}/src/superconductivity
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/superconductivity/cec.o src/superconductivity/cec.f

${OBJECTDIR}/src/integ2d/triangulation/setriang.o: src/integ2d/triangulation/setriang.f 
	${MKDIR} -p ${OBJECTDIR}/src/integ2d/triangulation
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/integ2d/triangulation/setriang.o src/integ2d/triangulation/setriang.f

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/sc
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
