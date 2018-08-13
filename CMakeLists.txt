cmake_minimum_required(VERSION 3.2 ) #FATAL_ERROR)
if(EXISTS "$ENV{SIMPLE_PATH}")
  message (STATUS " SIMPLE_PATH = $ENV{SIMPLE_PATH}")
else()
  message(STATUS "Could not find SIMPLE_PATH environment variable SIMPLE_PATH = $ENV{SIMPLE_PATH}")
endif()
set(SIMPLE_PATH "$ENV{SIMPLE_PATH}")
 message (STATUS " SIMPLE_PATH = ${SIMPLE_PATH}")
if(IS_ABSOLUTE "${SIMPLE_PATH}" AND IS_DIRECTORY "${SIMPLE_PATH}")
  message(STATUS " Found SIMPLE ")
  if(EXISTS "${SIMPLE_PATH}/lib/simple/CMake/SIMPLEConfigVersion.cmake")
    include("${SIMPLE_PATH}/lib/simple/CMake/SIMPLEConfigVersion.cmake")
    if(NOT EXISTS "${SIMPLE_SOURCE_PATH}" )
       message(FATAL_ERROR "Could not find SIMPLE_SOURCE_PATH ")
    endif()
    if(DEFINED "${SIMPLE_VERSION}")
       message(FATAL_ERROR "Could not find SIMPLE version")
    endif()
    endif()
endif()
# Define the library name lib<name>.so
set(SIMPLELIB "${SIMPLE_PATH}/lib/libSIMPLE${SIMPLE_VERSION}.a")
set(SIMPLE_INCLUDE_PATH "${SIMPLE_SOURCE_PATH}/src/inc")
set(CMAKE_MODULE_PATH "${SIMPLE_SOURCE_PATH}/cmake" "${SIMPLE_SOURCE_PATH}/cmake/Modules/")
include(FortranOverride)
project(simple_cheker Fortran)

set(PROJECT_BRIEF "Slof is a program package for checking SIMPLE's library operations in Fortran")

#################################################################
#
# VERSION
#
#################################################################
set(SLOF_VERSION_MAJOR 3)
set(SLOF_VERSION_MINOR 0)
set(SLOF_VERSION_PATCH 0)
set(SLOF_VERSION "${SLOF_VERSION_MAJOR}.${SLOF_VERSION_MINOR}.${SLOF_VERSION_PATCH}")

#################################################################
#
# OPTIONS
#
#################################################################
option(USE_OPENMP  "Use OpenMP for parallelization - debugging may disable OpemMP" ON )
option(USE_MPI     "Use OpenMPI for parallelization - in developement" OFF )
option(USE_OPENACC "Use OpenACC for auto GPU parallelization - debugging may disable OpenMP" OFF)
option(USE_CUDA    "GPU NVIDIA - in development" OFF)

option(${PROJECT_NAME}_BUILD_TESTS "Build the test examples" ON)
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/gui)
  option(${PROJECT_NAME}_BUILD_GUI "Build the simple GUI " ON)
endif()
option(BUILD_SHARED_LIBS "Build the simple library as shared or static (default)" OFF)

#################################################################
#
# SET default flags
#
#################################################################
# Add our local modules to the module path
#set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")
set(EXTRA_LIBS "")
set(NUM_JOBS 1)
include(ProcessorCount)
ProcessorCount(NUM_JOBS)
set(BUILD_NAME "${CMAKE_Fortran_COMPILER_ID}_${CMAKE_BUILD_TYPE}" CACHE STRING "Build name with compiler, type and FFT backend ")

############################################################
# Define the main targets
############################################################


set(EXECUTABLE_OUTPUT_DIRECTORY "${CMAKE_INSTALL_BINDIR}")
set(LIBRARY_OUTPUT_DIRECTORY "${CMAKE_INSTALL_LIBDIR}")


# Define some directories
include_directories(${SIMPLE_INCLUDE_PATH})
include_directories(${SIMPLE_PATH}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${SIMPLE_PATH}/lib)
link_directories(${SIMPLE_PATH}/lib)
#################################################################
# INCLUDES
#################################################################
# include(CMakeAddFortranSubdirectory)
include(CheckIncludeFiles)
include(SimpleFortranOptions)


# use _DEBUG since DEBUG or debug is used as a variable in some files
# _DEBUG is defined by Intel Fortran on Windows
if(CMAKE_BUILD_TYPE STREQUAL "DEBUG")
  add_definitions("-D_DEBUG")
endif()


#################################################################
#
# STATIC LIBRARIES
#
#################################################################
# Try to search first static libraries
if(NOT ${BUILD_SHARED_LIBS})
  set(CMAKE_FIND_LIBRARY_SUFFIXES "${CMAKE_FIND_LIBRARY_SUFFIXES} .a")
endif()


  #
  # CTest
  #
  enable_testing()
  # emulate GNU Autotools `make check`
  add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND})
  include(JsonFortran)

include ( HarperStd08 )

#################################################################
#
# ADD Test directories
#
#################################################################
file(GLOB TESTDIRS
  LIST_DIRECTORIES true
  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
  "*"
  )

add_subdirectory(tests           bin)
add_subdirectory(cublas          bin)
add_subdirectory(cufft           bin)
add_subdirectory(intel           bin)
add_subdirectory(memleaks        bin)
add_subdirectory(mpi_cuda_omp    bin)
add_subdirectory(mpimp           bin)
add_subdirectory(openacc         bin)
add_subdirectory(stats           bin)
#add_subdirectory(std08_harper    bin)
add_subdirectory(varying_strings bin)
