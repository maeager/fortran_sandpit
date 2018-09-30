
########################################
# Set up how to compile the tests
########################################
file(GLOB TESTDIRS
  LIST_DIRECTORIES true
  RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
  "*"
  )
if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  list(REMOVE_ITEM TESTDIRS "test_pgi")
else()
  list(REMOVE_ITEM TESTDIRS "test_pgi"
    "test_cufft")
endif()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  list(REMOVE_ITEM TESTDIRS "openacc"
    "accuracy")
else()
  list(REMOVE_ITEM TESTDIRS "intel")
endif()

if(NOT USE_CUDA)
 list(REMOVE_ITEM TESTDIRS "cublas")
endif()

if(NOT USE_MPI)
  list(REMOVE_ITEM TESTDIRS "mpimp_benchmark")
endif()


# if(USE_OPENACC)
#   message(STATUS "Searching for BLAS to test OpenACC")
#   set(BLA_F95 ON)
#   set(BLA_VENDOR "ATLAS")
#   find_package(BLAS QUIET)
#   if (BLAS_FOUND AND USE_OPENACC)
#     message(STATUS "Adding BLAS library to tests: L${BLAS_LINKER_FLAGS} ${BLAS_blas_LIBRARY}")
#     endif()
# endif()


message( STATUS "Test directories ${TESTDIRS}")

foreach(TESTNAME ${TESTDIRS})
  set(TESTEXE ${TESTNAME})
  file(GLOB_RECURSE TESTSOURCES LIST_DIRECTORIES true
    "${TESTNAME}/*.[fF][90][0538]")
  if("" STREQUAL "${TESTSOURCES}")
    # message(STATUS " skipping ${TESTNAME} -- no files in dir")
  else()

    add_executable(${TESTEXE} ${TESTSOURCES})
    target_link_libraries(${TESTEXE} ${EXTRA_LIBS})
    if(USE_MPI AND USE_OPENMP)
      target_link_libraries(${TESTEXE}  ${MPI_Fortran_LIBRARIES})
      set_target_properties(${TESTEXE} PROPERTIES
        COMPILE_FLAGS  "${OpenMP_Fortran_FLAGS} ${MPI_Fortran_FLAGS} -fno-range-check"
        LINK_FLAGS "${OpenMP_Fortran_FLAGS} ${MPI_Fortran_LINK_FLAGS}"
        LANGUAGE Fortran)
    elseif(USE_MPI)
      target_link_libraries(${TESTEXE}  ${MPI_Fortran_LIBRARIES})
      set_target_properties(${TESTEXE} PROPERTIES
        COMPILE_FLAGS  "${MPI_Fortran_FLAGS} -fno-range-check"
        LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}"
        LANGUAGE Fortran)
    elseif(USE_OPENMP)
      if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
	      set_target_properties(${TESTEXE} PROPERTIES
          COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
          LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
          LANGUAGE Fortran)
      else()
	      set_target_properties(${TESTEXE} PROPERTIES
          COMPILE_FLAGS "${OpenMP_Fortran_FLAGS} -fno-range-check"
          LINK_FLAGS "${OpenMP_Fortran_FLAGS}"
          LANGUAGE Fortran)
      endif()
    endif()
    if(USE_CUDA)
      file(GLOB_RECURSE TESTCUDASOURCES LIST_DIRECTORIES true
          "tests/${TESTNAME}/*.cu")
       if("" STREQUAL "${TESTCUDASOURCES}")
         # message(STATUS " skipping ${TESTNAME} -- no files in dir")
       else()
         CUDA_COMPILE( ${TESTEXE}_cuda_kernels ${TESTCUDASOURCES}
           OPTIONS
         DEBUG -g
         RELEASE --use_fast_math
         RELWITHDEBINFO --use_fast_math;-g
         MINSIZEREL --use_fast_math)
       target_link_libraries(${TESTEXE} ${TESTEXE}_cuda_kernels)
     endif()
     CUDA_ADD_CUBLAS_TO_TARGET(${TESTEXE})
     target_link_libraries(${TESTEXE} cuda_kernels stdc++)
    endif()
    # if(BLAS_FOUND AND USE_OPENACC)
    #   message(STATUS "Adding  ${BLAS_blas_LIBRARY} library to ${TESTEXE}")
    #   target_link_libraries(${TESTEXE} ${BLAS_blas_LIBRARY})
    #   set_target_properties(${TESTEXE} PROPERTIES
    #     COMPILE_FLAGS "${OpenMP_Fortran_FLAGS} -fno-range-check"
    #     LINK_FLAGS "${OpenMP_Fortran_FLAGS} -L${BLAS_LINKER_FLAGS}"
    #     LANGUAGE Fortran)
    # else()
   # endif()
    install(TARGETS ${TESTEXE}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_BINDIR}
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

    file(GLOB_RECURSE TESTTXTSOURCES LIST_DIRECTORIES true "tests/${TESTNAME}/*.txt")
    if(NOT "${TESTTXTSOURCES}" STREQUAL "")
    foreach(TXTNAME "${TESTTXTSOURCES}")
       install(FILES ${TXTNAME}
       DESTINATION ${CMAKE_INSTALL_PREFIX}/tests/${TESTNAME}
       CONFIGURATIONS DEBUG|RELEASE
       PERMISSIONS  OWNER_READ OWNER_WRITE GROUP_READ GROUP_WRITE WORLD_READ WORLD_WRITE
       )
endforeach()
    endif()
  endif()
endforeach(TESTNAME)
