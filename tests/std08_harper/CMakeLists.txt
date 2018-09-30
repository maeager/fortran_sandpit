
 add_custom_target(check_harper COMMAND ${CMAKE_CTEST_COMMAND})
 add_dependencies ( check  check_harper )
 file ( GLOB HARPER_TEST_SRCS "${CMAKE_SOURCE_DIR}/std08_harper/*.f90" )
 find_program ( DIFF     diff )
 set ( UNIT_TESTS '' )
  foreach ( UNIT_TEST ${HARPER_TEST_SRCS} )
    get_filename_component ( TEST ${UNIT_TEST} NAME_WE )
    if(MSVC_IDE)
    link_directories(${CMAKE_BINARY_DIR}/lib)
    endif()
    add_executable ( ${TEST} EXCLUDE_FROM_ALL ${UNIT_TEST} )
    target_link_libraries ( ${TEST} ${SIMPLELIB} )
    add_dependencies ( check_harper ${TEST} )
    set_target_properties ( ${TEST}
      PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
    add_test( NAME ${TEST}
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin
      COMMAND ./${TEST})
    list ( APPEND UNIT_TESTS ${TEST} )

  endforeach ( UNIT_TEST )
