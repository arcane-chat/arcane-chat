cmake_minimum_required (VERSION 2.8.11)
project(fuspr-chat)

add_subdirectory(doc)

find_package(PkgConfig REQUIRED)
find_package(Boost     REQUIRED program_options thread filesystem)

pkg_check_modules(ZMQ REQUIRED libzmq)
pkg_check_modules(MSG REQUIRED msgpack)
pkg_check_modules(QT5 REQUIRED Qt5Gui)
pkg_check_modules(TOX REQUIRED libtoxcore)

set(CMAKE_AUTOMOC ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Qt5Gui REQUIRED)
#find_package(LibObs REQUIRED)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} -g -std=c11 -Werror"
)

set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -g -std=c++14 -Werror"
)

include_directories(
  ${CMAKE_SOURCE_DIR}/include
  ${QT5_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIRS}
  ${ZMQ_INCLUDE_DIRS}
  ${MSG_INCLUDE_DIRS}
  ${TOX_INCLUDE_DIRS}
  #${LIBOBS_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
  ${ZMQ_LIBRARY_DIRS}
  ${MSG_LIBRARY_DIRS}
  ${TOX_LIBRARY_DIRS}
)

add_definitions(
  ${QT5_CFLAGS_OTHER}
  ${Boost_CFLAGS_OTHER}
  ${ZMQ_CFLAGS_OTHER}
  ${MSG_CFLAGS_OTHER}
  ${TOX_CFLAGS_OTHER}
)

set(GENERAL_LIBRARIES
  ${Boost_LIBRARIES}
  ${ZMQ_LIBRARIES}
  ${QT5_LIBRARIES}
  ${TOX_LIBRARIES}
  #${LIBOBS_LIBRARIES}
)

# set(PLUGIN_SOURCES
#   src/plugin.cpp
#   src/rpc.cpp
# )

# add_library(fuspr SHARED fuspr.cpp)
# target_link_libraries(fuspr ${GENERAL_LIBRARIES})

# add_executable(integration_test src/integration_test.cpp ${PLUGIN_SOURCES})
# target_link_libraries(integration_test ${GENERAL_LIBRARIES})

add_executable(chat src/chat.cpp src/core.cpp src/utils.cpp src/tracer.cpp)
target_link_libraries(chat ${GENERAL_LIBRARIES})

install(
  TARGETS chat
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
)
