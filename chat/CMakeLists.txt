cmake_minimum_required (VERSION 2.8.11)
project(fuspr-chat)

add_subdirectory(doc)

find_package(PkgConfig REQUIRED)
find_package(Boost     REQUIRED program_options thread filesystem)

pkg_check_modules(ZMQ REQUIRED libzmq)
pkg_check_modules(MSG REQUIRED msgpack)
pkg_check_modules(TOX REQUIRED libtoxcore)
pkg_check_modules(GST REQUIRED
  gstreamer-1.0
  gstreamermm-1.0
  gstreamer-base-1.0
  gstreamer-plugins-base-1.0
  gstreamer-app-1.0
  gstreamer-audio-1.0
  gstreamer-video-1.0
  gstreamer-check-1.0
  gstreamer-allocators-1.0
  gstreamer-controller-1.0
  gstreamer-pbutils-1.0
  gstreamer-fft-1.0
  gstreamer-riff-1.0
  gstreamer-net-1.0
  gstreamer-rtp-1.0
  gstreamer-rtsp-1.0
  gstreamer-sdp-1.0
  gstreamer-tag-1.0)

set(CMAKE_AUTOMOC ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(Qt5Gui REQUIRED)
find_package(Qt5Widgets REQUIRED)
#find_package(LibObs REQUIRED)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} -g -std=c11 -Werror"
)

set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -g -std=c++14 -Werror"
)

include_directories(
  ${CMAKE_SOURCE_DIR}/include
  ${Boost_INCLUDE_DIRS}
  ${ZMQ_INCLUDE_DIRS}
  ${MSG_INCLUDE_DIRS}
  ${TOX_INCLUDE_DIRS}
  ${GST_INCLUDE_DIRS}
  ${CMAKE_BINARY_DIR}
  #${LIBOBS_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${Qt5Widgets_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
  ${ZMQ_LIBRARY_DIRS}
  ${MSG_LIBRARY_DIRS}
  ${TOX_LIBRARY_DIRS}
  ${GST_LIBRARY_DIRS}
  ${Qt5Gui_LIBRARY_DIRS}
  ${Qt5Widgets_LIBRARY_DIRS}
)

add_definitions(
  ${Boost_CFLAGS_OTHER}
  ${ZMQ_CFLAGS_OTHER}
  ${MSG_CFLAGS_OTHER}
  ${TOX_CFLAGS_OTHER}
  ${GST_CFLAGS_OTHER}
  ${Qt5Gui_CFLAGS_OTHER}
  ${Qt5Widgets_CFLAGS_OTHER}
  -Wno-error=deprecated-declarations
)

set(GENERAL_LIBRARIES
  ${Boost_LIBRARIES}
  ${ZMQ_LIBRARIES}
  ${TOX_LIBRARIES}
  ${GST_LIBRARIES}
  ${Qt5Gui_LIBRARIES}
  ${Qt5Widgets_LIBRARIES}
  #${LIBOBS_LIBRARIES}
)

QT5_WRAP_UI(UIS_HDRS src/mainwindow.ui src/chatwidget.ui)

set(CHAT_SOURCES
  src/core.cpp
  src/utils.cpp
  src/tracer.cpp
  src/mainwindow.cpp
  src/friend.cpp
  src/channelmodel.cpp
  src/chatwidget.cpp
)

# add_library(fuspr SHARED fuspr.cpp)
# target_link_libraries(fuspr ${GENERAL_LIBRARIES})

# add_executable(integration_test src/integration_test.cpp ${PLUGIN_SOURCES})
# target_link_libraries(integration_test ${GENERAL_LIBRARIES})

add_executable(chat src/chat.cpp)
target_link_libraries(chat ${GENERAL_LIBRARIES})

add_executable(gst src/gst.cpp)
target_link_libraries(gst ${GENERAL_LIBRARIES})

install(
  TARGETS chat
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
)
