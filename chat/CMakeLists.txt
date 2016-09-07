cmake_minimum_required (VERSION 2.8.11)
project(arcane-chat)

add_subdirectory(doc)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
include(cotire)

find_package(PkgConfig REQUIRED)
find_package(Boost     REQUIRED program_options thread filesystem)

pkg_check_modules(ZMQ REQUIRED libzmq)
pkg_check_modules(MSG REQUIRED msgpack)
pkg_check_modules(TOX REQUIRED libtoxcore)
pkg_check_modules(X11 REQUIRED x11 x11-xcb)
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

find_package(Qt5Core      REQUIRED)
find_package(Qt5Gui       REQUIRED)
find_package(Qt5Widgets   REQUIRED)
find_package(Qt5GStreamer REQUIRED)
find_package(LibObs       REQUIRED)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} -g -std=c11 -Wall"
)

set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -g -std=c++17 -Wall -Wno-deprecated-declarations"
)

include_directories(
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_BINARY_DIR}
  ${Boost_INCLUDE_DIRS}
  ${ZMQ_INCLUDE_DIRS}
  ${MSG_INCLUDE_DIRS}
  ${TOX_INCLUDE_DIRS}
  ${GST_INCLUDE_DIRS}
  ${X11_INCLUDE_DIRS}
  ${LIBOBS_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
  ${ZMQ_LIBRARY_DIRS}
  ${MSG_LIBRARY_DIRS}
  ${TOX_LIBRARY_DIRS}
  ${GST_LIBRARY_DIRS}
  ${X11_LIBRARY_DIRS}
)

add_definitions(
  ${Boost_CFLAGS_OTHER}
  ${ZMQ_CFLAGS_OTHER}
  ${MSG_CFLAGS_OTHER}
  ${TOX_CFLAGS_OTHER}
  ${GST_CFLAGS_OTHER}
  ${X11_CFLAGS_OTHER}
)

set(GENERAL_LIBRARIES
  Qt5::Core
  Qt5::Gui
  Qt5::Widgets
  Qt5GLib
  Qt5GStreamer
  Qt5GStreamerUi
  Qt5GStreamerUtils
  ${Boost_LIBRARIES}
  ${ZMQ_LIBRARIES}
  ${TOX_LIBRARIES}
  ${GST_LIBRARIES}
  ${X11_LIBRARIES}
  ${LIBOBS_LIBRARIES}
)

QT5_WRAP_UI(UI_HEADERS src/mainwindow.ui src/chatwidget.ui src/infowidget.ui)

set(CHAT_LIB_SOURCES
  src/core.cpp
  src/friend.cpp
  src/utils.cpp
  src/options.cpp
  src/mainwindow.cpp
  src/chatwidget.cpp
  src/infowidget.cpp
  src/channelmodel.cpp
  ${UI_HEADERS}
)

add_library(chat SHARED ${CHAT_LIB_SOURCES})
target_link_libraries(chat ${GENERAL_LIBRARIES})
set_property(TARGET chat PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
cotire(chat)

add_executable(client src/client.cpp)
target_link_libraries(client chat ${GENERAL_LIBRARIES})
set_property(TARGET client PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
cotire(client)

add_executable(server src/server.cpp)
target_link_libraries(server chat ${GENERAL_LIBRARIES})
set_property(TARGET server PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
cotire(server)

add_executable(gst src/gst.cpp src/toxoutputstream.cpp)
target_link_libraries(gst ${GENERAL_LIBRARIES})
set_property(TARGET gst PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
cotire(gst)

install(
  TARGETS client gst chat
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
)
