project(QtGStreamer)
cmake_minimum_required(VERSION 2.8.9)
set(QTGSTREAMER_VERSION 1.2.0)
set(QTGLIB_API_VERSION 2.0)
set(QTGLIB_SOVERSION 0)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DQT_NO_KEYWORDS")
FIND_PACKAGE(PkgConfig REQUIRED)
find_package(GLIB2)
find_package(GObject)

pkg_search_module(GLIB glib-2.0)

set(QTGSTREAMER_CMAKE_CONFIG_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/${QTGSTREAMER_PACKAGE_NAME})
set(QTGSTREAMER_INCLUDES_INSTALL_DIR ${CMAKE_INSTALL_INCLUDEDIR}/${QTGSTREAMER_PACKAGE_NAME})


set(QtGLib_SRCS
    src/QGlib/quark.cpp
    src/QGlib/type.cpp
    src/QGlib/paramspec.cpp
    src/QGlib/object.cpp
    src/QGlib/value.cpp
    src/QGlib/signal.cpp
    src/QGlib/error.cpp
    src/QGlib/connect.cpp
    src/QGlib/wrap.cpp
    src/QGlib/init.cpp
)
set(QtGLib_INSTALLED_HEADERS
    src/QGlib/global.h      src/QGlib/Global
    src/QGlib/init.h        src/QGlib/Init
    src/QGlib/quark.h       src/QGlib/Quark
    src/QGlib/type.h        src/QGlib/Type
    src/QGlib/refpointer.h  src/QGlib/RefPointer
    src/QGlib/wrap.h
    src/QGlib/paramspec.h   src/QGlib/ParamSpec
    src/QGlib/object.h      src/QGlib/Object
    src/QGlib/value.h       src/QGlib/Value
    src/QGlib/qglib_signal.h src/QGlib/Signal
    src/QGlib/emitimpl.h
    src/QGlib/connect.h     src/QGlib/Connect
    src/QGlib/connectimpl.h
    src/QGlib/error.h       src/QGlib/Error
)
set(QTGLIB_LIBRARY Qt5GLib)
set(QTGSTREAMER_PC_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
set(QTGSTREAMER_PACKAGE_NAME Qt5GStreamer)
set(EXPORT_TARGET_SET ${QTGSTREAMER_PACKAGE_NAME}Targets)
set(QTGSTREAMER_INSTALL_TARGET_DEFAULT_ARGS
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Macro to install headers from the subdirs
macro(install_headers _dir_name)
    foreach(header ${ARGN})
        get_filename_component(header_path ${header} PATH)
        install(FILES ${header} DESTINATION ${QTGSTREAMER_INCLUDES_INSTALL_DIR}/${_dir_name}/${header_path})
    endforeach()
endmacro()


include_directories(
    ${GOBJECT_INCLUDE_DIR} ${GLIB_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)
add_library(${QTGLIB_LIBRARY} ${SHARED_OR_STATIC} ${QtGLib_SRCS})
set_target_properties(${QTGLIB_LIBRARY} PROPERTIES OUTPUT_NAME ${QTGLIB_LIBRARY}-${QTGLIB_API_VERSION}
                                          SOVERSION ${QTGLIB_SOVERSION}
                                            VERSION ${QTGSTREAMER_VERSION})
install(TARGETS ${QTGLIB_LIBRARY} EXPORT ${EXPORT_TARGET_SET} ${QTGSTREAMER_INSTALL_TARGET_DEFAULT_ARGS})
install_headers("QGlib" ${QtGLib_INSTALLED_HEADERS})

# Install pkgconfig files
file(GLOB_RECURSE PC_IN_FILES "*.pc.in")
foreach(pc_in_file ${PC_IN_FILES})
    get_filename_component(pc_out_file ${pc_in_file} NAME)
    string(REPLACE ".pc.in" ".pc" pc_out_file ${pc_out_file})
    string(REPLACE "Qt" "Qt5" pc_out_file ${pc_out_file})
    # Qt4 version of the QtGStreamerQuick .pc file should not be installed
    if (NOT ${pc_out_file} MATCHES "QtGStreamerQuick.*")
        configure_file(${pc_in_file} ${CMAKE_CURRENT_BINARY_DIR}/${pc_out_file} @ONLY)
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${pc_out_file}
                DESTINATION ${QTGSTREAMER_PC_INSTALL_DIR})
    endif()
endforeach()

# Install the cmake scripts that are used to find the installed library from external projects
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_SOURCE_DIR}/cmake/modules/QtGStreamerConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${QTGSTREAMER_PACKAGE_NAME}Config.cmake
    INSTALL_DESTINATION ${QTGSTREAMER_CMAKE_CONFIG_INSTALL_DIR}
    PATH_VARS QTGSTREAMER_INCLUDES_INSTALL_DIR
    NO_CHECK_REQUIRED_COMPONENTS_MACRO)

install(EXPORT ${EXPORT_TARGET_SET} DESTINATION ${QTGSTREAMER_CMAKE_CONFIG_INSTALL_DIR})

write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/${QTGSTREAMER_PACKAGE_NAME}ConfigVersion.cmake
    VERSION ${QTGSTREAMER_VERSION}
    COMPATIBILITY SameMajorVersion)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${QTGSTREAMER_PACKAGE_NAME}Config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${QTGSTREAMER_PACKAGE_NAME}ConfigVersion.cmake
              ${CMAKE_SOURCE_DIR}/cmake/modules/QtGStreamerConfigCommon.cmake
        DESTINATION ${QTGSTREAMER_CMAKE_CONFIG_INSTALL_DIR})
