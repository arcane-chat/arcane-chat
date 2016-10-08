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

set(QTGSTREAMER_PACKAGE_NAME Qt5GStreamer)
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
    global.h      Global
    init.h        Init
    quark.h       Quark
    type.h        Type
    refpointer.h  RefPointer
    wrap.h
    paramspec.h   ParamSpec
    object.h      Object
    value.h       Value
    qglib_signal.h Signal
    emitimpl.h
    connect.h     Connect
    connectimpl.h
    error.h       Error
)

set(QtGStreamer_INSTALLED_HEADERS
    global.h            Global
    init.h              Init
    enums.h
    structs.h           Fourcc
                        Fraction
                        IntRange
                        Int64Range
                        DoubleRange
                        FractionRange
    structure.h         Structure
    caps.h              Caps
    miniobject.h        MiniObject
    object.h            Object
    pad.h               Pad
    ghostpad.h          GhostPad
    element.h           Element
    pluginfeature.h     PluginFeature
    elementfactory.h    ElementFactory
    bin.h               Bin
    childproxy.h        ChildProxy
    pipeline.h          Pipeline
    message.h           Message
    bus.h               Bus
    parse.h             Parse
    urihandler.h        UriHandler
    videoorientation.h  VideoOrientation
    videooverlay.h      VideoOverlay
    streamvolume.h      StreamVolume
    colorbalance.h      ColorBalance
    query.h             Query
    clock.h             Clock
    buffer.h            Buffer
    sample.h            Sample
    allocator.h         Allocator
    memory.h            Memory
    event.h             Event
    clocktime.h         ClockTime
    taglist.h           TagList
    bufferlist.h        BufferList
    discoverer.h        Discoverer
    segment.h           Segment

    Ui/global.h
    Ui/videowidget.h            Ui/VideoWidget
    Ui/graphicsvideosurface.h   Ui/GraphicsVideoSurface
    Ui/graphicsvideowidget.h    Ui/GraphicsVideoWidget

    Utils/global.h
    Utils/applicationsink.h     Utils/ApplicationSink
    Utils/applicationsource.h   Utils/ApplicationSource
)

set(QTGLIB_LIBRARY Qt5GLib)
set(QTGSTREAMER_UTILS_LIBRARY Qt5GStreamerUtils)
set(QTGSTREAMER_LIBRARY Qt5GStreamer)

set(QTGSTREAMER_PC_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
set(QTGSTREAMER_PACKAGE_NAME Qt5GStreamer)
set(EXPORT_TARGET_SET ${QTGSTREAMER_PACKAGE_NAME}Targets)
set(QTGSTREAMER_INSTALL_TARGET_DEFAULT_ARGS
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Macro to install headers from the subdirs
macro(install_headers _dir_name prefix)
    foreach(header ${ARGN})
        get_filename_component(header_path ${header} PATH)
        install(FILES ${prefix}${header} DESTINATION ${QTGSTREAMER_INCLUDES_INSTALL_DIR}/${_dir_name}/${header_path})
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
install_headers("QGlib" "src/QGlib/" ${QtGLib_INSTALLED_HEADERS})
install_headers("QGst" "src/QGst/" ${QtGStreamer_INSTALLED_HEADERS})

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
