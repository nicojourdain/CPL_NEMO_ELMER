
cmake_minimum_required(VERSION 3.5.0)

PROJECT(fromVTKtoNetcdf)

set(VTK_DIR "/home/njourd/util/VTK-8.0.0/build")
find_package(VTK REQUIRED)
include(${VTK_USE_FILE})

add_executable(fromVTKtoNetcdf MACOSX_BUNDLE fromVTKtoNetcdf)

include_directories("/home/njourd/util/netcdf-cxx4-4.2.1/BUILD/include/")
#link_directories("/home/imerino/CODES/netcdf-cxx-4.2/BUILD/lib/")

#link_directories("/../../../CODES/netcdf-cxx-4.2/BUILD/lib/")

find_library(NETCDF_LIB NAMES netcdf_c++ PATHS /home/imerino/CODES/netcdf-cxx-4.2/BUILD/lib/)

target_link_libraries(fromVTKtoNetcdf ${NETCDF_LIB})

if(VTK_LIBRARIES)
  target_link_libraries(fromVTKtoNetcdf ${VTK_LIBRARIES})
else()
  target_link_libraries(fromVTKtoNetcdf vtkHybrid vtkWidgets)
endif()
