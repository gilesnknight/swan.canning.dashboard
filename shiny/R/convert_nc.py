import iris

cubes = iris.load('input.nc')       # each variable in the netcdf file is a cube
iris.save(cubes[0],'output.grib2')  # save a specific variable to grib
