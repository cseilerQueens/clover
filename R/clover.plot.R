################################################################################
#' Plots content of CLIMBER-X netcdf files that contain more than just time series
#' @description This function plots all variables stored in a CLIMBER-X netcdf file that contains more than just time series.
#' Note that variables are stored using different combinations of dimensions.
#'
#' @param dataDir A string that gives the path where the netcdf file is located
#' @param ncFileName The name of the netcdf file (e.g. "atm.nc")
#'
#' @return Figues for each variable
#'
#' @export

clover.plot <- function(dataDir, ncFileName) {
  nc <- paste(dataDir, ncFileName, sep = "/")
  nc <- ncdf4::nc_open(nc)

  # Get list of all dimension names
  get_dim_values <- function(nc, dimname) {
    d <- nc$dim[[dimname]]

    # If the dimension stores values (common in CF-compliant files)
    if (!is.null(d$vals)) {
      return(d$vals)
    }

    # Otherwise fall back to simple index
    return(seq_len(d$len))
  }

  dim_names <- names(nc$dim)

  dim_values <- lapply(dim_names, function(name) get_dim_values(nc, name))
  names(dim_values) <- dim_names

  # Extract time in years BP relative to 2000 CE
  time_bp <- dim_values$time

  # Convert to calendar years CE
  time <- time_bp + 2000

  # Extract info for each variable
  var_info <- lapply(nc$var, function(v) {
    list(
      dimensions = paste(sapply(v$dim, function(d) d$name), collapse = ", "),
      long_name  = if (!is.null(v$longname)) v$longname else NA,
      units      = if (!is.null(v$units)) v$units else NA
    )
  })

 
#---------------------------------------------
# Test all possible dimensions
# This will be deleted later
#---------------------------------------------

  variables <- names(nc$var)
  examples <- c(
  "solar",
  "solarm",
  "t_atl",
  "a_atl",
  "ptrop",
  "fw_pac_atl",
  "hftz",
  "vabz",
  "xz",
  "opsi",
  "mask_ice",
  "t_lake",
  "litterc_prof",
  "map_edge",
  "t",
  "vol",
  "w",
  "tam",
  "prc",
  "i_coast_nbr",
  "litter",
  "soilc",
  "lithology",
  "gpp",
  "seeds",
  "albsnw",
  "etot",
  "f_stp",
  "frst",
  "zs",
  "zsa",
  "gamma",
  "u3",
  "t3",
  "vbisl",
  "v",
  "vab",
  "tauy",
  "ubisl",
  "u",
  "uab",
  "taux",
  "str_s",
  "had_fi",
  "hft"
)

test.variables <- intersect(variables, examples)

 # Loop over variables

  # for (v in names(nc$var)) {
  for (v in test.variables) {
    dimensions <- var_info[[v]]$dim
    long_name <- var_info[[v]]$long_name
    units <- var_info[[v]]$units
    data <- ncdf4::ncvar_get(nc, v)

#---------------------------------------------
#	All dimensions
#---------------------------------------------
# lat, time fw_pac_atl   
# lon, lat mask_ice   
# mon, time had_fi   
# doy, lat, time solarm  
# lat, lev, time a_atl  
# lat, mon, time ptrop  
# latv, mon, time vabz  
# lon, lat, time zsa  
# doy, hour, lat, time solar 
# lat, lev, month, time t_atl 
# latv, lev, month, time hftz 
# latv, zlevw, mon, time xz 
# latv1, levw, month, time opsi 
# lon, lat, isles, time map_edge 
# lon, lat, lev, time vol 
# lon, lat, mon, time tam 
# lon, lat, month, time prc 
# lon, lat, nbr, time i_coast_nbr 
# lon, lat, ncarb, time soilc 
# lon, lat, nlit, time lithology 
# lon, lat, npft, time seeds 
# lon, lat, nsurf, time f_stp 
# lon, lat, st, time zs 
# lon, latv, mon, time vab 
# lon, latv, month, time tauy 
# lonu, lat, mon, time uab 
# lonu, lat, month, time taux 
# lonu, latv, month, time str_s 
# type, latv, month, time hft 
# lon, lat, depth, month, time t_lake
# lon, lat, depth, ncarb, time litterc_prof
# lon, lat, lev, month, time t
# lon, lat, levw, month, time w
# lon, lat, ncarb, month, time litter
# lon, lat, npft, month, time gpp
# lon, lat, nsoil, month, time albsnw
# lon, lat, nsurf, month, time etot
# lon, lat, st, mon, time frst
# lon, lat, zlay, mon, time gamma
# lon, lat, zlev, mon, time u3
# lon, lat, zlevw, mon, time t3
# lon, latv, isles, month, time vbisl
# lon, latv, lev, month, time v
# lonu, lat, isles, month, time ubisl
# lonu, lat, lev, month, time u
#---------------------------------------------

    if (dimensions == "time") {
      clover.intFun.plot.time(ncFileName, data, v, long_name, units, time)
    } # OK
# lat, time fw_pac_atl 
    if (dimensions == "lat, time") {
      clover.intFun.plot.lat.time(ncFileName, data, v, long_name, units, time)
    } # OK  
# lon, lat mask_ice   
# mon, time had_fi 
    if (dimensions == "mon, time") {
      clover.intFun.plot.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

# doy, lat, time solarm 
    if (dimensions == "doy, lat, time") {
      clover.intFun.plot.doy.lat.time(ncFileName, data, v, long_name, units, time, dim_values)
    } # OK 
# lat, lev, time a_atl  
# lat, mon, time ptrop 
    if (dimensions == "lat, mon, time") {
      clover.intFun.plot.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK 
# latv, mon, time vabz
    if (dimensions == "latv, mon, time") {
      clover.intFun.plot.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK  
# lon, lat, time zsa
    if (dimensions == "lon, lat, time") {
      clover.intFun.plot.lon.lat.time(ncFileName, data, v, long_name, units, time)
    } # OK  
# doy, hour, lat, time solar 
    if (dimensions == "doy, hour, lat, time") {
      clover.intFun.plot.doy.hour.lat.time(ncFileName, data, v, long_name, units, time, dim_values)
    } # OK
# lat, lev, month, time t_atl 
# latv, lev, month, time hftz 
# latv, zlevw, mon, time xz 
    if (dimensions == "latv, zlevw, mon, time") {
      clover.intFun.plot.latv.zlev.mon.time(ncFileName, data, v, long_name, units, time, dim_values)
    } # OK
# latv1, levw, month, time opsi 
# lon, lat, isles, time map_edge 
# lon, lat, lev, time vol 
# lon, lat, mon, time tam
    if (dimensions == "lon, lat, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK 
# lon, lat, month, time sst 
    if (dimensions == "lon, lat, month, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK 

# lon, lat, nbr, time i_coast_nbr 
# lon, lat, ncarb, time soilc 
# lon, lat, nlit, time lithology 
# lon, lat, npft, time seeds 
# lon, lat, nsurf, time f_stp 
# lon, lat, st, time zs 

# lon, latv, mon, time vab
    if (dimensions == "lon, latv, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK  
# lon, latv, month, time tauy 
    if (dimensions == "lon, latv, month, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK
# lonu, lat, mon, time uab
    if (dimensions == "lonu, lat, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK
# lonu, lat, month, time taux 
    if (dimensions == "lonu, lat, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK
# lonu, latv, month, time str_s 
    if (dimensions == "lon, latv, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

# type, latv, month, time hft 
# lon, lat, depth, month, time t_lake
# lon, lat, depth, ncarb, time litterc_prof

# lon, lat, lev, month, time t
    if (dimensions == "lon, lat, lev, month, time") {
      clover.intFun.plot.lon.lat.lev.month.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

# lon, lat, levw, month, time w
    if (dimensions == "lon, lat, levw, month, time") {
      clover.intFun.plot.lon.lat.lev.month.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK


# lon, lat, ncarb, month, time litter
# lon, lat, npft, month, time gpp
# lon, lat, nsoil, month, time albsnw
# lon, lat, nsurf, month, time etot
# lon, lat, st, mon, time frst

# lon, lat, zlay, mon, time gamma
    if (dimensions == "lon, lat, zlay, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

# lon, lat, zlev, mon, time u3
    if (dimensions == "lon, lat, zlev, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

# lon, lat, zlevw, mon, time t3
    if (dimensions == "lon, lat, zlevw, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

# lon, latv, isles, month, time vbisl
# lon, latv, lev, month, time v
# lonu, lat, isles, month, time ubisl
# lonu, lat, lev, month, time u

    plot.new()
  }
  ncdf4::nc_close(nc)
}
