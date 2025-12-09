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

  # Loop over variables

  test.variables <- c("had_fi", "ptrop", "vabz", "tam", "t2", "t3", "gamma", "uab", "vab", "u3", "xz", "solar", "solarm", "zsa", "zs", "fw_pac_atl")

  # for (v in names(nc$var)) {
  for (v in test.variables) {
    dimensions <- var_info[[v]]$dim
    long_name <- var_info[[v]]$long_name
    units <- var_info[[v]]$units
    data <- ncdf4::ncvar_get(nc, v)


    # 1                   mon, time
    # 2              lat, mon, time
    # 3             latv, mon, time
    # 4         lon, lat, mon, time
    # 5     lon, lat, st, mon, time
    # 6  lon, lat, zlevw, mon, time
    # 7   lon, lat, zlay, mon, time
    # 8        lonu, lat, mon, time
    # 9        lon, latv, mon, time
    # 10  lon, lat, zlev, mon, time
    # 11     latv, zlevw, mon, time
    # 12       doy, hour, lat, time
    # 13             doy, lat, time
    # 14             lon, lat, time
    # 15         lon, lat, st, time
    # 16                  lat, time

    if (dimensions == "time") {
      clover.intFun.plot.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "mon, time") {
      clover.intFun.plot.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "lat, mon, time") {
      clover.intFun.plot.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "latv, mon, time") {
      clover.intFun.plot.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "lon, lat, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "lon, lat, zlev, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

    if (dimensions == "lon, lat, zlevw, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

    if (dimensions == "lon, lat, zlay, mon, time") {
      clover.intFun.plot.lon.lat.zlev.mon.time(ncFileName, data, v, long_name, units, time, dimensions, dim_values)
    } # OK

    if (dimensions == "lonu, lat, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "lon, latv, mon, time") {
      clover.intFun.plot.lon.lat.mon.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "latv, zlevw, mon, time") {
      clover.intFun.plot.latv.zlev.mon.time(ncFileName, data, v, long_name, units, time, dim_values)
    } # OK

    if (dimensions == "doy, hour, lat, time") {
      clover.intFun.plot.doy.hour.lat.time(ncFileName, data, v, long_name, units, time, dim_values)
    } # OK
    if (dimensions == "doy, lat, time") {
      clover.intFun.plot.doy.lat.time(ncFileName, data, v, long_name, units, time, dim_values)
    }
    if (dimensions == "lon, lat, time") {
      clover.intFun.plot.lon.lat.time(ncFileName, data, v, long_name, units, time)
    } # OK

    if (dimensions == "lat, time") {
      clover.intFun.plot.lat.time(ncFileName, data, v, long_name, units, time)
    } # OK

    # Outputs for each surface type:

    if (dimensions == "lon, lat, st, time") {
      a <- 1
    }

    if (dimensions == "lon, lat, st, mon, time") {
      a <- 1
    }

    plot.new()
  }
  ncdf4::nc_close(nc)
}
