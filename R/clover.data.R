################################################################################
#' Extracts data and metadata from CLIMBER-X netcdf files
#' @description This function accesses the data and metadata stored in netcdf files for a given variable
#'
#' @param dataDir A string that gives the path where the netcdf file is located
#' @param ncFileName The name of the netcdf file (e.g. "atm.nc")
#' @param variableName variable name of interest (e.g. "amoc26N")
#'
#' @return A list with the data and metadata for a given variable
#'
#' @export
#'

clover.data <- function(dataDir, ncFileName, variableName) {
  nc <- paste(dataDir, ncFileName, sep = "/")
  nc <- ncdf4::nc_open(nc)

  if (!variableName %in% names(nc$var)) {
    stop("Variable not found in file.")
  }

  varinfo <- nc$var[[variableName]]
  dims <- varinfo$dim

  # Basic metadata
  long_name <- varinfo$longname
  units <- varinfo$units

  # Dimension names
  dim_names <- sapply(dims, function(d) d$name)
  dim_sizes <- sapply(dims, function(d) d$len)

  # Dimension values
  dim_values <- lapply(dims, function(d) {
    if (d$name %in% names(nc$var)) {
      ncdf4::ncvar_get(nc, d$name)
    } else {
      d$vals
    }
  })
  names(dim_values) <- dim_names

  # Time
  time_bp <- dim_values$time
  time <- time_bp + 2000 # Convert to calendar years CE

  # Extract data
  values <- ncdf4::ncvar_get(nc, variableName)

  ncdf4::nc_close(nc)

  # Return structured list
  return(list(
    values = values,
    variable = variableName,
    long_name = long_name,
    units = units,
    dimension_names = dim_names,
    dimension_sizes = dim_sizes,
    dimension_values = dim_values,
    time = time
  ))
}
