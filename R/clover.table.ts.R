################################################################################
#' Create a summary table for content of netcdf files with time series
#' @description This function reads the content of CLIMBER-X netcdf files that contain time series (e.g. atm_ts.nc) and produces a table in markdown format that summarizes the file's content
#'
#' @param dataDir A string that gives the path where the netcdf file is located
#' @param ncFileName The name of the netcdf file
#'
#' @return An Table in markdown with all variables, units, and some basic statistics (min, max, mean, sd)
#'
#' @export

clover.table.ts <- function(dataDir, ncFileName) {
  nc <- paste(dataDir, ncFileName, sep = "/")
  nc <- ncdf4::nc_open(nc)

  # Extract time in years BP relative to 2000 CE
  time_bp <- nc$dim$time$vals

  # Convert to calendar years CE
  time_ce <- time_bp + 2000

  # Get list of variable names
  vars <- names(nc$var)

  # Make a table of all variables, their units, and value ranges

  df <- data.frame(
    character(),
    character(),
    numeric(),
    numeric(),
    numeric(),
    numeric()
  )

  for (i in vars) {
    vname <- i
    var <- nc$var[[vname]]
    data <- ncdf4::ncvar_get(nc, vname)
    long_name <- ncdf4::ncatt_get(nc, vname, "long_name")$value
    units <- ncdf4::ncatt_get(nc, vname, "units")$value
    min.val <- round(min(data), 1)
    max.val <- round(max(data), 1)
    mean.val <- round(mean(data), 1)
    sd.val <- round(sd(data), 1)
    new.row <- data.frame(vname, long_name, units, mean.val, min.val, max.val, sd.val)
    df <- rbind(df, new.row)
  }
  colnames(df) <- c("Variable", "Long Name", "Unit", "Mean", "Min", "Max", "SD")

  knitr::kable(df, caption = ncFileName)
}
