################################################################################
#' Creates table that summarizes file content
#' @description This function creates a table in markdown format that summarizes the content of CLIMBER-X netcdf files that contain more than just time series (e.g. atm.nc)
#'
#' @param dataDir A string that gives the path where the netcdf file is located
#' @param ncFileName The name of the netcdf file
#'
#' @return Summary table of file content in markdown
#'
#' @export

clover.table <- function(dataDir, ncFileName) {
  nc <- paste(dataDir, ncFileName, sep = "/")
  nc <- ncdf4::nc_open(nc)

  # Extract info for each variable
  var_info <- lapply(nc$var, function(v) {
    list(
      dimensions = paste(sapply(v$dim, function(d) d$name), collapse = ", "),
      long_name  = if (!is.null(v$longname)) v$longname else NA,
      units      = if (!is.null(v$units)) v$units else NA
    )
  })

  # Turn into a data frame
  df <- data.frame(
    variable = names(var_info),
    long_name = sapply(var_info, `[[`, "long_name"),
    units = sapply(var_info, `[[`, "units"),
    dimensions = sapply(var_info, `[[`, "dimensions"),
    stringsAsFactors = FALSE
  )

  rownames(df) <- NULL

  knitr::kable(df, caption = ncFileName)
}
