################################################################################
#' Creates table that summarizes file content
#' @description This function finds all unique combinations of development across all CLIMBER-X outputs
#'
#' @param dataDir A string that gives the path where the netcdf file is located
#' @param ncFileName The name of the netcdf file
#'
#' @return Summary table of file content in markdown
#'
#' @export
#'

clover.dev.dimensions <- function(dataDir) {
  all_dimensions <- list()

  nc_files <- list.files(dataDir, pattern = "\\.nc$", full.names = TRUE)

  # Keep only files that do NOT end with "_ts.nc"
  nc_files <- nc_files[!grepl("_ts\\.nc$", nc_files)]

  for (i in nc_files) {
    nc <- ncdf4::nc_open(i)

    # Extract info for each variable
    var_info <- lapply(nc$var, function(v) {
      list(
        dimensions = paste(sapply(v$dim, function(d) d$name), collapse = ", "),
        long_name  = if (!is.null(v$longname)) v$longname else NA,
        units      = if (!is.null(v$units)) v$units else NA
      )
    })

    # Canonical signature WITHOUT sorting — preserves original order
    canonical <- sapply(var_info, function(x) {
      dims <- trimws(strsplit(x$dimensions, ",")[[1]])
      paste(dims, collapse = ", ") # no reorder
    })

    # For each unique (ordered!) combination, pick ONE example variable name
    example_file <- tapply(names(var_info), canonical, function(x) x[1])

    # Convert to data frame
    dim_df <- data.frame(
      dimensions = names(example_file),
      example_file = unname(example_file),
      row.names = NULL
    )

    all_dimensions <- append(all_dimensions, list(dim_df))
  }

  # Combine all results into one big data frame
  all_combinations <- do.call(rbind, all_dimensions)

  # Get the unique combinations preserving first example_file
  unique_combinations <- aggregate(example_file ~ dimensions,
    data = all_combinations,
    FUN = function(x) x[1]
  )

  return(unique_combinations)
}
