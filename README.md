
# Clover: Climber-X Output Visualization and Evaluation R Package

-   This R package plots the output from the Earth System Model
    CLIMBER-X

-   A demo of Clover can be accessed here:
    <https://storage.googleapis.com/clover-demo/index.html>

-   Clover reads in netcdf files produced by CLIMBER-X, extracts data
    and meta data, and generates figures in PNG format

-   The main functions are:

-   `clover.table.ts(dataDir, ncFileName)`

-   `clover.table(dataDir, ncFileName)`

-   `clover.plot(dataDir, ncFileName)`

-   The first two functions simply generate tables that provide an
    overview.

-   The third function is the main plotting function. To plot all
    outputs from a CLIMBER-X simulation, loop this function over all
    netcdf files located in the model output folder, e.g.:

``` r
# Load Clover
library(clover)

# Define model ouput directory
dataDir <- "/home/cseiler/CLIMBER-X/output/bench_v1.4.0/preind" # <-- CHANGE THIS

# List all netCDF files
nc_files <- list.files(dataDir, pattern = "\\.nc$", full.names = FALSE)

# Loop plotting function over all files
for (i in nc_files) {
  clover::clover.plot(dataDir, ncFileName = i)
  print(paste("CLOVER completed", i))
}
```

-   Once all PNG files have been created, knit `clover-report.Rmd` in
    order to organize figures and tables:

-   The plotting function `clover.plot(dataDir, ncFileName = i)`
    automatically detects the dimensions of each variable and calls a
    plotting function designed for that specific dimension set.

-   Note that CLIMBER-X output includes many variables with different
    dimension combinations. If a variable has dimensions for which no
    plotting function has been defined, a message will be printed, for
    example: “betas was not plotted because no function exists for the
    dimensions lon, lat, nsurf, month, time.” I will add additional
    plotting functions over time.

-   Clover is not yet suitable for comparing results from multiple model
    runs or against observations, but I plan to add this functionality
    in the future.
