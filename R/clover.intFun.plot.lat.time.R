################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export


clover.intFun.plot.lat.time <- function(ncFileName, data, v, long_name, units, time) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Annual values
  data.yr <- data
  nyears <- ncol(data.yr)
  r <- terra::rast(data.yr)
  terra::ext(r) <- c(min(time), max(time), -90, 90)
  r.yly <- r

  # Determine legend range once
  my.range <- range(terra::values(r.yly), na.rm = TRUE)

  # choose palette
  my.col <- terra::map.pal("viridis", 100)

  # Hovmoeller plot
  fname <- paste0("figs/", ncFileName, "_", v, "_HOV.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  par(mar = c(3, 3, 3, 3))

  terra::plot(r.yly,
    asp = nyears / 360,
    axes = TRUE,
    main = my.title,
    xlab = "Year",
    ylab = "Latitude",
    range = my.range
  )

  grid()

  dev.off()
}
