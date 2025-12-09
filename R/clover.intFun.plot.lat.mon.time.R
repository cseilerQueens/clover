################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.lat.mon.time <- function(ncFileName, data, v, long_name, units, time) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Annual values
  data.yr <- data[, 13, ]
  nyears <- ncol(data.yr)
  r <- terra::rast(data.yr)
  terra::ext(r) <- c(min(time), max(time), -90, 90)
  r.yly <- r

  # Monthly values
  data.mly <- data[, 1:12, ]
  data.mly <- matrix(data.mly, nrow = nrow(data.mly), ncol = 12 * nyears)
  r <- terra::rast(data.mly)
  terra::ext(r) <- c(min(time), max(time), -90, 90)
  r.mly <- r

  # Determine legend range once
  range.yly <- range(terra::values(r.yly), na.rm = TRUE)
  range.mly <- range(terra::values(r.mly), na.rm = TRUE)
  my.range <- range(range.yly, range.mly)

  # choose palette
  my.col <- terra::map.pal("viridis", 100)

  # Hovmoeller plot
  fname <- paste0("figs/", ncFileName, "_", v, "_HOV.png")
  png(fname, width = 15, height = 15, units = "cm", res = 150)
  par(mfrow = c(2, 1), oma = c(0, 0, 2, 0))

  terra::plot(r.yly,
    asp = nyears / 360,
    axes = TRUE,
    main = paste0("Annual ", v, " in ", units),
    xlab = "Year",
    ylab = "Latitude",
    range = my.range
  )

  terra::plot(r.mly,
    asp = nyears / 360,
    axes = TRUE,
    main = paste0("Monthly ", v, " in ", units),
    xlab = "Year",
    ylab = "Latitude",
    range = my.range
  )

  mtext(my.title, outer = TRUE, cex = 1, line = 1, font = 2)
  dev.off()
}
