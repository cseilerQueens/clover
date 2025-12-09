################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export


clover.intFun.plot.lon.lat.time <- function(ncFileName, data, v, long_name, units, time) {
  # Annual values
  data.yr <- data

  r <- terra::rast(data.yr)
  r <- terra::t(r)
  r <- terra::flip(r)
  terra::ext(r) <- c(-180, 180, -90, 90)
  terra::crs(r) <- "+proj=longlat +datum=WGS84"
  annualMean <- terra::mean(r)

  # Cell areas
  area <- terra::cellSize(r, unit = "m")

  # Total area of all grid cells
  totalArea <- terra::global(area, "sum")$sum

  # Correct weighted sum (cell-by-cell!)
  weighted <- r * area

  # Weighted global sum
  globalSum <- terra::global(weighted, "sum")$sum

  # Global mean
  globalMean <- globalSum / totalArea

  # Plot

  fname <- paste0("figs/", ncFileName, "_", v, "_YLY-MAP.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  # Annual mean map
  my.title <- paste0(long_name, "\n in ", units, " (", min(time), "-", max(time), ")")
  my.title.nb <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")
  terra::plot(annualMean,
    main = my.title
  )
  maps::map("world", add = TRUE, interior = FALSE, resolution = 0.5)
  abline(v = c(-90, 0, 90), lty = 3)
  abline(h = c(66.5, 23.5, -23.5, -66.5), lty = 3)
  dev.off()

  # Annual mean time series
  fname <- paste0("figs/", ncFileName, "_", v, "_YLY-TS.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  par(mar = c(5, 4, 4, 1))
  plot(
    x = time, y = globalMean, type = "l",
    main = my.title,
    xlab = NA, ylab = units
  )
  grid()
  dev.off()
}
