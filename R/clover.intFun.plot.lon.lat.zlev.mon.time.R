################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.lon.lat.zlev.mon.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Dimensions:
  # dim(data) gives 72 36 13 13 40
  # There are 13 vertical levels

  djf <- c(12, 1, 2)
  jja <- c(6, 7, 8)
  data.djf <- apply(data[, , , djf, ], c(1, 2, 3, 5), mean, na.rm = TRUE)
  data.jja <- apply(data[, , , jja, ], c(1, 2, 3, 5), mean, na.rm = TRUE)

  # calculate the zonal mean values
  djf.zonal <- apply(data.djf, c(2, 3), mean, na.rm = TRUE)
  jja.zonal <- apply(data.jja, c(2, 3), mean, na.rm = TRUE)
  lat <- dim_values$lat

  if (dimensions == "lon, lat, zlev, mon, time") {
    zlev <- dim_values$zlev
  }

  if (dimensions == "lon, lat, zlevw, mon, time") {
    zlev <- dim_values$zlevw
  }

  if (dimensions == "lon, lat, zlay, mon, time") {
    zlev <- dim_values$zlay
  }

  # Common z-limits
  zlim <- range(djf.zonal, jja.zonal, na.rm = TRUE)

  fname <- paste0("figs/", ncFileName, "_", v, "_MerCroSec.png")
  png(fname, width = 15, height = 25, units = "cm", res = 150)

  par(mfrow = c(2, 1), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 4))

  fields::image.plot(lat, zlev, djf.zonal,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Height (m)",
    main = "DJF"
  )
  # abline(v = c(66.5, 23.5, -23.5, -66.5), lty = 3)
  grid()

  fields::image.plot(lat, zlev, jja.zonal,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Height (m)",
    main = "JJA"
  )
  # abline(v = c(66.5, 23.5, -23.5, -66.5), lty = 3)
  grid()


  mtext(my.title,
    side = 3, # top of the figure
    outer = TRUE,
    line = 1, # adjust vertical spacing
    font = 2
  ) # bold

  dev.off()
}
