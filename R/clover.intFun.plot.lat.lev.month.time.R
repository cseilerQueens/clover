################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.lat.lev.month.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Dimensions:
  # dim(data) gives 37 14 13 40
  # There are 13 vertical levels

  djf <- c(12, 1, 2)
  jja <- c(6, 7, 8)
  data.djf <- apply(data[, , djf, ], c(1, 2, 4), mean, na.rm = TRUE)
  data.jja <- apply(data[, , jja, ], c(1, 2, 4), mean, na.rm = TRUE)

  # calculate the zonal mean values

  djf.mean <- apply(data.djf, c(1, 2), mean, na.rm = TRUE)
  jja.mean <- apply(data.djf, c(1, 2), mean, na.rm = TRUE)


  djf.mean <- djf.mean[, ncol(djf.mean):1]
  jja.mean <- djf.mean[, ncol(jja.mean):1]

  lat <- dim_values$lat
  lev <- dim_values$lev

  lev <- (-1) * rev(lev)

  # Common z-limits
  zlim <- range(djf.mean, jja.mean, na.rm = TRUE)

  fname <- paste0("figs/", ncFileName, "_", v, "_MerCroSec.png")
  png(fname, width = 15, height = 25, units = "cm", res = 150)

  par(mfrow = c(2, 1), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 4))

  fields::image.plot(lat, lev, djf.mean,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = "DJF"
  )
  contour(lat, lev, djf.mean, add = TRUE)
  grid()

  fields::image.plot(lat, lev, jja.mean,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = "JJA"
  )
  contour(lat, lev, djf.mean, add = TRUE)
  grid()

  mtext(my.title,
    side = 3, # top of the figure
    outer = TRUE,
    line = 1, # adjust vertical spacing
    font = 2
  )

  dev.off()
}
