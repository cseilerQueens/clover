################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.lat.lev.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Dimensions:
  # dim(data) gives 36 23 10
  # There are 23 vertical levels

  data.mean <- apply(data, c(1, 2), mean, na.rm = TRUE)

  data.mean <- data.mean[, ncol(data.mean):1]

  lat <- dim_values$lat
  lev <- dim_values$lev

  lev <- (-1) * rev(lev)

  # Common z-limits
  zlim <- range(data.mean, na.rm = TRUE)

  fname <- paste0("figs/", ncFileName, "_", v, "_MerCroSec.png")
  png(fname, width = 15, height = 12, units = "cm", res = 150)

  par(mfrow = c(1, 1), mar = c(4, 4, 2, 4))

  fields::image.plot(lat, lev, data.mean,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = my.title
  )
  contour(lat, lev, data.mean, add = TRUE)
  grid()
  dev.off()
}
