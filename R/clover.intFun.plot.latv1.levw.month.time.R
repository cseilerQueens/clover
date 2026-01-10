################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.latv1.levw.month.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")
  # Dimensions:
  # dim(data)
  # [1] 37 23 13 10
  # latv1, levw, month, time
  # There are 23 vertical levels

  data.yr <- data[, , 13, ]
  data.mean <- apply(data.yr, MARGIN = c(1, 2), FUN = mean, na.rm = TRUE)
  data.mean <- data.mean[, ncol(data.mean):1]
  data.mean[data.mean == 0] <- NA

  latv1 <- dim_values$latv1
  levw <- dim_values$levw
  levw <- (-1) * rev(levw)

  zlim <- range(data.mean, na.rm = TRUE)

  fname <- paste0("figs/", ncFileName, "_", v, "_MerCroSec.png")
  png(fname, width = 15, height = 12, units = "cm", res = 150)
  par(mar = c(4, 4, 2, 4))

  fields::image.plot(latv1, levw, data.mean,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = my.title
  )
  grid()
  contour(latv1, levw, data.mean, add = TRUE)

  dev.off()
}
