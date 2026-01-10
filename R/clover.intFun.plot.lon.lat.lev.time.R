################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.lon.lat.lev.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # Dimensions:
  # > dim(data)
  # [1] 72 36 23 10
  data.mean <- apply(data, MARGIN = c(1, 2, 3), FUN = mean, na.rm = TRUE)

  lat <- dim_values$lat
  lon <- dim_values$lon
  lev <- dim_values$lev

  # Ocean basin longitude boundaries
  pacific <- (lon >= 120 | lon < -70)
  atlantic <- (lon >= -70 & lon < 20)
  indian <- (lon >= 20 & lon < 120)

  # Index of ocean basin
  pacific.idx <- which(pacific)
  atlantic.idx <- which(atlantic)
  indian.idx <- which(indian)

  # Extract ocean basin
  data.pac <- data.mean[pacific.idx, , ]
  data.atl <- data.mean[atlantic.idx, , ]
  data.ind <- data.mean[indian.idx, , ]

  pac.zonal <- apply(data.pac, MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
  atl.zonal <- apply(data.atl, MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)
  ind.zonal <- apply(data.ind, MARGIN = c(2, 3), FUN = mean, na.rm = TRUE)

  pac.zonal <- pac.zonal[, ncol(pac.zonal):1]
  atl.zonal <- atl.zonal[, ncol(atl.zonal):1]
  ind.zonal <- ind.zonal[, ncol(ind.zonal):1]
  lev <- (-1) * rev(lev)

  # Common z-limits
  zlim <- range(pac.zonal, atl.zonal, ind.zonal, na.rm = TRUE)

  fname <- paste0("figs/", ncFileName, "_", v, "_MerCroSec.png")
  png(fname, width = 15, height = 30, units = "cm", res = 150)

  par(mfrow = c(3, 1), oma = c(0, 0, 4, 0), mar = c(4, 4, 2, 4))

  fields::image.plot(lat, lev, pac.zonal,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = "(a) Pacific (120 E to 70 W)"
  )
  contour(lat, lev, pac.zonal, add = TRUE)
  grid()

  fields::image.plot(lat, lev, atl.zonal,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = "(b) Atlantic (70 W to 20 E)"
  )
  contour(lat, lev, atl.zonal, add = TRUE)
  grid()

  fields::image.plot(lat, lev, ind.zonal,
    zlim = zlim,
    col = viridis::viridis(50),
    xlab = "Latitude", ylab = "Depth (m)",
    main = "(c) Indian (20 E to 120 E)"
  )
  contour(lat, lev, ind.zonal, add = TRUE)
  grid()

  mtext(my.title,
    side = 3, # top of the figure
    outer = TRUE,
    line = 1, # adjust vertical spacing
    font = 2
  )

  dev.off()
}
