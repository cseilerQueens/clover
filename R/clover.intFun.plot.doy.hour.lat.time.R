################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.doy.hour.lat.time <- function(ncFileName, data, v, long_name, units, time, dim_values) {
  my.title <- paste0(long_name, " in ", units, " (", min(time), "-", max(time), ")")

  # dims: [day, hour, lat, year]
  # [1] 360  24  36  40

  # Daily mean
  daily <- apply(data, c(1, 3, 4), mean) # dims -> [day, lat, year]

  # Mean over years
  daily.mean <- apply(daily, c(1, 2), mean) # dims -> [day, lat]

  lat <- dim_values$lat
  doy <- dim_values$doy

  fname <- paste0("figs/", ncFileName, "_", v, "_HOV.png")
  png(fname, width = 15, height = 15, units = "cm", res = 150)

  fields::image.plot(
    x = doy,
    y = lat,
    z = daily.mean,
    col = viridis::viridis(50),
    xlab = "Day of Year",
    ylab = "Latitude",
    main = my.title
  )
  grid()

  dev.off()
}
