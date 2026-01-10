################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.mon.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  # annual mean: row 13
  annual_data <- data[13, ]

  # monthly data: rows 1:12, columns = years
  monthly_data <- data[1:12, , drop = FALSE]

  fname <- paste0("figs/", ncFileName, "_", v, "-TS.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  # annual data
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  par(pty = "s")
  plot(
    x = time, y = annual_data,
    type = "l",
    xlab = "Years",
    ylab = paste(v, " (", units, ")", sep = ""),
    main = long_name
  )

  # seasonal cycle
  my.col <- rev(viridis::viridis(n = ncol(monthly_data)))

  plot(
    x = 1:12, y = monthly_data[, 1], col = NA,
    ylim = range(monthly_data),
    xlab = "Months",
    ylab = paste(v, " (", units, ")", sep = ""),
    main = long_name
  )
  for (yr in ncol(monthly_data)) {
    lines(
      x = 1:12, y = monthly_data[, 1],
      col = my.col[yr]
    )
  }
  legend("topleft", pch = 16, col = c(my.col[1], my.col[yr]), legend = c(time[1], time[yr]), bty = "n")
  dev.off()
}
