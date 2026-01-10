################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export

clover.intFun.plot.time <- function(ncFileName, data, v, long_name, units, time, dimensions, dim_values) {
  my.title <- paste0(long_name, "\n in ", units, " (", min(time), "-", max(time), ")")

  # Plot
  fname <- paste0("figs/", ncFileName, "_", v, "_TS.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  par(mar = c(4, 4, 4, 1))
  plot(
    x = time, y = data, type = "l",
    xlab = NA,
    ylab = paste(v, " (", units, ")", sep = ""),
    main = my.title
  )
  grid()
  dev.off()
}
