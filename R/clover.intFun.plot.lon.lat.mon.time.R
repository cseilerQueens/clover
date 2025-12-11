################################################################################
#' Plot CLIMBER-X output
#' @description Internal function used in clover.plot()
#' @export


clover.intFun.plot.lon.lat.mon.time <- function(ncFileName, data, v, long_name, units, time, dim_values) {
  # Annual values
  data.yr <- data[, , 13, ]

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
  globalSum <- terra::global(weighted, "sum", na.rm = TRUE)$sum

  # Global mean
  globalMean <- globalSum / totalArea

  # Monthly values

  results <- list()

  for (m in 1:12) {
    data.month <- data[, , m, ]
    r <- terra::rast(data.month)
    r <- terra::t(r)
    r <- terra::flip(r)
    terra::ext(r) <- c(-180, 180, -90, 90)
    terra::crs(r) <- "+proj=longlat +datum=WGS84"
    monthly.mean <- terra::mean(r)
    results[[m]] <- monthly.mean
  }

  monthly.clim <- terra::rast(results)

  # Seasonal cycle

  # Correct weighted sum (cell-by-cell!)
  weighted <- monthly.clim * area

  # Weighted global sum
  globalSeasonalMean <- terra::global(weighted, "sum", na.rm = TRUE)$sum / totalArea

  # Extract different latitudinal bands to plot seasonal cycle
  ext_arctic <- terra::ext(-180, 180, 66.5, 90)
  ext_mid.nh <- terra::ext(-180, 180, 23.5, 66.5)
  ext_tropics <- terra::ext(-180, 180, -23.5, 23.5)
  ext_mid.sh <- terra::ext(-180, 180, -66.5, -23.5)
  ext_antarctic <- terra::ext(-180, 180, -90, -66.5)

  arctic <- terra::crop(monthly.clim, ext_arctic)
  mid.nh <- terra::crop(monthly.clim, ext_mid.nh)
  tropics <- terra::crop(monthly.clim, ext_tropics)
  mid.sh <- terra::crop(monthly.clim, ext_mid.sh)
  antarctic <- terra::crop(monthly.clim, ext_antarctic)

  # Get seasonal cycle for each region
  regional.seasonal.cycles <- list()
  regions <- list(arctic, mid.nh, tropics, mid.sh, antarctic)
  for (i in seq_along(regions)) {
    region <- regions[[i]]
    area <- terra::cellSize(region, unit = "m")
    totalArea <- terra::global(area, "sum")$sum
    weighted <- region * area
    seasonalMean <- terra::global(weighted, "sum", na.rm = TRUE)$sum / totalArea
    regional.seasonal.cycles[[i]] <- seasonalMean
    rm(region, area, totalArea, weighted, seasonalMean)
  }

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

  # Seasonal maps
  fname <- paste0("figs/", ncFileName, "_", v, "_MLY-MAP.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)

  par(
    mfrow = c(4, 3),
    mar = c(0, 0, 0, 0),
    oma = c(1, 1, 3, 5)
  )

  # Determine legend range once
  my.range <- range(terra::values(monthly.clim), na.rm = TRUE)

  # First 12 maps
  for (m in 1:12) {
    terra::plot(
      monthly.clim[[m]],
      legend = FALSE,
      axes = FALSE,
      box = TRUE,
      main = NA,
      mar = 0,
      range = my.range
    )
    maps::map("world", add = TRUE, interior = FALSE, resolution = 0.5)
    legend("topleft", month.abb[m], text.col = "white", bty = "n")
    #  abline(v = c(-90, 0, 90), lty = 3)
    #  abline(h = c(66.5, 23.5, -23.5, -66.5), lty = 3)
  }

  # Add the shared legend in outer margin (right side)
  # par(fig = c(0.92, 0.98, 0.1, 0.9), new = TRUE)  # position of legend
  par(fig = c(0.94, 1.00, 0.1, 0.9), new = TRUE)
  terra::plot(
    monthly.clim[[1]],
    legend.only = TRUE,
    range = my.range,
    box = FALSE
  )

  # Add outer title
  mtext(my.title.nb, outer = TRUE, cex = 1, line = 1)
  dev.off()

  # Seasonal Cycle line graphs
  fname <- paste0("figs/", ncFileName, "_", v, "_MLY-SCY.png")
  png(fname, width = 15, height = 10, units = "cm", res = 150)
  par(mar = c(4, 4, 4, 1))
  my.range <- range(globalSeasonalMean, regional.seasonal.cycles)
  my.col <- viridis::viridis(n = length(regional.seasonal.cycles))
  plot(
    x = 1:12, y = globalSeasonalMean, col = NA, ylim = my.range,
    xaxt = "n", xlab = NA, ylab = paste0(v, " in ", units), main = my.title
  )
  axis(1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  lines(x = 1:12, y = globalSeasonalMean)
  points(x = 1:12, y = globalSeasonalMean, pch = 1)
  for (i in seq(regional.seasonal.cycles)) {
    lines(x = 1:12, regional.seasonal.cycles[[i]], col = my.col[i])
    points(x = 1:12, regional.seasonal.cycles[[i]], col = my.col[i], pch = i + 1)
  }

  grid()

  par(xpd = TRUE) # allow drawing outside plot region
  legend(
    "bottom",
    inset = -0.35, # moves legend below the plot region
    c("Global", "Arctic", "N. Midlat", "Tropics", "S. Midlat", "Antarctic"),
    cex = 0.8,
    col = c("black", my.col),
    pch = 1:6,
    horiz = TRUE,
    bty = "n"
  )
  par(xpd = FALSE)
  dev.off()
}
