deform <- function(canvas, maxdepth, resolution) {
  .Call('_aRtsy_deform', PACKAGE = 'aRtsy', canvas, maxdepth, resolution)
}

.createBasePolygon <- function(color, nlayers, corners, resolution, userMean) {
  if (nlayers == 1) {
    xmid <- (resolution / 2)
    ymid <- (resolution / 2)
  } else {
    xmid <- (resolution / 2) + (resolution / 3) * cos(2 * pi * color / nlayers) * stats::rnorm(1, mean = 0.75, sd = 0.25)
    ymid <- (resolution / 2) + (resolution / 3) * sin(2 * pi * color / nlayers) * stats::rnorm(1, mean = 0.75, sd = 0.25)
  }
  radiusx <- sample((resolution / 3):(resolution / 7.5), size = 1)
  radiusy <- sample((resolution / 3):(resolution / 7.5), size = 1)
  polyx <- xmid + radiusx * cos(2 * pi * 1:corners / corners)
  polyy <- ymid + radiusy * sin(2 * pi * 1:corners / corners)
  coords <- data.frame(x = polyx, y = polyy)
  coords[nrow(coords) + 1, ] <- coords[1, ]
  varsegments <- stats::rnorm(nrow(coords), mean = as.numeric(userMean), sd = 3) #user mean
  canvas <- data.frame(x = coords$x, y = coords$y, s = varsegments)
  canvas <- deform(canvas, maxdepth = 5, resolution)
  return(canvas)
}

my_canvas_watercolors <- function(colors, background = "#fafafa", layers = 50,
                                  depth = 2, resolution = 250, userMean = 1) {
  nlayers <- length(colors)
  plotData <- data.frame(x = numeric(), y = numeric(), s = numeric(), z = numeric())
  colorSequence <- rep(colors, times = ceiling(layers / 5), each = 5)
  labelSequence <- rep(1:length(colors), times = ceiling(layers / 5), each = 5)
  corners <- sample(3:10, size = nlayers, replace = TRUE)
  basePolygons <- list()
  for (i in 1:nlayers) {
    basePolygons[[i]] <- .createBasePolygon(i, nlayers, corners[i], resolution, userMean)
  }
  for (i in 1:length(colorSequence)) {
    canvas <- basePolygons[[labelSequence[i]]]
    canvas <- deform(canvas, maxdepth = depth, resolution)
    canvas <- cbind(canvas, z = i)
    plotData <- rbind(plotData, canvas)
  }
  artwork <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = factor(z))) +
    ggplot2::geom_polygon(alpha = 0.04) + #here, using geom_path or geom_polygon works well, and creates a different type of art
    ggplot2::scale_fill_manual(values = colorSequence) +
    ggplot2::xlim(c(0, resolution)) +
    ggplot2::ylim(0, resolution)
  artwork <- theme_canvas(artwork, background)
  return(artwork)
}
