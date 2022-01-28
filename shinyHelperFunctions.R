my_canvas_watercolors <- function(colors, background = "#fafafa", layers = 50,
                                  depth = 2, resolution = 250, userMean = 1) {
  nlayers <- length(colors)
  plotData <- data.frame(x = numeric(), y = numeric(), s = numeric(), z = numeric())
  colorSequence <- rep(colors, times = ceiling(layers / 5), each = 5)
  labelSequence <- rep(1:length(colors), times = ceiling(layers / 5), each = 5)
  corners <- sample(3:10, size = nlayers, replace = TRUE)
  basePolygons <- list()
  for (i in 1:nlayers) {
    basePolygons[[i]] <- aRtsy:::.createBasePolygon(i, nlayers, corners[i], resolution, userMean)
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

my_canvas_ribbons <- function(colors, background = "#fdf5e6", triangle = TRUE,
                              maxHeight = 75, minHeight = 25, ribbonWidth = 2.5) {

  maxHeight <- maxHeight - 12
  minHeight <- minHeight - 12
  # Create an empty figure
  artwork <- ggplot2::ggplot() +
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylim(0, 100)
  # Determine points on the triangle
  tpl <- data.frame(x = 16:49, y = seq(from = 16, to = 75, length.out = 34))
  tpl <- tpl[which(tpl$y < maxHeight & tpl$y > minHeight), ]
  tpr <- data.frame(x = 51:84, y = seq(from = 74, to = 16, length.out = 34))
  tpr <- tpr[which(tpr$y < maxHeight & tpr$y > minHeight), ]
  for (i in 1:length(colors)) {
    # Determine points on left side of triangle
    bpb <- data.frame(x = 0, y = sample(minHeight:maxHeight, size = 1))
    fpb <- tpl[sample(1:nrow(tpl), size = 1), ]
    spb <- tpr[sample(1:nrow(tpr), size = 1), ]
    epb <- data.frame(x = 100, y = sample(minHeight:maxHeight, size = 1))
    # Determine points on right side of triangle
    bpt <- data.frame(x = 0, y = bpb$y + ribbonWidth*2)
    fpt <- data.frame(x = fpb$x + ribbonWidth, y = fpb$y + ribbonWidth*2)
    spt <- data.frame(x = spb$x - ribbonWidth, y = spb$y + ribbonWidth*2)
    ept <- data.frame(x = 100, y = epb$y + ribbonWidth*2)
    # Combine polygon points
    polygon <- rbind(bpb, fpb, spb, epb, ept, spt, fpt, bpt)
    artwork <- artwork + ggplot2::geom_polygon(
      data = polygon, mapping = ggplot2::aes(x = x, y = y),
      fill = colors[i], color = NA,
      stat = "identity", alpha = 1
    )
  }
  # (Optionally) draw the triangle
  if (triangle) {
    artwork <- artwork + ggplot2::geom_polygon(
      data = data.frame(x = c(15, 50, 85), y = c(15, 75, 15)), mapping = ggplot2::aes(x = x, y = y),
      fill = NA, color = "black",
      stat = "identity", size = 1
    )
  }
  artwork <- theme_canvas(artwork, background)
  return(artwork)
}

my_canvas_collatz <- function(colors, background = "#fafafa", n = 200,
                           angle.even = 0.0075, angle.odd = 0.0145, side = FALSE,
                           strandSize = 1) {
  canvas <- data.frame(x = numeric(), y = numeric(), col = numeric(), z = numeric())
  if (length(n) == 1) {
    n <- sample(1:1000000, size = n, replace = F)
  }
  for (i in n) {
    series <- rev(aRtsy:::get_collatz_sequence(i))
    line <- matrix(0, nrow = length(series), ncol = 2)
    line <- aRtsy:::draw_collatz(line, series, angle.even, angle.odd)
    line <- data.frame(
      x = line[, 1],
      y = line[, 2],
      col = rep(sample(colors, size = 1), nrow(line)),
      z = i,
      size = nrow(line),
      alpha = nrow(line)
    )
    canvas <- rbind(canvas, line)
  }
  canvas$z <- as.factor(canvas$z)
  canvas$size <- 1 - (canvas$size / max(canvas$size))
  canvas$alpha <- 1 - canvas$size
  artwork <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y, group = z)) +
    ggplot2::geom_path(size = canvas$size*strandSize, color = canvas$col, alpha = canvas$alpha, lineend = "round") +
    ggplot2::xlim(range(canvas$x)) +
    ggplot2::ylim(range(canvas$y))
  if (side) {
    artwork <- artwork + ggplot2::coord_flip()
  }
  artwork <- aRtsy::theme_canvas(artwork, background)
  return(artwork)
}
