library(tidyverse)

my_canvas_ribbons <- function(colors,
                           background = "#fdf5e6",
                           triangle = TRUE,
                           seed = 1,
                           y_max_top = 75,
                           length = 34) {

  set.seed(seed)

  # Create an empty figure
  artwork <- ggplot2::ggplot() +
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylim(0, 100) +
    theme_classic()
  # Determine points on the triangle
  y_max_top <- y_max_top - 7
  tpl <- data.frame(x = 16:(16+length-1), y = seq(from = 16, to = 75, length.out = length))
  tpl <- tpl[which(tpl$y < y_max_top), ]
  tpr <- data.frame(x = 51:(51+length-1), y = seq(from = 74, to = 16, length.out = length))
  tpr <- tpr[which(tpr$y < y_max_top), ]
  for (i in 1:length(colors)) {
    # Determine points on left side of triangle
    bpb <- data.frame(x = 0, y = sample(10:90, size = 1))
    bpt <- data.frame(x = 0, y = bpb$y + 5)

    fpb <- tpl[sample(1:nrow(tpl), size = 1), ]
    fpt <- data.frame(x = fpb$x + 2.5, y = fpb$y + 5)

    spb <- tpr[sample(1:nrow(tpr), size = 1), ]
    spt <- data.frame(x = spb$x - 2.5, y = spb$y + 5)


    epb <- data.frame(x = 100, y = sample(10:90, size = 1))
    ept <- data.frame(x = 100, y = epb$y + 5)
    # Determine points on right side of triangle

    # Combine polygon points
    polygon <- rbind(bpb, fpb, spb, epb, ept, spt, fpt, bpt)
    if (i == 1) {print(paste(bpb, fpb, spb, epb, ept, spt, fpt, bpt))}

    # ggplot(polygon, aes(x = x, y = y)) +
    #   xlim(0, 100) +
    #   ylim(0, 100) +
    #   geom_polygon(fill = colors[i], color = NA,
    #                stat = "identity", alpha = 1)
    #
    artwork <- artwork + ggplot2::geom_polygon(
      data = polygon, mapping = ggplot2::aes(x = x, y = y),
      fill = colors[i], color = NA,
      stat = "identity", alpha = 1
    )

    if (i == length(colors)) {
      artwork <- artwork + ggplot2::geom_point(data = polygon, mapping = ggplot2::aes(x = x, y = y))
      print(polygon)
      }
  }
  # (Optionally) draw the triangle
  if (triangle) {
    artwork <- artwork + ggplot2::geom_polygon(
      data = data.frame(x = c(15, 50, 85), y = c(15, 75, 15)), mapping = ggplot2::aes(x = x, y = y),
      fill = NA, color = "black",
      stat = "identity", size = 1
    )
  }
  # artwork <- theme_canvas(artwork, background)
  return(artwork)
}

my_canvas_ribbons(colors = c("brown","lightblue","green"),
               y_max_top = 130,
               length = 50,
               # seed = runif(1, min = 1, max = 1000),
               seed = 3)

canvas_ribbons(colors = c("brown","lightblue","green"))

df <- tibble(x = 0,
             y = sample(1:100, 1))

triangle <- tibble(x = c(15, 50, 85), y = c(15, 75, 15))
square <- tibble(x = c(15, 15, 85, 85), y = c(15, 85, 85, 15))

square <- as.matrix(square) %*% matrix(c(cos(pi/4), -sin(pi/4), sin(pi/4), cos(pi/4)), 2, 2) %>%
  as_tibble()

names(square) <- c("x","y")

approx(x = c(15,15), y = c(15,85), ties = "min")

# TODO
# create starting point
# choose point along edge of polygon
# refract at that point to a random direction still within the polygon
# travel straight until reaching opposite edge
# refract second time

ggplot() +
  geom_polygon(data = square, aes(x = x, y = y))
