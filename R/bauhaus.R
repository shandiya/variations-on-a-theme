
# designs inspired by Bauhaus posters

library(ggplot2)
library(ggforce)
library(purrr)
library(dplyr)


# concentric circles -----
concentric <- data.frame(x0 = rep(1, 4), 
                         y0 = rep(1, 4), 
                         r = rev(seq(5, 14, 3)),
                         colour = as.factor(1:4))

orange_pal <- c("#ee9b00", "#ca6702", "#bb3e03", "#ae2012") 

ggplot(concentric) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = colour), colour = NA) +
  scale_fill_manual(values = orange_pal) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e9d8a6" , colour = NA),
        plot.background = element_rect(fill = "#e9d8a6", colour = NA),
        legend.position = "none")
ggsave("images/concentric.png", width = 3, height = 3, units = "in")


# expanding circles -------
angle = pi/4
r <- c(7:1)
x0 <- r * sin(angle)
y0 =  r * cos(angle)
expanding <- data.frame(x0, y0, r, colour = as.factor(1:7))

blue_pal <- c("#94D2BD", "#7BBEB0", "#62ABA4", "#4A9898", "#31858B", "#18727F", "#005F73")
 
ggplot(expanding) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = colour), colour = NA) +
  scale_fill_manual(values = blue_pal) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e9d8a6" , colour = NA),
        plot.background = element_rect(fill = "#e9d8a6", colour = NA),
        legend.position = "none")
ggsave("images/expanding.png", width = 3, height = 3, units = "in")


# composite arcs in squares ------
# step 1: set up a grid
get_square <- function(x1, y1) {
  # coords start at bottom left and go anti-clockwise
  x2 = x1 + 1
  x3 = x2
  x4 = x1
  x <- c(x1, x2, x3, x4)
  
  y2 = y1
  y3 = y2 + 1
  y4 = y3
  y <- c(y1, y2, y3, y4)
  
  df <- data.frame(x, y)
  return(df)
}

# bottom left corner of each sqaure for a 4x4 grid
starting_points <- data.frame(x = rep(0:3, times = 4),
                              y = rep(0:3, each = 4))

grid16 <- starting_points |> 
  pmap_dfr(get_square, .id = "group") |> 
  mutate(vertex = rep(1:4, times = 16))


# step 2: set up functions to draw quarter circles with centres 
# in each of the four vertices of a square

# centre in bottom left i.e. vertex == 1
bottom_left <- function(centre_x, centre_y, ...) {
  
  angle <- seq(0, pi/2, length.out = 25)
  x = centre_x + sin(angle)
  y = centre_y + cos(angle)
  centre <- c(centre_x, centre_y)
  df <- data.frame(x, y)
  df <- rbind(df, centre)
  return(df)
  
}
# centre in bottom right i.e. vertex == 2
bottom_right <- function(centre_x, centre_y, ...) {
  
  angle <- seq(0, pi/2, length.out = 25)
  x = centre_x - sin(angle)
  y = centre_y + cos(angle)
  centre <- c(centre_x, centre_y)
  df <- data.frame(x, y)
  df <- rbind(df, centre)
  return(df)
  
}
# centre in top right i.e. vertex == 3
top_right <- function(centre_x, centre_y, ...) {
  
  angle <- seq(0, pi/2, length.out = 25)
  x = centre_x - sin(angle)
  y = centre_y - cos(angle)
  centre <- c(centre_x, centre_y)
  df <- data.frame(x, y)
  df <- rbind(df, centre)
  return(df)
  
}
# centre in top left i.e. vertex == 4
top_left <- function(centre_x, centre_y, ...) {
  
  angle <- seq(0, pi/2, length.out = 25)
  x = centre_x + sin(angle)
  y = centre_y - cos(angle)
  centre <- c(centre_x, centre_y)
  df <- data.frame(x, y)
  df <- rbind(df, centre)
  return(df)
  
}

# function to choose arc plotting function based on vertex value
arc_option <- function(df) {
  
  vertex <- df$vertex
  
  switch(vertex,
         "1" = bottom_left(df$x, df$y),
         "2" = bottom_right(df$x, df$y),
         "3" = top_right(df$x, df$y),
         "4" = top_left(df$x, df$y),
         stop("Invalid `vertex` value")
  )
}

# select a random vertex for each square in the grid
random_centres <- grid16 |> 
  group_by(group) |> 
  slice_sample(n = 1) |> 
  ungroup() |> 
  mutate(vertex = as.character(vertex)) 

# turn each row into a dataframe and output as a list, then apply 
# function to each element of list (i.e. each row)
arc16 <- random_centres |> 
  pmap(data.frame) |> 
  map_dfr(arc_option, .id = "group")

# plot!
blues <- colorRampPalette(c("#94D2BD", "#005F73"))(6)
pinks <- colorRampPalette(c("#ff758f", "#ffccd5"))(4)
yellows <- colorRampPalette(c("#ffaa00", "#ffdd00"))(6)
bpy <- c(blues, pinks, yellows)

ggplot() +
  geom_polygon(data = grid16,
               aes(x, y, group = group, fill = group), 
               colour = NA) +
  geom_polygon(data = arc16,
               aes(x, y, group = group, fill = group),
               colour = NA) +
  scale_fill_manual(values = bpy) +
  coord_equal() +
  theme_void()  +
  theme(panel.background = element_rect(fill = "#e9d8a6" , colour = NA),
        plot.background = element_rect(fill = "#e9d8a6", colour = NA),
        legend.position = "none")










 





