
# textures using lines and points

library(ggplot2)
library(purrr)
library(dplyr)


theme_txtr <- theme_void() +
  theme(panel.background = element_rect(fill = "#444444" , colour = NA),
        plot.background = element_rect(fill = "#444444", colour = NA),
        plot.margin = unit(c(1,1,1,1), "cm"))

# 1. overlapping circles ------
circles <- function(r, w, h, ...) {
  
  angle <- seq(0, 2*pi, length.out = 50)
  x <- w + r*sin(angle)
  y <- h + r*cos(angle)
  data.frame(x, y)
  
}

r <- runif(n = 600, min = 0.1, max = 1)
w <- runif(n = 600, min = 0, max = 1)
h <- runif(n = 600, min = 0, max = 1)
df <- pmap_dfr(list(r, w, h), circles, .id = "group")

# thin lines + high alpha
ggplot() +
  geom_path(data = df, 
            aes(x, y, group = group), 
            alpha = 0.7,
            colour = "#dddddd",
            size = 0.2) +
  coord_equal() +
  xlim(c(quantile(df$x, 0.1), quantile(df$x, 0.9))) +
  ylim(c(quantile(df$y, 0.1), quantile(df$y, 0.9))) + 
  theme_txtr

ggsave("images/texture01.png", width = 3, height = 3, units = "in")


# 2. interpolated points -----

get_points <- function(...) {
  
  v <- runif(n = 2, min = 0, max = 1)
  w <- runif(n = 2, min = 0, max = 1)
  df <- data.frame(approx(x = v, 
                          y = w, 
                          xout = seq(v[1], v[2], length.out = 25)))
  return(df)
  
}

df <- map_dfr(1:3000, get_points)

ggplot() +
  geom_point(data = df,
             aes(x, y),
             alpha = 0.5,
             colour = "#dddddd",
             size = 0.01,
             shape = 16) +
  coord_equal() +
  theme_txtr

ggsave("images/texture02.png", width = 3, height = 3, units = "in")
  

