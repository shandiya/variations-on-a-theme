
# simple radial patterns with an element of randomness

library(purrr)
library(ggplot2)

# radial patterns ------

radial <- function(...) {
  
  x_start <- runif(1, 0, 4)
  x_end <- x_start + runif(1, 0, 20)
  x <- seq(x_start, x_end)
  y <- rep(runif(1, 0, 5), times = length(x))
  alpha <- runif(1, 0.5, 1)
  data.frame(x, y, alpha)
  
}

n <- c(1:120)

radial_df <- n |> 
  map_dfr(radial, .id = "group")

# using geom_point makes the lines more uniform
ggplot(radial_df) +
  geom_jitter(aes(x, y, group = group, alpha = alpha), 
              colour = "#862a1c",
              size = 2,
              shape = 16) + 
  coord_polar() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#f7e7d4" , colour = NA),
        plot.background = element_rect(fill = "#f7e7d4", colour = NA), 
        legend.position = "none")

# straight lines
ggplot(radial_df) +
  geom_line(aes(x, y, group = group, alpha = alpha), 
            colour = "#862a1c") + 
  coord_polar() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#f7e7d4" , colour = NA),
        plot.background = element_rect(fill = "#f7e7d4", colour = NA), 
        legend.position = "none")

# connects observations and shows where changes occur
ggplot(radial_df) +
  geom_step(aes(x, y, alpha = alpha), 
            colour = "#862a1c") + 
  coord_polar() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "#f7e7d4" , colour = NA),
        plot.background = element_rect(fill = "#f7e7d4", colour = NA), 
        legend.position = "none")
