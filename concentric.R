
library(ggplot2)
library(ggforce)

concentric <- data.frame(x0 = rep(1, 3), 
                         y0 = rep(1, 3), 
                         r = c(10, 7.5, 4.5),
                         colour = as.factor(1:3))


ggplot(concentric) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = colour), colour = NA) +
  scale_fill_manual(values = c("#e29c37", "#d96236", "#d23634")) +
  coord_equal() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#f3f4ef" , colour = NA),
        plot.background = element_rect(fill = "#f3f4ef", colour = NA),
        legend.position = "none")

# colours 
#d23634 dark orange
#d96236 medium orange
#e29c37 light orange