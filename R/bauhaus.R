
# designs inspired by Bauhaus posters

library(ggplot2)
library(ggforce)

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

