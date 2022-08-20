
# shapes that twirl and whirl

library(ggplot2)
library(ggpointdensity)

# no random elements -------
twirly_whirly <- function(n, x_1, y_1, a, b) {

# create empty vectors
x <- vector(mode = "double", length = n)
y <- vector(mode = "double", length = n)

# starting values
x[1] <- x_1
y[1] <- y_1

# populate vectors
for (i in 2:n) {
  
  x[i] <- a*cos(x[i-1]) + b*sin(y[i-1])
  y[i] <- a*sin(x[i-1]) - b*cos(y[i-1])
  
}

# create data frame
data.frame(x = x, y = y)

}

# create a few twirly whirlies
df1 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.5, b = 2.2)
df2 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.2, b = -1.2)
df3 <- twirly_whirly(n = 100000, x_1 = 1.2, y_1 = 1.6, a = 1.4, b = 1.5)

# other interesting shapes
#df4 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 2, b = 0.5)
#df5 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 0.7, b = 5)
#df6 <- twirly_whirly(n = 100000, x_1 = 1.2, y_1 = 0.9, a = 1.2, b = 1.8)

# plot and save
plotting_fun <- function(df) {

  ggplot(df) +
    geom_point(aes(x, y),
               shape = 16,
               alpha = 0.1,
               size = 0.05) +
    theme_void()
}

tw1 <- plotting_fun(df1)
ggsave("images/tw1.png", tw1, height = 3, width = 3, units = "in")

tw2 <- plotting_fun(df2)
ggsave("images/tw2.png", tw2, height = 3, width = 3, units = "in")

tw3 <- plotting_fun(df3)
ggsave("images/tw3.png", tw3, height = 3, width = 3, units = "in")


# add colour based on density -------
df_col <- twirly_whirly(n = 100000, x_1 = 1.2, y_1 = 0.9, a = 1.2, b = 1.8)

ggplot(df_col) +
  geom_pointdensity(aes(x, y), shape = 16, alpha = 0.2, size = 0.1) +
  scale_color_gradient(low = "yellow", high = "olivedrab") +
  theme_void() +
  theme(legend.position = "none")


# add a random element (reduce convergence?) ----


