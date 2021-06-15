# Create a nice schematic diagram of model fixed effects for workflow diagram
# John Godlee (johngodlee@gmail.com)
# 2021-06-15

# Packages
library(ggplot2)
library(dplyr)

# Create fake data
x <- seq(0,100,1)
y1 <- x * 2 + 2 + rnorm(length(x), 1, 50)
y2 <- x * 1.5 - 10 + rnorm(length(x), 1, 50)
y3 <- x * 5 + 8 + rnorm(length(x), 1, 50)

dat <- data.frame(x, y1, y2, y3) %>%
  gather(key, value, -x)

pdf(file = "../img/fe_schem.pdf", height = 3, width = 3)
ggplot(dat) + 
  geom_smooth(method = "lm", aes(x = x, y = value, colour = key)) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  labs(x = "Pred.", y = "Resp.") + 
  theme(axis.ticks = element_blank(),
    axis.text = element_blank())
dev.off()


