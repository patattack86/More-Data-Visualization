# More-Data-Visualization
Need to turn this into a function, results in nice 3x3 subplot of correlation plots, good format for publication


setwd("E:/Thesis/Thesis-Excel-files/8-23/HSI3-Spec-comparison")


wvdata23 <- read.csv("Spec.HSI3.aug23.csv")

library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)

x = wvdata23$Spec.857.
y = wvdata23$HSI3.857.5

fit23 <- lm(x~y)
summary(fit23)

p1.439 <- ggplot(wvdata23, aes(x=Spec.438.8, y=HSI3.438.8)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.07", parse=TRUE) +
  ggtitle("Wavelength 439 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)
