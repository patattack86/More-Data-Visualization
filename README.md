# More-Data-Visualization
#Need to turn this into a function, results in nice 3x3 subplot of correlation plots, good format for publication, using this for my thesis at the moment


```{r setup, include=FALSE}
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

p2.489 <- ggplot(wvdata23, aes(x=Spec.488.8, y=HSI3.488.8)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.57", parse=TRUE) +
  ggtitle("Wavelength 489 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)

 p3.529 <- ggplot(wvdata23, aes(x=Spec.528.6, y=HSI3.528.6)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.63", parse=TRUE) +
  ggtitle("Wavelength 529 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)
 
 p4.579 <- ggplot(wvdata23, aes(x=Spec.578.5, y=HSI3.578.5)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.75", parse=TRUE) +
  ggtitle("Wavelength 579 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)
 
 p5.658 <- ggplot(wvdata23, aes(x=Spec.658.2, y=HSI3.658.2)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.78", parse=TRUE) +
  ggtitle("Wavelength 658 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)
 
 p6.738 <- ggplot(wvdata23, aes(x=Spec.737.9, y=HSI3.737.9)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.19", parse=TRUE) +
  ggtitle("Wavelength 738 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)

p7.787 <- ggplot(wvdata23, aes(x=Spec.787.8, y=HSI3.787.8)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.28", parse=TRUE) +
  ggtitle("Wavelength 787 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)

p8.857 <- ggplot(wvdata23, aes(x=Spec.857.5, y=HSI3.857.5)) + 
  geom_point(shape=1, size = 3) +
  theme_classic() +
  geom_abline(intercept = 0) +
  scale_y_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  scale_x_continuous(limits = c(0, 7), expand = c(0,0), breaks = 1:7) +
  annotate("text", x=3, y=5, label = "R == 0.13", parse=TRUE) +
  ggtitle("Wavelength 857 nm") +
  xlab("SP Reflectance (%)") + 
  ylab("HSI3 Reflectance (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  coord_fixed(ratio = 1)

aug23grid <- grid.arrange(p1.439,p2.489,p3.529,p4.579, p5.658,p6.738,p7.787,p8.857, ncol=3, 
                          top = textGrob("Correlation between HSI3 and Spectroradiometer reflectance, 23 August 2017
                                         
                                         ", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)))


ggsave("aug23grid.png", plot = aug23grid, width = 15, height = 15)

getwd()
```
