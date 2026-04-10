#
# Example 4: Plots and Figures
#
# This script contain code to make plots and figures using example data from folder Ex 4.
# 

# install.packages("ggplot2")

library(ggplot2)  # must call library to be able to use it in script

rm(list=ls(all=TRUE)) # clears all data in environment to start with clean slate

# Simple scatter plot
x <- c(1, 2, 3, 4, 5)
y <- c(2.3, 4.1, 3.8, 6.2, 5.9)

plot(x, y,
     main = "My Plot Title",
     xlab = "X Axis Label",
     ylab = "Y Axis Label",
     pch  = 16,      # filled circle points
     col  = "steelblue")


data <- data.frame(x = c(1, 2, 3, 4, 5),
                   y = c(2.3, 4.1, 3.8, 6.2, 5.9))

ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "My Plot Title",
       x     = "X Axis Label",
       y     = "Y Axis Label") +
  theme_bw()

# school concentrations plot with lines for SAL
ui.plot <- ggplot(ui.samp, aes(x = school, y = Result)) +
  geom_point(shape = 1, size = 2) + 
  theme_classic() +
  labs(title = "UI Air Concentration Measurements in Schools",
       x = "Schools in Arbitrary Order",
       y = "Concentration (ng/m3)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray")) +
  geom_hline(yintercept = 30, color = "blue", linetype = "dashed", linewidth = 1) + 
  geom_hline(yintercept = 60, color = "orange", linetype = "dashed", linewidth = 1) + 
  geom_hline(yintercept = 100, color = "red", linetype = "dashed", linewidth = 1)
ui.plot