library(ggplot2)

# Import data

dat1 <- read.csv("Output/ttest.csv")

# Smoky histogram

hist_vals <- hist(dat1$mean_SR_smoky, breaks = seq(floor(min(dat1$mean_SR_smoky)), ceiling(max(dat1$mean_SR_smoky)), 0.5), plot = FALSE)
plot(hist_vals, col = "light green", border = "black", main = "Smoky",
     xlab = "Mean Singing Rate", ylab = "Number of Sites", ylim = c(0, 12))

# Non-smoky histogram

hist_vals <- hist(dat1$mean_SR_non_smoky, breaks = seq(floor(min(dat1$mean_SR_non_smoky)), ceiling(max(dat1$mean_SR_non_smoky)), 0.5), plot = FALSE)
plot(hist_vals, col = "light green", border = "black", main = "Non-smoky",
     xlab = "Mean Singing Rate", ylab = "Number of Sites", ylim = c(0, 12))

# Paired Box Plot
ggplot(tags1, aes(x = treatment, y = mean_singing_rate, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Paired Box Plot", x = "Treatment", y = "Mean Singing Rate", fill = "Treatment")
