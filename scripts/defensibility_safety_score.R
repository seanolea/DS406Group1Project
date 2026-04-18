# defensibility_safety_score.R
# Description: TThe purpose of thid script is to see if combining incidents, fatal accidents and
# fatalities into a single score is defensible. It calculates the
# correlation matrix of the components and performs "principal component
# analysis" (PCA) to see if a single safety factor explains the data well.





library(dplyr)
library(ggplot2)
library(tidyr)





# recreate the components
data_airlines <- read.csv("../data/1AirlineSafety.csv")
data_airlines <- data_airlines %>%
  mutate(
    tot_incidents = incidents_85_99 + incidents_00_14,
    tot_fatal_accidents = fatal_accidents_85_99 + fatal_accidents_00_14,
    tot_fatalities = fatalities_85_99 + fatalities_00_14,
    incidents_freq_ratio = tot_incidents / avail_seat_km_per_week,
    fatal_accidents_frq_ratio = tot_fatal_accidents / avail_seat_km_per_week,
    fatalities_freq_ratio = tot_fatalities / avail_seat_km_per_week
  )
# recalculate the z score
calculate_zscore <- function(rate, mean_rate, ask) {
  diff <- mean_rate - rate
  weighted_diff <- diff * sqrt(ask)
  z_score <- scale(weighted_diff, center = TRUE, scale = TRUE)
  return(as.numeric(z_score))
}
data_airlines <- data_airlines %>%
  mutate(
    zscr_incidents = calculate_zscore(incidents_freq_ratio, mean(incidents_freq_ratio), avail_seat_km_per_week),
    zscr_fatal_accidents = calculate_zscore(fatal_accidents_frq_ratio, mean(fatal_accidents_frq_ratio), avail_seat_km_per_week),
    zscr_fatalities = calculate_zscore(fatalities_freq_ratio, mean(fatalities_freq_ratio), avail_seat_km_per_week)
  )





# extract just the three components
components <- data_airlines %>% select(zscr_incidents, zscr_fatal_accidents, zscr_fatalities)



# correlation matrix
cor_matrix <- cor(components)
print(cor_matrix)



# plot the correlation matrix with ggplot2
cor_df <- as.data.frame(as.table(cor_matrix))
colnames(cor_df) <- c("Var1", "Var2", "Corr")

p_cor <- ggplot(cor_df, aes(Var1, Var2, fill = Corr)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), name = "Correlation"
  ) +
  theme_minimal() +
  labs(title = "Correlation of Safety Components", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))




ggsave("../plots/correlation_safety_components.png", plot = p_cor, width = 6, height = 6)


# pca
pca_result <- prcomp(components, scale. = TRUE)
print(summary(pca_result))



# plot pca
pca_var <- pca_result$sdev^2
pca_var_perc <- pca_var / sum(pca_var) * 100
var_df <- data.frame(PC = c("PC1", "PC2", "PC3"), Variance = pca_var_perc)

p <- ggplot(var_df, aes(x = PC, y = Variance, fill = PC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Principal Component Analysis",
    x = "Principal Component",
    y = "Variance (%)"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ggsave("../plots/pca_safety_score.png", plot = p, width = 8, height = 6)
