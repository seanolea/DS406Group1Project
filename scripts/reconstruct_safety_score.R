# 1_reconstruct_safety_score.R
# Description: This script reproduces Nate Silver's safety score methodology
# over the entire 30-year period (1985-2014). It calculates rates per ASK (avail_seat_km_per_week), standardizes them
# according to the specific methodology detailed in the article, and computes a composite
# safety score. Finally, it visualizes the scores.


library(dplyr)
library(ggplot2)
library(tidyr)


# read the data from the csv file (1AirlineSafety.csv))
data_airlines <- read.csv("../data/1AirlineSafety.csv")
# compute the total number of occurrences from 1985 to 2014
data_airlines <- data_airlines %>%
  mutate(
    tot_num_incidents = incidents_85_99 + incidents_00_14,
    tot_num_fatal_accidents = fatal_accidents_85_99 + fatal_accidents_00_14,
    tot_num_fatalities = fatalities_85_99 + fatalities_00_14
  )


# extract the ASK column (avail_seat_km_per_week)
ask <- data_airlines$avail_seat_km_per_week
# compute the rates for tot_num_incidents, tot_num_fatal_accidents and tot_num_fatalities
data_airlines <- data_airlines %>%
  mutate(
    incidents_freq_ratio = tot_num_incidents / avail_seat_km_per_week,
    accidents_freq_ratio = tot_num_fatal_accidents / avail_seat_km_per_week,
    fatalities_freq_ratio = tot_num_fatalities / avail_seat_km_per_week
  )


# calculate the mean rates for the three categories (incidents, fatal accidents, fatalities)
mean_incidents_freq_ratio <- mean(data_airlines$incidents_freq_ratio)
mean_rate_fatal_accidents <- mean(data_airlines$accidents_freq_ratio)
mean_fatalities_freq_ratio <- mean(data_airlines$fatalities_freq_ratio)

# apply the Silver's methodology to each  category.
# 1)  subtract the airline's rate from the average rate for all the airlines
# 2) multiply the result by the sqrt of number of seat kilometers flown
# 3) standardize the score for each category
calculate_zscr <- function(rate, mean_rate, ask) {
  diff <- mean_rate - rate
  weigted_difference <- diff * sqrt(ask)
  z_score <- scale(weigted_difference, center = TRUE, scale = TRUE)
  return(as.numeric(z_score))
}


data_airlines <- data_airlines %>%
  mutate(
    scr_z_incidents = calculate_zscr(incidents_freq_ratio, mean_incidents_freq_ratio, avail_seat_km_per_week),
    scr_z_fatal_accidents = calculate_zscr(accidents_freq_ratio, mean_rate_fatal_accidents, avail_seat_km_per_week),
    scr_z_fatalities = calculate_zscr(fatalities_freq_ratio, mean_fatalities_freq_ratio, avail_seat_km_per_week)
  )

# calculate the composite safety score
data_airlines <- data_airlines %>%
  mutate(safety_score = (scr_z_incidents + scr_z_fatal_accidents + scr_z_fatalities) / 3)


# sort the airlines by safety score and plot the top 10 and bottom 10
data_airlines_sorted <- data_airlines %>% arrange(desc(safety_score))
top10 <- head(data_airlines_sorted, 10)
bottom10 <- tail(data_airlines_sorted, 10)


plot_data_airlines <- bind_rows(top10, bottom10)
plot_data_airlines$airline <- factor(plot_data_airlines$airline, levels = plot_data_airlines$airline[order(plot_data_airlines$safety_score)])


p <- ggplot(plot_data_airlines, aes(x = airline, y = safety_score, fill = safety_score > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), guide = "none") +
  theme_minimal() +
  labs(
    title = "Top 10 and Bottom 10 Airlines by Safety Score (1985-2014)",
    x = "Airline",
    y = "Composite Safety Score"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


ggsave("../plots/safety_score_top_bottom_10.png", plot = p, width = 10, height = 8)
