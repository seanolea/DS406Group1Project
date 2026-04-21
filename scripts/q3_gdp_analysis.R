# q3_gdp_analysis.R
# Description: This script investigates whether the GDP per capita of a country explains airline
# safety better than past safety records. It compares the following using correlation and regression:
# 1) Past vs future incident rates
# 2) GDP per capita vs a composite safety score (Silver's method)

library(dplyr)
library(ggplot2)

# source composite safety score from other file
source("scripts/reconstruct_safety_score.R", chdir = TRUE)

# create past and future incident rates
data_airlines <- data_airlines %>%
  mutate(
    # Past incident rate (1985–1999)
    inc_rate_past = incidents_85_99 / avail_seat_km_per_week,
    
    # Future incident rate (2000–2014)
    inc_rate_future = incidents_00_14 / avail_seat_km_per_week
  )


# linking airlines to a country to later merge with the GDP data
# first cleaning airline names with asterisks in it
data_airlines$airline <- gsub("\\*", "", data_airlines$airline)
# manually matching airlines to country
country_lookup <- data.frame(
  airline = c(
    "Aer Lingus","Aeroflot","Aerolineas Argentinas","Aeromexico",
    "Air Canada","Air France","Air India","Air New Zealand",
    "Alaska Airlines","Alitalia","All Nippon Airways","American",
    "Austrian Airlines","Avianca","British Airways","Cathay Pacific",
    "China Airlines","Condor","COPA","Delta / Northwest",
    "Egyptair","El Al","Ethiopian Airlines","Finnair",
    "Garuda Indonesia","Gulf Air","Hawaiian Airlines","Iberia",
    "Japan Airlines","Kenya Airways","KLM","Korean Air",
    "LAN Airlines","Lufthansa","Malaysia Airlines",
    "Pakistan International","Philippine Airlines","Qantas",
    "Royal Air Maroc","SAS","Saudi Arabian","Singapore Airlines",
    "South African","Southwest Airlines","Sri Lankan / AirLanka",
    "SWISS","TACA","TAM","TAP - Air Portugal",
    "Thai Airways","Turkish Airlines","United / Continental",
    "US Airways / America West","Vietnam Airlines",
    "Virgin Atlantic","Xiamen Airlines"
  ),
  country = c(
    "Ireland","Russia","Argentina","Mexico",
    "Canada","France","India","New Zealand",
    "United States","Italy","Japan","United States",
    "Austria","Colombia","United Kingdom","Hong Kong",
    "Taiwan","Germany","Panama","United States",
    "Egypt","Israel","Ethiopia","Finland",
    "Indonesia","Bahrain","United States","Spain",
    "Japan","Kenya","Netherlands","South Korea",
    "Chile","Germany","Malaysia",
    "Pakistan","Philippines","Australia",
    "Morocco","Sweden","Saudi Arabia","Singapore",
    "South Africa","United States","Sri Lanka",
    "Switzerland","El Salvador","Brazil","Portugal",
    "Thailand","Turkey","United States",
    "United States","Vietnam",
    "United Kingdom","China"
  )
)

data_airlines <- merge(data_airlines, country_lookup, by = "airline")

# loading gdp data
gdp <- read.csv("data/gdp-per-capita-worldbank.csv")

# Only use country and gdp per capita columns
gdp <- gdp %>%
  filter(Year == 1999) %>%
  select(Entity, GDP.per.capita)

# renaming columns
gdp <- gdp %>%
  rename(
    country = Entity
  )

# combine each dataset by country
merged <- merge(data_airlines, gdp, by = "country")
# Taiwan was dropped as it's not included in the GDP dataset, however 55 matched is enough
# (compared to 56)

# analysing relationships:
# 1) past vs future incident rates
# 2) GDP vs composite safety score 

cor_past_future <- cor(merged$inc_rate_past, merged$inc_rate_future)
cor_gdp_safety <- cor(log(merged$GDP.per.capita), merged$safety_score)
print(cor_past_future)
print(cor_gdp_safety)

# Plot for Past vs Future safety
p1 <- ggplot(merged, aes(x = inc_rate_past, y = inc_rate_future)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Past vs Future Incident Rates",
    x = "Past Incident Rate",
    y = "Future Incident Rate"
  )
ggsave("plots/past_vs_future.png", plot = p1, width = 10, height = 8)

# Plot for GDP vs Safety score
p2 <- ggplot(merged, aes(x = log(GDP.per.capita), y = safety_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "GDP per Capita vs Safety Score",
    x = "Log GDP per Capita",
    y = "Safety Score"
  )
ggsave("plots/gdp_vs_safety.png", plot = p2, width = 10, height = 8)

# regression models
model_past <- lm(inc_rate_future ~ inc_rate_past, data = merged)
model_gdp <- lm(safety_score ~ log(GDP.per.capita), data = merged)
summary(model_past)
summary(model_gdp)

# Interpretation
# From the correlation analysis we can see a weak positive relationship between past and future incident rates
# (correlation = 0.375), indicating that airlines with higher incident rates in the past tend to have slightly
# higher future rates. This relationship is statistically significant, with a p-value of 0.005, however the 
# explanatory power is low (R^2 = 0.141), meaning past results explain only a small proportion of variation in 
# future outcomes.
#
# In contrast a stronger relationship can be seen between GDP per capita and composite safety score
# (correlation = 0.69). This positive relationship suggests that airlines based in more wealthy countries
# tend to have significantly better safety outcomes. This effect is also statistically significant, with a  much higher 
# explanatory power of R^2 = 0.48
#
# Overall, GDP per capita shows a substantially stronger relationship with airline safety than past incident
# rates when safety is measured using a composite score. Therefore, GDP per capita explains airline safety better
# than an airline’s own historical safety record.
#
# This aligns very closely with Silver's findings, where a strong correlation of 0.7 was found when using the 
# composite safety measure