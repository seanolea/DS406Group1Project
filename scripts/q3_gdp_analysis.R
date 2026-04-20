# q3_gdp_analysis.R
# Description: This script investigates whether the GDP per capita of a country explains its airline
# safety better than past safety records. It compares the following using correlation and regression:
# 1) GDP vs future incident rates
# 2) past vs future incident rates

library(dplyr)
library(ggplot2)

# read in original data provided
data_airlines <- read.csv("data/1AirlineSafety.csv")

# normalising by "available seat kilometers per week" (ASK) for a rate to compare airlines fairly since
# incident counts are misleading due to airlines differing in size
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
    "USA","Italy","Japan","USA",
    "Austria","Colombia","UK","Hong Kong",
    "Taiwan","Germany","Panama","USA",
    "Egypt","Israel","Ethiopia","Finland",
    "Indonesia","Bahrain","USA","Spain",
    "Japan","Kenya","Netherlands","South Korea",
    "Chile","Germany","Malaysia",
    "Pakistan","Philippines","Australia",
    "Morocco","Sweden","Saudi Arabia","Singapore",
    "South Africa","USA","Sri Lanka",
    "Switzerland","El Salvador","Brazil","Portugal",
    "Thailand","Turkey","USA",
    "USA","Vietnam",
    "UK","China"
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
# some airlines have been dropped due to country names not matching, however 46 matched is enough
# (compared to 56)

# analysing correlation to compare strength of relationships:
# 1) past safety -> future safety
# 2) gdp -> future safety

cor_past_future <- cor(merged$inc_rate_past, merged$inc_rate_future)
cor_gdp_future <- cor(log(merged$GDP.per.capita), merged$inc_rate_future)

print(cor_past_future)
print(cor_gdp_future)

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

# Plot for GDP vs Future Safety
p2 <- ggplot(merged, aes(x = log(GDP.per.capita), y = inc_rate_future)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "GDP per Capita vs Future Incident Rates",
    x = "Log GDP per Capita",
    y = "Future Incident Rate"
  )
ggsave("plots/gdp_vs_future.png", plot = p2, width = 10, height = 8)

# regression models
model_past <- lm(inc_rate_future ~ inc_rate_past, data = merged)
model_gdp <- lm(inc_rate_future ~ log(GDP.per.capita), data = merged)

summary(model_past)
summary(model_gdp)

# Interpretation
# From the correlation analysis we can see a weak positive relationship between past and future incident rates
# (correlation = 0.355), indicating that airlines with higher incident rates in the past tend to have slightly
# higher future rates. This relationship is statistically significant, with a p-value of 0.016, however the 
# explanatory power is low (R^2 = 0.126), meaning past results explains only a small proportion of variation in 
# future outcomes.
#
# A similar strength correlation can be seen between GDP per capita and future incident rates, however here it 
# is a negative one (correlation = -0.349). This suggests that airlines based in more wealthy countries tend
# to have slightly lower incident rates. This effect is also statistically significant with a p-value of 0.018,
# however once again, the explanatory power is low at 0.122.
#
# Overall, both variables show weak yet statistically significant relationships with future incident rates, and
# neither one outperforms the other in terms of explanatory strength. Therefore, GDP per capita does not explain
# airline safety better than historical safety records.