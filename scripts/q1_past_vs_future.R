# q1_past_vs_future_analysis.R
# Description: This script investigates whether an airline's past crash rate
# is a statistically significant predictor of its future crash rate.
# It assesses the relationship between crash rates from 1985–1999 and 2000–2014
# using both correlation and linear regression.

library(dplyr)
library(ggplot2)

# Read in the data from our csv file 
data_airlines<-read.csv("data/1AirlineSafety.csv")

# Create crash rates (normalised by airline size)
data_airlines<-data_airlines%>%
  mutate(
    rate_past=incidents_85_99/avail_seat_km_per_week,
    rate_future=incidents_00_14/avail_seat_km_per_week)

# Remove any missing values
data_airlines<-na.omit(data_airlines)

# Correlation analysis of past vs future crash rates 
cor_past_future<-cor(data_airlines$rate_past,data_airlines$rate_future)
print(cor_past_future)

# Plot of the past vs future crash rates
p1<-ggplot(data_airlines,aes(x=rate_past,y=rate_future))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme_minimal()+
  labs(
    title="Past vs Future Airline Crash Rates",
    x="Past Crash Rate (1985–1999)",
    y="Future Crash Rate (2000–2014)"
  )
ggsave("plots/q1_past_vs_future.png",plot=p1,width=10,height=8)
print(p1)

# Regression model to evaluate the question “Does past crash rate help predict future crash rate?”
model_past<-lm(rate_future~rate_past,data=data_airlines)
summary(model_past)
png("plots/q1_residuals.png",width=800,height=600)
plot(model_past,which=1)
dev.off()

# Model diagnostics using a residuals vs fitted values plot in order to assess the suitability of the linear model.
plot(model_past,which=1)

# Interpretation 

# The correlation analysis shows a weak to moderately positive relationship between both past and future crash rates
# (r = 0.369), indicating that airlines with higher past crash rates tend to have slightly higher
# future crash rates. However, this relationship is not strong.

# The scatterplot supports this finding, showing a slight upward trend but with substantial dispersion
# of data points. Many airlines are clustered very close to zero crash rates in both periods, reflecting the
# rarity of such crashes actually occurring. Additionally, several outliers are present, which may have an influence on the fitted
# regression line.

# The linear regression model indicates that the past crash rate is a statistically significant predictor
# of future crash rates (β = 0.155, p = 0.005). This suggests that there is evidence of an association
# between past and future crash rates.

# However, the explanatory power of the model is low (R² = 0.136), meaning that only around 
# 13.6% of the variation in future crash rates is actually explained by past crash rates. This indicates that
# the majority of the variation is due to other factors not captured in the model.

# The residuals vs fitted values plot shows a largely random scatter around zero, suggesting that the
# assumptions of linear regression are reasonably satisfied. However, the wide spread of residuals
# indicates a considerable amount of unexplained variability, reinforcing the weak predictive performance of the model.

# In conclusion, while there is a statistically significant positive relationship between past and future crash
# rates, the relationship is weak and has limited practical predictive value. Therefore, an airline’s past
# crash history is not a reliable standalone indicator of its future safety performance.

# This weak relationship is likely due to the rarity of airline incidents and improvements in aviation
# safety over time, which reduces the influence of historical performance on future outcomes.