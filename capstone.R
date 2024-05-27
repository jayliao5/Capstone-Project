setwd("/Users/emilyzhang/Desktop/FUQUA/capstone")
data2 <- read_excel("Data 2nd version.xlsx")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gplots)

### Data summary
data2$GDP_trillion <- data2$GDP/1000
summary(data2)
head(data2)
str(data2)
summary(data2$GDP)
summary(data2$`Unemployment rate`)
summary(data2$`Interest rate - long term`)
summary(data2$`Inflation rate (CPI)`)

data2 <- na.omit(data2)
data2$lag_GDP <- lag(data2$GDP_trillion,1)
data2$GDP_increase_rate <- (data2$GDP_trillion-data2$lag_GDP)/data2$lag_GDP

### Visualization of macroeconomic factors over Time
## Trend graph of GDP, unemployment rate, interest rate and inflation rate over time
ggplot(data2, aes(x = observation_date)) +
  geom_line(aes(y = GDP_trillion, color = "GDP")) +
  geom_line(aes(y = `Unemployment rate`, color = "Unemployment Rate")) +
  geom_line(aes(y = `Interest rate - long term`, color = "Interest Rate")) +
  geom_line(aes(y = `Inflation rate (CPI)`, color = "Inflation Rate")) +
  labs(title = "Macroeconomic Factors Over Time", x = "Time", y = "Macroeconomic factors")
## Box plot of unemployment rate, interest rate and inflation rate showing their distributions
par(mfrow=c(1,1))
boxplot(data2$`Unemployment rate`,data2$`Interest rate - long term`,data2$`Inflation rate (CPI)`, data = data2, 
        main = "Boxplot of Value by Group",
        xlab = "Group", ylab = "Value",
        col = c("lightblue", "lightgreen", "lightpink"))
## Scatter plot of unemployment rate, interest rate and inflation rate over GDP
par(mfrow=c(1,3)) 
plot(data2$GDP_trillion, data2$`Unemployment rate`, main="GDP vs. Unemployment Rate", 
     xlab="GDP (billion of dollars)", ylab="Unemployment Rate (%)")
plot(data2$GDP_trillion, data2$`Interest rate - long term`, main="GDP vs. Interest Rate", 
     xlab="GDP (billion of dollars)", ylab="Interest Rate - longterm (%)")
plot(data2$GDP_trillion, data2$`Inflation rate (CPI)`, main="GDP vs. Inflation Rate", 
     xlab="GDP (billion of dollars)", ylab="Inflation Rate (%)")

### Correlation between each variable

dat_frame <- data_frame(data2$GDP_trillion, data2$`Unemployment rate`, data2$`Interest rate - long term`, data2$`Inflation rate (CPI)`)
correlation_matrix <- cor(dat_frame)
correlation_matrix

## Heat map showing correlation
library(ggplot2)
library(reshape2)
# Melt the correlation matrix to a long format
cor_melted <- melt(correlation_matrix)
# Create the heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), low = "lightblue", high = "lightpink", mid = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))

### Credit risk data
ggplot(data2, aes(x = observation_date)) +
  geom_line(aes(y = `Delinquency rate on all loans; All commercial banks (Seasonally adjusted)`, color = "All loans")) +
  geom_line(aes(y = `Delinquency rate on loans to finance agricultural production; All commercial banks (Seasonally adjusted)`, color = "Finance agricultural production loans")) +
  geom_line(aes(y = `Delinquency rate on business loans; All commercial banks (Seasonally adjusted)`, color = "Business loans")) +
  geom_line(aes(y = `Delinquency rate on loans secured by real estate; All commercial banks (Seasonally adjusted)`, color = "Loans secured by real estate")) +
  geom_line(aes(y = `Delinquency rate on consumer loans; All commercial banks (Seasonally adjusted)`, color = "Consumer loans")) +
  geom_line(aes(y = `Delinquency rate on single-family residential mortgages, booked in domestic offices; All commercial banks (Seasonally adjusted)`, color = "Single-family residential mortgages")) +
  geom_line(aes(y = `Delinquency rate on lease financing receivables; All commercial banks (Seasonally adjusted)`, color = "Lease financing receivables")) +
  geom_line(aes(y = `Delinquency rate on credit card loans; All commercial banks (Seasonally adjusted)`, color = "Credit card loans")) +
  geom_line(aes(y = `Delinquency rate on other consumer loans; All commercial banks (Seasonally adjusted)`, color = "Other consumer loans")) +
  geom_line(aes(y = `Delinquency rate on farmland loans, booked in domestic offices; All commercial banks (Seasonally adjusted)`, color = "Farmland loans")) +
  geom_line(aes(y = `Delinquency rate on commercial real estate loans (excluding farmland), booked in domestic offices; All commercial banks (Seasonally adjusted)`, color = "Commercial real estate loans (excluding farmland)")) +
  labs(x = "Date", y = "Delinquency Rate", title = "Trend of Delinquency Rates") +
  theme_minimal()


### Modeling
data2$all_loan <- data2$`Delinquency rate on all loans; All commercial banks (Seasonally adjusted)`
data2$financial_agricultural <- data2$`Delinquency rate on loans to finance agricultural production; All commercial banks (Seasonally adjusted)`
data2$business_loan <- data2$`Delinquency rate on business loans; All commercial banks (Seasonally adjusted)`
data2$real_estate <- data2$`Delinquency rate on loans secured by real estate; All commercial banks (Seasonally adjusted)`
data2$consumer_loan <- data2$`Delinquency rate on consumer loans; All commercial banks (Seasonally adjusted)`
data2$residential_mortgage <- data2$`Delinquency rate on single-family residential mortgages, booked in domestic offices; All commercial banks (Seasonally adjusted)`
data2$lease_financing_receivable <- data2$`Delinquency rate on lease financing receivables; All commercial banks (Seasonally adjusted)`
data2$credit_card_loan <- data2$`Delinquency rate on credit card loans; All commercial banks (Seasonally adjusted)`
data2$other_consumer_loan <- data2$`Delinquency rate on other consumer loans; All commercial banks (Seasonally adjusted)`
data2$farmland_loan <- data2$`Delinquency rate on farmland loans, booked in domestic offices; All commercial banks (Seasonally adjusted)`
data2$commercial_real_estate <- data2$`Delinquency rate on commercial real estate loans (excluding farmland), booked in domestic offices; All commercial banks (Seasonally adjusted)`
data2 <- subset(data2, select = -c(`Delinquency rate on all loans; All commercial banks (Seasonally adjusted)`,`Delinquency rate on loans to finance agricultural production; All commercial banks (Seasonally adjusted)`,
                                   `Delinquency rate on business loans; All commercial banks (Seasonally adjusted)`,`Delinquency rate on loans secured by real estate; All commercial banks (Seasonally adjusted)`,
                                   `Delinquency rate on consumer loans; All commercial banks (Seasonally adjusted)`, `Delinquency rate on single-family residential mortgages, booked in domestic offices; All commercial banks (Seasonally adjusted)`,
                                   `Delinquency rate on lease financing receivables; All commercial banks (Seasonally adjusted)`, `Delinquency rate on credit card loans; All commercial banks (Seasonally adjusted)`,
                                   `Delinquency rate on other consumer loans; All commercial banks (Seasonally adjusted)`,`Delinquency rate on farmland loans, booked in domestic offices; All commercial banks (Seasonally adjusted)`,
                                   `Delinquency rate on commercial real estate loans (excluding farmland), booked in domestic offices; All commercial banks (Seasonally adjusted)`))

data2$lag_all_loan <- lag(data2$all_loan, 1)
data2$lag_finc_agricultural <- lag(data2$financial_agricultural, 1)
data2$lag_business_loan <- lag(data2$business_loan, 1)
data2$lag_real_estate <- lag(data2$real_estate, 1)
data2$lag_consumer_loan <- lag(data2$consumer_loan, 1)
data2$lag_residential_mortgage <- lag(data2$residential_mortgage, 1)
data2$lag_lease_finc_receivable <- lag(data2$lease_financing_receivable, 1)
data2$lag_credit_card <- lag(data2$credit_card_loan, 1)
data2$lag_other_consumer <- lag(data2$other_consumer_loan, 1)
data2$lag_farmland_loan <- lag(data2$farmland_loan, 1)
data2$lag_commercial_real_estate <- lag(data2$commercial_real_estate,1)

model_all_loan <- lm(all_loan ~ lag_all_loan + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_finc_agricultual <- lm(financial_agricultural ~ lag_finc_agricultural + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_business_loan <- lm(business_loan ~ lag_business_loan + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_real_estate <- lm(real_estate ~ lag_real_estate + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_consumer_loan <- lm(consumer_loan ~ lag_consumer_loan + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_residential_mortgage <- lm(residential_mortgage ~ lag_residential_mortgage + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_lease_finc_receivable <- lm(lease_financing_receivable ~ lag_lease_finc_receivable + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_credit_card <- lm(credit_card_loan ~ lag_credit_card + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_other_consumer <- lm(other_consumer_loan ~ lag_other_consumer + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_farmland_loan <- lm(farmland_loan ~ lag_farmland_loan + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
model_commercial_real_estate <- lm(commercial_real_estate ~ lag_commercial_real_estate + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)

# Load necessary library
library(stargazer)
model_list <- list(model_all_loan, model_finc_agricultual, model_business_loan, model_real_estate, model_consumer_loan,
                   model_residential_mortgage, model_lease_finc_receivable, model_credit_card, model_other_consumer,
                   model_farmland_loan, model_commercial_real_estate)

stargazer(model_list,
          type = "text",
          title = "Time Series Model Results",
          align=TRUE,no.space=TRUE,
          out = "regression_table.txt")


## Time series model with the delinquency rate of previous period
credit_risk <- c("all_loan", "financial_agricultural", "business_loan",
                 "real_estate", "consumer_loan", "residential_mortgage", "lease_financing_receivable",
                 "credit_card_loan", "other_consumer_loan", "farmland_loan", "commercial_real_estate")
lag_model_results <- list()
for (i in credit_risk) {
  lag_i <- lag(data2[[i]], 1)
  model2 <- lm(data2[[i]] ~ lag_i + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
  lag_model_results[[i]] <- summary(model2)
}
lag_model_results


### Prediction
newdata <- read_excel("newdata.xlsx")
newdata$GDP_trillion <- newdata$GDP/1000
newdata$lag_GDP <- lag(newdata$GDP_trillion,1)
newdata$GDP_increase_rate <- (newdata$GDP_trillion-newdata$lag_GDP)/newdata$lag_GDP

predictions_0701 <- list()
newdata <- newdata[-1, ]
predictiondata <- data2[130, ]
newdata_20230701 <- newdata[1, ]
for (i in credit_risk) {
  lag_i <- lag(data2[[i]], 1)
  model2 <- lm(data2[[i]] ~ lag_i + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
  lag_model_results[[i]] <- summary(model2)
  # Extract coefficients from the model
  coefficients <- coef(model2)
  
  # Extract predictor variables from newdata
  lag_i_new <- predictiondata[[i]]
  GDP_increase_rate_new <- newdata_20230701$GDP_increase_rate
  Unemployment_rate_new <- newdata_20230701$`Unemployment rate`
  Interest_rate_long_term_new <- newdata_20230701$`Interest rate - long term`
  Inflation_rate_CPI_new <- newdata_20230701$`Inflation rate (CPI)`
  
  # Calculate predicted values manually using coefficients
  prediction <- coefficients[1] + 
    coefficients[2] * lag_i_new + 
    coefficients[3] * GDP_increase_rate_new + 
    coefficients[4] * Unemployment_rate_new + 
    coefficients[5] * Interest_rate_long_term_new + 
    coefficients[6] * Inflation_rate_CPI_new
  
  predictions_0701[[i]] <- prediction
}


predictions_1001 <- list()
newdata_20231001 <- newdata[2,]
for (i in credit_risk) {
  lag_i <- lag(data2[[i]], 1)
  model2 <- lm(data2[[i]] ~ lag_i + GDP_increase_rate + `Unemployment rate` + `Interest rate - long term` + `Inflation rate (CPI)`, data = data2)
  lag_model_results[[i]] <- summary(model2)
  # Extract coefficients from the model
  coefficients <- coef(model2)
  
  # Extract predictor variables from newdata
  lag_i_new <- predictions_0701[[i]]
  GDP_increase_rate_new <- newdata_20231001$GDP_increase_rate
  Unemployment_rate_new <- newdata_20231001$`Unemployment rate`
  Interest_rate_long_term_new <- newdata_20231001$`Interest rate - long term`
  Inflation_rate_CPI_new <- newdata_20231001$`Inflation rate (CPI)`
  
  # Calculate predicted values manually using coefficients
  prediction <- coefficients[1] + 
    coefficients[2] * lag_i_new + 
    coefficients[3] * GDP_increase_rate_new + 
    coefficients[4] * Unemployment_rate_new + 
    coefficients[5] * Interest_rate_long_term_new + 
    coefficients[6] * Inflation_rate_CPI_new
  
  predictions_1001[[i]] <- prediction
}
predictions_0701
predictions_1001

