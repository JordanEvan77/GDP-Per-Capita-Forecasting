# 5305 Data Translation Challenge
# title: Group 1 Final Project - R code in R file
# author: Jennifer Grosz, Sohrab Rajabi, Josh Wolfe, Jordan Gropper
# date: 12/8/2021

# Ireland's growth behavior is unusual when compared to the other leading countries of the world.
# As a group we have looked at the GDP per capita for several different countries and found
# that Ireland's growth after the early 2000s is very robust, outpacing the large industrial
# powers of the world. We want to take a look at the behavior of this country through Time Series
# Analysis and see the patterns of this growth and if it is forecasted to be sustainable. Can 
# A country like Ireland continue to grow so rapidly coming out of the pandemic?

# The quarterly data that we gathered is from the Organisation For Economic Cooperation and 
# Development. It begins tracking in the 1990's all the way up through 2020.
# The data source originally has an international view that we reduce down to a single country
# Data Source:
# https://stats.oecd.org/index.aspx?queryid=66948#

# Load relevant packages
library(readxl)
library(forecast)
library(dynlm)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(Metrics)  # mae
library(urca)
library(tseries) 
library(TSstudio)
library(forecast)
library(vtable)
library(latticeExtra)
library(scales)
library(urca) # for kpss test
library(patchwork) # use to put graphs together in the same frame
library("reshape2")
# Data cleaning

## Load data
oecd <- read_csv("../data/oecd.csv")
head(oecd)

# Look at data
vtable(oecd)
# there are two different measurements: HCPCARSA and HVPVOBARSA
unique(oecd$MEASURE) 

#Per the documentation for this data set - (https://stats.oecd.org/fileview2.aspx?IDFile=a7e60ef1-5139-4a2c-9331-cfd135033b8a)
# HCPCARSA: US dollars per head, at current prices and current PPPs, annual levels, s.a.
# HVPVOBARSA: US dollars per head, at 2015 price level and 2015 PPPs, annual levels, s.a.
#I'm assuming we'll want HVPVOBARSA, but I will see if I can confirm that is the variable we want to use.

############################# Data tidying ############################# 
########################################################################

quarterly_oecd <- oecd %>% 
  filter(FREQUENCY == "Q", MEASURE == "HVPVOBARSA") %>%
  separate(Period, into = c("Quarter", "Year"), sep = "-") %>%
  mutate(Quarter = factor(Quarter), Year = factor(Year)) %>%
  select(Country, Quarter, Year, Unit, Value) 
vtable(quarterly_oecd)

# Find 5 countries with highest GDP in 2019
quarterly_oecd %>% 
  filter(Year == 2019) %>%
  group_by(Country) %>%
  summarize(avg_annual_gdp = (sum(Value)/4)) %>%
  arrange(-avg_annual_gdp)
# top5: Luxembourg, Ireland, Switzerland, Norway, and United States
# check data for top5 - looking to see what time periods each country has data for in this dataset

## Luxembourg
Luxembourg_quarterly <- quarterly_oecd %>%
  filter(Country == "Luxembourg") 
Luxembourg_quarterly %>% group_by(Year) %>% count()
# Luxembourg data starts in Q1 of 1995 through Q1 of 2021

## Ireland
Ireland_quarterly <- quarterly_oecd %>%
  filter(Country == "Ireland") 
Ireland_quarterly %>% group_by(Year) %>% count()
# Ireland data starts in Q1 of 1995 through Q2 of 2021

## Switzerland
Switzerland_quarterly <- quarterly_oecd %>%
  filter(Country == "Switzerland") 
Switzerland_quarterly %>% group_by(Year) %>% count()
#Switzerland data starts in Q1 of 1991 through Q1 of 2021

## Norway
Norway_quarterly <- quarterly_oecd %>%
  filter(Country == "Norway") 
Norway_quarterly %>% group_by(Year) %>% count()
#Norway data starts in Q1 of 1995 through Q2 of 2021

## United States
US_quarterly <- quarterly_oecd %>%
  filter(Country == "United States") 
US_quarterly %>% group_by(Year) %>% count()
# US data starts in Q1 of 1990 through Q3 of 2021

top_5_1995_to_2020 <- quarterly_oecd %>%
  filter(Country %in% c("Luxembourg", "Ireland", "Switzerland", "Norway", "United States"),
         Year %in% c(1995:2020)) 
q1_2021 <- quarterly_oecd %>%
  filter(Country %in% c("Luxembourg", "Ireland", "Switzerland", "Norway", "United States"), 
         Year == 2021, Quarter == "Q1")
df <- bind_rows(top_5_1995_to_2020, q1_2021)
df <- df %>%
  mutate(Country = factor(Country),
         Unit = factor(Unit)) %>%
  group_by(Country) %>%
  arrange(Year)
vtable(df)
# each country now has 105 observations

################################## EDA ################################# 
########################################################################

### Non-Graphical EDA
head(df)
# factor variables
table(df$Value)
table(df$Quarter)
df %>% group_by(Country, Quarter) %>% summarise(sum(Value))

### Graphical EDA
# create customized theme function starting with theme_classic()
clean_theme <- theme_classic() +
  theme(legend.direction = "horizontal", # create horizontal legend
        legend.position = "bottom", # put legend at bottom of graph
        legend.justification='left', # align legend to the left
        legend.title = element_blank(), # remove legend title
        axis.line.y = element_blank(), # remove y-axis line
        axis.ticks.y = element_blank(), # remove y-axis ticks
        axis.ticks.x = element_blank(), # remove x-axis ticks
        plot.title = element_text(face = "bold", size = 15)) # make graph title bold and a larger font

df %>% ggplot(mapping = aes(x = Country, y = Value, color = Country)) +
  geom_boxplot()+
  labs(title = "Variations of GDP by Country",
       subtitle = "Luxembourg has the hieghst GDP",
       x = "Country", y = "Values by USD",
       caption = "Source : OECD.org", tag = "Figure 1") +
  scale_y_continuous(labels = comma, breaks = seq(10000, 150000, 10000))

df %>% ggplot(mapping = aes(x = Year, y = Value, color = Country)) +
  geom_point() + 
  geom_smooth()

df %>% ggplot(mapping = aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_line() +
  geom_point()

# top 5 full plot 
top_5_full <- df %>% 
  group_by(Country, Year) %>% 
  summarise(Value = mean(Value)) %>%
  ggplot(mapping = aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_line() +
  labs(title = "Variations of GDP (per capita) over time by Country",
       subtitle = "Ireland has the most variation in GDP",
       x = "", y = "GDP per capita (in USD)",
       caption = "Source : OECD.org") +
  scale_y_continuous(labels=scales::dollar_format(), breaks = seq(10000, 150000, 10000)) +
  scale_x_discrete(breaks = c(1995, 2000, 2005, 2010, 2015, 2020 )) +
  labs(tag = "Figure 1") +
  clean_theme
top_5_full # output plot
ggsave(filename = "../figures/top_5_full.png", plot = top_5_full) # save plot

# final data set for project - filter to only include Ireland
df <-  df %>% 
  filter(Country == 'Ireland')

# Create Growth Rate data set for Linear Model
growth_rate_df <- df %>% 
  ungroup() %>%
  mutate(Quarter = case_when(
    Quarter == "Q1"~ 1,
    Quarter =="Q2" ~ 2,
    Quarter == "Q3" ~ 3,
    Quarter == "Q4" ~ 4)) %>%
  unite(Period,c(Year, Quarter), sep = "-")  %>% 
  mutate(Period = as.yearqtr(Period)) %>%
  select(-Unit, -Country)

growth_rate_df<- growth_rate_df %>%
  # first sort by year
  arrange(Period) %>%
  mutate(Diff_year = Period - lag(Period),  # Difference in time (just in case there are gaps)
         Diff_growth = Value - lag(Value), # Difference in Value between years
         Rate_percent = (Diff_growth / Diff_year)/Value) # growth rate  - not in percent

######################### Project Work Below ###########################
########################################################################

######################### RESEARCH QUESTION:
# Is the development of Ireland sustainable from this point forward, considering how aggressively
# they have grown over the past 30 years

# create time series data set
df1 <- ts(df$Value, frequency=4, start = c(1995,1))
plot(df1,col='red')
hist(df1) 
######################### Create Stationary Time Series Data
# perform log difference transformation on the time series - converts data to per capita GDP Growth Rate over years 
logdf1 <- diff(log(df1))

## ADF/ Augmented Dickey Fuller - test for stationarity
# origional time series:
adf.test(df1) # P-val = 0.98 = non-stationary
dickey_fuller_og <- adf.test(df1)  # save for plot
# transformed time series:
adf.test(logdf1) # p-val = 0.08 = non-stationary
dickey_fuller_transformed <- adf.test(logdf1) # save for plot
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for stationarity
logdf1 %>% ur.kpss() %>% summary() # confirms we're good at 10 % level

# plot time series
plot(logdf1)  # plot time series data
hist(logdf1)  # plot histogram of time series data

######################### ACF
acf(logdf1, lag.max = 20, plot = TRUE)
acf(logdf1, lag.max = 20, plot = FALSE)
######################### PACF
pacf(logdf1, lag.max = 20, plot = TRUE)
pacf(logdf1, lag.max = 20, plot = FALSE)

########################### Model Creation ############################
#######################################################################

########################### Simple Models #############################
# Linear Model
linear_mod <- lm(Rate_percent ~ Period, data = growth_rate_df)
summary(linear_mod)  # view model
aic_lm <- AIC(linear_mod) # get AIC
bic_lm <- BIC(linear_mod) # get bBIC
checkresiduals(model) # plot residuals

# Fixed Scheme Model
fixed_scheme <- numeric(20) 
model <- dynlm(logdf1 ~ stats::lag(logdf1, -1) + stats::lag(logdf1,-3), start = c(1955,1), end = c(2016,1)) 
aic_fixed <- AIC(model) 
bic_fixed <- BIC(model)
checkresiduals(model)

# Recursive Scheme Model
recursive_scheme <- numeric(20) 
for (i in 1:20){
  model <- dynlm(logdf1 ~ stats::lag(logdf1, 1)+ stats::lag(logdf1,2) + stats::lag(logdf1,3), start = c(1955,1),end = c(2016,1))
  recursive_scheme[i] <- coef(model)[1]+coef(model)[2]*logdf1[84+i]+coef(model)[3]*logdf1[82+i]}
aic_recursive <- AIC(model)
bic_recursive <- BIC(model)
checkresiduals(model)

# Rolling Scheme Model
rolling_scheme <- numeric(20)
for (i in 1:20){
  model <- dynlm(logdf1 ~ stats::lag(logdf1, 2)+ stats::lag(logdf1,4) + stats::lag(logdf1,6), start = c(1955,1),end = c(2016,1))
  rolling_scheme[i] <- coef(model)[1]+coef(model)[2]*logdf1[84+i]+coef(model)[3]*logdf1[82+i]}
aic_rolling <- AIC(model)
bic_rolling <- BIC(model)
checkresiduals(model)

########################### Complex Models #############################

# Create AR(1) model
ar1 <- arima(logdf1, order = c(1,0,0))
summary(ar1) # MAE: 0.02004366
autoplot(ar1) # Check invertability
checkresiduals(ar1) # Q* = 12.27, df = 6, p-value = 0.05621
aic1 <- AIC(ar1)  #Akaike IC -420.3541
bic1 <- BIC(ar1) #Bayesian IC -412.4209

# Create AR(2) model
ar2 <- arima(logdf1, order = c(2,0,0))
summary(ar2) # MAE: 0.0199994
autoplot(ar2)# Check invertability
checkresiduals(ar2) # Q* = 8.2398, df = 5, p-value = 0.1435 - good
aic2 <- AIC(ar2) # -420.4014
bic2 <- BIC(ar2)#  -409.8238

# Create MA(1) Model 
ma1 <- arima(logdf1, order = c(0,0,1))
summary(ma1) # MAE 0.02037859
autoplot(ma1)# Check invertability
checkresiduals(ma1) # Q* = 14.624, df = 6, p-value = 0.02339 - NOT GOOD
aic3 <- AIC(ma1) # -416.9754
bic3 <- BIC(ma1) # -409.0422

# Create MA(2) Model
ma2 <- arima(logdf1, order = c(0,0,2))
summary(ma2) # MAE 0.01970498
autoplot(ma2)# Check invertability
checkresiduals(ma2) # Q* = 5.8637, df = 5, p-value = 0.3197 - good
aic4 <- AIC(ma2)# -422.1683
bic4 <- BIC(ma2) # -411.5907

# Create ARMA(1,1) model
arma11 <- arima(logdf1, order = c(1,0,1))
summary(arma11)# MAE  0.02006957
autoplot(arma11)# Check invertability
checkresiduals(arma11) # Q* = 9.8709, df = 5, p-value = 0.07898 - good
aic5 <- AIC(arma11) # -419.5352
bic5 <- BIC(arma11) # -408.9577

# Create ARMA(2,2) model
arma22 <- arima(logdf1, order = c(2,0,2))
summary(arma22) # MAE: 0.01920198
autoplot(arma22)# Check invertability
checkresiduals(arma22) # Q* = 3.5449, df = 3, p-value = 0.315 - good
aic6 <- AIC(arma22) # -420.6613
bic6 <- BIC(arma22) # -404.7949

# Create ARMA(1,2) model
arma12 <- arima(logdf1, order = c(1,0,2))
summary(arma12)# MAE 0.01915874
autoplot(arma12)# Check invertability
checkresiduals(arma12) # Q* = 3.5449, df = 3, p-value = 0.315 - good
aic7 <- AIC(arma12) # -420.6613
bic7 <- BIC(arma12) # -404.7949

# Create ARMA(2,1) model
arma21 <- arima(logdf1, order = c(2,0,1))
summary(arma21) # MAE: 0.01920357
autoplot(arma21)# Check invertability
checkresiduals(arma21) # Q* = 3.7862, df = 4, p-value = 0.4357
aic8 <- AIC(arma21) # -422.5554
bic8 <- BIC(arma21) # -409.3335

########################### Validating Models ##########################
########################################################################

## If we wanted to focus on three specific models, I'd recommend these ones for our forecasting projections
AIC <- round(c(aic_lm, aic_fixed, aic_recursive, aic_rolling, aic1, aic2, aic3, aic4, aic5,aic6,aic7,aic8), 3)
min(AIC) ## -422.555 from ****ARMA(2,1)****
BIC <- round(c(bic_lm, bic_fixed, bic_recursive, bic_rolling, bic1, bic2, bic3, bic4, bic5, bic6, bic7, bic8), 3)
min(BIC) ## 412.421 ****AR1(1)****

# Build results table
names = c("Linear OLS", "Fixed Scheme", "Recursive Scheme", "Rolling Scheme", "AR(1)", "AR2(2)", "MA(1)", "MA(2)", "ARMA(1,1)", "ARMA(2,2)","ARMA(1,2)", "ARMA(2,1)")
aicbic_results <- data.frame("Name" = names,
                             "AIC" = AIC,
                             "BIC" = BIC)
# Table output:
aicbic_results %>%
  arrange(AIC) %>%
  kable(caption = 'Information Criteria Table', booktabs = TRUE) %>%
  kable_styling(latex_options = "striped", full_width = F)

###################### Out of Sample Evaluation ########################
########################################################################

# Split data into train and test sets
train <- window(logdf1, end = c(2016,1))
test <- window(logdf1, start=c(2016,2))

########################### Chosen Models ##############################
# ARMA(2,1) Model
arma21_train <- arima(train, order = c(2,0,1)) # create model with training set
arma21_fcast <- forecast(arma21_train, h = 20) # create forecast

# MA(2) Model
ma2_train <- arima(train, order = c(0,0,2))
ma2_fcast <- forecast(ma2_train, h = 20)

# AR(1) Model
ar1_train <- arima(train, order = c(1,0,0))
ar1_fcast <- forecast(ar1_train, h = 20)

###################### Validate Model Accuracy  ########################
########################################################################

# accuracy of ARMA(2,1) forecast of test data
errors_arma21 <- forecast::accuracy(arma21_fcast, test) %>%
  as.data.frame()
mae_arma21 <- errors_arma21["MAE"][2,1]
mae_arma21_train <- errors_arma21["MAE"][1,1]

# accuracy of MA(2) forecast of test data
errors_ma2 <-forecast::accuracy(ma2_fcast, test) %>%
  as.data.frame()
mae_ma2 <- errors_ma2["MAE"][2,1]
mae_ma2_train <- errors_ma2["MAE"][1,1]

# accuracy of AR(1) forecast of test data
errors_ar1 <-forecast::accuracy(ar1_fcast, test) %>%
  as.data.frame()
mae_ar1 <- errors_ar1["MAE"][2,1]
mae_ar1_train <- errors_ar1["MAE"][1,1]

# Build results table
MAE_Training <- c(mae_arma21_train, mae_ar1_train, mae_ma2_train)
MAE_Test <- c(mae_arma21, mae_ar1, mae_ma2)
Model <- c("ARMA(2,1)","AR(1)", "MA(2)")
forecasting_error_results <- data.frame(Model, MAE_Training, MAE_Test)

# Table output:
forecasting_error_results  %>%
  arrange(MAE_Test) %>%
  kable(caption = 'Out of Sample Test Results', booktabs = TRUE) %>%
  kable_styling(latex_options = "striped", full_width = F)

# Statistical Test to Validate Results from Out of Sample Evaluation Model Accuracy #
####################################################################################

# t testing errors
error_arma21 <- arma21_fcast$residuals
error_ar1 <- ar1_fcast$residuals
error_ma2 <- ma2_fcast$residuals

# output results
t.test(error_arma21, error_ar1)
t.test(error_arma21, error_ma2)
t.test(error_ar1, error_ma2)

################## Plots for Out of Sample Validation ##################
########################################################################

# ARMA(2,1)
# autoplot actual vs predicted
auto_arma21_predicted_vs_actual <- autoplot(arma21_fcast) + autolayer(test)  # plots predictions vs actual
auto_arma21_predicted_vs_actual # shows confidence interval

################## create custom plot 
arma21_preds<- as.data.frame(arma21_fcast) # convert forecast to data frame
arma21_preds <- arma21_preds["Point Forecast"] # extract predicted values
arma21_preds <- as_vector(arma21_preds) # convert predicted values to a vector
arma21_preds <- ts(arma21_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
prediction <- arma21_preds
testandpred <- cbind(test, prediction) # put up above for safety
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
test_periods <- growth_rate_df %>%
  filter(Period > "2016 Q1") # get dates from this data set
period <- test_periods["Period"] # get periods from this df
d <- cbind(period, value) # column bind into one data set
# melt data set for plotting
d <- d %>%
  melt( id.vars=c("Period"), value.name=c("Value"), variable.name=c("Dataset")) 
# create plot
arma21_predicted_vs_actual <- d %>%
  group_by(Dataset) %>%
  ggplot(aes(x = Period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line() +
  ggtitle("ARMA(2,1) predictions vs actual") +
  ylab("")+
  xlab("")+
  theme_bw() +
  clean_theme +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') 
arma21_predicted_vs_actual


# MA(2) 
#autoplot actual vs predicted
auto_ma2_predicted_vs_actual <- autoplot(ma2_fcast) + autolayer(test)  # plots predictions vs actual
auto_ma2_predicted_vs_actual # shows confidence interval

################## create custom plot 
ma2_preds <- as.data.frame(ma2_fcast) # convert forecast to data frame
ma2_preds <- ma2_preds["Point Forecast"] # extract predicted values
ma2_preds <- as_vector(ma2_preds) # convert predicted values to a vector
ma2_preds <- ts(ma2_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
prediction <- ma2_preds
testandpred <- cbind(test, prediction) # put up above for safety
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
period <- test_periods["Period"] # get periods from this df
d <- cbind(period, value) # column bind into one data set
d <- d %>%
  melt( id.vars=c("Period"), value.name=c("Value"), variable.name=c("Dataset")) 
ma2_predicted_vs_actual <- d %>%
  group_by(Dataset) %>%
  ggplot(aes(x = Period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line() +
  ggtitle("MA(2) predictions vs actual", 
          subtitle = "Predicting last 20 periods (2016-Q2 to 2021-Q1)")  +
  ylab("")+
  xlab("")+
  theme_bw() +
  clean_theme +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') 
ma2_predicted_vs_actual


# AR(1) 
# Autoplot actual vs predicted
auto_ar1_predicted_vs_actual <- autoplot(ar1_fcast) + autolayer(test)
auto_ar1_predicted_vs_actual

################## create custom plot 
ar1_preds<- as.data.frame(ar1_fcast) # convert forecast to data frame
ar1_preds <- ar1_preds["Point Forecast"] # extract predicted values
ar1_preds <- as_vector(ar1_preds) # convert predicted values to a vector
ar1_preds <- ts(ar1_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
prediction <- ar1_preds
testandpred <- cbind(test, prediction) # put up above for safety
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
period <- test_periods["Period"] # get periods from this df
d <- cbind(period, value) # columm bind into one data set
d <- d %>%
  melt( id.vars=c("Period"), value.name=c("Value"), variable.name=c("Dataset")) 
ar1_predicted_vs_actual <- d %>%
  group_by(Dataset) %>%
  ggplot(aes(x = Period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line() +
  ggtitle("AR(1) predictions vs actual")  +
  ylab("")+
  xlab("")+
  theme_bw() +
  clean_theme +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') 
ar1_predicted_vs_actual

# combine for paper - custom plots for out of sample evaluation 
chosen_actual_vs_predicted_test <- ma2_predicted_vs_actual / ar1_predicted_vs_actual / arma21_predicted_vs_actual
chosen_actual_vs_predicted_test
ggsave(filename = "../figures/TestingModels/chosen_actual_vs_predicted_test.png", plot = chosen_actual_vs_predicted_test)

# create single plot for out of sample evaluation
plot(test,col='black')
lines(test, col = 'black')
lines(ma2_preds,col='red',  lwd=2.0)
lines(ar1_preds,col='green',  lwd=2.0)
lines(arma21_preds,col='purple',  lwd=2.0)
legend("topleft", legend = c("Actual", "MA(2)", "AR(1) ", "ARMA(2,1)"),
       col = c("black", "red", "green", "purple"), pch = c(19,19),  cex = 0.9) 
title(main = "Actual vs Predicted (Test Set)", adj = 0)

########################### Future Forecasts ######################### 
# ARMA(2,1)
arma21_future_fcast <- forecast(arma21, h = 20) 
autoplot(arma21_future_fcast) + autolayer(logdf1)  

# MA(2)
ma2_future_fcast <- forecast(ma2, h = 20) # create forecast 40 periods after end of data set (Q1 2021) - through 2031 Q1
# autoplot of current data + projection  through 2026 Q1
autoplot(ma2_future_fcast) + autolayer(logdf1)  

# AR(1)
ar1_future_fcast <- forecast(ar1, h = 20) 
# autoplot of current data + projection  through 2026 Q1
autoplot(arma21_future_fcast) + autolayer(logdf1)  

########################### Create Final Plots ########################### 
##########################################################################
# create visual for showing original ts data vs log-difference ts data
origional_time_series <- df1
transformed_time_series <- logdf1
Quarterly_GDP <- cbind(origional_time_series, transformed_time_series) # put up above for safety
head(Quarterly_GDP)
# replace NA with zero
Quarterly_GDP <- Quarterly_GDP %>%
  replace_na(replace = 0.00)
components <- decompose(Quarterly_GDP) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
period <- growth_rate_df["Period"] # get periods from this df
d <- cbind(period, value) # column bind into one data set
subtitle_og <- paste("Augmented Dickey Fuller p-value: ", as.character(round(dickey_fuller_og$p.value, 4))) # set plot subtitle
# plot original ts data
original_data_plotted <- d %>%
  ggplot(aes(x = Period, y = origional_time_series)) +
  geom_line(color = "red")+
  ggtitle("Time series plotted over Quarters", 
          subtitle = subtitle_og)+
  ylab("GDP (per capita)") +
  xlab("") +
  scale_y_continuous(labels=scales::dollar_format()) +
  clean_theme +
  labs(tag = "Figure 2")
d %>%
  select(transformed_time_series) %>%
  summarize(mean(transformed_time_series)) # mean growth rate 0.01074465 * 100  = 1.074465% growth rate
subtitle_transformed = paste("Transformed time series is a measure of growth rate", "\nAugmented Dickey Fuller p-value: ",as.character(round(dickey_fuller_transformed$p.value, 4))) # set plot subtitle
# plot transformed data
logdf1_plotted <- d %>%
  ggplot(aes(x = Period, y = transformed_time_series)) +
  geom_line(color = "red")+
  ggtitle("Stationary time series plotted over Quarters", 
          subtitle = subtitle_transformed)+
  ylab("Growth Rate of GDP (per capita)") +
  xlab("") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_hline(yintercept=0.01074465, linetype="dashed", color = "black") +
  annotate("text", x = 1997, y =- 0.0, label = "1.07% mean growth rate", color = "black", fontface =2) +
  clean_theme +
  labs(caption = "Source : OECD.org")
# combine into one plot
plotted_tsdata <- original_data_plotted / logdf1_plotted # save graphs together into one image
plotted_tsdata # out plot 
ggsave(filename = "../figures/plotted_tsdata.png", plot = plotted_tsdata)



# create quarters df
q1 <- as.yearqtr(c(1995:2026)) + 0/4
q2 <- as.yearqtr(c(1995:2026)) + 1/4
q3 <- as.yearqtr(c(1995:2026)) + 2/4
q4 <- as.yearqtr(c(1995:2026)) + 3/4
period <- c(q1,q2,q3,q4)
quarters <- as.data.frame(period)
quarters <- quarters %>%
  arrange(period) %>% 
  filter(period <"2026 Q2", period >"1995 Q1")

# create final plot for ARMA(2,1)
arma21_final <- arima(train, order = c(2,0,1)) # create model with training set
arma21_final_fcast <- forecast(arma21_final, h = 40) # create forecast out 20 periods for validation and another 20 periods for future projection (ends in 2026 Q1)
arma21_final_fcast_preds <- as.data.frame(arma21_final_fcast) # convert forecast to data frame
arma21_final_fcast_preds <- arma21_final_fcast_preds["Point Forecast"] # extract predicted values
arma21_final_fcast_preds <- as_vector(arma21_final_fcast_preds) # convert predicted values to a vector
arma21_final_fcast_preds <- ts(arma21_final_fcast_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
testandpred <- cbind(logdf1, arma21_final_fcast_preds) # put up above for safety
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
d <- cbind(quarters, value) # columm bind into one data set
d <- d %>%
  melt( id.vars=c("period"), value.name=c("Value"), variable.name=c("Dataset")) 
d %>%
  filter(Dataset == "arma21_final_fcast_preds") %>%
  drop_na() %>%
  summarize(mean(Value))
# create plot
arma21_optimal_forecast <- d %>%
  mutate(Dataset = case_when(
    Dataset == "logdf1" ~ "actual",
    Dataset == "arma21_final_fcast_preds" ~ "ARMA(2, 1) forecast")) %>%
  group_by(Dataset) %>%
  ggplot(aes(x = period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line(stat = "identity") +
  ggtitle("ARMA(2,1)",
          subtitle = "Future projections show an average growth rate of 1.1%") +
  ylab("")+
  xlab("")+
  clean_theme +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') +
  geom_vline(xintercept =  2016.2, linetype="dotted", 
             color = "blue", size=1) +
  geom_vline(xintercept =  2021, linetype="dotted", 
             color = "blue", size=1) + 
  annotate("text", x = 2005, y = .15, label = "Estimation Period", color = "blue") +
  annotate("text", x = 2018.5, y = .15, label = "Validation \nPeriod", color = "blue") +
  annotate("text", x = 2024, y = .15, label = "Future \nForecasts", color = "blue") +
  annotate("text", x = 2027, y = 0.01100336, label = "1.1%", color = "blue")
arma21_optimal_forecast

# create final plot for AR(1)
ar1_final <- arima(train, order = c(1,0,0)) # create model with training set
ar1_final_fcast <- forecast(ar1_final, h = 40) # create forecast out 20 periods for validation and another 20 periods for future projection (ends in 2026 Q1)
ar1_final_fcast_preds <- as.data.frame(ar1_final_fcast) # convert forecast to data frame
ar1_final_fcast_preds <- ar1_final_fcast_preds["Point Forecast"] # extract predicted values
ar1_final_fcast_preds <- as_vector(ar1_final_fcast_preds) # convert predicted values to a vector
ar1_final_fcast_preds <- ts(ar1_final_fcast_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
testandpred <- cbind(logdf1, ar1_final_fcast_preds) 
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
d <- cbind(quarters, value) # columm bind into one data set
d <- d %>%
  melt( id.vars=c("period"), value.name=c("Value"), variable.name=c("Dataset")) 
d %>%
  filter(Dataset == "ar1_final_fcast_preds") %>%
  drop_na() %>%
  summarize(mean(Value))
ar1_optimal_forecast <- d %>%
  mutate(Dataset = case_when(
    Dataset == "logdf1" ~ "actual",
    Dataset == "ar1_final_fcast_preds" ~ "AR(1) forecast")) %>%
  group_by(Dataset) %>%
  ggplot(aes(x = period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line(stat = "identity") +
  ggtitle("AR(1)",
          subtitle =  "Future projections show an average growth rate of 0.95%") +
  ylab("")+
  xlab("")+
  theme_bw() +
  clean_theme +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') +
  geom_vline(xintercept =  2016.2, linetype="dotted", 
             color = "blue", size=1) +
  geom_vline(xintercept =  2021, linetype="dotted", 
             color = "blue", size=1) + 
  annotate("text", x = 2005, y = .15, label = "Estimation Period", color = "blue") +
  annotate("text", x = 2018.5, y = .15, label = "Validation \nPeriod", color = "blue") +
  annotate("text", x = 2024, y = .15, label = "Future \nForecasts", color = "blue") +
  annotate("text", x = 2027, y = 0.009453891, label = "0.95%", color = "blue")
ar1_optimal_forecast

# create final plot for MA(2)
ma2_final <- arima(train, order = c(0,0,2))# create model with training set
ma2_final_fcast <- forecast(ma2_final, h = 40) # create forecast out 20 periods for validation and another 20 periods for future projection (ends in 2026 Q1)
ma2_final_fcast_preds <- as.data.frame(ma2_final_fcast) # convert forecast to data frame
ma2_final_fcast_preds <- ma2_final_fcast_preds["Point Forecast"] # extract predicted values
ma2_final_fcast_preds <- as_vector(ma2_final_fcast_preds) # convert predicted values to a vector
ma2_final_fcast_preds <- ts(ma2_final_fcast_preds, frequency = 4,  start=c(2016,2)) # convert vector of predicted values to a time series
testandpred <- cbind(logdf1, ma2_final_fcast_preds) # put up above for safety
testandpred
#testandpred <- testandpred %>% replace_na(0)
head(testandpred)
components <- decompose(testandpred) # get values out of ts
value <- as_tibble(components[["x"]]) # make into tibble of components
d <- cbind(quarters, value) # columm bind into one data set
d # check d looks correct
d <- d %>%
  melt( id.vars=c("period"), value.name=c("Value"), variable.name=c("Dataset"))  
d %>%
  filter(Dataset == "ma2_final_fcast_preds") %>%
  drop_na() %>%
  summarize(mean(Value))
ma2_optimal_forecast <-d %>%
  mutate(Dataset = case_when(
    Dataset == "logdf1" ~ "actual",
    Dataset == "ma2_final_fcast_preds" ~ "MA(2) forecast")) %>%
  group_by(Dataset) %>%
  ggplot(aes(x = period,y = Value,  group = Dataset, color = Dataset)) +
  geom_line(stat = "identity") +
  ggtitle("MA(2)",
          subtitle = "Future projections show an average growth rate of 0.93%") +
  ylab("")+
  xlab("")+
  theme_bw() +
  clean_theme +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_hline(yintercept= 0, color = "black",linetype='dotted') +
  geom_vline(xintercept =  2016.2, linetype="dotted", 
             color = "blue", size=1) +
  geom_vline(xintercept =  2021, linetype="dotted", 
             color = "blue", size=1) + 
  annotate("text", x = 2005, y = .15, label = "Estimation Period", color = "blue") +
  annotate("text", x = 2018.5, y = .15, label = "Validation \nPeriod", color = "blue") +
  annotate("text", x = 2024, y = .15, label = "Future \nForecasts", color = "blue")  +
  annotate("text", x = 2027, y = 0.009336972, label = "0.93%", color = "blue")
ma2_optimal_forecast

all_optimal_forecast_together <- arma21_optimal_forecast / ma2_optimal_forecast / ar1_optimal_forecast 
all_optimal_forecast_together
ggsave(filename = "../figures/TestingModels/complete_process_optimal_forecasts.png", plot = all_optimal_forecast_together)


