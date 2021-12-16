# 5305 DTC
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
# we will want to view data stationarity to confirm that it is useful
# apply ACF and PACF analysis. Below is just some sample code for outline:



# Data cleaning

oecd <- read_csv("data/oecd.csv")

quarterly_oecd <- oecd %>% 
  filter(FREQUENCY == "Q", MEASURE == "HVPVOBARSA") %>%
  separate(Period, into = c("Quarter", "Year"), sep = "-") %>%
  mutate(Quarter = factor(Quarter), Year = factor(Year)) %>%
  select(Country, Quarter, Year, Unit, Value) 

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



df %>% group_by(Country, Quarter) %>% summarise(sum(Value))

#Plotting----
df %>% ggplot(mapping = aes(x = Country, y = Value, color = Country)) +
  geom_boxplot()+
  labs(title = "Variations of GDP by Country",
       subtitle = "Luxembourg has the highest GDP",
       x = "Country", y = "Values by USD",
       caption = "Source : OECD.org", tag = "Figure 1") +
  scale_y_continuous(breaks = seq(10000, 150000, 10000))


df %>% ggplot(mapping = aes(x = Year, y = Value, color = Country)) +
  geom_point() + 
  geom_smooth()

df %>% ggplot(mapping = aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_line() +
  geom_point()


top_5_full <- df %>% group_by(Country, Year) %>% summarise(Value = mean(Value)) %>%
  ggplot(mapping = aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "Variations of GDP per Year by Country",
       subtitle = "Ireland has the most variation in GDP",
       x = "Years", y = "Values by USD",
       caption = "Source : OECD.org", tag = "Figure 2") +
  scale_y_continuous(breaks = seq(10000, 150000, 10000))

ggsave(filename = "top_5_full.png", plot = top_5_full)

# Is the development of Ireland sustainable from this point forward, considering how aggressively
# they have grown over the past 30 years

df <-  df %>% 
  filter(Country == 'Ireland')
df1 <- ts(df$Value, frequency=4, start = 1995)
df1sp <- ts(df$Value, frequency=4, start = 1995)


ireland_plot <- plot(df1sp,col='red')
#lines(df2,col='blue')
#lines(df3,col='brown')
#lines(df4,col='purple')
#lines(df5,col='black')
ggsave(filename = "ireland_plot.png", plot = ireland_plot)


# df1 adjustment
hist(df1)
acf(df1, lag.max = 20)
pacf(df1, lag.max = 20)

logdf1 <- diff(log(df1))
write.csv(logdf1, "data/logdf1.csv")
summary(logdf1)
print(logdf1)

plot(logdf1)  # is the variance instability a concern?
hist(logdf1)  
adjusted_acf <- acf(logdf1, lag.max = 20, plot = TRUE)
acf(logdf1, lag.max = 20, plot = FALSE)
adjusted_pacf <- pacf(logdf1, lag.max = 20)




#Guidance:
#  Try multiple models (ch9) try and apply them and measure their error and difference, then compare_tbls()
#Work to measure the differences in p val and t tests etc.


# SIMPLE Model Attempts: ----
dfq <- ts(df$Value, frequency=4, start = c(1995,1))
#dfq <- as.numeric(dfq) # turn off for now
dfq <- as.numeric(logdf1)
dfq
linear_mod <- lm(formula = Value ~ Year, data = df)
summary(linear_mod)  # why is our linear model so terrible? R2 - 0.09204

fcast1 <- numeric(20)#empty vector
model <- dynlm(dfq ~ lag(dfq, 1)+ lag(dfq,3), start=1, end =85)
model
for (i in 1:20){
  fcast1[i] <- coef(model)[1]+coef(model)[2]*dfq[83+i]+coef(model)[3]*dfq[81+i]
}#fills in forecasted coefficients.

# three lag periods
fcast2 <- numeric(20) #empty
for (i in 1:20){
  model <- dynlm(dfq ~ lag(dfq, 1)+ lag(dfq,2) + lag(dfq,3), start=1, end =85)
  #model <- dynlm(dfq ~lag(dfq, -1)+ lag(dfq, -2), start=c(1995,1), end=c(2015,4+i))#continuely builds
  fcast2[i] <- coef(model)[1]+coef(model)[2]*dfq[83+i]+coef(model)[3]*dfq[82+i]#fills in values with forecast
}
#large three lag
fcast3 <- numeric(20)
for (i in 1:20){
  model <- dynlm(dfq ~ lag(dfq, 2)+ lag(dfq,4) + lag(dfq,6), start=1, end=85)
  fcast3[i] <- coef(model)[1]+coef(model)[2]*dfq[83+i]+coef(model)[3]*dfq[80+i]
}

#NAIVE MODEL:
fcast4 <- numeric(20)#empty
for (i in 1:20){
  fcast4[i] <- dfq[83+i]
}

#average of previous 4:

fcast5 <- numeric(20)
for (i in 1:20){
  fcast5 <- (dfq[83+i]+dfq[82+i]+dfq[81+i]+dfq[80+i])/4 
}

#g0<-window(df1sp, start = c(2017))
g0<-window(logdf1, start = c(2017))
f1<-ts(fcast1, frequency = 4, start=c(2017))
f2<-ts(fcast2, frequency = 4, start=c(2017))
f3<-ts(fcast3, frequency = 4, start=c(2017))
f4<-ts(fcast4, frequency = 4, start=c(2017))
f5<-ts(fcast5, frequency = 4, start=c(2017))
plot(g0,col='blue')
lines(f1,col='red')
lines(f2,col='green')
lines(f3,col='brown')
lines(f4,col='purple')
lines(f5,col='black')


# ----

Box.test(logdf1, lag = 1, type = c("Ljung-Box"), fitdf = 0)  # X-squared = 0.001637, df = 1, p-value = 0.9677 

ggplot(data = df, aes(x= linear_mod$residuals)) + 
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'residuals', y = 'count')  # hist of residuals
# should we have a hist of residuals for our arma models?

# how do our set of models look?
# how does the residuals hist look?
# what should we do about our fit (as far as it looks when it is graphed?)
# are there more testing mechanisms that we should use beyond AIC, MAE, R^2 and Ljung?

########################### Nov 22 Updates

## ADF/ Agumented Dickey Fuller
adf.test(df1) # P-val = 0.98 = non-stationary
adf.test(logdf1) # p-val = 0.08 = non-stationary

# Create AR(1) model
ar1 <- arima(logdf1, order = c(1,0,0))
################################################# QUESTION FOR PROFESSOR - when do we do include mean = FALSE?
summary(ar1) # MAE: 0.02004366
autoplot(ar1) # Check invertability
checkresiduals(ar1) # Q* = 12.198, df = 7, p-value = 0.09422 - good
# If the p value is greater than 0.05 then the residuals are independent 
# which we want for the model to be correct
r1 <- cor(fitted(ar1),logdf1)^2   # R2 - 0.1036239
aic1 <- AIC(ar1)  #Akaike IC -420.3541
bic1 <- BIC(ar1) #Bayesian IC -412.4209

# Create AR(2) model
ar2 <- arima(logdf1, order = c(2,0,0))
summary(ar2) # MAE: 0.0199994
autoplot(ar2)# Check invertability
checkresiduals(ar2) # Q* = 8.2398, df = 5, p-value = 0.1435 - good
r2 <- cor(fitted(ar2),logdf1)^2   # .1214867
aic2 <- AIC(ar2) # -420.4014
bic2 <- BIC(ar2)#  -409.8238

# Create MA(1) Model - WORST MODEL
ma1 <- arima(logdf1, order = c(0,0,1))
summary(ma1) # MAE 0.02037859
autoplot(ma1)# Check invertability
checkresiduals(ma1) # Q* = 14.624, df = 6, p-value = 0.02339 - NOT GOOD
r3 <- cor(fitted(ma1),logdf1)^2  # 0.07734524
aic3 <- AIC(ma1) # -416.9754
bic3 <- BIC(ma1) # -409.0422

# Create MA(2) Model
ma2 <- arima(logdf1, order = c(0,0,2))
summary(ma2) # MAE 0.01970498
autoplot(ma2)# Check invertability
checkresiduals(ma2) # Q* = 5.8637, df = 5, p-value = 0.3197 - good
r4 <- cor(fitted(ma2),logdf1)^2  # 0.1366673
aic4 <- AIC(ma2)# -422.1683
bic4 <- BIC(ma2) # -411.5907

# Create ARMA(1,1) model
arma11 <- arima(logdf1, order = c(1,0,1))
summary(arma11)# MAE  0.02006957
autoplot(arma11)# Check invertability
checkresiduals(arma11) # Q* = 9.8709, df = 5, p-value = 0.07898 - good
r5 <- cor(fitted(arma11),logdf1)^2  # 0.1139519
aic5 <- AIC(arma11) # -419.5352
bic5 <- BIC(arma11) # -408.9577

# Create ARMA(2,2) model
arma22 <- arima(logdf1, order = c(2,0,2))
summary(arma22) # MAE: 0.01920198
autoplot(arma22)# Check invertability
checkresiduals(arma22) # Q* = 3.5449, df = 3, p-value = 0.315 - good
r6 <- cor(fitted(arma22),logdf1)^2  # 0.1581108
aic6 <- AIC(arma22) # -420.6613
bic6 <- BIC(arma22) # -404.7949

# Create ARMA(1,2) model
arma12 <- arima(logdf1, order = c(1,0,2))
summary(arma12)# MAE 0.01915874
autoplot(arma12)# Check invertability
checkresiduals(arma12) # Q* = 3.5449, df = 3, p-value = 0.315 - good
r7 <- cor(fitted(arma12),logdf1)^2  # 0.1466347
aic7 <- AIC(arma12) # -420.6613
bic7 <- BIC(arma12) # -404.7949

# Create ARMA(2,1) model
arma21 <- arima(logdf1, order = c(2,0,1))
summary(arma21) # MAE: 0.01920357
autoplot(arma21)# Check invertability
checkresiduals(arma21) # Q* = 3.7862, df = 4, p-value = 0.4357
r8 <- cor(fitted(arma21),logdf1)^2  # 0.1572173
aic8 <- AIC(arma21) # -422.5554
bic8 <- BIC(arma21) # -409.3335


## If we wanted to focus on three specific models, I'd recommend these ones for our forcasting projections
AIC <- round(c(aic1, aic2, aic3, aic4, aic5,aic6,aic7,aic8), 3)
min(AIC) ## -422.555 from ****ARMA(2,1)****
rsquared <- round(c(r1, r2, r3, r4, r5, r6, r7, r8), 3)
max(rsquared) ## 0.158 from ****ARMA(2,2)****
BIC <- round(c(bic1, bic2, bic3, bic4, bic5, bic6, bic7, bic8), 3)
min(BIC) ## 412.421 ****AR1(1)****


names = c("AR(1)", "AR2(2)", "MA(1)", "MA(2)", "ARMA(1,1)", "ARMA(2,2)","ARMA(1,2)", "ARMA(2,1)")
aicbic_results <- data.frame("Name" = names,
                             "AIC" = AIC,
                             "BIC" = BIC,
                             "R2" = rsquared)
#vtable(aicbic_results)
# Table output:
aicbic_results %>%
  kable(caption = 'Information Criteria and R2 Table', booktabs = TRUE) %>%#,  padding = -80) %>%
  kable_styling(latex_options = "striped", full_width = F)


#################### question for professor - do we split data into a training and testing set for forecasting?
train <- window(logdf1, end = c(2016,1))
test <- window(logdf1, start=c(2016,2))
test <- as_vector(test) # make into vector for plotting
# create period vector for plotting
x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)


# Forecasting these thre emodels  were best in terms of R2, AIC or BIC from above
########################### ARMA21 - without using test/train set
forecast(arma21, h=20)
autoplot(forecast(arma21, h=20)) # autoplot with whole logdiff1

########################### ARMA21 with using train/test set
## arma21 forecasting
arma21_train <- arima(train, order = c(2,0,1))
#ar1_coef <- coef(arma21)[1]
#ar2_coef <- coef(arma21)[2]
#ma1_coef <- coef(arma21)[3]
# intercept <- coef(arma21)[4]

fcast1 <- forecast(arma21_train, h = 20)
plot(fcast1, include = 20)
fcast1<- as.data.frame(fcast1)

estimated_values <- fcast1["Point Forecast"]
estimated_values <- as_vector(estimated_values)

### actual vs predicted ppoints plotted ARMA(1,2)
plot(x, test, col = "blue", ylab  = "Log-Difference GDP", xlab = "Period")
points(x, estimated_values, col = "red")
legend("top", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,19),  cex = 0.9) 

########################### ARMA22
arma22 <- arima(logdf1, order = c(2,0,2))
forecast(arma22, h=20)
autoplot(forecast(arma22, h=20)) # autoplot with whole logdiff1
residuals <- residuals(arma22)
fitted <- test - residuals
plot(logdf1, col = "blue")
points(fitted, type = "l", col = "red")
legend("topleft", legend = c("Actual", "Residual"),
       col = c("blue", "red"), pch = c(19,19),  cex = 0.9) 


## with using train/test set
## arma22 forecasting
arma22_train <- arima(train, order = c(2,0,2))
fcast2 <- forecast(arma22_train, h = 20)
plot(fcast2, include = 20)
fcast2<- as.data.frame(fcast2)

estimated_values <- fcast2["Point Forecast"]
estimated_values <- as_vector(estimated_values)

### actual vs predicted ppoints plotted ARMA(2,2)
plot(x, test, col = "blue", ylab  = "Log-Difference GDP", xlab = "Period")
points(x, estimated_values, col = "red")
legend("top", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,19),  cex = 0.9) 


########################### AR1
forecast(ar1, h=20)
autoplot(forecast(ar1, h=20)) # autoplot with whole logdiff1

## with using train/test set
## arma22 forecasting
ar1_train <- arima(train, order = c(1,0,0))
fcast3 <- forecast(ar1_train, h = 20)
plot(fcast3, include = 20)
fcast3<- as.data.frame(fcast3)

estimated_values <- fcast3["Point Forecast"]
estimated_values <- as_vector(estimated_values)

### actual vs predicted ppoints plotted ARMA(2,2)
plot(x, test, col = "blue", ylab  = "Log-Difference GDP", xlab = "Period")
points(x, estimated_values, col = "red")
legend("top", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,19),  cex = 0.9) 

############################ end of deeper forcasting analysis - below are the other forcasts from models
## AR2
forecast(ar2, h=20)
autoplot(forecast(ar2, h=20))
## MA1
forecast(ma1, h=20)
autoplot(forecast(ma1, h=20))
## MA2
forecast(ma2, h=20)
autoplot(forecast(ma2, h=20))
## ARMA11
forecast(arma11, h=20)
autoplot(forecast(arma11, h=20))
## ARMA12
forecast(arma12, h=20)
autoplot(forecast(arma12, h=20))


################################################# QUESTION FOR PROFESSOR - when do we do include mean = FALSE?
################################################ this section include.mean = FALSE
# Create AR(1) model
ar1 <- arima(logdf1, order = c(1,0,0), include.mean = FALSE)

summary(ar1) # MAE: 0.02277215
autoplot(ar1) # Check invertability
checkresiduals(ar1) #Q* = 12.198, df = 7, p-value = 0.09422
r1 <- cor(fitted(ar1),logdf1)^2   
aic1 <- AIC(ar1)  
bic1 <- BIC(ar1) 

# Create AR(2) model
ar2 <- arima(logdf1, order = c(2,0,0), include.mean = FALSE)
summary(ar2) # MAE: 0.02075153
autoplot(ar2)# Check invertability
checkresiduals(ar2) #Q* = 10.858, df = 6, p-value = 0.09286
r2 <-cor(fitted(ar2),logdf1)^2   
aic2 <- AIC(ar2) 
bic2 <- BIC(ar2)

# Create MA(1) Model - WORST MODEL
ma1 <- arima(logdf1, order = c(0,0,1), include.mean = FALSE)
summary(ma1) # MAE 0.02278158
autoplot(ma1)# Check invertability
checkresiduals(ma1) #Q* = 15.523, df = 7, p-value = 0.02985 - NOT GOOD
r3 <- cor(fitted(ma1),logdf1)^2 
aic3 <- AIC(ma1)
bic3 <- BIC(ma1)


# Create MA(2) Model
ma2 <- arima(logdf1, order = c(0,0,2), include.mean = FALSE)
summary(ma2) # MAE 0.03240396
autoplot(ma2)# Check invertability
checkresiduals(ma2) 
r4 <- cor(fitted(ma2),logdf1)^2 
aic4 <- AIC(ma2)
bic4 <- BIC(ma2) 


# Create ARMA(1,1) model
arma11 <- arima(logdf1, order = c(1,0,1), include.mean = FALSE)
summary(arma11)# MAE  0.02207605
autoplot(arma11)# Check invertability
checkresiduals(arma11) 
r5 <- cor(fitted(arma11),logdf1)^2  
aic5 <- AIC(arma11) 
bic5 <- BIC(arma11) 

# Create ARMA(2,2) model
arma22 <- arima(logdf1, order = c(2,0,2), include.mean = FALSE)
summary(arma22) # MAE: 0.01943755
autoplot(arma22)# Check invertability -- THIS LOOKS DIFFERENT
checkresiduals(arma22) 
r6 <- cor(fitted(arma22),logdf1)^2 
aic6 <- AIC(arma22)
bic6 <- BIC(arma22) 

# Create ARMA(1,2) model
arma12 <- arima(logdf1, order = c(1,0,2), include.mean = FALSE)
summary(arma12)# MAE 0.01958828
autoplot(arma12)# Check invertability
checkresiduals(arma12) # -- this also looks different
r7 <- cor(fitted(arma12),logdf1)^2  
aic7 <- AIC(arma12) 
bic7 <- BIC(arma12) 

# Create ARMA(2,1) model
arma21 <- arima(logdf1, order = c(2,0,1), include.mean = FALSE)
summary(arma21) # MAE: 0.01941412
autoplot(arma21)# Check invertability
checkresiduals(arma21) # Q* = 3.7862, df = 4, p-value = 0.4357
r8 <- cor(fitted(arma21),logdf1)^2  # 0.1572173
aic8 <- AIC(arma21) # -422.5554
bic8 <- BIC(arma21) # -409.3335


## If we wanted to focus on three specific models, I'd recommend these ones for our forcasting projections
AIC <- round(c(aic1, aic2, aic3, aic4, aic5,aic6,aic7,aic8), 3)
min(AIC) ## -421.005 from ARMA(2,1)
rsquared <- round(c(r1, r2, r3, r4, r5, r6, r7, r8), 3)
max(rsquared) ## .146 from ARMA(2,2)
BIC <- round(c(bic1, bic2, bic3, bic4, bic5, bic6, bic7, bic8), 3)
min(BIC) ## 412.421 AR1(1)


names = c("AR(1)", "AR2(2)", "MA(1)", "MA(2)", "ARMA(1,1)", "ARMA(2,2)","ARMA(1,2)", "ARMA(2,1)")
aicbic_results <- data.frame("Name" = names,
                             "AIC" = AIC,
                             "BIC" = BIC,
                             "R2" = rsquared)
#vtable(aicbic_results)
# Table output:
aicbic_results %>%
  kable(caption = 'Information Criteria Table (include.mean = FALSE)', booktabs = TRUE) %>%#,  padding = -80) %>%
  kable_styling(latex_options = "striped", full_width = F)


# is using the ARIMA package sufficient for model? Or do we have to pull out coefficients and build lag equation?
# do we do split of testing and training?
# how do our set of models look?
# how does the residuals hist look?
# what should we do about our fit (as far as it looks when it is graphed?)
# are there more testing mechanisms that we should use beyond AIC, MAE, R^2 and Ljung?
# when we have our forecast, should it be reverse adjuzted from the log dif fit to the real world scale?
