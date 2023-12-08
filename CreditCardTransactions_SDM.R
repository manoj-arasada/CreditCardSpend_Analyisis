library(readxl)
rm(list=ls())
df <- read_excel("CreditCardTransactions.xlsx", sheet='Data')
str(df)
View(df)
#changing column names to lower
colnames(df)=tolower(make.names(colnames(df)))
attach(df)
str(df)
df_model = df[,-c(2)]
str(df_model)
#converting categorical values as factors
df_model$wealthtag=as.factor(df_model$wealthtag)
df_model$cardtype=as.factor(df_model$cardtype)
df_model$revolvingindicator=as.factor(df_model$revolvingindicator)
df_model$spendcategory=as.factor(df_model$spendcategory)
levels(df_model$wealthtag)
levels(df_model$cardtype)
levels(df_model$revolvingindicator)
levels(df_model$spendcategory)

hist((df_model$transamount))
hist((df_model$transcount))


hist(log(df_model$transamount))
hist(log((df_model$transcount)))
plot(df_model$spendcategory,log(df_model$transamount))
# let us calculate 


colSums(is.na(df_model))
df_clean=na.omit(df_model)

install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)
chart.Correlation(df_clean)

sc_model = lm(log(transamount)~wealthtag+cardtype+revolvingindicator+spendcategory, data = df_clean)
summary(sc_model)
plot(2.71^sc_model$fitted.values,df_clean$transamount)
abline(0,1)

sc_model_2 = lm(log(transamount)~spendcategory, data = df_clean)
summary(sc_model_2)
plot(2.71^sc_model_2$fitted.values,df_clean$transamount)
abline(0,1)

sc_model_3 = lm(log(transamount)~wealthtag+spendcategory+revolvingindicator, data = df_clean)
summary(sc_model_3)
plot(2.71^sc_model_3$fitted.values,df_clean$transamount)
abline(0,1)


sc_model_count = lm(log(transcount)~wealthtag+cardtype+revolvingindicator+spendcategory, data = df_clean)
summary(sc_model_count)
plot(2.71^sc_model_count$fitted.values,(df_clean$transcount))
abline(0,1)


install.packages("stargazer")
library(stargazer)
stargazer(sc_model,sc_model_2,sc_model_3 , type='text', single.row=TRUE)

#' Test for assumptions

plot(sc_model)
plot(sc_model_count)

shapiro.test(sc_model$res)                        # Shapiro-Wilk's test of multivariate normality
shapiro.test(sc_model_count$residuals)

bartlett.test(list(sc_model$res, sc_model$fit))         # Bartlett's test of homoskedasticity
bartlett.test(list(m5$res, m5$fit))

install.packages("car")
library(car)                              # Multicollinearity test
vif(sc_model)
vif(sc_model_count)

install.packages("lmtest")
library(lmtest)
dwtest(sc_model)                                  # Durbin-Watson test of autocorrelation
dwtest(sc_model_count)


