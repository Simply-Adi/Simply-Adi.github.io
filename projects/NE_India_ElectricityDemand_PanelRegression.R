library(plm)
library(AER)
library(stargazer)
data("Fatalities")
str(Fatalities)
summary(Fatalities[,c(1,2)])
levels(Fatalities$state)
48*7 #year levels* state levels=number of observation
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000 #automatically appends column
# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")
# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
summary(fatal1982_mod)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
#coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1");another way of summary
# plot the observations and add the estimated regression line for 1982 data
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = Fatalities1982$state)
ggplot(Fatalities1982)+geom_point(aes(x =beertax,y = fatal_rate,color=state))
#adds reference lines or regression line;works on a plot() or hist()
abline(fatal1982_mod, lwd = 1.5)
#main concern is tax rate,have to adjust for ommitted bias
new=rbind(Fatalities1982,Fatalities1988)
levels(new$year)
summary(lm(fatal_rate ~ beertax+year,new))
#Coefficients:
#  (Intercept)      beertax     year1988  
#1.946919     0.268138    -0.005982 (if beer tax remained the same,the mean Y would shift by this cf)
#state level characteristics that don't differ across time
# compute the differences
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax
# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)
summary(fatal_diff_mod)
plot(x=diff_beertax,y=diff_fatal_rate,pch=19,col="steelblue")
abline(fatal_diff_mod,lw=1.5)
#result is now intuitive-raise in beer tax lowers death, but the cf is still biased
#Fixed Effects Model
levels(Fatalities$state)
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state, data = Fatalities)
fatal_fe_lm_mod
# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax,
                    data = Fatalities,
                    index = c("state", "year"),model="within")
summary(fatal_fe_mod)
#Adj. R-Squared: -0.11969.....meaning
