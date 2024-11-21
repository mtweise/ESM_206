#Lab 7
#Linear regression and multiple linear regression
###########################
#clea the environment
rm(list=ls())

#libraries for this lab
library(here)
library(tidyverse)
library(janitor)
library(car) #ncvTest

#load the data from homework #3
data <- read.csv(here("esm_206_nov20/data", "revenue_data.csv"))

#modify the data for just the variables needed and cleaning it
data_mod <- data |> clean_names() |>
  select(pc_gdp, elasticity, region, year) |>
  drop_na()


#remake figure 1 in budolfson
budolfson_plot_data <- ggplot(data_mod, aes(x=pc_gdp, y=elasticity))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="GDP per capita (2005 US$", y="Elasticity")+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_bw()
budolfson_plot_data

#Running the OLS linear regression
budolfson_output <- lm(elasticity ~ log10(pc_gdp), data=data_mod)
budolfson_output
#output is intercept (y int) and slope of the line (log10)
summary(budolfson_output)
#null for is that intercept is 0,0
#how much of the variability is explained by the model? multiple r square (21%)

#plot using the output of the limear model
budolfson_plot_data <- ggplot(data_mod, aes(x=pc_gdp, y=elasticity))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="GDP per capita (2005 US$", y="Elasticity")+
  geom_abline(intercept=coef(budolfson_output)[1], slope=coef(budolfson_output)[2])+
  theme_bw()
budolfson_plot_data


#looking at assumptions
residuals <- ggplot(budolfson_output, aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()
residuals
#look at how the data are scattered around that fitted value

#lets make a qqplot to look at normality of the residuals
qq_residuals <- ggplot(budolfson_output, aes(sample=.resid))+
  geom_qq()+
  geom_qq_line()+
  theme_bw()
qq_residuals

#test the assumptions
shapiro.test(budolfson_output$residuals)
#we reject the null therefore you can do nonpramatric or transform data
#linear regression is very robust against slight assumptions of the normality

ncvTest(budolfson_output)
#failt to reject so equal variance and homoscedasticity

#first model is that none of these variables acount for any of the data

m.0 <- lm(elasticity~ 1, data=data_mod)
#E=B0 x 1
summary(m.0)
#intercept is not zero (we rejected the null)
#this is the equivalent of a one sample t test (is mean different than 0)

m.1 <- lm(elasticity~ log10(pc_gdp), data=data_mod)
summary(m.1)


m.2 <- lm(elasticity~ as.factor(region), data=data_mod)
summary(m.2)
#This is the same pvalue as ANOVA difference is I can look at all values and see 
#that africa is different than africa, latin america no, oceana no,usa yes (it compares
#everything to africa bc it's alphabetical)
#E(of the EU)=1.35- 0.069(0) -0.4(1) +Bn(0)
#you can choose a reference by specifying the order
#mean for middle east is 1.39-0.379

m.3 <- lm(elasticity~ year, data=data_mod)
summary(m.3)
#explains less than 1% of variability in data

m.3.5 <- lm(elasticity~ as.factor(year), data=data_mod)
summary(m.3.5)
#2015 mighttttt have something going on

m.4 <- lm(elasticity~ log10(pc_gdp)+ year, data=data_mod)
summary(m.4)

m.5 <- lm(elasticity~ log10(pc_gdp)+ as.factor(region), data=data_mod)
summary(m.5)
#not awesome only 21%

m.6 <- lm(elasticity~ year+ as.factor(region), data=data_mod)
summary(m.6)
#year isnt really helpingmuch excpet when in combo with gdp 
#usa, eu, japan significant but doesnt do much with elasticity

m.7 <- lm(elasticity~ log10(pc_gdp)+ as.factor(region) +year, data=data_mod)
summary(m.7)
#not bad 23% of data

BIC_models <- c(BIC(m.0), BIC(m.1), BIC(m.2), BIC(m.3), BIC(m.4), BIC(m.5), 
                BIC(m.6), BIC(m.7))
BIC_models
#values only matter in relation to the other values

best_model <- min(BIC_models)
delta_BIC <- BIC_models-best_model
delta_BIC
#delta means how many coin flips in a row ??? you dont get 100 heads in a row
#model 1 and 4 are about the same retain year 
