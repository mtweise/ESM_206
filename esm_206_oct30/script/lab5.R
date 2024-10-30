#isntall and load libraries


rm(list = ls())

#Load libraries
library(tidyverse)
library(here)
library(janitor)
library("patchwork") #good for organizing plots
library(effsize) #for measuring effect sizes
library(car) #companion for R regression is need for levene's test

#Import the data
trawl_clean <- read_csv(here("esm_206_oct30/data", "trawl_data.csv")) |>
  clean_names()

#Step 1: clean data

trawl_cortisol <- trawl_clean|>
  select(trawl_led, plasma_cortisol) |>
  drop_na()

write_csv(trawl_cortisol, here("esm_206_oct30/data", "trawl_cortisol.csv"))

#Step 2: exploratory data analysis (EDA)
trawl_cortisol$trawl_led <- as.factor(trawl_cortisol$trawl_led)
#this makes the 1 vs 0 two factors ie if the LED light is on or off

led <- trawl_cortisol|> filter(trawl_led == 1)
no_led <- trawl_cortisol |> filter(trawl_led == 0)

#make a boxplot
fig1a <- ggplot(trawl_cortisol, aes(x=plasma_cortisol, y=trawl_led)) +
  geom_boxplot() +
  labs(x= "Cortisol (ng/mL)", y= "LED(1=on, 0=off") +
  theme_bw()

fig1b <- ggplot(led, aes(sample=plasma_cortisol)) +
  stat_qq() +
  stat_qq_line()+
  xlab("Normal prediction") +
  ylab("Observed data") +
  ggtitle("LED on")+
  theme_bw()

fig1c <- ggplot(no_led, aes(sample=plasma_cortisol)) +
  stat_qq() +
  stat_qq_line()+
  xlab("Normal prediction") +
  ylab("Observed data") +
  ggtitle("LED off")+
  theme_bw()


combined <- fig1a/ (fig1b+ fig1c)

#summary statistics

summary_table <- trawl_cortisol|>
  group_by(trawl_led) |>
  summarize(min_p= min(plasma_cortisol),
            IQ25_p = quantile(plasma_cortisol, probs= 0.25),
            mean_p =mean(plasma_cortisol),
            median_p = median(plasma_cortisol),
            IQ75_p = quantile(plasma_cortisol, probs= 0.75),
            max_p = max(plasma_cortisol))

write_csv(summary_table, here("esm_206_oct30/data", "summary_table_cortisol.csv"))

#Check the assumptions
#Normality
#Shapiro
#H0: data are from a normal distribution
#HA: data are not from a normal distribution
shapiro.test(led$plasma_cortisol)
#if W is 1 all of our points are on the line. more scatter =lower W value
#p-value= we fail to reject the null

shapiro.test(no_led$plasma_cortisol)

#Equal variance tests
#H0: data for each group has hte same variance
#HA: data for at least one group is different
# ~ means = as distributed as
leveneTest(plasma_cortisol ~ trawl_led, data=trawl_cortisol, center= "mean")
#the results mean one has a variance thats different than the other


#Step 4: state hypotheses and run that t-test
#H0: The means of the two groups are the same
#HA: The means of the two groups are different
t.test(plasma_cortisol ~ trawl_led, data=trawl_cortisol, var.equal= TRUE)


t.test(plasma_cortisol ~ trawl_led, data=trawl_cortisol, var.equal= FALSE)


#Caveats
#non-parametric test
#H0: The medians of the groups are the same
#HA: The medians are different
wilcox.test(plasma_cortisol ~ trawl_led, data=trawl_cortisol)


#Calculation of effect size
#equal variance
cohen.d(plasma_cortisol ~ trawl_led, data=trawl_cortisol)
#how many standard deviations apart are the means

#unequal variance
cohen.d(plasma_cortisol ~ trawl_led, data=trawl_cortisol, pooled= FALSE)








