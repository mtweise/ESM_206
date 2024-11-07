#Lab 6: Bootstrap and ANOVA


#clear the environment
rm(list=ls())

#Load libraries
library(tidyverse)
library(here)
library(janitor)
library(patchwork)
library(car) #for lveleneTest() for equal variances
library(scales)
library(simpleboot)
library(boot) #for bootstrapping



#load data
plastic_data_raw <- read_csv(here("esm_206_nov6/data", "npgp_data.csv"))

#clean the data to what you need
plastic_data_clean <- plastic_data_raw |>
  clean_names() |>
  select(area, plastic_uncorrected) |>
  drop_na()

#remove raw data
rm(plastic_data_raw)

#make area a factor
#go into plastic data clean area column and change it to a factor and replace itself
plastic_data_clean$area <- as.factor(plastic_data_clean$area)

#break out data by area
area_A <- plastic_data_clean |> filter(area=="A")
area_B <- plastic_data_clean |> filter(area=="B")
area_C <- plastic_data_clean |> filter(area=="C")

#exploratory data analysis
plastic_area_obs <- ggplot(plastic_data_clean, aes(y=plastic_uncorrected,
                                                   x=area))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position=position_jitter(width=0.2, height=0), size=2.5,
              color="dodgerblue", alpha=0.5)+
  labs(x="Area", y="Plastic density (counts/km*km)")+
  scale_y_continuous(trans="log10", labels=scales::comma)+
  theme_bw()

plastic_area_obs

#summary statistics
summary_table <- plastic_data_clean|>
  group_by(area) |>
  summarize(min=min(plastic_uncorrected),
            var=var(plastic_uncorrected),
            mean=mean(plastic_uncorrected),
            median=median(plastic_uncorrected),
            max=max(plastic_uncorrected))
print(summary_table)

#testing our assumptions for observed data
shapiro.test(area_A$plastic_uncorrected)
shapiro.test(area_B$plastic_uncorrected)
shapiro.test(area_C$plastic_uncorrected)

leveneTest(plastic_uncorrected ~ area, data=plastic_data_clean)

#testing out assumptions for transformed data
plastic_data_clean_log10 <- plastic_data_clean |>
  mutate(plastic_uncorrected_log10=log10(plastic_uncorrected))


shapiro.test(log10(area_A$plastic_uncorrected)) #adding log10 also fail to reject the null aka it is normal
shapiro.test(log10(area_B$plastic_uncorrected)) #same above
shapiro.test(log10(area_C$plastic_uncorrected)) #before it looked like it wasnt normal, but now it's transformed and normal

leveneTest(plastic_uncorrected_log10 ~ area, 
           data=plastic_data_clean_log10) #fail to reject, we do have equal variance

#we did a log10 of uncorrected plastic and when we didi that it became normal and 
#variances are equal. now we can proceed without violating assumptions


# tests of means and medians (ANOVA and KW)

#clean data
plastic_aov <- aov(plastic_uncorrected ~ area, data=plastic_data_clean)
summary(plastic_aov)

#log10 transformed data
plastic_aov_log10 <- aov(plastic_uncorrected_log10 ~ area,
                         data=plastic_data_clean_log10)
summary(plastic_aov_log10)

#nonparametric 
kruskal.test(plastic_uncorrected ~ area, data=plastic_data_clean)


#multiple comparison tests
TukeyHSD(plastic_aov_log10)
#b-c means could have come from the same population

pairwise.wilcox.test(plastic_data_clean$plastic_uncorrected,
                     plastic_data_clean$area, p.adjust.method = "bonferroni")

#DONE WITH ANOVA

#start of bootstrap
set.seed(1345)
#get the data into a vector
area_C_vec <- as.vector(area_C$plastic_uncorrected)
#make a vector of length 1000 of means from replicates
C.mean <- one.boot(area_C_vec, mean, 1000)

#get the bootstrapped confidence interval
boot.ci(C.mean, conf=0.95) #use the percentile output







