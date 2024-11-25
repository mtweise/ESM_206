#HW 3
#In this study, the authors measured the consumption elasticity of the initial burden of
#a carbon or gasoline tax (in the absence of tax revenue redistribution) across regions of the
#world from 1992 to 2017. An elasticity <1 means the initial tax burden falls disproportionately
#on the poor, whereas a value >1 indicates the initial tax burden falls disproportionately on the
#rich. You are tasked with assessing if the mean elasticity between the regions of Africa (“Africa”),
#the European Union (“EU”), LaIn America (“LatAm”), and the United States (“USA”) are
#different. Assume observations are independent between years. You are tasked with identifying
#which regions are different from each other, and if there is evidence for regionally
#disproportional burdens between rich and poor.

#Load libraries
library(tidyverse)
library(here)
library(janitor)
library(car) #for lveleneTest() for equal variances
library(scales)
library(dplyr)

#################################
#Question 1
#(Fundamental) Explore and clean the data to assess the assumptions of ANOVA (or
#similar non-parametric approach) and provide a one-paragraph description of your
#assessment and the data. You may include one figure and one table.


#load data
raw_data <- read.csv(here("hw_3/data", "revenue_data.csv"))

clean_region <- raw_data |> 
  filter(region %in% c("Africa", "EU", "LatAm", "USA")) |>
  select(region:elasticity) |>
  arrange(region)

#make region a factor ??
clean_region$region <- as.factor(clean_region$region)

#break out data by region
africa <- clean_region |> filter(region=="Africa")
eu <- clean_region |> filter(region=="EU")
latam <- clean_region |> filter(region=="LatAm")
usa <- clean_region |> filter(region=="USA")


#exploratory data analysis
elasticity_obs <- ggplot(clean_region, aes (x= region, y= elasticity))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(position = position_jitter(width=0.2, height=0), size= 2.5,
              color= "dodgerblue", alpha=0.5)+
  labs(x="Region", y="Elasticity")+
  theme_bw()

elasticity_obs


#summary statistics
summary_table <- clean_region|>
  group_by(region) |>
  summarize(min=min(elasticity),
            var=var(elasticity),
            mean=mean(elasticity),
            median=median(elasticity),
            max=max(elasticity))
print(summary_table)

#testing our assumptions for observed data

#Shapiro- normaility
#H0: data are from a normal distribution
#HA: data are not from a normal distribution
#if W is 1 all of our points are on the line. more scatter =lower W value

shapiro.test(africa$elasticity) #p-value= we fail to reject the null (normal)
shapiro.test(eu$elasticity) #p-value= we fail to reject the null (normal)
shapiro.test(latam$elasticity) #normal
shapiro.test(usa$elasticity) #normal


#Equal variance tests
#H0: data for each group has hte same variance
#HA: data for at least one group is different

leveneTest(elasticity ~ region, data=clean_region)
#pr is 0.62 which means equal variance



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









#####################
#Question 2
#(Fundamental) Conduct the ANOVA (or similar non-parametric approach), provide a
#suitable visualization of the data that includes demarcation of the elasticity threshold at
#1, and explain your results and conclusions of the statistical assessment in 1-2
#paragraphs. Discuss any caveats, limitations, or assumptions about your analysis

# tests of means and medians (ANOVA and KW)


#ANOVA
#H0- All means are equal
#HA- at least two means are not equal

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



####################
#Question 3
#(Proficient) Explain the pairwise assessment of the regional consumption elasticity and if
#there is evidence for disproportional burdens between rich and poor in 1-2 paragraphs.
#You may use a figure OR table to support your discussion






