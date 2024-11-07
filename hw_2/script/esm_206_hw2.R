#HW 2
#ESM 206
#####################################

library(tidyverse)
library(janitor)
library(here)
library(tidyr)
library(dplyr)
library(patchwork)
library(effsize) #for measuring effect sizes
library(car)

#load data
npgp_raw <- read_csv(here("hw_2/data", "npgp_data.csv")) |>
  clean_names()



#Question 1
#Using your data science prowess, clean, transform, and save only the
#necessary data for your analysis. Provide the data in a table at the end of the write-up.

#drop area B
#need to change to species/category w name then concentration

npgp_q1 <-  npgp_raw |>
  select(area,plastic_uncorrected) |>
  drop_na() |> filter(area != "B")
  
#WRITE CSV

##############################
#Question 2

#Conduct EDA and provide a data visualization with a caption describing
#the data distribution(s). Make sure you visually assess normality

#make area a factor
#go into plastic data clean area column and change it to a factor and replace itself
npgp_q1$area <- as.factor(npgp_q1$area)


#separate by area
#led <- trawl_cortisol|> filter(trawl_led == 1)
#no_led <- trawl_cortisol |> filter(trawl_led == 0)

areaA <- npgp_q1 |> filter(area== "A")
areaC <- npgp_q1 |> filter(area== "C")

#Make a boxplot

q2_figA <- ggplot(npgp_q1, aes(x=area, y=plastic_uncorrected)) +
  geom_boxplot() +
  labs(x= "Area", y= "Concentration of plastic particles (per km^2)") +
  ggtitle("Plastic concentration according to area")+
  theme_bw()
q2_figA

#fig1b <- ggplot(led, aes(sample=plasma_cortisol)) +
#stat_qq() +
#  stat_qq_line()+
#  xlab("Normal prediction") +
#  ylab("Observed data") +
#  ggtitle("LED on")+
#  theme_bw()

##Normality
#area A
q2_figB <- ggplot(areaA, aes(sample= plastic_uncorrected)) +
  stat_qq()+
  stat_qq_line()+
  xlab("Normal prediction")+
  ylab("Observed data")+
  ggtitle("Area A")+
  theme_bw()
q2_figB


#area B
q2_figC <- ggplot(areaC, aes(sample= plastic_uncorrected)) +
  stat_qq()+
  stat_qq_line()+
  xlab("Normal prediction")+
  ylab("Observed data")+
  ggtitle("Area C")+
  theme_bw()
q2_figC


#combined <- fig1a/ (fig1b+ fig1c)

q2_combined <- q2_figA/ (q2_figB+ q2_figC)
q2_combined





#################################
#Question 3

#Provide a statistical summary of the data in a table and provide a table
#title following best practices


q3_summary_table <- npgp_q1 |>
  group_by(area) |>
  summarize(var=var(plastic_uncorrected),
            min=min(plastic_uncorrected),
            IQ25_p = quantile(plastic_uncorrected, probs= 0.25),
            mean=mean(plastic_uncorrected),
            median=median(plastic_uncorrected),
            IQ75_p = quantile(plastic_uncorrected, probs= 0.75),
            max=max(plastic_uncorrected))
q3_summary_table

#####
#Check the assumptions

#Normality- Shapiro
#H0: data are from a normal distribution
#HA: data are not from a normal distribution

#if W is 1 all of our points are on the line. more scatter =lower W value

shapiro.test(areaA$plastic_uncorrected)
#p-value = 0.06322
#we fail to reject the null (not normally distributed)

shapiro.test(areaC$plastic_uncorrected)
#p-value = 3.44e-07
#we reject the null (normally distributed)

#Equal variance tests- Levene's test
#H0: data for each group has the same variance
#HA: data for at least one group is different

leveneTest(plastic_uncorrected ~ area, data=npgp_q1, center= "mean")
#Pr = 0.0003752
#reject the null
#the results mean one has a variance that's different than the other

#Summary: area A is not normally distributed and variances are not equal

#################################
#Question 4

#State the null and alternative hypotheses for a two-sample test. Conduct the
#test and provide the R output of the analysis

#T-test
#H0: The means of the two groups are the same
#HA: The means of the two groups are different
t.test(plastic_uncorrected ~ area, data=npgp_q1, var.equal= TRUE)
#p-value = 0.00675

t.test(plastic_uncorrected ~ area, data=npgp_q1, var.equal= FALSE)
#p-value = 0.001291

#either way, reject null (the means are different)
#DO I ASSUME EQUAL VARIANCE OR NOT??

################################
#Question 5

#Communicate your conclusion to the State using the necessary statistical
#information, noting any caveats you have in interpretation

#Caveats
#non-parametric test
#H0: The medians of the groups are the same
#HA: The medians are different
wilcox.test(plastic_uncorrected ~ area, data=npgp_q1)
#p-value = 2.914e-12
#reject null (medians are different)

#Calculation of effect size
#equal variance
cohen.d(plastic_uncorrected ~ area, data=npgp_q1)
#how many standard deviations apart are the means
#lower (-1.535) upper (-0.229)

#unequal variance
cohen.d(plastic_uncorrected ~ area, data=npgp_q1, pooled= FALSE)
#lower (-1.535) upper (-0.2292)

################################
#Question 6

#Compare your two-sample test to confidence intervals formulated from
#analytical derivation or bootstrapping of the mean plastic density for Area A and Area C.
#Results can be shown graphically or in a table. Provide a discussion of the similarities
#and differences in your conclusion 



