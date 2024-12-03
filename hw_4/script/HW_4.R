#HW 4

#Objective: You are working for CDFW this summer as a fisheries biodiversity intern, and the
#state is considering adopting a new survey strategy based on environmental DNA (eDNA). Your
#supervisor shares a 2020 paper by Mary McElroy showing that eDNA surveys for fish work as
#well as, or better, than traditional fish surveys using nets or electrofishing. However, your
#supervisor wants to know if it will work to test the theory of island biogeography, which they
#use to project the fish biodiversity in unsampled lakes (Barbour & Brown 1974). The theory
#states that the log10 of species richness (number of unique fish in the lake) increases linearly
#with the log10 of the lake area. Specifically, your supervisor wants to know if the slope and
#intercept for a regression using eDNA-based estimates of species richness is the same as the
#slope and intercept using conventional surveys. The data from the McElroy paper and the area
#of each lake surveyed are provided. 

library(tidyverse)
library(here)
library(janitor)
library(car) #for lveleneTest() for equal variances
library(dplyr)

########################################
#Question 1
#(Fundamental) Conduct a multiple linear regression to assess if eDNA produces the
#same slope and intercept as conventional sampling and provide any caveats in the
#analysis using your knowledge about the assumptions. Provide all necessary statistical
#information to make your argument and any visual aids (tables/figures) (5 pts)

#load the data
lakes_data <- read.csv(here("hw_4/data", "lakes_data.csv"))

#modify the data for just the variables needed and cleaning it
#clean_lakes_data <- lakes_data |> clean_names() |>
   select(area_ha, dna_richness, trad_richness) |>
  drop_na()

#mutate so we can take log10 by adding 1 to species richness
#mod_clean_lakes_data <- clean_lakes_data |>
  mutate(dna_richness= dna_richness +1,
         trad_richness= trad_richness +1)

#remake figure 1 in budolfson
#budolfson_plot_data <- ggplot(mod_clean_lakes_data, aes(x=area_ha, y=dna_richness))+
  geom_point()+
  scale_y_continuous(trans='log10')+
  labs(x="Area", y="dna richness")+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_bw()
budolfson_plot_data

#Running the OLS linear regression
#budolfson_output_edna <- lm(area_ha ~ log10(dna_richness), data=mod_clean_lakes_data)
budolfson_output_edna
#output is intercept (y int= -250731) and slope of the line (log10= 308797)

summary(budolfson_output_edna)
#null for is that intercept is 0,0
#how much of the variability is explained by the model? multiple r square (0.09559)

#plot using the output of the linear model
#b_plot_data <- ggplot(mod_clean_lakes_data, aes(x=area_ha, y=dna_richness))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="Area (hectares)", y="Species Richness")+
  geom_abline(intercept=coef(budolfson_output_edna)[1], slope=coef(budolfson_output_edna)[2])+
  theme_bw()
b_plot_data

#????????? what happened
#do i repeat with trad data?

#looking at assumptions
#residuals <- ggplot(budolfson_output_edna, aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()
residuals
#look at how the data are scattered around that fitted value

#lets make a qqplot to look at normality of the residuals
#qq_residuals <- ggplot(budolfson_output_edna, aes(sample=.resid))+
  geom_qq()+
  geom_qq_line()+
  theme_bw()
qq_residuals

#test the assumptions
#shapiro.test(budolfson_output_edna$residuals)
#we reject the null therefore you can do nonpramatric or transform data
#linear regression is very robust against slight assumptions of the normality

#ncvTest(budolfson_output_edna)
#we reject so unequal variance

########################################
#Question 2
#(Fundamental) Produce a plot showing the best-fit regression(s) with a caption and the
#data (5 pts).

########################################
#Question 3
#(Proficient) Make a one-paragraph recommendation to your supervisor if they should
#use the cost-effective eDNA approach for fisheries surveys in California or not (3 pts)

########################################







####Office hours
#log area lake and log area richness


#import data
data_mod <- lakes_data |>
  select(area_ha, dna_richness, trad_richness) |>
  pivot_longer(dna_richness:trad_richness,
               names_to = "method", values_to = "richness") |>
  mutate(area_log10=log10(area_ha)) |>
  mutate(richness_log10_plus1=log10(richness+1))
data_mod

#simple linear regression and testing assumptions
#visually inspect linear and it was linear
#calc bic
#calc ncv dont have to report is there evidence for homo vs hetro. normality with residuals?
#summary table
#it's not normal or homo. not do anything different but should report it
#could transform
dna_only <- data_mod |>
  filter(method== "dna_richness")
trad_only <- data_mod |>
  filter(method== "trad_richness")

#null for DNA
m.dna.0 <- lm(richness_log10_plus1 ~1, data=dna_only)
summary(m.dna.0)
BIC(m.dna.0) #only needed if using an info criteria approach
ncvTest(m.dna.0) #test assumption of homoscedasticity is not necessary here. should be p=1


#Linear model for DNA
m.dna.1 <- lm(richness_log10_plus1 ~ area_log10, data=dna_only)
summary(m.dna.1) #multiple r= 0.3319 pvalue= 5.081e-07
BIC(m.dna.1) #only neeeded if using an information criteria approach
ncvTest(m.dna.1) #test assumption of homoscedasticty. pvalue=0.11783

#plot for both null and SLR DNA
mean_y_dna <- mean(dna_only$richness_log10_plus1)
slr_plot_dna <- ggplot(dna_only, aes(x=area_log10, y=richness_log10_plus1))+
  geom_hline(yintercept = mean_y_dna, color="darkgray", size=1.25)+
  geom_point(color="black", alpha=0.5, size=2)+
  geom_smooth(method="lm", color="blue")+
  xlab("Log10(Area(ha))")+
  ylab("Log10(Species Richness +1)")+
  ggtitle("eDNA")+
  annotate("text", x=5, y=1.05, label=paste("Null model"), color= "darkgray", size=3)+
  annotate("text", x=5, y=1.75, label=paste("SLR"), color= "blue", size=3)+
  theme_bw()
slr_plot_dna
ggsave(here("hw_4/figures", "SLR_dna.png"), slr_plot_dna, dpi=300, height=4, width=6, unit="in")


  
#null for traditional
m.trad.0 <- lm(richness_log10_plus1 ~1, data=trad_only)
summary(m.trad.0)
BIC(m.trad.0) #only needed if using an info criteria approach

#Linear model for traditional
m.trad.1 <- lm(richness_log10_plus1 ~area_log10, data=trad_only)
summary(m.trad.1) #multiple r^2=0.3649, pvalue= 4.925e-08
BIC(m.trad.1) #only needed if using an info criteria approach
ncvTest(m.trad.1) #pvalue=0.15058
shapiro.test(m.trad.1$residuals) #pvalue= 0.05093

#plot for both null and SLR TRAD
mean_y_trad <- mean(trad_only$richness_log10_plus1)
slr_plot_trad <- ggplot(trad_only, aes(x=area_log10, y=richness_log10_plus1))+
  geom_hline(yintercept = mean_y_trad, color="darkgray", size=1.25)+
  geom_point(color="black", alpha=0.5, size=2)+
  geom_smooth(method="lm", color="blue")+
  xlab("Log10(Area(ha))")+
  ylab("Log10(Species Richness +1)")+
  ggtitle("Traditional")+
  annotate("text", x=5, y=1.05, label=paste("Null model"), color= "darkgray", size=3)+
  annotate("text", x=5, y=1.75, label=paste("SLR"), color= "blue", size=3)+
  theme_bw()
slr_plot_trad
ggsave(here("hw_4/figures", "SLR_trad.png"), slr_plot_dna, dpi=300, height=4, width=6, unit="in")



#multiple linear regression
#null
m.0 <- lm(richness_log10_plus1 ~1, data_mod)
summary(m.0) 


#model with area only
m.1 <- lm(richness_log10_plus1~area_log10, data_mod)
summary(m.1)
#model is significant, 33% of the variability explained
residuals <- as.data.frame(m.1$residuals) |> rename(residuals="m.1$residuals")
m.1_residuals <- ggplot(residuals, aes(sample=residuals))+
  stat_qq()+
  stat_qq_line()+
  theme_bw()
m.1_residuals

#test assumptions
ncvTest(m.1) #pvalue=0.0375 -alternative is heteroscedastic null is homo
shapiro.test(m.1$residuals) #pvalue= 2.259e-06
#violation of homoscedasticty and it's not normal

m.2 <- lm(richness_log10_plus1 ~method, data_mod)
summary(m.2)

m.3 <- lm(richness_log10_plus1 ~method + area_log10, data_mod)
summary(m.3)
#int 0.75 slope is 0.15 b2 is 0.056 (but not sig)

#when i force it to have same parallel no difference in

#model with interaction term
m.4 <- lm(richness_log10_plus1 ~method* area_log10, data_mod)
summary(m.4)
#just from coefficients methods never matters

#BIC Lowest value is the best model. here it is model m.1
BIC(m.0)
BIC(m.1)
BIC(m.2)
BIC(m.3)
BIC(m.4)

#plot for overlay of eDNA and traditional
best_model <- ggplot(data=trad_only, aes(x=area_log10, y=richness_log10_plus1))+
  geom_point(data=dna_only, aes(x=area_log10, y=richness_log10_plus1), color='red', alpha=0.25, size=2)+
  geom_point(color='blue', alpha=0.25, size=2)+
  geom_smooth(method="lm", color="black")+
  xlab("Log10(Area (ha))") +
  ylab("Log10(Species richness +1)")+
  theme_bw()
best_model








