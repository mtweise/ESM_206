#HW 4

#ObjecPve: You are working for CDFW this summer as a fisheries biodiversity intern, and the
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
clean_lakes_data <- lakes_data |> clean_names() |>
   select(area_ha, dna_richness, trad_richness, shared) |>
  drop_na()

#mutate so we can take log10 by adding 1 to species richness
mod_clean_lakes_data <- clean_lakes_data |>
  mutate(dna_richness= dna_richness +1,
         trad_richness= trad_richness +1,
         shared= shared +1)

#remake figure 1 in budolfson
budolfson_plot_data <- ggplot(mod_clean_lakes_data, aes(x=area_ha, y=shared))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="Area", y="Shared richness")+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_bw()
budolfson_plot_data

#Running the OLS linear regression
budolfson_output_edna <- lm(area_ha ~ log10(dna_richness), data=mod_clean_lakes_data)
budolfson_output_edna
#output is intercept (y int= -250731) and slope of the line (log10= 308797)


summary(budolfson_output_edna)
#null for is that intercept is 0,0
#how much of the variability is explained by the model? multiple r square (0.09559)

#plot using the output of the linear model
b_plot_data <- ggplot(mod_clean_lakes_data, aes(x=area_ha, y=dna_richness))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="Area (hectares)", y="Species Richness")+
  geom_abline(intercept=coef(budolfson_output_edna)[1], slope=coef(budolfson_output_edna)[2])+
  theme_bw()
b_plot_data

#????????? what happened
#do i repeat with trad data?

#looking at assumptions
residuals <- ggplot(budolfson_output_edna, aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()
residuals
#look at how the data are scattered around that fitted value

#lets make a qqplot to look at normality of the residuals
qq_residuals <- ggplot(budolfson_output_edna, aes(sample=.resid))+
  geom_qq()+
  geom_qq_line()+
  theme_bw()
qq_residuals

#test the assumptions
shapiro.test(budolfson_output_edna$residuals)
#we reject the null therefore you can do nonpramatric or transform data
#linear regression is very robust against slight assumptions of the normality

ncvTest(budolfson_output_edna)
#we reject so unequal variance




########################################
#Question 2
#(Fundamental) Produce a plot showing the best-fit regression(s) with a caption and the
#data (5 pts).
















########################################
#Question 3
#(Proficient) Make a one-paragraph recommendation to your supervisor if they should
#use the cost-effective eDNA approach for fisheries surveys in California or not (3 pts)

















