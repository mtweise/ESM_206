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


#remake figure 1 in budolfson
budolfson_plot_data <- ggplot(clean_lakes_data, aes(x=area_ha, y=shared))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="Area", y="Shared richness")+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_bw()
budolfson_plot_data

#Running the OLS linear regression
budolfson_output_edna <- lm(area_ha ~ log10(dna_richness), data=clean_lakes_data)
######add 1 to all values for we can take log10
budolfson_output_edna
#output is intercept (y int) and slope of the line (log10)
summary(budolfson_output)
#null for is that intercept is 0,0
#how much of the variability is explained by the model? multiple r square (21%)

#plot using the output of the linear model
b_plot_data <- ggplot(data_mod, aes(x=pc_gdp, y=elasticity))+
  geom_point()+
  scale_x_continuous(trans='log10')+
  labs(x="GDP per capita (2005 US$", y="Elasticity")+
  geom_abline(intercept=coef(budolfson_output)[1], slope=coef(budolfson_output)[2])+
  theme_bw()
b_plot_data








########################################
#Question 2
#(Fundamental) Produce a plot showing the best-fit regression(s) with a caption and the
#data (5 pts).
















########################################
#Question 3
#(Proficient) Make a one-paragraph recommendation to your supervisor if they should
#use the cost-effective eDNA approach for fisheries surveys in California or not (3 pts)

















