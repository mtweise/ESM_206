#HW 2
#ESM 206
#####################################

library(tidyverse)
library(janitor)
library(here)
library(tidyr)
library(dplyr)
library(patchwork)

#load data
npgp_raw <- read_csv(here("hw_2/data", "npgp_data.csv")) |>
  clean_names()



#Question 1
#Using your data science prowess, clean, transform, and save only the
#necessary data for your analysis. Provide the data in a table at the end of the write-up.

#drop area B
#need to change to species/category w name then concentration

#SHould i drop all of the species and only have plastic???

npgp_q1_species <- npgp_raw |> 
  pivot_longer(cols= plastic_uncorrected:fish,
               names_to= "category", values_to = "concentration") |>
  drop_na() |> filter(area != "B")

npgp_q1 <-  npgp_raw |>
  select(sample_id:plastic_uncorrected) |>
  drop_na() |> filter(area != "B")
  
#WRITE CSV

##############################
#Question 2

#Conduct EDA and provide a data visualization with a caption that
#describes the distribution of the data.


#make a boxplot
#fig1a <- ggplot(trawl_cortisol, aes(x=plasma_cortisol, y=trawl_led)) +
#  geom_boxplot() +
#  labs(x= "Cortisol (ng/mL)", y= "LED(1=on, 0=off") +
#  theme_bw()

#Make a boxplot

#fig1c <- ggplot(no_led, aes(sample=plasma_cortisol)) +
  stat_qq() +
  stat_qq_line()+
  xlab("Normal prediction") +
  ylab("Observed data") +
  ggtitle("LED off")+
  theme_bw()



q2_figC <- ggplot(data = npgp_q1, aes(sample = plastic_uncorrected)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Normal Prediction") +
  ylab("Observed Data") +
  ggtitle("Q-Q Plot of Plastic Uncorrected Data by Area") +
  theme_bw() +
  facet_wrap(~ Area)


q2_figA <- ggplot(npgp_q1, aes(x=area, y=plastic_uncorrected)) +
  geom_boxplot() +
  labs(x= "Area", y= "Concentration of plastic particles (per km^2)") +
  theme_bw()








#################################
#Question 3

#Provide a statistical summary of the data in a table and discuss

#summary_table <- trawl_cortisol|>
#group_by(trawl_led) |>
#  summarize(min_p= min(plasma_cortisol),
#            IQ25_p = quantile(plasma_cortisol, probs= 0.25),
 #           mean_p =mean(plasma_cortisol),
  #          median_p = median(plasma_cortisol),
   #         IQ75_p = quantile(plasma_cortisol, probs= 0.75),
    #        max_p = max(plasma_cortisol))











