#HW 3
#In this study, the authors measured the consumption elasticity of the initial burden of
#a carbon or gasoline tax (in the absence of tax revenue redistribution) across regions of the
#world from 1992 to 2017. An elasticity <1 means the initial tax burden falls disproportionately
#on the poor, whereas a value >1 indicates the initial tax burden falls disproportionately on the
#rich. You are tasked with assessing if the mean elasticity between the regions of Africa (“Africa”),
#the European Union (“EU”), Latin America (“LatAm”), and the United States (“USA”) are
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
  ggtitle("Elasticity by Region")+
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
write.csv(summary_table, "hw_3/data/summary_table.csv", row.names = FALSE)



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
elasticity_aov <- aov(elasticity ~ region, data=clean_region)
summary(elasticity_aov)
#p-value is 0.000298 (at least two means are not equal)


#nonparametric 
kruskal.test(elasticity ~ region, data=clean_region)
#p-value is 0.0003089

elasticity_obs2 <- ggplot(clean_region, aes(x = region, y = elasticity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), 
              size = 2.5, color = "dodgerblue", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Elasticity by Region", 
       x = "Region", y = "Elasticity") +
  theme_bw() +
  theme(axis.line = element_line(color = "black"))
print(elasticity_obs2)

####################
#Question 3
#(Proficient) Explain the pairwise assessment of the regional consumption elasticity and if
#there is evidence for disproportional burdens between rich and poor in 1-2 paragraphs.
#You may use a figure OR table to support your discussion

#multiple comparison tests
TukeyHSD(elasticity_aov)
#USA-LatAm,  USA_Africa are significant
#adjusted alpha is 0.008333

# Run the Tukey HSD test
tukey_results <- TukeyHSD(elasticity_aov)

# Extract the comparisons for the region factor
tukey_df <- as.data.frame(tukey_results$region)

# Add row names as a column for clarity
tukey_df <- cbind(Comparison = rownames(tukey_df), tukey_df)

# Save the data frame to a CSV file
write.csv(tukey_df, "hw_3/data/tukey_results.csv", row.names = FALSE)


pairwise.wilcox.test(clean_region$elasticity,
                     clean_region$region, p.adjust.method = "bonferroni")
#all of USA comparisons

####################
#OH
#after ANOVA, reject null is favor of alt. one mean is different. which mean?
#Tukey for q3 = tukey output no alpha adjustment so you need to calc that
#it gives you p-adjusted 0.05/ however many comparisons (6)

#make a boxplot with regionson x add 1:1 line for elasticity to compare means eu is both groups


