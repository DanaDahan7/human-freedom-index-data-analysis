library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("plyr")
install.packages("ggmap")
install.packages("ggplot")
install.packages("maps")
library("ggmap")
library("maps")
library("plyr")
install.packages("ggpubr")
library("ggpubr")

getwd()
freedom_origin_data <- read.csv("The Human Freedom.csv", TRUE, ",")%>%
view()


##edit South Asia data for modeling:
South_Asia <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  #removing the the columns of over 10 rank data from the origin data set 
  filter(year == 2018 | year == 2017 | year == 2016) %>%
  filter(region == "South Asia")%>%
  view()

RowMeanSouthAsia <- rowMeans(South_Asia[6:111], na.rm = TRUE, dims = 1)
South_Asia["row_means"] <- RowMeanSouthAsia #adding new column of row means
South_Asia%>%
  view()

##edit East Asia data for modeling:
East_Asia <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  #removing the the columns of over 10 rank data from the origin data set 
  filter(year == 2018 | year == 2017 | year == 2016) %>%
  filter(region == "East Asia")%>%
  view()

RowMeanEastAsia <- rowMeans(East_Asia[6:111], na.rm = TRUE, dims = 1)
East_Asia["row_means"] <- RowMeanEastAsia #adding new column of row means
East_Asia%>%
  view()

## edit Western Europe data for modeling: 
Western_Europe <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  #removing the the columns of over 10 rank data from the origin data set 
  filter(year == 2018 | year == 2017 | year == 2016) %>%
  filter(region == "Western Europe")%>%
  view()
RowMeanWesternEurope <- rowMeans(Western_Europe[6:111], na.rm = TRUE, dims = 1)
Western_Europe["row_means"] <- RowMeanWesternEurope #adding new column of row means
Western_Europe%>%
  view()

#Testing hypotheses about equality of variance between two populations (East and West, south east and west) - 
#This is a preliminary stage for testing hypotheses about the expectation difference.
#I have centralized the data to be relevant only to region related to the research question:
freedom_data <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  filter(year == 2018 | year == 2017 | year == 2016) %>%
  filter(region == "East Asia" | region == "South Asia" | region == "Western Europe")%>%
  view()
RowMeanFreedomData <- rowMeans(freedom_data[6:111], na.rm = TRUE, dims = 1)
freedom_data["row_means"] <- RowMeanFreedomData
freedom_data%>% #create a row that represent the mean of freedom at each country
  view()

#Visualization to distinguish the variance with the help of a density graph:
ggplot(freedom_data, aes(color = region, x = row_means)) +
  geom_density()
#it's seems the variance in south and east ans south Asia is much bigger than the variance in western Europe









#1) Testing hypotheses about equality of variance between two populations East and West
## H0: mu_West = mu_East
## H1: mu_West > mu_East
t.test(Western_Europe %>% pull(row_means),
       East_Asia %>% pull(row_means), alternative = "greater")

#Becasue p_value is bigger than alpha, we are accepting H0, which say's there is no big difference 
#in the level of freedom between east asia and western europe. 

#here we'll check comparison of variance between east asia and western europe:
## H0: sigma_West / sigma_East = 1
## H1:  sigma_West / sigma_East < 1
var.test(Western_Europe %>% pull(row_means),
         East_Asia %>% pull(row_means), alternative = "less")
#Because p_value is smaller that alpha we're rejecting H0
#it means the true ratio of variances is less than 1


EastVSWest <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank)) %>% #editing the data for only east and west information
  filter(region == "Western Europe"| region == "East Asia", 
         year == 2018 | year == 2017 | year == 2016)
EastVSWest_row_means <- c(rowMeans(EastVSWest[6:111], na.rm = TRUE, dims = 1))
EastVSWest["row_means"] <- EastVSWest_row_means
EastVSWest%>%
  view() 

qplot(data = EastVSWest ,
      y = row_means,x = region ,
      main = "East Asia and Western Europe Freedom ",
      fill = region,geom = "boxplot")+
    scale_y_continuous(name="freedom level")

#2) Testing hypotheses about equality of variance between two populations South Asia and western europe:
## H0: mu_West = mu_South
## H1: mu_West > mu_South
t.test(Western_Europe %>% pull(row_means),
       South_Asia %>% pull(row_means), alternative = "greater")
#Becasue p_value is smaller than alpha, we are rejecting H0. Therefore, there is actual difference 
#in the level of freedom between South Asia and western europe. 

#here we'll check comparison of variance between south Asia and western europe:
## H0: sigma_West / sigma_South = 1
## H1:  sigma_West / sigma_South < 1
var.test(Western_Europe %>% pull(row_means),
         South_Asia %>% pull(row_means), alternative = "less")
#Because p_value is smaller that alpha we're rejecting H0
#it means the true ratio of variances is less than 1

SouthVSWest <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank)) %>% 
  filter(region == "Western Europe"| region == "South Asia", 
         year == 2018 | year == 2017 | year == 2016)
SouthVSWest_row_means <- c(rowMeans(SouthVSWest[6:111], na.rm = TRUE, dims = 1))
SouthVSWest["row_means"] <- SouthVSWest_row_means
SouthVSWest%>%
  view() 

qplot(data = SouthVSWest ,
      y = row_means,x = region , 
      main = "South Asia and Western Europe Freedom level",
      fill = region,geom = "boxplot") +
    scale_y_continuous(name="freedom level")


#3) Testing hypotheses about equality of means between east Asia and south Asia:
## H0: mu_east = mu_south
## H1: mu_east != mu_south
t.test(South_Asia %>% pull(row_means),
       East_Asia %>% pull(row_means))
#Becasue p_value is bigger than alpha, we are accepting H0, which say's there is no big difference 
#in the level of freedom between east asia and south Asia. 

#here we'll check comparison of variance between east Asia and South Asia:
## H0: sigma_east / sigma_south = 1
## H1:  sigma_east / sigma_south != 1
var.test(South_Asia %>% pull(row_means),
         East_Asia %>% pull(row_means))
#Because p_value is bigger that alpha we're accepting H0
#it means the ratio of variances is close to 1

SouthVSEast <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank)) %>% 
  filter(region == "East Asia"| region == "South Asia", 
         year == 2018 | year == 2017 | year == 2016)
SouthVSEast_row_means <- c(rowMeans(SouthVSEast[6:111], na.rm = TRUE, dims = 1))
SouthVSEast["row_means"] <- SouthVSEast_row_means
SouthVSEast%>%
  view() 

qplot(data = SouthVSEast ,
      y = row_means,x = region , 
      main = "South Asia and East Asia Freedom level",
      fill = region,geom = "boxplot") +
  scale_y_continuous(name="freedom level")




##Jonathan's visualization east-west:   

East_and_West <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank)) %>% #editing the data for only east and west information
  filter(region == "Western Europe"| region =="South Asia" | region == "East Asia", 
         year == 2018) %>%
  view()
  

east_and_west_row_means <- c(rowMeans(East_and_West[6:111], na.rm = TRUE, dims = 1))%>%
  view() #calculate the mean of any row of the data

East_and_West["total_freedom"] <- east_and_west_row_means #adding new column
East_and_West%>%
  view() 

qplot(data = East_and_West ,
      y = total_freedom,x = region ,
      main = "East and West Freedom ",
      fill = region,geom = "boxplot") #creating a boxplot that visualize the different levels of freedom between the east and the west.




##editing of women and complementary data for modeling the world impact of women freedom on general freedom: 


freedom_data_world <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  filter(year == 2018 )
women_data <- freedom_data_world %>% #edit the data for only women information
  filter(year == 2018 ) %>%
  select(X, region, countries, pf_ss_women_fgm, pf_ss_women_inheritance, pf_ss_women, 
         pf_movement_women, pf_identity_sex_female, womens_freedom)%>%
  view()

women_data_row_means <- c(rowMeans(women_data[4:9], na.rm = TRUE, dims = 1))%>%
#calculate the mean of any row of the data
  view()

women_data["row_means"] <- women_data_row_means  #adding to women_data a new column of row means
women_data%>%
  view()

complementary_data <- subset(freedom_origin_data, select = 
                               -c(pf_ss_women_fgm, pf_ss_women_inheritance, 
                                  pf_ss_women, pf_movement_women, 
                                  pf_identity_sex_female, womens_freedom,
                                  ef_rank, hf_rank))%>%
  #removing the the columns of women data from the origin data set 
  filter(year == 2018 )%>%
  view()

complementary_data_row_means <- c(rowMeans(complementary_data[6:105], 
                                           na.rm = TRUE, dims = 1))
#calculate the mean of the rest of the data

complementary_data["row_means"] <- complementary_data_row_means 
#adding to complementary_data a new column of row means
complementary_data%>%
  view()

impact_means <- women_data_row_means/complementary_data_row_means #creating a column
freedom_data_world["impact_means"] <- impact_means #adding new column of the impact of women on the complementary means
view(impact_means)

world_map <- map_data("world")
view(world_map)#create world map 

world_with_freedom <-left_join(world_map,freedom_data_world, by =  c("region" = "countries") )# combine the data set of the map world with the data set of freedom data

ggplot(world_with_freedom, 
       aes(x = long, y = lat, group = group, fill = impact_means)) + 
  geom_polygon() + 
  scale_fill_viridis_c() #create visualization of the impact on the freedom data on the generall freedom of each country.



###########################################################################
#Linear Regression model to examine the dependence of women's level of freedom 
#on the general level of freedom in the country



#edit origin data set for linear regression test:
# x = women freedom's level (depended variable) 
# y= country freedom's level (independent variable)

lr_data <- subset(freedom_origin_data, select = -c(ef_rank, hf_rank))%>%
  filter(year == 2018)
total_means <- c(rowMeans(lr_data[6:111], na.rm = TRUE, dims = 1))
lr_data["row_means"] <- total_means
lr_data <- lr_data %>%
  mutate(FreedomLevel = row_means,
         womensfreedom = womens_freedom)%>%
  group_by(FreedomLevel, womensfreedom) %>%
  tally()

#visual women's freedom density and country's freedom density:
density_women <- ggplot(data=lr_data, aes(womensfreedom)) + 
  geom_density(col="white",fill="#FFCC00", alpha=0.8) + 
  ggtitle("Density Plot of women freedom's level")
density_country <- ggplot(data=lr_data, aes(FreedomLevel)) + 
  geom_density(col="white", fill="#FF9900", alpha=0.8) + 
  ggtitle("Density Plot of country freedom's level")

#summary of women's freedom and country's freedom:
lr_data %>%
  summarise(
    min = min(FreedomLevel),
    median = median(FreedomLevel),
    max = max(FreedomLevel))

lr_data %>%
  summarise(
    min = min(womensfreedom),
    median = median(womensfreedom),
    max = max(womensfreedom))





#First, we will check if there is a correlation between women freedom's level 
#and country freedom's level linear regression model:
lr_mod <- lm(formula = log(lr_data$FreedomLevel) ~ 
               lr_data$womensfreedom)
print(lr_mod)


#visualization for linear regrration 
lr_points <- lr_data %>% 
  ggplot(aes(y=FreedomLevel, x= womensfreedom))+
  geom_point(color="#6699FF")+
  theme_bw() +
  scale_y_log10()+
  stat_smooth(method = "lm",color="#000099")+
  xlab("country freedom's level") + ylab("women freedom's level")

scatter.smooth(x=lr_data$womensfreedom, y=lr_data$FreedomLevel, 
               main="women ~ country", xlab = "country freedom's level",
               ylab = "women freedom's level") 

summary(lr_mod)

#The R-Squared is pretty low, around 0.38 which means that only 38% of the variation in the dependent variable is explained by the independent variable. 
#However, We can consider a linear model to be statistically significant only when p-Value is less that the statistical significance level, which is 0.05. This is visually interpreted by the significance stars at the end of the row. The more the stars beside the variable’s p-Value, the more significant the variable. Here we can see that there is three stars which is the most significant.
#Therefore we are rejecting the null hypothesis that the coefficient equals zero, and we are accepting the alternative hypothesis, which means there is some connection between the dependency of women's level of freedom on the country's level of freedom.

attributes(lr_mod)
lr_mod$residuals

