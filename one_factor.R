#One-Way Analysis of Variance (ANOVA) - Balanced Design

#clear global environment
rm(list = ls(all.names = TRUE))

#Libraries----
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(car)
library(gridExtra)


#----Functions-----------------------------------------------


#created function to clean citibike input data and eliminate trips greater than 11 hours and less than 5 minutes.
citi_clean <- function(input_df){
  
  function_df <-  input_df %>% 
    filter(!(tripduration < 300 & `start station id` == `end station id`)) %>%  
    filter(tripduration < 43200 & (day(starttime) %in% c(1:28))) %>% 
    mutate(day = day(starttime)) %>% mutate(month = month(starttime, label = TRUE))
  
  return(function_df)
}



#----import and clean-------------------------------------------------


#load citibike data for 2019 by month
january <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201901-citibike-tripdata.csv", col_names = TRUE)
april <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201904-citibike-tripdata.csv", col_names = TRUE)
july <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201907-citibike-tripdata.csv", col_names = TRUE)
november <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201911-citibike-tripdata.csv", col_names = TRUE)


#clean and retructure data with citi_clean() function using lapply()
months_raw <- list(january, april, july, november)

months_cleaned <- lapply(months_raw, citi_clean)



#----Main------------------------------------------


# prepare data for visual inspection and anova
january_vi <- months_cleaned[[1]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month)

april_vi <- months_cleaned[[2]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month)

july_vi <- months_cleaned[[3]]%>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month)

november_vi <- months_cleaned[[4]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month)


# graphics for assumption check via visual inspection
january_hist <- ggplot(data = january_vi, mapping = aes(x = trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "January")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))
  
april_hist <- ggplot(data = april_vi, mapping = aes(x = trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "April")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))

july_hist <- ggplot(data = july_vi, mapping = aes(x = trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "July")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))

november_hist <- ggplot(data = november_vi, mapping = aes(x = trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "November")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))


#arrange into a grid for easy viewing
grid.arrange(january_hist,  april_hist, july_hist, november_hist, ncol = 2)


#anaova ready data
anova_data <- rbind(january_vi, april_vi, july_vi, november_vi) %>% group_by(month, day)
anova_data$month <- as.factor(anova_data$month)


#to apply shapiro-wilk
months_list <- list(january_vi$trips, april_vi$trips, july_vi$trips, november_vi$trips)


#Shapiro-Wilk normality tests - 1st assumption for ANOVA - population
sw_results <- lapply(months_list, shapiro.test)
sw_pvalues <- c(sw_results[[1]][2], sw_results[[2]][2], sw_results[[3]][2], sw_results[[4]][2])

  #results
sw_pvalues > .05 #if all true then fail to reject Ho


#Levene's Test for equality of variances across populations - 2nd assumption for ANOVA - population
leveneTest(trips ~ month, data = anova_data) #fails Levene's Test confirming visual inspection



#----ANOVA-Ready-Main---------------------------------------------


#extract some summary statistics for each group (month)
sum_stats <- anova_data %>% group_by(month) %>% 
  summarise(observations = n(), mean = mean(trips), st.dev = sd(trips)) # na.rm not necessary


#visualize the samples via boxplot using ggplot2 package
ggplot(anova_data, mapping = aes( x = month, y = trips))+
  geom_boxplot(notch = TRUE)+
  stat_summary(fun=mean, geom="point", shape=21, size=4, color = "darkgreen")+
  scale_y_log10()+
  ylab("Quantity of Trips")+
  xlab("Month")+
  labs(title = "Month Comparison via Notched Boxplot")+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = .5))


#ANOVA analysis
output_anova <- aov(trips ~ month, data = anova_data)
summary.aov(output_anova)


#Tukey method for comparison
TukeyHSD(output_anova)


#----sample size reductions to remove outliers---



# prepare data for visual inspection and anova
january_vir <- months_cleaned[[1]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(trips)

april_vir <- months_cleaned[[2]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(trips)

july_vir <- months_cleaned[[3]]%>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(trips)

november_vir <- months_cleaned[[4]] %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(trips)


#anaova ready data
anova_data_r <- rbind(january_vir[2:28,], april_vir[2:28,], july_vir[2:28,], november_vir[2:28,]) %>% group_by(month, day)
anova_data_r$month <- as.factor(anova_data_r$month)


#to apply shapiro-wilk
months_list_r <- list(january_vir$trips, april_vir$trips, july_vir$trips, november_vir$trips)


#Shapiro-Wilk normality tests - 1st assumption for ANOVA - population
sw_results_r <- lapply(months_list_r, shapiro.test)
sw_pvalues_r <- c(sw_results[[1]][2], sw_results[[2]][2], sw_results[[3]][2], sw_results[[4]][2])

#results
sw_pvalues_r > .05 #if all true then fail to reject Ho


#Levene's Test for equality of variances across populations - 2nd assumption for ANOVA - population
leveneTest(trips ~ month, data = anova_data_r) #fails Levene's Test confirming visual inspection



#----ANOVA-Ready-Main---------------------------------------------


#extract some summary statistics for each group (month)
sum_stats <- anova_data %>% group_by(month) %>% 
  summarise(observations = n(), mean = mean(trips), st.dev = sd(trips)) # na.rm not necessary


#visualize the samples via boxplot using ggplot2 package
ggplot(anova_data_r, mapping = aes( x = month, y = trips))+
  geom_boxplot(notch = TRUE)+
  stat_summary(fun=mean, geom="point", shape=21, size=4, color = "darkgreen")+
  scale_y_log10()+
  ylab("Quantity of Trips")+
  xlab("Month")+
  labs(title = "Month Comparison via Notched Boxplot")+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = .5))


#ANOVA analysis
output_anova_r <- aov(trips ~ month, data = anova_data_r)
summary.aov(output_anova_r)


#Tukey method for comparison
TukeyHSD(output_anova_r)


# sinusoidal curve
x <-  seq(0,10,.1)
y <- sin(x)
sin_df <- as.data.frame(cbind(x,y))

sin_curve <- ggplot(data = sin_df, mapping = aes(x = x, y = y))+
  geom_line()+
  theme_classic()+
  geom_text( x = pi/2, y = 1.1, label = "Summer")+
  geom_text( x = 1.5*pi, y = -1.1, label = "Winter")+
  geom_text( x = pi/2+2*pi, y = 1.1, label = "Summer")+
  geom_text( x = 3*pi/3.5, y = 0, label = "Spring")+
  geom_text( x = 3*pi/3.5 + 1.25*pi, y = 0, label = "Fall")+
  ylim(c(-1.5,1.5))+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        axis.line = element_blank(), plot.title = element_text(hjust = .5))+
  ggtitle("Seasonal Temperature Cycle")
  


