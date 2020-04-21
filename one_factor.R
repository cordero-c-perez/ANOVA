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


#created function to clean citibike input data and eliminate trips greater than 24 hours and less than 2 minutes.
citi_clean <- function(input_df){
  
  function_df <-  input_df %>% filter(tripduration > 120 & tripduration < 86400 & (day(starttime) %in% c(1:28))) %>% 
    mutate(day = day(starttime)) %>% mutate(month = month(starttime, label = TRUE))
  
  return(function_df)
}


#----Main-------------------------------------------------


#load citibike data for 2019 by month
january <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201901-citibike-tripdata.csv", col_names = TRUE)
february <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201902-citibike-tripdata.csv", col_names = TRUE)
march <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201903-citibike-tripdata.csv", col_names = TRUE)
april <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201904-citibike-tripdata.csv", col_names = TRUE)
may <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201905-citibike-tripdata.csv", col_names = TRUE)
june <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201906-citibike-tripdata.csv", col_names = TRUE)
july <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201907-citibike-tripdata.csv", col_names = TRUE)
august <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201908-citibike-tripdata.csv", col_names = TRUE)
september <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201909-citibike-tripdata.csv", col_names = TRUE)
october <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201910-citibike-tripdata.csv", col_names = TRUE)
november <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201911-citibike-tripdata.csv", col_names = TRUE)
december <- read_csv("~/Documents/R/RProjects-Public/ANOVA-Data/201912-citibike-tripdata.csv", col_names = TRUE)


#clean and retructure data with citi_clean() function using lapply()
months_raw <- list(january, february, march, april, may, june, july, august, september, october, november, december)

months_cleaned <- lapply(months_raw, citi_clean)


#----Seasons----Main------------------------------------------


#collection by season
winter <- rbind(months_cleaned[[1]], months_cleaned[[2]], months_cleaned[[3]]) %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month) %>% 
  mutate(season = "winter")

spring<- rbind(months_cleaned[[4]], months_cleaned[[5]], months_cleaned[[6]]) %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month) %>% 
  mutate(season = "spring")

summer<- rbind(months_cleaned[[7]], months_cleaned[[8]], months_cleaned[[9]]) %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month) %>% 
  mutate(season = "summer")

fall<- rbind(months_cleaned[[10]], months_cleaned[[11]], months_cleaned[[12]]) %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month) %>% 
  mutate(season = "fall")


#seasons graphics for assumption check via visual inspection
w_hist <- ggplot(data = winter, mapping = aes(x = winter$trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "Winter")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))
  
sp_hist <- ggplot(data = spring, mapping = aes(x = spring$trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "Spring")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))

su_hist <- ggplot(data = summer, mapping = aes(x = summer$trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "Summer")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))

f_hist <- ggplot(data = fall, mapping = aes(x = fall$trips))+geom_histogram(bins = 8, fill = "grey", color = "black")+ 
  theme_classic()+ labs(title = "Fall")+ ylab("Frequency")+ xlab("Trips")+
  theme(plot.title = element_text(hjust = .25))


#arrange into a grid for easy viewing
grid.arrange(w_hist,  sp_hist, su_hist, f_hist, ncol = 2)


#anaova ready data
anova_data_s <- rbind(winter, spring, summer, fall) %>% group_by(season, month, day)
anova_data$season <- as.factor(anova_data_s$season)
anova_data_s <- anova_data_s[,c(4,1:3)] #order data


#to apply shapiro-wilk
seasons_list <- list(winter$trips, spring$trips, summer$trips, fall$trips)


#Shapiro-Wilk normality tests - 1st assumption for ANOVA - population
sw_results <- lapply(seasons_list, shapiro.test)
sw_pvalues <- c(sw_results[[1]][2], sw_results[[2]][2], sw_results[[3]][2], sw_results[[4]][2])


#Levene's Test for equality of variances across populations - 2nd assumption for ANOVA - population
leveneTest(trips ~ season, data = anova_data_s) #fails Levene's Test confirming visual inspection


#----Monthly----Main--------------------------------------------


#collection by month
anova_data_monthly <- rbind(months_cleaned[[1]], months_cleaned[[2]], months_cleaned[[3]],
                    months_cleaned[[4]], months_cleaned[[5]], months_cleaned[[6]],
                    months_cleaned[[7]], months_cleaned[[8]], months_cleaned[[9]],
                    months_cleaned[[10]], months_cleaned[[11]], months_cleaned[[12]]) %>% 
  select(day, month) %>% group_by(month,day) %>% summarise(trips = n()) %>% arrange(month)


#to apply shapiro-wilk
months_list <- list(anova_data_monthly$trips[1:28], anova_data_monthly$trips[29:56], anova_data_monthly$trips[57:84],
                    anova_data_monthly$trips[85:112], anova_data_monthly$trips[113:140], anova_data_monthly$trips[141:168],
                    anova_data_monthly$trips[169:196], anova_data_monthly$trips[197:224], anova_data_monthly$trips[225:252],
                    anova_data_monthly$trips[253:280], anova_data_monthly$trips[281:308], anova_data_monthly$trips[309:336])


#Shapiro-Wilk normality tests - 1st assumption for ANOVA - population
sw_results_monthly <- lapply(months_list, shapiro.test)

sw_pvalues_monthly <- c(sw_results_monthly[[1]][[2]], sw_results_monthly[[2]][[2]], sw_results_monthly[[3]][[2]], sw_results_monthly[[4]][[2]],
                        sw_results_monthly[[5]][[2]], sw_results_monthly[[6]][[2]], sw_results_monthly[[7]][[2]], sw_results_monthly[[8]][[2]],
                        sw_results_monthly[[9]][[2]], sw_results_monthly[[10]][[2]], sw_results_monthly[[11]][[2]], sw_results_monthly[[12]][[2]])


#Kolmogorov-Smirnov normality tests (for months that fail Shapiro-Wilk) - 1st assumption for ANOVA - population
ks_results_monthly <- list(ks.test( x = months_list[[5]], y = rnorm(28, mean(months_list[[5]]), sd(months_list[[5]]))), 
                           ks.test( x = months_list[[6]], y = rnorm(28, mean(months_list[[6]]), sd(months_list[[6]]))), 
                           ks.test( x = months_list[[8]], y = rnorm(28, mean(months_list[[8]]), sd(months_list[[8]]))), 
                           ks.test( x = months_list[[9]], y = rnorm(28, mean(months_list[[9]]), sd(months_list[[9]]))), 
                           ks.test( x = months_list[[10]], y = rnorm(28, mean(months_list[[10]]), sd(months_list[[10]]))))

ks_pvalues_monthly <- c(ks_results_monthly[[1]][[2]], ks_results_monthly[[2]][[2]], ks_results_monthly[[3]][[2]], ks_results_monthly[[4]][[2]],
                        ks_results_monthly[[5]][[2]])

#Levene's Test for equality of variances across populations - 2nd assumption for ANOVA - population
leveneTest(trips ~ month, data = anova_data_monthly)


#----ANOVA-Ready-Main---------------------------------------------


#extract some summary statistics for each group (month)
sum_stats <- anova_data_monthly %>% group_by(month) %>% 
  summarise(observations = n(), mean = mean(trips), st.dev = sd(trips)) # na.rm not necessary


#visualize the samples via boxplot using ggplot2 package
ggplot(anova_data_monthly, mapping = aes( x = month, y = trips))+
  geom_boxplot(notch = TRUE)+
  stat_summary(fun=mean, geom="point", shape=21, size=4, color = "darkgreen")+
  scale_y_log10()+
  ylab("Quantity of Trips")+
  xlab("Month")+
  labs(title = "Month Comparison via Notched Boxplot")+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = .5))


#ANOVA analysis
output_anova <- aov(trips ~ month, data = anova_data_monthly)
summary.aov(output_anova)


#Tukey method for comparison
TukeyHSD(output_anova)




