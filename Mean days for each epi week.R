library(dplyr)

# Create dataframe if it's not created already 

Sample<- c(1,2,3,4,5,6)
Collection <- c("2017-03-23","2017-03-24","2017-03-24","2017-03-28","2017-03-24","2017-03-28")
Submission<- c("2017-03-24","2017-03-28","2017-04-24","2017-05-28","2017-06-24","2017-07-28")
Epiweek<- c(1,1,2,2,3,3)

df <- data.frame(Sample, Collection, Submission, Epiweek)

#Subtract dates to get time difference in days 

df$diff_in_days<- difftime(df$Submission ,df$Collection , units = c("days"))

#you need dplyr loaded for this next section. 
#This gives a basic stat summary of the days it took for each epi week 

df%>% # we specify which df we would like to use
  group_by(Epiweek)%>% # we specify what we would like to group by 
  summarise(Mean=mean(diff_in_days), # we have to specify which column we would like to get the mean of
            Max=max(diff_in_days), 
            Min=min(diff_in_days),
            Median=median(diff_in_days), 
            Std=sd(diff_in_days))
