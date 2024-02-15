library (ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)

#Dataset 1 

#import excel 

new_dataset1 <-Dataset1_SARS_CoV_2casedataforinterview_20230202 #Duplicate data to keep original raw
new_dataset1$year <- strftime(new_dataset1$Specimencollectionday, "%Y") #Creating year column  
new_dataset1$month <- strftime(new_dataset1$Specimencollectionday, "%m") #Creating month column  
new_dataset1$epiweek <- lubridate::epiweek(ymd(new_dataset1$Specimencollectionday)) #generate epiweek
new_dataset1$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
new_dataset1$epiweek2 <- do.call(paste, c(new_dataset1[my_cols],sep ="")) #created new variable using concat columns


#Cases by epiweek and district 

windows()
new_dataset1%>% 
  filter(year =="2022")%>%
  filter(district != "n/a") %>%
  ggplot(aes(x=epiweek, fill=district))+
geom_bar(stat = "count")+
  ggthemes::theme_tufte()+
  theme(axis.ticks.x= element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position="top",
        text = element_text(size=12))+
  labs(x= "\n Epidemiological Week(2022)", y= "Number of Cases \n ")+
  scale_y_continuous() +
  scale_x_continuous(breaks=seq(1,52,by=1))
  

#Table of location and number of samples received per epiweek filtered by district 

new_dataset1%>%
  filter(district== "City of Johannesburg Metro")%>%
  filter( year== "2021") %>%
  aggregate(caseid~ district+epiweek2,
            FUN= length)

#Dataset2 
#Imported spreadsheetfrom excel - change date from <double> to <date>

new_dataset2 <- Dataset2_Wastewater_data
new_dataset2$epiweek <- lubridate::epiweek(ymd(new_dataset2$dateofcollection)) 
new_dataset2$week <- "w" #added column with w
my_col <- c("Year", "week", "epiweek") #new data object with 3 columns combined
new_dataset2$epiweek2 <- do.call(paste, c(new_dataset2[my_col],sep ="")) #created new variable using concat columns



#Table of the number of cases by epiweek and district 

aggregate(LabNumber~ SiteName+epiweek2,
          data=new_dataset2,
          FUN= length)


#Question 2 cases levels by epiweek and district

new_dataset2.1 <- subset(new_dataset2, !is.na(NgeneCt)) #remove na


windows()
new_dataset2.1%>%
  filter(Year == "2022")%>% #filter by year
  ggplot(aes(epiweek, 1/NgeneCt, colour=SiteName)) + #higher ct = lower concentration therefore inverse to see increase over time
  geom_point(size=3, alpha=0.3)+
  geom_line(size=1)+
  theme_tufte()+
  theme(axis.ticks.x= element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position="top", text = element_text(size=12)) +
  labs(x = "Epidemiological Week (2022)")+
  scale_x_continuous(breaks=seq(1,52,by=1))

#Stat Summary by epiweek across all districts

new_dataset2%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Mean=mean(NgeneCt), # we have to specify which column we would like to get the mean of
            Max=max(NgeneCt), 
            Min=min(NgeneCt),
            Median=median(NgeneCt),
            Std=sd(NgeneCt),
            na.rm=TRUE)


#Association lab confirmed, water levels, omicron appearance

windows()
pl <- ggplot(NULL, aes(x= epiweek2))
pl <- pl+ geom_bar(data= new_dataset1, stat = "count", size= 0.1) 
#pl <- pl + geom_line(data = new_dataset2.1, aes(y= 1/NgeneCt,group=1, colour="red"))
#pl <- pl + scale_y_continuous(name = "Number of Cases",
                              #sec.axis = sec_axis( ~. , name="1/Ct"))
pl <- pl + ggthemes::theme_tufte()
pl <- pl + theme(axis.ticks.x= element_blank(), 
                 axis.text.x = element_text(angle = 90, hjust = 0),
                 legend.position="top",
                 text = element_text(size=12)) #this rotates text on x-axis to 90 degree
pl <- pl + labs(x= "\n Epidemiological Week", y= "1/Ct ")# this \n is a line break, it basically adds in an extra line before/after the label
pl



