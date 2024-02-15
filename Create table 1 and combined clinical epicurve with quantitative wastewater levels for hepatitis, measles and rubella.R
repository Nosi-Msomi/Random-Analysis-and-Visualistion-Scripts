
library (ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(rlang)
library(writexl)
library(janitor) 
library(gtsummary)
library(gt)
library(flextable)

########################################################################
#load clinical cases (each year a diff excel file)

hav21<- read_xlsx("~/HAV21.xlsx")
hav22<- read_xlsx("~/HAV22.xlsx")
hav23<- read_xlsx("~/HAV23.xlsx")

hev21<- read_xlsx("~/HEV21.xlsx")
hev22<- read_xlsx("~/HEV22.xlsx")
hev23<- read_xlsx("~/HEV23.xlsx")

measles21<- read_xlsx("~/Measles21.xlsx")
measles22<- read_xlsx("~/Measles22.xlsx")
measles23<- read_xlsx("~/Measles23.xlsx")

rubella21<- read_xlsx("~/Rubella21.xlsx")
rubella22<- read_xlsx("~/Rubella22.xlsx")
rubella23<- read_xlsx("~/Rubella23.xlsx")

#merging clinical cases
#bind rows is from tidyverse and joins df one under the other

havcases <- bind_rows(hav21,hav22,hav23)
hevcases <- bind_rows(hev21,hev22,hev23)
measlescases <- bind_rows(measles21,measles22,measles23)
rubellascases <- bind_rows(rubella21,rubella22,rubella23)

#filtering laboratory-confirmed cases only

havcases <- havcases %>% 
  filter(Diagnosis_Method == "Laboratory confirmed")

hevcases <- hevcases %>% 
  filter(Diagnosis_Method == "Laboratory confirmed")

measlescases <- measlescases %>% 
  filter(Diagnosis_Method == "Laboratory confirmed")

rubellascases <- rubellascases %>% 
  filter(Diagnosis_Method == "Laboratory confirmed")

#setting up epiweeks for x-axis - epiweeks based on notification date as only 
#consistenlty available. may be a few days off from symptoms date

havcases$newcoldate <- format(as.Date(havcases$Notification_Date, format = "%Y/%m/%d"), "%Y-%m-%d") #changing the format of the date from y/m/d to ymd 
havcases$epiweek <- lubridate::epiweek(ymd( havcases$newcoldate)) #generate epiweek
havcases$year <- strftime(havcases$newcoldate, "%Y") #Creating year column  
havcases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
havcases$epiweek2 <- do.call(paste, c(havcases[my_cols],sep ="")) #created new variable using concat columns

hevcases$newcoldate <- format(as.Date(hevcases$Notification_Date, format = "%Y/%m/%d"), "%Y-%m-%d") #changing the format of the date from y/m/d to ymd 
hevcases$epiweek <- lubridate::epiweek(ymd(hevcases$newcoldate)) #generate epiweek
hevcases$year <- strftime(hevcases$newcoldate, "%Y") #Creating year column  
hevcases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
hevcases$epiweek2 <- do.call(paste, c(hevcases[my_cols],sep ="")) #created new variable using concat columns

measlescases$newcoldate <- format(as.Date(measlescases$Notification_Date, format = "%Y/%m/%d"), "%Y-%m-%d") #changing the format of the date from y/m/d to ymd 
measlescases$epiweek <- lubridate::epiweek(ymd(measlescases$newcoldate)) #generate epiweek
measlescases$year <- strftime(measlescases$newcoldate, "%Y") #Creating year column  
measlescases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
measlescases$epiweek2 <- do.call(paste, c(measlescases[my_cols],sep ="")) #created new variable using concat columns

rubellascases$newcoldate <- format(as.Date(rubellascases$Notification_Date, format = "%Y/%m/%d"), "%Y-%m-%d") #changing the format of the date from y/m/d to ymd 
rubellascases$epiweek <- lubridate::epiweek(ymd(rubellascases$newcoldate)) #generate epiweek
rubellascases$year <- strftime(rubellascases$newcoldate, "%Y") #Creating year column  
rubellascases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
rubellascases$epiweek2 <- do.call(paste, c(rubellascases[my_cols],sep ="")) #created new variable using concat columns

##############################################################################
#load wastewater samples
sacases <- read_csv("~/Book1.csv")

sacases <- sacases %>%
  clean_names() #clean names removes unique characters in headings and replaces spaces with _ 

sacases$epiweek <- lubridate::epiweek(ymd( sacases$sample_collection_date)) #generate epiweek
sacases$year <- strftime(sacases$sample_collection_date, "%Y")#Creating year column  
sacases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
sacases$epiweek2 <- do.call(paste, c(sacases[my_cols],sep ="")) #created new variable using concat columns


# Setting so gtsummary doesn't add commas in large numbers 

list("style_number-arg:big.mark" = "") %>%
  set_gtsummary_theme()


#HAV
########################################################################


#Tabulate number of samples we've received 

hav_samples <- havcases %>%
  group_by(epiweek2) %>%
  count(epiweek2, na.rm=TRUE)

#filter for samples with HAV result 

hav_water <- sacases %>% 
  filter(hav_result != "NA")

#selecting columns I want

#check column names 
#names(hav_water)

hav_water <- hav_water %>% 
  select(epiweek2, site_name, site_province, district_name, 
         hav_concentration_copies_u_l, hav_ci_95_percent, hav_partitions_valid, 
         hav_partitions_positive, hav_partitions_negative,hav_result,) %>% 
  filter(epiweek2 != "NAwNA")

#dPCR back calculation uL to mL 

hav_water$gc_ml <- hav_water$hav_concentration_copies_u_l*12*(50/8)*(1000/200)/70

hav_water <- hav_water %>%
  mutate(gc_ml = na_if(gc_ml, gc_ml < 0)) %>%
  mutate(hav_partitions_positive = na_if(hav_partitions_positive, hav_partitions_positive < 0))

hav_water$log_gc_ml <- log10(hav_water$gc_ml)

hav_water2 <- hav_water 

hav_water2$"Province"  <- hav_water2$site_province 
hav_water2$"dPCR Test Result" <- hav_water2$hav_result 
hav_water2$"Valid Partitions"<- hav_water2$hav_partitions_valid  
hav_water2$"Positive Partitions"<-hav_water2$hav_partitions_positive  
hav_water2$"Negative Partitions"<-hav_water2$hav_partitions_negative  
hav_water2$"Genome Copies per mL"<-hav_water2$gc_ml  

#Table 1 

table1 <- hav_water2 %>%
  tbl_summary(include = c( "Province", 
                           "dPCR Test Result",
                           "Valid Partitions",
                           "Positive Partitions",
                           "Negative Partitions",
                           "Genome Copies per mL"),
              by = "Province",# split table by group
              missing = "no", # don't list missing data separately
              #statistic = list(all_continuous() ~ "{mean} ({sd}) "), #{min} {max}
              type = c("Valid Partitions",
                       "Positive Partitions",
                       "Negative Partitions",
                       "Genome Copies per mL") ~ "continuous"
  )  %>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall()%>%
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  modify_spanning_header(c("stat_4") ~ "**Province**") %>% 
  modify_caption("**Table 1. HAV**") 


table1 %>%
  as_flex_table() %>%
  save_as_docx(path = "~/HAV_table.docx")


#merge the two df 

havcases_vs_water<- full_join(hav_samples, hav_water, by= "epiweek2")

#selecting columns I want

havcases_vs_water <- havcases_vs_water %>% 
  select(epiweek2, n, site_name, site_province, district_name, hav_concentration_copies_u_l, hav_result) %>% 
  filter(epiweek2 != "NAwNA")

havcases_vs_water<- havcases_vs_water %>%
  mutate(tested1 = case_when( (hav_result == "Positive") ~ -0.3, 
                              (hav_result == "Negative") ~ -0.3)) 

havcases_vs_water$epiweek3 <- havcases_vs_water$epiweek2

havcases_vs_water <- havcases_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


havcases_vs_water<-  havcases_vs_water[ #ordering by year first then week
  with(havcases_vs_water, order(year, week)), ]

havcases_vs_water$epiweek2 <- factor(havcases_vs_water$epiweek2, levels = unique(havcases_vs_water$epiweek2), ordered = T)

havcases_vs_water2 <- havcases_vs_water %>% 
  group_by(epiweek2, site_province)%>%
  summarise(sum_genomes = mean(hav_concentration_copies_u_l,na.rm = TRUE),
            .groups = 'keep')

havcases_vs_water3<- full_join(hav_samples, havcases_vs_water2, by= "epiweek2")

havcases_vs_water3$loglevels <- log10(havcases_vs_water3$sum_genomes)

havcases_vs_water3$epiweek3 <- havcases_vs_water3$epiweek2

havcases_vs_water3 <- havcases_vs_water3%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


havcases_vs_water3<-  havcases_vs_water3[ #ordering by year first then week
  with(havcases_vs_water3, order(year, week)), ]

havcases_vs_water3$epiweek2 <- factor(havcases_vs_water3$epiweek2, levels = unique(havcases_vs_water3$epiweek2), ordered = T)


png("~/HAV_prov.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()  

havprovplot <- ggplot(havcases_vs_water3) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",colour="gray")+
  geom_point(aes(x=epiweek2, y= sum_genomes*30, group= site_province,  col = site_province))+
  geom_line(aes(x=epiweek2, y= sum_genomes*30, group= site_province, col = site_province)) +
  scale_y_continuous(sec.axis=sec_axis(~ . /30,name="Mean Genome Copies/mL \n"),
                     #breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  labs(x="\nEpidemiological week",y="HAV Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=9 ),
    axis.text.y = element_text(color="black", size=12 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=12),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

havprovplot

dev.off()
##############################################################################

#HEV
##############################################################################

#Tabulate number of samples we've received 

hev_samples <- hevcases %>%
  group_by(epiweek2) %>%
  count(epiweek2, na.rm=TRUE)

#filter for samples with HAV result 

hev_water <- sacases %>% 
  filter(hev_result != "NA")

#selecting columns I want

hev_water <- hev_water%>% 
  select(epiweek2, site_name, site_province, district_name, 
         hev_concentration_copies_u_l, hev_ci_95_percent, hev_partitions_valid, 
         hev_partitions_positive, hev_partitions_negative,hev_result,) %>% 
  filter(epiweek2 != "NAwNA")

#dPCR back calculation uL to mL 

hev_water$gc_ml <- hev_water$hev_concentration_copies_u_l*12*(50/8)*(1000/200)/70

hev_water <- hev_water %>%
  mutate(gc_ml = na_if(gc_ml, gc_ml < 0))%>%
  mutate(hev_partitions_positive = na_if(hev_partitions_positive, hev_partitions_positive < 0))

hev_water$log_gc_ml <- log10(hev_water$gc_ml)


hev_water2 <- hev_water 

hev_water2$"Province"  <- hev_water2$site_province 
hev_water2$"dPCR Test Result" <- hev_water2$hev_result 
hev_water2$"Valid Partitions"<- hev_water2$hev_partitions_valid  
hev_water2$"Positive Partitions"<-hev_water2$hev_partitions_positive  
hev_water2$"Negative Partitions"<-hev_water2$hev_partitions_negative  
hev_water2$"Genome Copies per mL"<-hev_water2$gc_ml  

#Table 1 

table2 <- hev_water2 %>%
  tbl_summary(include = c( "Province", 
                           "dPCR Test Result",
                           "Valid Partitions",
                           "Positive Partitions",
                           "Negative Partitions",
                           "Genome Copies per mL"),
              by = "Province",# split table by group
              missing = "no", # don't list missing data separately
              #statistic = list(all_continuous() ~ "{mean} ({sd}) "), #{min} {max}
              type = c("Valid Partitions",
                       "Positive Partitions",
                       "Negative Partitions",
                       "Genome Copies per mL") ~ "continuous"
  )  %>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall()%>%
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  modify_spanning_header(c("stat_4") ~ "**Province**") %>% 
  modify_caption("**Table 1. HEV**") 


table2 %>%
  as_flex_table() %>%
  save_as_docx(path = "~/HEV_table.docx")



#merge the two df 

hevcases_vs_water<- full_join(hev_samples, hev_water, by= "epiweek2")

#selecting columns I want

hevcases_vs_water <- hevcases_vs_water %>% 
  select(epiweek2, n, site_name, site_province, district_name, hev_concentration_copies_u_l, hev_result) %>% 
  filter(epiweek2 != "NAwNA")

hevcases_vs_water<- hevcases_vs_water %>%
  mutate(tested1 = case_when( (hev_result == "Positive") ~ -0.3, 
                              (hev_result == "Negative") ~ -0.3)) 

hevcases_vs_water$epiweek3 <- hevcases_vs_water$epiweek2

hevcases_vs_water <- hevcases_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


hevcases_vs_water<-  hevcases_vs_water[ #ordering by year first then week
  with(hevcases_vs_water, order(year, week)), ]

hevcases_vs_water$epiweek2 <- factor(hevcases_vs_water$epiweek2, levels = unique(hevcases_vs_water$epiweek2), ordered = T)

hevcases_vs_water2 <-hevcases_vs_water %>% 
  group_by(epiweek2, site_province)%>%
  summarise(sum_genomes = mean(hev_concentration_copies_u_l,na.rm = TRUE),
            .groups = 'keep')

hevcases_vs_water3<- full_join(hev_samples, hevcases_vs_water2, by= "epiweek2")

hevcases_vs_water3$loglevels <- log10(hevcases_vs_water3$sum_genomes)

hevcases_vs_water3$epiweek3 <- hevcases_vs_water3$epiweek2

hevcases_vs_water3 <- hevcases_vs_water3%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


hevcases_vs_water3<-  hevcases_vs_water3[ #ordering by year first then week
  with(hevcases_vs_water3, order(year, week)), ]

hevcases_vs_water3$epiweek2 <- factor(hevcases_vs_water3$epiweek2, levels = unique(hevcases_vs_water3$epiweek2), ordered = T)


png("~/HEV_prov.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()  

hevprovplot <- ggplot(hevcases_vs_water3) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",colour="gray")+
  geom_point(aes(x=epiweek2, y= sum_genomes*0.5, group= site_province,  col = site_province))+
  geom_line(aes(x=epiweek2, y= sum_genomes*0.5, group= site_province, col = site_province)) +
  scale_y_continuous(sec.axis=sec_axis(~ . /0.5,name="Mean Genome Copies/uL \n"),
                     #breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  labs(x="\nEpidemiological week",y="HEV Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=9 ),
    axis.text.y = element_text(color="black", size=12 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=12),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

hevprovplot

dev.off()
##############################################################################

#Measles
##############################################################################

#Tabulate number of samples we've received 

measles_samples <- measlescases %>%
  group_by(epiweek2) %>%
  count(epiweek2, na.rm=TRUE)

#filter for samples with HAV result 

measles_water <- sacases %>% 
  filter(measles_result != "NA")

#selecting columns I want

measles_water <- measles_water%>% 
  select(epiweek2, site_name, site_province, district_name, 
         measles_concentration_copies_u_l, measles_ci_95_percent, measles_partitions_valid, 
         measles_partitions_positive, measles_partitions_negative,measles_result,) %>% 
  filter(epiweek2 != "NAwNA")

#dPCR back calculation uL to mL 

measles_water$gc_ml <- measles_water$measles_concentration_copies_u_l*12*(50/8)*(1000/200)/70

measles_water <- measles_water %>%
  mutate(gc_ml = na_if(gc_ml, gc_ml < 0))%>%
  mutate(measles_partitions_positive = na_if(measles_partitions_positive, measles_partitions_positive < 0))

measles_water$log_gc_ml <- log10(measles_water$gc_ml)


measles_water2 <- measles_water 

measles_water2$"Province"  <- measles_water2$site_province 
measles_water2$"dPCR Test Result" <- measles_water2$measles_result 
measles_water2$"Valid Partitions"<- measles_water2$measles_partitions_valid  
measles_water2$"Positive Partitions"<-measles_water2$measles_partitions_positive  
measles_water2$"Negative Partitions"<-measles_water2$measles_partitions_negative  
measles_water2$"Genome Copies per mL"<-measles_water2$gc_ml  

#Table 1 


table3 <- measles_water2 %>%
  tbl_summary(include = c( "Province", 
                           "dPCR Test Result",
                           "Valid Partitions",
                           "Genome Copies per mL"),
              by = "Province",# split table by group
              missing = "no", # don't list missing data separately
              #statistic = list(all_continuous() ~ "{mean} ({IQR}) "),
              type = c("Valid Partitions",
                       "Genome Copies per mL") ~ "continuous"
  )  %>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall(
    col_label = "**National**"
  )%>%
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3","stat_4","stat_5","stat_6", "stat_7") ~ "**Province**") %>% 
  modify_caption("**Table 1. Measles**") 

#Save table as word document

table3 %>%
  as_flex_table() %>%
  save_as_docx(path = "~/measles_table.docx")

# save table as .png 

gt::gtsave(as_gt(table3), file = file.path("~/", "measles_table.png"), vwidth = 2500, vheight = 1500)

#merge the two df 

measlescases_vs_water<- full_join(measles_samples, measles_water, by= "epiweek2")

#selecting columns I want

measlescases_vs_water <- measlescases_vs_water %>% 
  select(epiweek2, n, site_name, site_province, district_name, measles_concentration_copies_u_l, measles_result) %>% 
  filter(epiweek2 != "NAwNA")

measlescases_vs_water<- measlescases_vs_water %>%
  mutate(tested1 = case_when( (measles_result == "Positive") ~ -0.3, 
                              (measles_result == "Negative") ~ -0.3)) 

measlescases_vs_water$epiweek3 <- measlescases_vs_water$epiweek2

measlescases_vs_water <- measlescases_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


measlescases_vs_water<-  measlescases_vs_water[ #ordering by year first then week
  with(measlescases_vs_water, order(year, week)), ]

measlescases_vs_water$epiweek2 <- factor(measlescases_vs_water$epiweek2, levels = unique(measlescases_vs_water$epiweek2), ordered = T)

measlescases_vs_water2 <-measlescases_vs_water %>% 
  group_by(epiweek2, site_province)%>%
  summarise(sum_genomes = mean(measles_concentration_copies_u_l,na.rm = TRUE),
            .groups = 'keep')

measlescases_vs_water3<- full_join(hev_samples, measlescases_vs_water2, by= "epiweek2")

measlescases_vs_water3$loglevels <- log10(measlescases_vs_water3$sum_genomes)

measlescases_vs_water3$epiweek3 <- measlescases_vs_water3$epiweek2

measlescases_vs_water3 <- measlescases_vs_water3%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


measlescases_vs_water3<-  measlescases_vs_water3[ #ordering by year first then week
  with(measlescases_vs_water3, order(year, week)), ]

measlescases_vs_water3$epiweek2 <- factor(measlescases_vs_water3$epiweek2, levels = unique(measlescases_vs_water3$epiweek2), ordered = T)


png("~/measles_prov.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()  

measlesprovplot <- ggplot(measlescases_vs_water3) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",colour="gray")+
  geom_point(aes(x=epiweek2, y= sum_genomes*0.75, group= site_province,  col = site_province))+
  geom_line(aes(x=epiweek2, y= sum_genomes*0.75, group= site_province, col = site_province)) +
  scale_y_continuous(sec.axis=sec_axis(~ . /0.75,name="Mean Genome Copies/uL \n"),
                     #breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  labs(x="\nEpidemiological week",y="Measles Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=9 ),
    axis.text.y = element_text(color="black", size=12 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=12),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

measlesprovplot

dev.off()

##############################################################################

#Rubella
##############################################################################
#Tabulate number of samples we've received 

rubella_samples <- rubellascases %>%
  group_by(epiweek2) %>%
  count(epiweek2, na.rm=TRUE)

#filter for samples with HAV result 

rubella_water <- sacases %>% 
  filter(rubella_result != "NA")

#selecting columns I want

rubella_water <- rubella_water%>% 
  select(epiweek2, site_name, site_province, district_name, 
         rubella_concentration_copies_u_l, rubella_ci_95_percent, rubella_partitions_valid, 
         rubella_partitions_positive, rubella_partitions_negative, rubella_result,) %>% 
  filter(epiweek2 != "NAwNA")

#dPCR back calculation uL to mL 

rubella_water$gc_ml <- rubella_water$rubella_concentration_copies_u_l*12*(50/8)*(1000/200)/70

rubella_water <- rubella_water %>%
  mutate(gc_ml = na_if(gc_ml, gc_ml < 0))%>%
  mutate(rubella_partitions_positive = na_if(rubella_partitions_positive, rubella_partitions_positive < 0))

rubella_water$log_gc_ml <- log10(rubella_water$gc_ml)


rubella_water2 <- rubella_water 

rubella_water2$"Province"  <-rubella_water2$site_province 
rubella_water2$"dPCR Test Result" <- rubella_water2$rubella_result 
rubella_water2$"Valid Partitions"<- rubella_water2$rubella_partitions_valid  
rubella_water2$"Positive Partitions"<-rubella_water2$rubella_partitions_positive  
rubella_water2$"Negative Partitions"<-rubella_water2$rubella_partitions_negative  
rubella_water2$"Genome Copies per mL"<-rubella_water2$gc_ml  

#Table 1 

table4 <- rubella_water2 %>%
  tbl_summary(include = c( "Province", 
                           "dPCR Test Result",
                           "Valid Partitions",
                           "Genome Copies per mL"),
              by = "Province",# split table by group
              missing = "no", # don't list missing data separately
              #statistic = list(all_continuous() ~ "{mean} ({IQR}) "),
              type = c("Valid Partitions",
                       "Genome Copies per mL") ~ "continuous"
  )  %>%
  add_n() %>% # add column with total number of non-missing observations
  add_overall(
    col_label = "**National**"
  )%>%
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3","stat_4","stat_5","stat_6", "stat_7") ~ "**Province**") %>% 
  modify_caption("**Table 1. Rubella**") 

#Save table as word document

table4 %>%
  as_flex_table() %>%
  save_as_docx(path = "~/rubella_table.docx")

# save table as .png 

gt::gtsave(as_gt(table4), file = file.path("~/", "rubella_table.png"), vwidth = 2500, vheight = 1500)



#merge the two df 

rubellacases_vs_water<- full_join(rubella_samples, rubella_water, by= "epiweek2")

#selecting columns I want

rubellacases_vs_water <- rubellacases_vs_water %>% 
  select(epiweek2, n, site_name, site_province, district_name, rubella_concentration_copies_u_l, rubella_result) %>% 
  filter(epiweek2 != "NAwNA")

rubellacases_vs_water<- rubellacases_vs_water %>%
  mutate(tested1 = case_when( (rubella_result == "Positive") ~ -0.3, 
                              (rubella_result == "Negative") ~ -0.3)) 

rubellacases_vs_water$epiweek3 <- rubellacases_vs_water$epiweek2

rubellacases_vs_water <- rubellacases_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


rubellacases_vs_water<-  rubellacases_vs_water[ #ordering by year first then week
  with(rubellacases_vs_water, order(year, week)), ]

rubellacases_vs_water$epiweek2 <- factor(rubellacases_vs_water$epiweek2, levels = unique(rubellacases_vs_water$epiweek2), ordered = T)

rubellacases_vs_water2 <-rubellacases_vs_water %>% 
  group_by(epiweek2, site_province)%>%
  summarise(sum_genomes = mean(rubella_concentration_copies_u_l,na.rm = TRUE),
            .groups = 'keep')

rubellacases_vs_water3<- full_join(hev_samples, rubellacases_vs_water2, by= "epiweek2")

rubellacases_vs_water3$loglevels <- log10(rubellacases_vs_water3$sum_genomes)

rubellacases_vs_water3$epiweek3 <- rubellacases_vs_water3$epiweek2

rubellacases_vs_water3 <- rubellacases_vs_water3%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 


rubellacases_vs_water3<-  rubellacases_vs_water3[ #ordering by year first then week
  with(rubellacases_vs_water3, order(year, week)), ]

rubellacases_vs_water3$epiweek2 <- factor(rubellacases_vs_water3$epiweek2, levels = unique(rubellacases_vs_water3$epiweek2), ordered = T)


png("~/rubella_prov.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()  

rubellaprovplot <- ggplot(rubellacases_vs_water3) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",colour="gray")+
  geom_point(aes(x=epiweek2, y= sum_genomes*0.75, group= site_province,  col = site_province))+
  geom_line(aes(x=epiweek2, y= sum_genomes*0.75, group= site_province, col = site_province)) +
  scale_y_continuous(sec.axis=sec_axis(~ . /0.75,name="Mean Genome Copies/uL \n"),
                     #breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  labs(x="\nEpidemiological week",y="Rubella Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=9 ),
    axis.text.y = element_text(color="black", size=12 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=12),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

rubellaprovplot

dev.off()
