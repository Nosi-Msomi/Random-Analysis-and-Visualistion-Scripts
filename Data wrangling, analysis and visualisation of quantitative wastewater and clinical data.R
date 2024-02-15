#This documentation provides information on how to generate the quantitative levels

#A useful resource to get started with R https://www.w3schools.com/r/r_get_started.asp

#This script was generated using R 4.2.2

#For instructions in downloading and installing R

#The following librabries are required in order to be able to run the script

#Routine Reporting 
library (ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(rlang)
library(writexl)


#note: if the change n so that current epiweek displayed, n will be either 2 or 3; scale_x_discrete(breaks = every_nth(n = 2)) 

##################################################################################
#preparing clinical case data 

cov211<- read_xlsx("~/covid_21_1.xlsx")
cov212<- read_xlsx("~/covid_21_2.xlsx")
cov213<- read_xlsx("~/covid_21_3.xlsx")
cov214<- read_xlsx("~/covid_21_4.xlsx")
cov215<- read_xlsx("~/covid_21_5.xlsx")
cov216<- read_xlsx("~/covid_21_6.xlsx")
cov217<- read_xlsx("~/covid_21_7.xlsx")
cov218<- read_xlsx("~/covid_21_8.xlsx")
cov219<- read_xlsx("~/covid_21_9.xlsx")
cov2110<- read_xlsx("~/covid_21_10.xlsx")
cov2111<- read_xlsx("~/covid_21_11.xlsx")
cov221<- read_xlsx("~/covid_22_1.xlsx")
cov222<- read_xlsx("~/covid_22_2.xlsx")
cov223<- read_xlsx("~/covid_22_3.xlsx")
cov23<- read_xlsx("~/covid_23.xlsx")
cov24<- read_xlsx("~/covid_24.xlsx")

covcases <- bind_rows(cov211, cov212, cov213, cov214, cov215, cov216, cov217, cov218, cov219, 
                      cov2110, cov2111, cov221, cov222,cov223, cov23, cov24)


#View(cases)

#counting Districts in this dataset 

#table(covcases$District)

#filtering the data by Districts our wwtp are in 

covcases <- covcases %>%
  filter(District == "GP CITY OF JOHANNESBURG METRO" | District == "City of Johannesburg Metro" |
           District == "FS MANGAUNG METRO" | District == "Mangaung Metro" |
           District == "EC NELSON MANDELA BAY METRO" | District == "eThekwini Metro" |
           District == "GP CITY OF TSHWANE METRO" | District == "City of Tshwane Metro" |
           District == "GP EKURHULENI METRO" | District == "Ekurhuleni Metro" |
           District == "KZN ETHEKWINI METRO" | District == "eThekwini Metro" |
           District == "WC CITY OF CAPE TOWN METRO" | District == "City of Cape Town Metro" |
           District == "EC BUFFALO CITY METRO"| District == "Buffalo City Metro" |
           District == "NW BOJANALA PLATINUM"| District == "Bojanala Platinum" |
           District == "NW DR KENNETH KAUNDA"| District ==  "Dr Kenneth Kaunda" )


#Setting up epiweeks for x-axis

covcases <- covcases %>% 
  filter(Diagnosis_Method == "Laboratory confirmed")
covcases$newcoldate <- format(as.Date(covcases$Notification_Date, format = "%Y/%m/%d"), "%Y-%m-%d") #changing the format of the date from y/m/d to ymd 
covcases$epiweek <- lubridate::epiweek(ymd( covcases$newcoldate)) #generate epiweek
covcases$year <- strftime(covcases$newcoldate, "%Y") #Creating year column  
covcases$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
covcases$epiweek2 <- do.call(paste, c(covcases[my_cols],sep ="")) #created new variable using concat columns

cases1 <- covcases

#filtering by individual Districts 

joburg_cases <- cases1 %>%
  filter(District == "GP CITY OF JOHANNESBURG METRO" | District == "City of Johannesburg Metro")

mangaung_cases <- cases1 %>%
  filter(District == "FS MANGAUNG METRO" | District == "Mangaung Metro")

nelson_cases <- cases1 %>%
  filter(District == "EC NELSON MANDELA BAY METRO" | District == "eThekwini Metro" )

tshwane_cases <- cases1 %>%
  filter( District == "GP CITY OF TSHWANE METRO" | District == "City of Tshwane Metro")

ekurhuleni_cases <- cases1 %>%
  filter(District == "GP EKURHULENI METRO" | District == "Ekurhuleni Metro" )

ethekwini_cases <- cases1 %>%
  filter(District == "KZN ETHEKWINI METRO" | District == "eThekwini Metro")

capetown_cases <- cases1 %>%
  filter(District == "WC CITY OF CAPE TOWN METRO" | District == "City of Cape Town Metro" )

buffalo_cases <- cases1 %>%
  filter(District == "EC BUFFALO CITY METRO"| District == "Buffalo City Metro" )

bojanala_cases <- cases1 %>%
  filter(District == "NW BOJANALA PLATINUM"| District == "Bojanala Platinum" )

jb_marks_cases <- cases1 %>%
  filter(District == "NW DR KENNETH KAUNDA"| District ==  "Dr Kenneth Kaunda" )

################################################################################



###############################################################################
#Preparing wastewater levels           

water <- read.csv("~/Book1.csv")

water$sam_col_date <- as.Date(water$Sample.Collection.Date)
water$Date.Processed. <- as.Date(water$Date.Processed.)
water$Date.tested.at.laboratory <- as.Date(water$Date.tested.at.laboratory)
water$Extraction.date <- as.Date(water$Extraction.date)
water$Date.tested.at.laboratory. <- as.Date(water$Date.tested.at.laboratory.)
water$PCR.date <- as.Date(water$PCR.date)
water$Tapestation.date <- as.Date(water$Tapestation.date)
water$Date.received.from.sequencing <- as.Date(water$Date.received.from.sequencing)
water$Date.submitted.for.Sequencing <- as.Date(water$Date.submitted.for.Sequencing)
water$Report.date <- as.Date(water$Report.date)



###############################################################################
#Setting up epiweeks for x-axis 

water1 <- water 
water1 <- water1 %>% arrange(ymd(water1$sam_col_date))
water1$epiweek <- lubridate::epiweek(ymd(water1$sam_col_date)) #generate epiweek
water1$year <- strftime(water1$sam_col_date, "%Y") #Creating year column  
water1$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
water1$epiweek2 <- do.call(paste, c(water1[my_cols],sep ="")) #created new variable using concat columns

#setting up columns
water1$levels <- water1$Genome.copies.mL...N.gene..
water1$Date <- water1$sam_col_date
water1$Result <-water1$SARS.CoV.2.PCR.result
water1 <- water1 %>%
  mutate(levels = na_if(levels, levels < 0))
water1 <- water1 %>%
  mutate(levels = if_else(levels<2.34, 2.34, levels)) #replace lower than 2.34 with 2.34 (limit of quantification)
water1$loglevels <- log10(water1$levels)

#correcting which type of PCR was used

water1 <- water1 %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) 



###############################################################################
#plotting bar graph and line graph on same plot and two y-axis 

#South Africa 

#Tabulate number of samples we've received 

rsa_samples <- cases1 %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

rsa_water <- water1 %>% 
  filter( Site.Name. == "Goudkoppies Wastewater Treatment Works" |
            Site.Name. == "Northern Wastewater Treatment Works"| 
            Site.Name. == "Rooiwal Wastewater Treatment Works" |
            Site.Name. == "Daspoort Wastewater Treatment Works" |
            Site.Name. == "ERWAT Vlakplaat Wastewater Treatment Works" |
            Site.Name. == "Hartebeesfontein Waterworks" |
            Site.Name. == "Zandvleit Wastewater Treatment Works" |
            Site.Name. == "Borcheds Quarry Wastewater Treatment Works" |
            Site.Name. == "East Bank Wastewater Treatment Works " |
            Site.Name. == "Mdantsane Wastewater Treatment Works"|
            Site.Name. == "Brickfield Pre-treatment Works  " |
            Site.Name. == "Kwanobuhle Wastewater Treatment Works"|
            Site.Name. == "Sterkwater Wastewater Treatment Works " |
            Site.Name. == "Bloemspruit Wastewater Treatment Works"| 
            Site.Name. == "Central Wastewater Treatment Works " |
            Site.Name. == "Northern Wastewater Treatment Works"|
            Site.Name. == "Mahikeng Water Treatment Works"|
            Site.Name. == "Mmabatho Water Treatment Work"|
            Site.Name. == "Rustenburg Wastewater Treatment Works")

rsacopies <- rsa_water %>% 
  group_by(epiweek2)%>%
  summarise(sum_genomes = sum(levels,na.rm = TRUE),
            .groups = 'keep')

rsacopies["sum_genomes"][rsacopies["sum_genomes"] == 0] <- NA 

rsacases_vs_water<- full_join(rsa_samples, rsa_water, by= "epiweek2")
rsacases_vs_water<- full_join(rsacases_vs_water, rsacopies, by= "epiweek2")

#repeating this just for weeks where no ww samples but clinical cases- otherwise would be blank

rsacases_vs_water <- rsacases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(PCR = if_else(is.na(PCR), "qPCR", PCR)) 


rsacases_vs_water$final_result <- rsacases_vs_water$SARS.CoV.2.PCR.result

rsacases_vs_water <- rsacases_vs_water %>%
  select(epiweek2, n, sum_genomes, Date, PCR)

rsacases_vs_water$loglevels <- log10(rsacases_vs_water$sum_genomes)

rsacases_vs_water<- rsacases_vs_water %>%
  mutate(tested = case_when( (sum_genomes > 0) ~ -0.3)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


rsacases_vs_water$epiweek3 <- rsacases_vs_water$epiweek2

rsacases_vs_water <- rsacases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

rsacases_vs_water <- rsacases_vs_water %>%
  filter(year != 2020)


rsacases_vs_water <-  rsacases_vs_water[ #ordering by year first then week
  with(rsacases_vs_water, order(year, week)),
]

rsacases_vs_water$epiweek2 <- factor(rsacases_vs_water$epiweek2, levels = unique(rsacases_vs_water$epiweek2), ordered = T)

rsacases_vs_water$Country <- "South African SARS-CoV-2 Wastewater Levels"

rsacases_vs_water$Date <- "w"

#remove duplicated rows 

rsacases_vs_water <- rsacases_vs_water %>% distinct()

rsacases_vs_water$end <- lubridate::parse_date_time(paste(rsacases_vs_water$year, rsacases_vs_water$week,0, sep="-"),'Y-W-w') + days(6)
#lubridate::parse_date_time(year, week, 0= week start on sunday, sep = formwat you want)
#this gives start of epiweek so add 6 days to get end of epiweek

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

png("~/overall_rsa.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows() 
#rsaplot <- ggplot(rsacases_vs_water[!is.na(rsacases_vs_water$loglevels),]) +
rsaplot <- ggplot(rsacases_vs_water) +
  geom_point(aes(x=epiweek2, y=tested * 10000, group = 1, col = "Sample Collection"), stat="identity", size=1, shape = 15)+
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=loglevels *10000, 
                 group=Country, col=Country),stat="identity", size=2)+
  geom_line(aes(x=epiweek2, y=loglevels *10000,  
                group=Country, col=Country),stat="identity", size=1) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_colour_manual(values = c("Sample Collection" = "darkblue", "South African SARS-CoV-2 Wastewater Levels" = "#009ADE")) +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16),
                                                  linetype = c(0, 1)) )) + 
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free_x")+
  ggthemes::theme_tufte()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=12 ),
    axis.text.y = element_text(color="black", size=12 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=12),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(size = 12) )

rsaplot

dev.off()

##############################################################################

#COJ

#Tabulate number of samples we've received 

jhb_samples <- joburg_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for jhb www 

jhb_water <- water1 %>% 
  filter(District.Name == "Johannesburg MM") %>%
  filter( Site.Name. == "Goudkoppies Wastewater Treatment Works" |
            Site.Name. == "Northern Wastewater Treatment Works (GP)")

#merge the two df 

jhbcases_vs_water<- full_join(jhb_samples, jhb_water, by= "epiweek2")
jhbcases_vs_water$final_result <- jhbcases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want


jhbcases_vs_water <- jhbcases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province., District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA")


jhbcases_vs_water<- jhbcases_vs_water %>%
  mutate(tested1 = case_when( (Site.Name. == "Goudkoppies Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Goudkoppies Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Northern Wastewater Treatment Works (GP)" & final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Northern Wastewater Treatment Works (GP)" & final_result == "Negative") ~ -0.1))


jhbcases_vs_water$epiweek3 <- jhbcases_vs_water$epiweek2

jhbcases_vs_water <- jhbcases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

jhbcases_vs_water <- jhbcases_vs_water %>%
  filter(year != 2020)


jhbcases_vs_water <-  jhbcases_vs_water[ #ordering by year first then week
  with(jhbcases_vs_water, order(year, week)),
]

jhbcases_vs_water$epiweek2 <- factor(jhbcases_vs_water$epiweek2, levels = unique(jhbcases_vs_water$epiweek2), ordered = T)

jhbcases_vs_water <- jhbcases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(is.na(PCR), "qPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

png("~/Johannesburg.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()  

#jhbplot <- ggplot(jhbcases_vs_water[!is.na(jhbcases_vs_water$loglevels),] ) + #this ignores na in y allowing graph points to connect
jhbplot <- ggplot(jhbcases_vs_water) + #this ignores na in y allowing graph points to connect
  
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " Goudkoppies Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Northern Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000, group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000, group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  
  scale_colour_manual(values = c(" Goudkoppies Sample Collection" = "darkred", "Northern Sample Collection" = "darkblue", 
                                 "Goudkoppies Wastewater Treatment Works" = "#FC4E2A","Northern Wastewater Treatment Works (GP)" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  scale_x_discrete(breaks = every_nth(n=5)) +
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

jhbplot

dev.off()

###########################################################################

#Tshwane


#Tabulate number of samples we've received 

tshwane_samples <- tshwane_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

tshwane_water <- water1 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Rooiwal Wastewater Treatment Works" |
            Site.Name. == "Daspoort Wastewater Treatment Works")

#merge the two df 

tshwanecases_vs_water<- full_join(tshwane_samples, tshwane_water, by= "epiweek2")
tshwanecases_vs_water$final_result <- tshwanecases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

tshwanecases_vs_water <- tshwanecases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province., District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA")


tshwanecases_vs_water<- tshwanecases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "Rooiwal Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Rooiwal Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


tshwanecases_vs_water$epiweek3 <- tshwanecases_vs_water$epiweek2

tshwanecases_vs_water <- tshwanecases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer))  %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

tshwanecases_vs_water<- tshwanecases_vs_water %>%
  filter(year != 2020)


tshwanecases_vs_water <-  tshwanecases_vs_water[ #ordering by year first then week
  with(tshwanecases_vs_water, order(year, week)),
]

tshwanecases_vs_water$epiweek2 <- factor(tshwanecases_vs_water$epiweek2, levels = unique(tshwanecases_vs_water$epiweek2), ordered = T)

tshwanecases_vs_water <- tshwanecases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(is.na(PCR), "qPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#plot 

png("~/tshwane.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
#tshwaneplot <- ggplot(tshwanecases_vs_water[!is.na(tshwanecases_vs_water$loglevels),] ) +
tshwaneplot <- ggplot(tshwanecases_vs_water) +
  
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " Rooiwal Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Daspoort Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Rooiwal Sample Collection" = "darkred", "Daspoort Sample Collection" = "darkblue", 
                                 "Rooiwal Wastewater Treatment Works" = "#FC4E2A","Daspoort Wastewater Treatment Works" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

tshwaneplot

dev.off()

#############################################################################

# Ekurhuleni

#Tabulate number of samples we've received 

ekurhuleni_samples <- ekurhuleni_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

ekurhuleni_water <- water1 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "ERWAT Vlakplaat Wastewater Treatment Works" |
            Site.Name. == "Hartebeesfontein Waterworks")

#merge the two df 

ekurhulencases_vs_water<- full_join(ekurhuleni_samples, ekurhuleni_water, by= "epiweek2")
ekurhulencases_vs_water$final_result <- ekurhulencases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

ekurhulencases_vs_water<- ekurhulencases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province., District.Name, Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


ekurhulencases_vs_water<- ekurhulencases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "ERWAT Vlakplaat Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "ERWAT Vlakplaat Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Hartebeesfontein Waterworks" & final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Hartebeesfontein Waterworks" & final_result == "Negative") ~ -0.1))


ekurhulencases_vs_water$epiweek3 <- ekurhulencases_vs_water$epiweek2

ekurhulencases_vs_water <- ekurhulencases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

ekurhulencases_vs_water<- ekurhulencases_vs_water %>%
  filter(year != 2020)


ekurhulencases_vs_water <-  ekurhulencases_vs_water[ #ordering by year first then week
  with(ekurhulencases_vs_water, order(year, week)),
]

ekurhulencases_vs_water$epiweek2 <- factor(ekurhulencases_vs_water$epiweek2, levels = unique(ekurhulencases_vs_water$epiweek2), ordered = T)

ekurhulencases_vs_water <- ekurhulencases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#plot 


png("~/ekurhuleni.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
#ekurhuleniplot <- ggplot(ekurhulencases_vs_water[!is.na(ekurhulencases_vs_water$loglevels),]) +
ekurhuleniplot <- ggplot(ekurhulencases_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = "ERWAT Vlakplaat Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Hartebeesfontein Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_x_discrete(breaks = every_nth(n = 5 )) +
  scale_colour_manual(values = c("ERWAT Vlakplaat Sample Collection" = "darkred", "Hartebeesfontein Sample Collection" = "darkblue", 
                                 "ERWAT Vlakplaat Wastewater Treatment Works" = "#FC4E2A","Hartebeesfontein Waterworks" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

ekurhuleniplot

dev.off()

################################################################################

#eThekwini

#Tabulate number of samples we've received 

ethekwini_samples <- ethekwini_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

ethekwini_water <- water1 %>% 
  filter(District.Name == "Ethekwini MM") 



#merge the two df 

ethekwinicases_vs_water<- full_join(ethekwini_samples, ethekwini_water, by= "epiweek2")
ethekwinicases_vs_water$final_result <- ethekwinicases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

ethekwinicases_vs_water<- ethekwinicases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province., District.Name, Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


ethekwinicases_vs_water<- ethekwinicases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "Central Wastewater Treatment Works (KZN)" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Central Wastewater Treatment Works (KZN)" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Northern Wastewater Treatment Works (KZN)"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Northern Wastewater Treatment Works (KZN)" & final_result == "Negative") ~ -0.1))


ethekwinicases_vs_water$epiweek3 <- ethekwinicases_vs_water$epiweek2

ethekwinicases_vs_water <- ethekwinicases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

ethekwinicases_vs_water<- ethekwinicases_vs_water%>%
  filter(year != 2020)


ethekwinicases_vs_water <-  ethekwinicases_vs_water[ #ordering by year first then week
  with(ethekwinicases_vs_water, order(year, week)),
]

ethekwinicases_vs_water$epiweek2 <- factor(ethekwinicases_vs_water$epiweek2, levels = unique(ethekwinicases_vs_water$epiweek2), ordered = T)

ethekwinicases_vs_water <- ethekwinicases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#plot 


png("~/ethekwini.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
ethekwiniplot <- ggplot(ethekwinicases_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = "Northern Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Central Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 2),
                     labels = label_comma()) + 
  scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_colour_manual(values = c("Northern Sample Collection" = "darkred", "Central Sample Collection" = "darkblue", 
                                 "Northern Wastewater Treatment Works (KZN)" = "#FC4E2A" ,"Central Wastewater Treatment Works (KZN)" = "#009ADE")) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 15, 16, 15),
                                                  linetype = c(1, 0, 1, 0)) )) + 
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

ethekwiniplot

dev.off()

################################################################################

#Mangaung

#Tabulate number of samples we've received 

mangaung_samples <- mangaung_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

mangaung_water <- water1 %>% 
  filter(District.Name == "Mangaung MM") 


mangaungcases_vs_water<- full_join(mangaung_samples, mangaung_water, by= "epiweek2")
mangaungcases_vs_water$final_result <- mangaungcases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

mangaungcases_vs_water<- mangaungcases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province.,District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


mangaungcases_vs_water<- mangaungcases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "Bloemspruit Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Bloemspruit Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Sterkwater Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Sterkwater Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


mangaungcases_vs_water$epiweek3 <- mangaungcases_vs_water$epiweek2

mangaungcases_vs_water<- mangaungcases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

mangaungcases_vs_water<- mangaungcases_vs_water%>%
  filter(year != 2020)


mangaungcases_vs_water<-  mangaungcases_vs_water[ #ordering by year first then week
  with(mangaungcases_vs_water, order(year, week)),
]

mangaungcases_vs_water$epiweek2 <- factor(mangaungcases_vs_water$epiweek2, levels = unique(mangaungcases_vs_water$epiweek2), ordered = T)

mangaungcases_vs_water <- mangaungcases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#plot 


png("~/mangaung.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
mangaungplot <- ggplot(mangaungcases_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " Bloemspruit Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Sterkwater Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Bloemspruit Sample Collection" = "darkred", "Sterkwater Sample Collection" = "darkblue", 
                                 "Bloemspruit Wastewater Treatment Works" = "#FC4E2A","Sterkwater Wastewater Treatment Works" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = ),
                     labels = label_comma()) + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

mangaungplot

dev.off()
###############################################################################

#Nelson Mandela 

#Tabulate number of samples we've received 

nelson_samples <- nelson_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

nelson_water <- water1 %>% 
  filter(District.Name == "Nelson Mandela Bay MM") 


#merge the two df 

nelsoncases_vs_water<- full_join(nelson_samples, nelson_water, by= "epiweek2")
nelsoncases_vs_water$final_result <- nelsoncases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

nelsoncases_vs_water<- nelsoncases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province.,District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


nelsoncases_vs_water<- nelsoncases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "Brickfield Pre-treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Brickfield Pre-treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Kwanobuhle Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Kwanobuhle Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


nelsoncases_vs_water$epiweek3 <- nelsoncases_vs_water$epiweek2

nelsoncases_vs_water<- nelsoncases_vs_water %>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

nelsoncases_vs_water<- nelsoncases_vs_water%>%
  filter(year != 2020)


nelsoncases_vs_water<-  nelsoncases_vs_water[ #ordering by year first then week
  with(nelsoncases_vs_water, order(year, week)),
]

nelsoncases_vs_water$epiweek2 <- factor(nelsoncases_vs_water$epiweek2, levels = unique(nelsoncases_vs_water$epiweek2), ordered = T)

nelsoncases_vs_water <- nelsoncases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
#plot 

png("~/nelson.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
nelsonplot <- ggplot(nelsoncases_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " Brickfield Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Kwanobuhle Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Brickfield Sample Collection" = "darkred", "Kwanobuhle Sample Collection" = "darkblue", 
                                 "Brickfield Pre-treatment Works" = "#FC4E2A","Kwanobuhle Wastewater Treatment Works" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  scale_x_discrete(breaks = every_nth(n = 2)) +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

nelsonplot

dev.off()

################################################################################

#Buffalo City 

#Tabulate number of samples we've received 

buffalo_samples <- buffalo_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

buffalo_water <- water1 %>% 
  filter(District.Name == "Buffalo City MM") 

#merge the two df 

buffalocases_vs_water<- full_join(buffalo_samples, buffalo_water, by= "epiweek2")
buffalocases_vs_water$final_result <- buffalocases_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

buffalocases_vs_water<- buffalocases_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province.,District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


buffalocases_vs_water<- buffalocases_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "East Bank Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "East Bank Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Mdantsane Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Mdantsane Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


buffalocases_vs_water$epiweek3 <- buffalocases_vs_water$epiweek2

buffalocases_vs_water<- buffalocases_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

buffalocases_vs_water<- buffalocases_vs_water%>%
  filter(year != 2020)


buffalocases_vs_water<-  buffalocases_vs_water[ #ordering by year first then week
  with(buffalocases_vs_water, order(year, week)),
]

buffalocases_vs_water$epiweek2 <- factor(buffalocases_vs_water$epiweek2, levels = unique(buffalocases_vs_water$epiweek2), ordered = T)

buffalocases_vs_water <- buffalocases_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
#plot 


png("~/buffalo.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
buffaloplot <- ggplot(buffalocases_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " East Bank Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Mdantsane Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" East Bank Sample Collection" = "darkred", "Mdantsane Sample Collection" = "darkblue", 
                                 "East Bank Wastewater Treatment Works" = "#FC4E2A","Mdantsane Wastewater Treatment Works" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  scale_x_discrete(breaks = every_nth(n = 23)) +
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

buffaloplot
dev.off()
###############################################################################

#Cape Town

#Tabulate number of samples we've received 

capetown_samples <-capetown_cases %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#filter for wwtp 

capetown_water <- water1 %>% 
  filter(District.Name == "Cape Town MM") 


#merge the two df 


capetown_vs_water<- full_join(capetown_samples, capetown_water, by= "epiweek2")
capetown_vs_water$final_result <- capetown_vs_water$SARS.CoV.2.PCR.result


#selecting columns I want

capetown_vs_water<- capetown_vs_water %>% 
  select(epiweek2, n, Site.Name., 
         Site.Province.,District.Name,Genome.copies.mL...N.gene.., levels, loglevels, Date, final_result, PCR) %>% 
  filter(epiweek2 != "NAwNA") %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0


capetown_vs_water<- capetown_vs_water%>%
  mutate(tested1 = case_when( (Site.Name. == "Borcheds Quarry Wastewater Treatment Works" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Borcheds Quarry Wastewater Treatment Works" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Zandvleit Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Zandvleit Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


capetown_vs_water$epiweek3 <- capetown_vs_water$epiweek2

capetown_vs_water<- capetown_vs_water%>%
  separate(epiweek3, sep = "w", into = c("year", "week")) %>%
  mutate(across(c("year", "week"), as.integer)) 

capetown_vs_water<- capetown_vs_water%>%
  filter(year != 2020)


capetown_vs_water<-  capetown_vs_water[ #ordering by year first then week
  with(capetown_vs_water, order(year, week)),
]

capetown_vs_water$epiweek2 <- factor(capetown_vs_water$epiweek2, levels = unique(capetown_vs_water$epiweek2), ordered = T)

capetown_vs_water <- capetown_vs_water %>%
  mutate(PCR = if_else(year == 2021, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2022, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week < 30, "qPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2023 & week > 30, "dPCR", PCR)) %>%
  mutate(PCR = if_else(year == 2024, "dPCR", PCR)) %>%
  mutate(n = if_else(is.na(n), 0, n)) #replace na with 0

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
#plot 

png("~/capetown.png", 
    width = 5*950,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
capetownplot <- ggplot(capetown_vs_water) +
  geom_bar(aes(x=epiweek2, y=n), stat="identity", fill="gray",
           colour="gray")+
  geom_point(aes(x=epiweek2, y=tested1 * 10000, group = 1, col = " Borcheds Quarry Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=tested2 * 10000, group = 1, col = "Zandvleit Sample Collection"),stat="identity", size=1, shape = 15)+
  geom_point(aes(x=epiweek2, y=loglevels *10000,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2, )+
  geom_line(aes(x=epiweek2, y=loglevels * 10000,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Borcheds Quarry Sample Collection" = "darkred", "Zandvleit Sample Collection" = "darkblue", 
                                 "Borcheds Quarry Wastewater Treatment Works" = "#FC4E2A","Zandvleit Wastewater Treatment Works" =  "#009ADE" )) +
  scale_y_continuous(sec.axis=sec_axis(~ . /10000,name="Log Genome Copies/ml (N Gene)\n"),
                     breaks = scales::pretty_breaks(n = 4),
                     labels = label_comma()) + 
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  scale_x_discrete(breaks = every_nth(n = 19)) +
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  labs(x="\nEpidemiological week",y="Laboratory confirmed cases\n")+
  ggthemes::theme_tufte()+
  theme(
    #axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=16 ),
    axis.text.y = element_text(color="black", size=16 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=16),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12))

capetownplot

dev.off()

################################################################################
###############################################################################


#merging all province into 1 dataframe

capetown_vs_water$epiweek2<-as.character(capetown_vs_water$epiweek2)
buffalocases_vs_water$epiweek2<-as.character(buffalocases_vs_water$epiweek2)
nelsoncases_vs_water$epiweek2<-as.character(nelsoncases_vs_water$epiweek2)
mangaungcases_vs_water$epiweek2<-as.character(mangaungcases_vs_water$epiweek2)
ethekwinicases_vs_water$epiweek2<-as.character(ethekwinicases_vs_water$epiweek2)
ekurhulencases_vs_water$epiweek2<-as.character(ekurhulencases_vs_water$epiweek2)
tshwanecases_vs_water$epiweek2<-as.character(tshwanecases_vs_water$epiweek2)
jhbcases_vs_water$epiweek2<-as.character(jhbcases_vs_water$epiweek2)

provincial <- bind_rows(capetown_vs_water, buffalocases_vs_water, nelsoncases_vs_water,
                        mangaungcases_vs_water, ethekwinicases_vs_water,ekurhulencases_vs_water,
                        tshwanecases_vs_water,jhbcases_vs_water) #bind rows is from tidyverse and joins df one under the other

provincial$Site <-provincial$Site.Name.
provincial$Province <-provincial$Site.Province.
provincial$District <-provincial$District.Name
provincial$GC_per_ml <-provincial$Genome.copies.mL...N.gene..

provincial <- provincial %>%
  select(epiweek2, n, Site, Province, District,GC_per_ml, 
         levels, loglevels, Date, year)

write.csv(provincial,"~/provincial_cases_vs_levels.csv",row.names=FALSE)


################################################################################
################################################################################
################################################################################ 

#catchment sites 


#Tabulate number of samples we've received 

water2 <- water1 %>% 
  filter(epiweek2 == "2023w14" | epiweek2 == "2023w15" | epiweek2 == "2023w16" | 
           epiweek2 == "2023w17" | epiweek2 == "2023w18" | epiweek2 == "2023w19" | 
           epiweek2 == "2023w20" | 
           epiweek2 == "2023w21" |  epiweek2 == "2023w22"|  epiweek2 == "2023w23"|
           epiweek2 == "2023w24"| epiweek2 == "2023w25"|  epiweek2 == "2023w26" |
           epiweek2 == "2023w27" | epiweek2 == "2023w28" | epiweek2 == "2023w29"| 
           epiweek2 == "2023w29" |epiweek2 == "2023w30"| epiweek2 == "2023w31" |
           epiweek2 == "2023w32" |epiweek2 == "2023w33"|epiweek2 == "2023w34"| 
           epiweek2 == "2023w35"|epiweek2 == "2023w36"|epiweek2 == "2023w37"|
           epiweek2 == "2023w38"|epiweek2 == "2023w39"| epiweek2 == "2023w40"|
           epiweek2 == "2023w41"| epiweek2 == "2023w42"|epiweek2 == "2023w43"| 
           epiweek2 == "2023w44"| epiweek2 == "2023w45"| epiweek2 == "2023w46"|
           epiweek2 == "2023w47"| epiweek2 == "2023w48"| epiweek2 == "2023w49"| 
           epiweek2 == "2023w50"| epiweek2 == "2023w51"| epiweek2 == "2023w52"|
           year == 2024)

water2$Date <- as.Date(water2$Date)

water2 <- water2 %>%
  mutate(PCR = if_else(is.na(PCR), "qPCR", PCR))


#filter for jhb catchment sites  


###############################################################################
Bara_mot <- water2 %>% 
  filter(District.Name == "Johannesburg MM") %>%
  filter( Site.Name. == "Chris Hani Baragwanath Hospital"| 
            Site.Name. == "Motsoaledi")

Bara_mot$final_result <- Bara_mot$SARS.CoV.2.PCR.result


Bara_mot<- Bara_mot%>%
  mutate(tested1 = case_when( (Site.Name. == "Chris Hani Baragwanath Hospital" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Chris Hani Baragwanath Hospital" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Motsoaledi"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Motsoaledi" & final_result == "Negative") ~ -0.1))


png("~/Bara_Motsoaledi.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
baraplot <- ggplot(Bara_mot[!is.na(Bara_mot$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Chris Hani Baragwanath Hospital Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Motsoaledi Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Chris Hani Baragwanath Hospital Sample Collection" = "darkred", "Motsoaledi Sample Collection" = "darkblue", 
                                 "Chris Hani Baragwanath Hospital" = "#FC4E2A","Motsoaledi" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 16, 15),
                                                  linetype = c(0, 1,1, 0)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 

baraplot

dev.off()

##############################################################################

Klip <- water2 %>% 
  filter(District.Name == "Johannesburg MM") %>%
  filter( Site.Name. == "Klipspruit Veggies (After tunnel)"| 
            Site.Name. == "Klipsruit Yard (Before tunnel)")

Klip$final_result <- Klip$SARS.CoV.2.PCR.result


Klip<- Klip%>%
  mutate(tested1 = case_when( (Site.Name. == "Klipsruit Yard (Before tunnel)" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Klipsruit Yard (Before tunnel)" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Klipspruit Veggies (After tunnel)" & final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Klipspruit Veggies (After tunnel)" & final_result == "Negative") ~ -0.1))


png("~/Klipspruit Before_After tunnel.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Klipplot <- ggplot(Klip[!is.na(Klip$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Klipsruit Yard Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Klipspruit Veggies Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Klipsruit Yard Sample Collection" = "darkred", "Klipspruit Veggies Sample Collection" = "darkblue", 
                                 "Klipsruit Yard (Before tunnel)" = "#FC4E2A","Klipspruit Veggies (After tunnel)" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 


Klipplot

dev.off()

###############################################################################
Bush <- water2 %>% 
  filter(District.Name == "Johannesburg MM") %>%
  filter( Site.Name. == "Bushkoppies Inlet" |
            Site.Name. == "Bushkoppies Final") 
Bush $final_result <- Bush$SARS.CoV.2.PCR.result


Bush <- Bush %>%
  mutate(tested1 = case_when( (Site.Name. == "Bushkoppies Inlet" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Bushkoppies Inlet" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Bushkoppies Final"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Bushkoppies Final" & final_result == "Negative") ~ -0.1))


png("~/Bushkoppies Inlet_final.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Bushplot <- ggplot(Bush[!is.na(Bush$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Bushkoppies Inlet Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Bushkoppies Final Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Bushkoppies Inlet Sample Collection" = "darkred", "Bushkoppies Final Sample Collection" = "darkblue",
                                 "Bushkoppies Inlet" = "#FC4E2A", 
                                 "Bushkoppies Final" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") )

Bushplot

dev.off()

###############################################################################
Saps_das <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "SAPS Training College"| 
            Site.Name. == "Daspoort Wastewater Treatment Works")

Saps_das$final_result <- Saps_das$SARS.CoV.2.PCR.result


Saps_das<- Saps_das%>%
  mutate(tested1 = case_when( (Site.Name. == "SAPS Training College" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "SAPS Training College" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Daspoort Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))

png("~/SAPS_Daspoort.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Saps_dasplot <- ggplot(Saps_das[!is.na(Saps_das$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " SAPS Training College Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Daspoort Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" SAPS Training College Sample Collection" = "darkred", "Daspoort Sample Collection" = "darkblue", 
                                 "SAPS Training College" = "#FC4E2A","Daspoort Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") )


Saps_dasplot

dev.off()

###############################################################################
groen <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Groenkloof"| 
            Site.Name. == "Rooiwal Wastewater Treatment Works")
groen$final_result <- groen$SARS.CoV.2.PCR.result


groen<- groen%>%
  mutate(tested1 = case_when( (Site.Name. == "Groenkloof" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Groenkloof" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Rooiwal Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Rooiwal Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


png("~/Groenkloof_Daspoort.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
groenplot <- ggplot(groen[!is.na(groen$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Groenkloof Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Rooiwal Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Groenkloof Sample Collection" = "darkred", "Rooiwal Sample Collection" = "darkblue", 
                                 "Groenkloof" = "#FC4E2A","Rooiwal Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") )

groenplot

dev.off()

###############################################################################

Att_das <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Atteridgeville Train Station"| 
            Site.Name. == "Rooiwal Wastewater Treatment Works")
Att_das$final_result <- Att_das$SARS.CoV.2.PCR.result


Att_das<- Att_das%>%
  mutate(tested1 = case_when( (Site.Name. == "Atteridgeville Train Station" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Atteridgeville Train Station" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Rooiwal Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Rooiwal Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


png("~/Atteridgeville_Daspoort.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Att_dasplot <- ggplot(Att_das[!is.na(Att_das$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Atteridgeville Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Rooiwal Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Atteridgeville Sample Collection" = "darkred", "Rooiwal Sample Collection" = "darkblue", 
                                 "Atteridgeville Train Station" = "#FC4E2A","Rooiwal Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") ) 
Att_dasplot

dev.off()

##############################################################################

Ver_Das <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Vergenoeg"| 
            Site.Name. == "Daspoort Wastewater Treatment Works")
Ver_Das$final_result <- Ver_Das$SARS.CoV.2.PCR.result


Ver_Das<- Ver_Das%>%
  mutate(tested1 = case_when( (Site.Name. == "Vergenoeg" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Vergenoeg" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Daspoort Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


png("~/Vergenoeg_Daspoort.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Ver_Dasplot <- ggplot(Ver_Das[!is.na(Ver_Das$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Vergenoeg Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Daspoort Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Vergenoeg Sample Collection" = "darkred", "Daspoort Sample Collection" = "darkblue", 
                                 "Vergenoeg" = "#FC4E2A","Daspoort Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 

Ver_Dasplot

dev.off()

#############################################################################

Saps_Kal <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Kalafong Hospital"| 
            Site.Name. == "Daspoort Wastewater Treatment Works")

Saps_Kal$final_result <- Saps_Kal$SARS.CoV.2.PCR.result


Saps_Kal<- Saps_Kal%>%
  mutate(tested1 = case_when( (Site.Name. == "Kalafong Hospital" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Kalafong Hospital" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


png("~/Kalafong Hospital_SAPS.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Saps_Kalplot <- ggplot(Saps_Kal[!is.na(Saps_Kal$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Kalafong Hospital Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Daspoort Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Kalafong Hospital Sample Collection" = "darkred", "Daspoort Sample Collection" = "darkblue", 
                                 "Kalafong Hospital" = "#FC4E2A","Daspoort Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 16, 15),
                                                  linetype = c(0, 1,1, 0)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") )

Saps_Kalplot

dev.off()
##############################################################################
Lau_Das <- water2 %>% 
  filter(District.Name == "Tshwane MM") %>%
  filter( Site.Name. == "Laudium"| 
            Site.Name. == "Daspoort Wastewater Treatment Works")
Lau_Das$final_result <- Lau_Das$SARS.CoV.2.PCR.result


Lau_Das<- Lau_Das%>%
  mutate(tested1 = case_when( (Site.Name. == "Laudium" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Laudium" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Daspoort Wastewater Treatment Works"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Daspoort Wastewater Treatment Works" & final_result == "Negative") ~ -0.1))


png("~/Laudium_Daspoort.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
Lau_Dasplot <- ggplot(Lau_Das[!is.na(Lau_Das$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Laudium Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Daspoort Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Laudium Sample Collection" = "darkred", "Daspoort Sample Collection" = "darkblue", 
                                 "Laudium" = "#FC4E2A","Daspoort Wastewater Treatment Works" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 
Lau_Dasplot

dev.off()
#################################################################################

tem_mall <- water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Midstream"| 
            Site.Name. == "Olifantsfontein") %>%
  filter( year == "2023")

tem_mall$final_result <- tem_mall$SARS.CoV.2.PCR.result


tem_mall<- tem_mall%>%
  mutate(tested1 = case_when( (Site.Name. == "Midstream" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Midstream" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Olifantsfontein"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.1))


png("~/Midstream_Olifantsfontein.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
tem_mallplot <- ggplot(tem_mall[!is.na(tem_mall$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = "Midstream Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c("Midstream Sample Collection" = "darkred", "Olifantsfontein Sample Collection" = "darkblue", 
                                 "Midstream" = "#FC4E2A","Olifantsfontein" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"))

tem_mallplot

dev.off()
####################################################################################
tem_mall <- water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Tembisa Mall"| 
            Site.Name. == "Olifantsfontein") %>%
  filter( year == "2023")

tem_mall$final_result <- tem_mall$SARS.CoV.2.PCR.result


tem_mall<- tem_mall%>%
  mutate(tested1 = case_when( (Site.Name. == "Tembisa Mall" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Tembisa Mall" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Olifantsfontein"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.1))


png("~/Tembisa_Olifantsfontein.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
tem_mallplot <- ggplot(tem_mall[!is.na(tem_mall$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Tembisa Mall Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Tembisa Mall Sample Collection" = "darkred", "Olifantsfontein Sample Collection" = "darkblue", 
                                 "Tembisa Mall" = "#FC4E2A","Olifantsfontein" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white"))

tem_mallplot

dev.off()
###############################################################################
hosp_oli <-water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Tembisa Hospital Downstream"| 
            Site.Name. == "Olifantsfontein") %>%
  filter( year == "2023")

hosp_oli $final_result <- hosp_oli$SARS.CoV.2.PCR.result


hosp_oli <- hosp_oli%>%
  mutate(tested1 = case_when( (Site.Name. == "Tembisa Hospital Downstream" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Tembisa Hospital Downstream" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Olifantsfontein"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.1))


png("~/Tembisa Hospital Downstream_Olifantsfontein.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
hosp_oliplot <- ggplot(hosp_oli[!is.na(hosp_oli$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Tembisa Hospital Downstream Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Tembisa Hospital Downstream Sample Collection" = "darkred", "Olifantsfontein Sample Collection" = "darkblue", 
                                 "Tembisa Hospital Downstream" = "#FC4E2A","Olifantsfontein" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 

hosp_oliplot

dev.off()
##############################################################################
mall_mol <- water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Olifantsfontein"| 
            Site.Name. == "Namane Drive")

mall_mol$final_result <- mall_mol$SARS.CoV.2.PCR.result


mall_mol<- mall_mol%>%
  mutate(tested1 = case_when( (Site.Name. == "Olifantsfontein" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Namane Drive"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Namane Drive" & final_result == "Negative") ~ -0.1))

png("~/Tembisa Mall_Namane.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
mall_molplot <- ggplot(mall_mol[!is.na(mall_mol$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Namane Drive Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Olifantsfontein Sample Collection" = "darkred", "Namane Drive Sample Collection" = "darkblue", 
                                 "Olifantsfontein" = "#FC4E2A","Namane Drive" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 16, 15, 16),
                                                  linetype = c(0, 1,0, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 

mall_molplot

dev.off()
##############################################################################
sas_nam <- water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Moloreng"| 
            Site.Name. == "Olifantsfontein")

sas_nam$final_result <- sas_nam$SARS.CoV.2.PCR.result


sas_nam<- sas_nam%>%
  mutate(tested1 = case_when( (Site.Name. == "Moloreng" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Moloreng" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Olifantsfontein"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.1))


png("~/Moloreng_Sasol.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
sas_namplot <- ggplot(sas_nam[!is.na(sas_nam$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Moloreng Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Moloreng Sample Collection" = "darkred", "Olifantsfontein Sample Collection" = "darkblue", 
                                 "Moloreng" = "#FC4E2A","Olifantsfontein" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white") )
sas_namplot

dev.off()


###########################################################################

sas <- water2 %>% 
  filter(District.Name == "Ekurhuleni MM") %>%
  filter( Site.Name. == "Sasol Garage"| 
            Site.Name. == "Olifantsfontein")

sas$final_result <- sas$SARS.CoV.2.PCR.result


sas<- sas%>%
  mutate(tested1 = case_when( (Site.Name. == "Sasol Garage" & final_result == "Positive") ~ -0.3, 
                              (Site.Name. == "Sasol Garage" & final_result == "Negative") ~ -0.3)) %>%
  mutate(tested2 = case_when( (Site.Name. == "Olifantsfontein"& final_result == "Positive") ~ -0.1, 
                              (Site.Name. == "Olifantsfontein" & final_result == "Negative") ~ -0.1))


png("~/Sasol_Olifantsfontein.png", 
    width = 5*900,
    height = 5*300, 
    res = 300,
    pointsize = 8)

#windows()    
sas_plot <- ggplot(sas[!is.na(sas$loglevels),]) +
  geom_point(aes(x=Date, y=tested1, group = 1, col = " Sasol Garage Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=tested2, group = 1, col = "Olifantsfontein Sample Collection"),stat="identity", size=2, shape = 15)+
  geom_point(aes(x=Date, y=loglevels ,  
                 group=Site.Name., col=Site.Name.),stat="identity", size=2)+
  geom_line(aes(x=Date, y=loglevels,  
                group=Site.Name., col=Site.Name.),stat="identity", size=1) +
  scale_colour_manual(values = c(" Sasol Garage Sample Collection" = "darkred", "Olifantsfontein Sample Collection" = "darkblue", 
                                 "Sasol Garage" = "#FC4E2A","Olifantsfontein" =  "#009ADE" )) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b %y") +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15, 16, 16),
                                                  linetype = c(0, 0,1, 1)) )) + 
  labs(x="\nDate",y=" Log Genome Copies/ml (N Gene)\n")+
  facet_grid(~factor(PCR, levels=c('qPCR', 'dPCR')), scales = "free_x", space= "free")+
  ggthemes::theme_tufte()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0,color="black", size=15 ),
    axis.text.y = element_text(color="black", size=15 ),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(color="black", size=15),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    strip.background = element_rect(fill = "white")) 
sas_plot

dev.off()