library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(lubridate)
#library(ComplexHeatmap)
#library(circlize)

#################################################################################
Bar Graph of Sites Collected for that specific week
Remember to change epiweek 

report <- read_excel("~/Routine Reporting/sampling site received status.xlsx")

#View(report)

#Heatmap of sites collected of all time
################################################################################

#First heatmap is for samples collected
report3 <- read_excel("~/Routine Reporting/sampling site received status.xlsx", sheet = "test")

report3 <- report3 %>% 
  column_to_rownames("Sampling Site") %>%  #converting first column into row name
  as.matrix() #need to make it a matrix for heatmap to work

prov <- read_excel("~/Routine Reporting/sampling site received status.xlsx", sheet = "prov")

prov <- prov %>% 
  column_to_rownames("Sampling Site") %>%  #converting first column into row name
  as.matrix()

clrs = structure(1:5, names = c("0","1", "2", "3", "4")) # black, red, green, blue


windows()
H1 <- Heatmap(report3, name = "Sampling Site Collection Status", 
              cluster_rows = FALSE,
              show_column_dend = FALSE, 
              col = clrs, rect_gp = gpar(col = "white", lwd = 2),
              row_names_gp = gpar(fontsize = 12),
              column_names_gp = gpar(fontsize = 12),
              column_names_side = "top",
              row_names_side = "left",
              column_order = sort(colnames(report3)))

H2 <- Heatmap(prov, name= "Province", column_names_side = "top", show_row_names = FALSE)

H1 + H2

#second heatmap is for provinces 
report4 <- read_excel("~/Routine Reporting/sampling site received status.xlsx", sheet = "prov")

report4 <- report4 %>% 
  column_to_rownames("Sampling Site") %>%  #converting first column into row name
  as.matrix() #need to make it a matrix for heatmap to work


windows()
heatmap(report4,Rowv = NA, Colv = NA, scale= "column", col=c("gray", "red", "orange", "yellow", "green") )
legend(0.85,1, 
       legend=c("Not scheduled\n", "Scheduled but not collected\n", 
                "Scheduled and collected once\n", "Scheduled and collected twice\n", 
                "Scheduled and collected three times\n"),
       bty="n",
       cex = 0.8, 
       fill= col=c("gray", "red", "orange", "yellow", "green")
)
################################################################################

# Calling data from RedCap using an API


token <- "insert-api-token-here"
url <- "insert-redcap-url-here"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
#print(result)

##################################################################


#Summary 

water <- result

water$sam_col_date <- as.Date(water$sam_col_date)
water$proc_date <- as.Date(water$proc_date)
water$pcr_test_date <- as.Date(water$pcr_test_date)
water$extraction_date <- as.Date(water$extraction_date )
water$pcr_date <- as.Date(water$pcr_date)
water$tapestation_date <- as.Date(water$tapestation_date )
water$date_rec_sequences <- as.Date(water$date_rec_sequences)
water$date_sub_sequencing <- as.Date(water$date_sub_sequencing)
water$seq_report_date <- as.Date(water$seq_report_date)

##########################################################################

water$epiweek <- lubridate::epiweek(ymd(water$sam_col_date)) #generate epiweek
water$year <- strftime(water$sam_col_date, "%Y") #Creating year column  
water$month <- strftime(water$sam_col_date, "%m") #Creating month column  
water$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
water$epiweek2 <- do.call(paste, c(water[my_cols],sep ="")) #created new variable using concat columns


#Turn around times for quantitative results 

water <- water %>% 
  filter(sam_col_date > "2023-04-30")

#########################################################
#calculate TAT for each sample 

water$pro_tat <- difftime(water$proc_date,water$sam_col_date, units = c("days"))
water$lab_tat <- difftime(water$pcr_test_date,water$proc_date, units = c("days"))
water$overall_tat <- difftime(water$pcr_test_date,water$sam_col_date, units = c("days"))

#calculate mean TAT for each epiweek

mean_pro <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Meanpro=mean(pro_tat, na.rm = TRUE))

mean_lab <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Meanlab=mean(lab_tat, na.rm = TRUE))

mean_overall <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Meanoverall=mean(overall_tat, na.rm = TRUE))


#Tabulate number of samples we've received 

freq_samples <- water %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#create new df with mean TAT and numbers
tat_vs_freq <- merge(mean_pro, mean_lab, by= "epiweek2", na.rm =TRUE)
tat_vs_freq <- merge(tat_vs_freq, mean_overall, by= "epiweek2", na.rm =TRUE)
tat_vs_freq <- merge(tat_vs_freq, freq_samples, by= "epiweek2", na.rm =TRUE)

#Plot 

png("C:/Users/NosihleM/Documents/Routine Reporting/sample tracking/quantitative.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

#windows() 
tat_plot <- ggplot(tat_vs_freq) +
  geom_bar(aes(x=epiweek2, y=n),stat="identity", fill="darkgray",
           colour="darkgray")+
  geom_line(aes(x=epiweek2, y=Meanpro*1, colour= "Meanpro", group=1),stat="identity", size=1.5)+
  geom_line(aes(x=epiweek2, y=Meanlab*1, colour= "Meanlab", group=1),stat="identity", size=1.5)+
  geom_line(aes(x=epiweek2, y=Meanoverall*1, colour= "Meanoverall", group=1),stat="identity", size=1.5)+
  scale_colour_manual(labels = c("Sample Preparation", "Quantification", "Overall"), 
                      breaks = c("Meanpro", "Meanlab","Meanoverall" ),
                      values = c("forestgreen", "black", "skyblue")) +
  scale_y_continuous(sec.axis=sec_axis(~ . /1,name="Mean TAT in days\n")) + 
  labs(x="Epiweek",y="Number of samples collected\n")+
  ggthemes::theme_hc()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0, size = 20),
    axis.text.y = element_text(size = 20),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text( size=20),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1)) +
  geom_text(aes(label = n, x=epiweek2, y=n), vjust = -0.5, colour = "black", size = 6)

tat_plot

dev.off()

#TAT for Sequencing 

#sub-setting the data so that if sample was repeated, and therefore
#the sample id repeated, it will only keep last duplicate, i.e repeat

#seq2 <- seq1[!duplicated(seq1$`Lab Number`), fromLast=T] #double check 

#seq_water <- merge(water1, seq2, by= "Lab Number", na.rm =TRUE)


#filter for those that went for sequencing 

water <- water %>% 
  filter(!is.na(water$date_sub_sequencing))

#calculate TAT for each sample 

water$extraction_tat <- difftime(water$extraction_date , 
                                 water$pcr_test_date
                                 , units = c("days"))

water$pcr_tat <- difftime(water$pcr_date, 
                          water$extraction_date , 
                          units = c("days"))

water$tapestation_tat <- difftime(water$tapestation_date , 
                                  water$pcr_date, 
                                  units = c("days"))

water$sequencing_tat <- difftime(water$date_rec_sequences,
                                 water$date_sub_sequencing, 
                                 units = c("days"))

water$seq_report_tat <- difftime(water$seq_report_date,
                                 water$date_rec_sequences, 
                                 units = c("days"))

water$overall <- difftime(water$seq_report_date ,water$pcr_test_date, units = c("days"))


#calculate mean TAT for each epiweek

mean_extraction_tat <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Extraction=abs(mean(extraction_tat, na.rm = TRUE)))

mean_pcr_tat <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(PCR=mean(pcr_tat, na.rm = TRUE))

mean_tapestation_tat <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Tapestation=mean(tapestation_tat, na.rm = TRUE))

mean_sequencing_tat <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Sequencing=mean(sequencing_tat, na.rm = TRUE))

mean_seq_report_tat <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(`Sequencing Report`=mean(seq_report_tat, na.rm = TRUE))

mean_overall <- water%>% # we specify which df we would like to use
  group_by(epiweek2)%>% # we specify what we would like to group by 
  summarise(Overall=mean(overall, na.rm = TRUE))


#Tabulate number of samples we've received 

freq_samples2 <- water %>%
  group_by(epiweek2)%>%
  count(epiweek2, na.rm=TRUE)

#create new df with mean TAT and numbers
#merge only takes two values at time so we gonna merge two first then the last one 
seqtat_vs_freq <- merge(mean_extraction_tat, mean_pcr_tat, by="epiweek2", na.rm =TRUE)
seqtat_vs_freq <- merge(seqtat_vs_freq, mean_tapestation_tat, by="epiweek2", na.rm =TRUE)
seqtat_vs_freq <- merge(seqtat_vs_freq, mean_sequencing_tat, by="epiweek2", na.rm =TRUE)
seqtat_vs_freq <- merge(seqtat_vs_freq, mean_seq_report_tat, by="epiweek2", na.rm =TRUE)
seqtat_vs_freq <- merge(seqtat_vs_freq, mean_overall, by="epiweek2", na.rm =TRUE)
seqtat_vs_freq <- merge(seqtat_vs_freq, freq_samples2, by="epiweek2", na.rm =TRUE)

#convert seqtat_vs_freq df into long format 

#seqtat_vs_freq2 <- seqtat_vs_freq %>%
#gather(Lab, n, -c(epiweek2, n))

#For some odd reason produced epiweeks with na.rm under lab, so just dropping those

#seqtat_vs_freq2 <- seqtat_vs_freq2 %>%
#filter(Lab == "Meancore" | Lab == "Meanlab") 


#Plot 

png("C:/Users/NosihleM/Documents/Routine Reporting/sample tracking/sequencing.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)


#windows() 
tat_plot2 <- ggplot(seqtat_vs_freq) +
  geom_bar(aes(x=epiweek2, y=n),stat="identity", fill="darkgray",
           colour="darkgray")+
  geom_line(aes(x=epiweek2, y=Extraction, colour= "Extraction", group=1),stat="identity", size=1.5) +
  
  geom_line(aes(x=epiweek2, y=PCR, colour= "PCR", group=1),stat="identity", size=1.5) +
  
  geom_line(aes(x=epiweek2, y=Tapestation, colour= "Tapestation", group=1),stat="identity", size=1.5) +
  
  geom_line(aes(x=epiweek2, y=Sequencing, colour= "Sequencing", group=1),stat="identity", size=1.5)+
  
  geom_line(aes(x=epiweek2, y=`Sequencing Report`, colour= "Sequencing Report", group=1),stat="identity", size=1.5)+
  
  geom_line(aes(x=epiweek2, y=Overall, colour= "Overall", group=1),stat="identity", size=1.5)+
  
  scale_y_continuous(sec.axis=sec_axis(~ . ,name="Mean TAT in days\n")) + 
  labs(x="Epiweek",y="Number of samples submitted for sequencing\n")+
  ggthemes::theme_hc()+
  theme(
    axis.ticks.x= element_blank(), 
    axis.text.x = element_text(angle = 90, hjust = 0, size = 20),
    axis.text.y = element_text(size = 20),
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text( size=20),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1)) +
  geom_text(aes(label = n, x=epiweek2, y=n), vjust = -0.5, colour = "black", size = 6)

tat_plot2

dev.off()




