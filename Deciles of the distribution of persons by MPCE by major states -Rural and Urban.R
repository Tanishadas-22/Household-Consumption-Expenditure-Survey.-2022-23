rm(list=ls())
#install.packages("haven")
library(haven)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("survey")
library(survey)
#install.packages("Hmisc")
library(Hmisc)

setwd("C:/Users/SOSU/Downloads/SPSS File")

data<-read_sav("lvl1.sav")
data2<-read_sav("lvl3.sav")
data3<-read_sav("lvl14.sav")
data4<-read_sav("lvl15.sav")

names(data)
names(data4)



#############################################################################

#setwd("D:/ces_22_23/data")
######################################################################
# level3+Level14

common_data = data2%>% subset(select = c("Common_ID"))
#common_data<-common_data%>%mutate(hhid=paste(fsu,sss,shhldn,sep=""))
common_data$hh_size_c = data4[data4$Questionnaire_No== 'C' ,]$Household_size 
common_data$hh_size_d = data4[data4$Questionnaire_No == 'D',]$Household_size 
common_data$hh_size = data2$hh_size
#check if same hhid's have same hh sizes.

summary(common_data)

# Write the data to a CSV file
#write_csv(common_data, "D:\\ces_22_23\\data\\common.csv")
# level3+Level14

lvl14 = data3 %>% left_join(common_data %>% subset(select = c(Common_ID, hh_size, hh_size_c, hh_size_d)), by = "Common_ID")
dim(lvl14)
lvl14$Value <-as.numeric(lvl14$Value )
summary(lvl14)

item_365 = as.character(c(409, 419, 899, 379, 399, 389, 629, 609, 619, 599, 579, 559, 569, 639, 649))
#item_365 = as.character(c(409, 419, 379, 399, 389, 629, 609, 619, 599, 579, 559, 569, 639, 649))
item_365 = c(item_365, '099')

item_7 = as.character(c(309, 319, 329, 169,219, 239, 249, 199, 189, 269, 279, 289, 299))
lvl14[lvl14$Item_Code == "539",]$Value  <- 0

lvl14$Value  <- as.numeric(as.character(lvl14$Value ))

lvl14[lvl14$Item_Code %in% item_365,]$Value  = lvl14[lvl14$Item_Code %in% item_365,]$Value  * (30/365)
lvl14[lvl14$Item_Code %in% item_7,]$Value  = lvl14[lvl14$Item_Code %in% item_7,]$Value  * (30/7)
summary(lvl14)

lvl14$hh_size <- as.numeric(as.character(lvl14$hh_size))
lvl14$hh_size_c <- as.numeric(as.character(lvl14$hh_size_c))
lvl14$hh_size_d <- as.numeric(as.character(lvl14$hh_size_d))

#lvl14 <- lvl14[!is.na(lvl14$hh_size) & !is.na(lvl14$hh_size_c) & !is.na(lvl14$hh_size_d), ]

lvl14[lvl14$Questionnaire_No == 'C',]$Value  = lvl14[lvl14$Questionnaire_No == 'C',]$Value  * lvl14[lvl14$Questionnaire_No == 'C',]$hh_size/lvl14[lvl14$Questionnaire_No == 'C',]$hh_size_c 
lvl14[lvl14$Questionnaire_No == 'D',]$Value  = lvl14[lvl14$Questionnaire_No == 'D',]$Value  * lvl14[lvl14$Questionnaire_No == 'D',]$hh_size/lvl14[lvl14$Questionnaire_No == 'D',]$hh_size_d 
summary(lvl14)

# Define features to combine and sum
combine_features14 <- unique(c(lvl14$Item_Code))

# Separate the features to be combined and summed
combined_data14 <- lvl14 %>%
  filter(Item_Code %in% combine_features14) %>%
  group_by(Common_ID) %>%
  dplyr:::summarize(Value  = sum(Value ), .groups = 'drop') 
summary(combined_data14)
common_data$total_lvl14 <- as.numeric(as.character(combined_data14$Value ))
common_data$hh_size <- as.numeric(as.character(common_data$hh_size))
common_data$MPCE_lvl14 = common_data$total_lvl14 / common_data$hh_size
colnames(common_data)

#setwd("D:/ces_22_23/data")
#data11<-read_sav("lvl1.sav")
new_data = common_data %>% left_join(data, by = "Common_ID")
common_data$Multiplier  = new_data$Multiplier/100
common_data$Sector = new_data$Sector
common_data$State = new_data$State

df_r = common_data[common_data$Sector == '1',]
df_u = common_data[common_data$Sector == '2',]

df_tri=common_data[common_data$State=="19",]

df_r_tri = df_tri[df_tri$Sector == '1',]
df_u_tri = df_tri[df_tri$Sector == '2',]

sum(df_r$MPCE_lvl14*df_r$hh_size*df_r$Multiplier )/sum(df_r$hh_size*df_r$Multiplier )
sum(df_u$MPCE_lvl14*df_u$hh_size*df_u$Multiplier )/sum(df_u$hh_size*df_u$Multiplier )

sum(df_r_tri$MPCE_lvl14*df_r_tri$hh_size*df_r_tri$Multiplier )/sum(df_r_tri$hh_size*df_r_tri$Multiplier )
sum(df_u_tri$MPCE_lvl14*df_u_tri$hh_size*df_u_tri$Multiplier )/sum(df_u_tri$hh_size*df_u_tri$Multiplier )


                                   #--- dECILES PART -----------

common_data$MPCE_lvl14 = common_data$total_lvl14 / common_data$hh_size
summary(common_data$MPCE_lvl14)
est_pop_per_sample = common_data$hh_size * common_data$Multiplier
data = data.frame(cbind(common_data, common_data$MPCE_lvl14, est_pop_per_sample)) 
View(data)
data_rural = subset(data,State ==17 &  Sector == 1)
data_urban = subset(data,State ==17 & Sector == 2)
#data_urban = subset(data,Sector == 2)
table(data_rural$Sector)
table(data_urban$Sector)
mydata = data.frame(data_urban)
#View(mydata)
#mydata = data.frame(data_rural)
mydata_sorted = data.frame(mydata[order(mydata$common_data.MPCE_lvl14), ])
View(mydata_sorted)
cum_est_pop = cumsum(mydata_sorted$est_pop_per_sample)
tot_pop = sum(mydata_sorted$hh_size * mydata_sorted$Multiplier)
tot_pop
percent_cum_est_pop = cum_est_pop/tot_pop
quint = c()
quint
min(mydata_sorted$common_data.MPCE_lvl14)
pseq = seq(0.1,1.0,by=0.1)
pseq
plength = 9
for(i in c(1:plength)) {
  ser = (1:length(cum_est_pop))[ percent_cum_est_pop <= pseq[i]]
  quint[i] = mydata_sorted$common_data.MPCE_lvl14[max(ser)]
}
print(quint)


