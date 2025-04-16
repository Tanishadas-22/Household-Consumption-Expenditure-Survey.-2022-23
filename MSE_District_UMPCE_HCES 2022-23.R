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
#install.packages("boot")
library(boot)


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

my_common_id = common_data$Common_ID
my_district = rep("", length(my_common_id))
my_district = substr(my_common_id, 20, 21)
my_district = substr(my_common_id, 20, 21)              #-- district code appears in 20-21 column
# my_district = as.numeric(my_district)
# View(my_district)
#length(my_district)
#View(my_district)
my_common_data = data.frame(cbind(common_data, my_district))
View(my_common_data)


df_tri = my_common_data[my_common_data$State=="09" ,]

# df_r_tri = df_tri[df_tri$Sector == '1' & df_tri$my_district == "50",]
# df_u_tri = df_tri[df_tri$Sector == '2' & df_tri$my_district == "50",]

df_r_tri = df_tri[df_tri$Sector == '1',]
df_u_tri = df_tri[df_tri$Sector == '2',]

sum(df_r$MPCE_lvl14*df_r$hh_size*df_r$Multiplier )/sum(df_r$hh_size*df_r$Multiplier )
sum(df_u$MPCE_lvl14*df_u$hh_size*df_u$Multiplier )/sum(df_u$hh_size*df_u$Multiplier )

sum(df_r_tri$MPCE_lvl14*df_r_tri$hh_size*df_r_tri$Multiplier )/sum(df_r_tri$hh_size*df_r_tri$Multiplier )
sum(df_u_tri$MPCE_lvl14*df_u_tri$hh_size*df_u_tri$Multiplier )/sum(df_u_tri$hh_size*df_u_tri$Multiplier )

#------------- Trial for districts -----------------

mystate = "09"  #-Uttar Pradesh --
table(my_common_data$my_district[my_common_data$State == mystate])
tab1 = table(my_common_data$my_district[my_common_data$State == mystate])
dim(tab1)
ndistrict = dim(tab1)
ndistrict
tab1
tab1[2]


# Convert to data frame for easier manipulation
freq_df <- as.data.frame(tab1)

# Extract only the codes and frequencies
codes_frequencies <- freq_df[, c("Var1", "Freq")]  # Var1 contains the codes, Freq contains the frequencies

# Rename columns for clarity
colnames(codes_frequencies) <- c("Code", "Frequency")

# View the result
print(codes_frequencies)
# Example 2: Frequency Table for Numerical Data
# If you have numerical data and want to create a frequency table for specific ranges or bins, you can use the cut() function along with table().

print(codes_frequencies$Code)
print(codes_frequencies$Frequency)

district_codes =  codes_frequencies$Code


for(idistrict in c(1:ndistrict))
{
  
  df_r_tri = df_tri[df_tri$Sector == '1' & df_tri$my_district == district_codes[idistrict],]
  df_u_tri = df_tri[df_tri$Sector == '2' & df_tri$my_district == district_codes[idistrict],]
  
  x <- sum(df_r_tri$MPCE_lvl14*df_r_tri$hh_size*df_r_tri$Multiplier )/sum(df_r_tri$hh_size*df_r_tri$Multiplier )
  y <- sum(df_u_tri$MPCE_lvl14*df_u_tri$hh_size*df_u_tri$Multiplier )/sum(df_u_tri$hh_size*df_u_tri$Multiplier )
  
  rural_hh_mpce_vec = df_r_tri$MPCE_lvl14
  urban_hh_mpce_vec = df_u_tri$MPCE_lvl14
  
rural_my_data = rural_hh_mpce_vec
urban_my_data = urban_hh_mpce_vec

  # Function to calculate the mean
  statistic_function <- function(my_data, indices) {
    return(mean(my_data[indices]))
  }
  
   # rural_mean = statistic_function(rural_my_data, indices)
  # urban_mean = statistic_function(urban_my_data, indices)
  
  # Generate bootstrap samples
  if(length(rural_my_data) != 0) {
    rural_bootstrap_results <- boot(data = rural_my_data, statistic = statistic_function  , R = 1000)
    rural_bootstrap_variance <- var( rural_bootstrap_results$t)
    rural_bootstrap_se <- sqrt(rural_bootstrap_variance)
    rural_bootstrap_rse = (rural_bootstrap_se/x)*100.0
  }
  else {
    rural_bootstrap_variance = 0
    rural_bootstrap_se = 0
    rural_bootstrap_rse = (rural_bootstrap_se/x)*100.0
  }
  
  if(length(urban_my_data) != 0) {
    urban_bootstrap_results <- boot(data = urban_my_data, statistic = statistic_function  , R = 1000)
    urban_bootstrap_variance <- var( urban_bootstrap_results$t)
    urban_bootstrap_se <- sqrt(urban_bootstrap_variance)
    urban_bootstrap_rse = (urban_bootstrap_se/y)*100.0
    
  }
  else {
    urban_bootstrap_variance = 0
    urban_bootstrap_se = 0
    urban_bootstrap_rse = (urban_bootstrap_se/y)*100.0
  }
  
  
 # rural_bootstrap_results <- boot(data = rural_my_data, statistic = statistic_function  , R = 1000)
 # urban_bootstrap_results <- boot(data = urban_my_data, statistic = statistic_function , R = 1000)
  # Calculate bootstrap variance
  #rural_bootstrap_variance <- var( rural_bootstrap_results$t)
  #urban_bootstrap_variance <- var( urban_bootstrap_results$t)
  #print(paste("Bootstrap Variance:", bootstrap_variance))
  
  
  # Calculate standard error
  #rural_bootstrap_se <- sqrt(rural_bootstrap_variance)
  #urban_bootstrap_se <- sqrt(urban_bootstrap_variance)
  #print(paste("Bootstrap Standard Error:", bootstrap_se))
  
  
    print(c(district_codes[idistrict],x, rural_bootstrap_rse , y, urban_bootstrap_rse ))
}


####################################################################

#___________________ Weighted bootstrap _______________________________

# Load necessary package
# install.packages("boot")  # Uncomment this line if you haven't installed the package
library(boot)

# Example data
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)
weights <- runif(100, 1, 5)   # Random weights

# Function to compute the weighted mean
weighted_mean <- function(data, indices) {
  d <- data[indices, ]  # Resample the data
  weights <- d$weights   # Extract weights from the resampled data
  return(sum(d$y * weights) / sum(weights))  # Weighted mean
}

# Add weights to the data frame
data$weights <- weights

# Perform bootstrap
set.seed(123)  # For reproducibility
results <- boot(data = data, statistic = weighted_mean, R = 1000)

# Print the bootstrap results
print(results)

# Calculate the bootstrap variance
bootstrap_variance <- var(results$t)
print(bootstrap_variance)

# Calculate confidence intervals
ci <- boot.ci(results, type = "basic")
print(ci)



