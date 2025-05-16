rm(list=ls())
library(dplyr)
library(readxl)
data_breakup_mult<- read_excel("C:/Users/SOSU/Downloads/hces1_multfile_UP.xlsx", sheet= "Sheet1")
data_breakup_mult$h <- as.numeric(data_breakup_mult$h)
sum(data_breakup_mult$h)
data_mycommon <- read.csv("C:/Users/SOSU/Downloads/SPSS File/my_common_data_HCES(2022-23)_DISTRICT.csv")
num_rows_state_9 <- sum(data_mycommon$State == 9)  
print(num_rows_state_9)



##################################################################################################################
 
#---- Handling of my_common_data and 
#-- and UP-multiplier break-up file --

#---- Reading the HCES-2022-23 MPCE values of hhs -----------
rm(list=ls())

library(readxl)

setwd("C:/Users/SOSU/Downloads/SPSS File")

my_common_data = read.csv("my_common_data_HCES(2022-23)_DISTRICT.csv")
View(my_common_data)

#--- Sector, State and my-district are to be coverted as.factor() -------------

names(my_common_data)
ncol(my_common_data)
nrow(my_common_data)

#-- Extracting stratum, sub-stratum, etc from Common_ID -----
#--- byte positions are collcted from Layout file ---

my_common_id = my_common_data$Common_ID

my_fsu_ser = rep("", length(my_common_id))
my_fsu_ser = substr(my_common_id, 9, 13)  

my_nss_region = rep("", length(my_common_id))
my_nss_region = substr(my_common_id, 17, 19)  

my_strm = rep("", length(my_common_id))
my_strm = substr(my_common_id, 22, 23)  

my_sstrm = rep("", length(my_common_id))
my_sstrm = substr(my_common_id, 24, 25)

my_panel = rep("", length(my_common_id))
my_panel = substr(my_common_id, 26, 27)

my_ss = rep("", length(my_common_id))
my_ss = substr(my_common_id, 28, 28)

my_fod_sub_region = rep("", length(my_common_id))
my_fod_sub_region = substr(my_common_id, 29, 32)

my_su = rep("", length(my_common_id))
my_su = substr(my_common_id, 33, 34)

my_sub_division = rep("", length(my_common_id))
my_sub_division = substr(my_common_id, 35, 35)

my_sss = rep("", length(my_common_id))
my_sss = substr(my_common_id, 36, 36)

my_sample_hhid = rep("", length(my_common_id))
my_sample_hhid = substr(my_common_id, 37, 38)

View(cbind(my_fsu_ser, my_nss_region, my_strm, my_sstrm, my_panel, my_ss, my_fod_sub_region, my_su, my_sub_division, my_sss, my_sample_hhid))

my_common_data = cbind(my_common_data, my_fsu_ser, my_nss_region, my_strm, my_sstrm, my_panel, my_ss, my_fod_sub_region, my_su, my_sub_division, my_sss, my_sample_hhid)
View(my_common_data)

#-------------------------

df_r = my_common_data[my_common_data$Sector == 1 ,]
df_r = data.frame(df_r)
View(df_r)

df_u = my_common_data[my_common_data$Sector == 2 ,]
df_u = data.frame(df_u)
View(df_u)

df_tri = my_common_data[my_common_data$State == 9 ,]
df_tri = data.frame(df_tri)
View(df_tri)

# df_r_tri = df_tri[df_tri$Sector == '1' & df_tri$my_district == "50",]
# df_u_tri = df_tri[df_tri$Sector == '2' & df_tri$my_district == "50",]

df_r_tri = df_tri[df_tri$Sector == 1,]
df_u_tri = df_tri[df_tri$Sector == 2,]

#--- All India Rural and Urban MPCE estimates are as follows ---
sum(df_r$MPCE_lvl14*df_r$hh_size*df_r$Multiplier )/sum(df_r$hh_size*df_r$Multiplier )
sum(df_u$MPCE_lvl14*df_u$hh_size*df_u$Multiplier )/sum(df_u$hh_size*df_u$Multiplier )

#--- State Rural and Urban MPCE estimates are as follows ---
sum(df_r_tri$MPCE_lvl14*df_r_tri$hh_size*df_r_tri$Multiplier )/sum(df_r_tri$hh_size*df_r_tri$Multiplier )
sum(df_u_tri$MPCE_lvl14*df_u_tri$hh_size*df_u_tri$Multiplier )/sum(df_u_tri$hh_size*df_u_tri$Multiplier )

up_n_rural = nrow(df_r_tri) ; print(up_n_rural);
up_n_urban = nrow(df_u_tri) ; print(up_n_urban);

print(up_n_rural + up_n_urban)

View(df_r_tri)

table(df_r_tri$my_fsu_ser)
table(df_r_tri$my_nss_region)
table(df_r_tri$my_strm)
table(df_r_tri$my_sstrm)
table(df_r_tri$my_panel)
table(df_r_tri$my_ss)
table(df_r_tri$my_su)
table(df_r_tri$my_sub_division)
table(df_r_tri$my_sss)
table(df_r_tri$my_sample_hhid)

table(df_r_tri$my_fod_sub_region)
table(df_u_tri$my_fod_sub_region)

View(df_u_tri)

mytab_fsu = table(df_r_tri$my_fsu_ser)
mytab_fsu_df = as.data.frame(mytab_fsu)
View(mytab_fsu_df)
subset_mytab_fsu_df = subset(mytab_fsu_df, Freq != 0)
View(subset_mytab_fsu_df)
nrow(subset_mytab_fsu_df)


##--------------------------------------------------------------##
#-----------------------------------------------------------#
#---- Reading multiplier break-up file ----

setwd("C:/Users/SOSU/Downloads")
up_mult = read_excel("hces1_multfile_UP.xlsx")
View(up_mult)

nrow(up_mult)
ncol(up_mult)
names(up_mult)

up_mult$smallh_numeric <- as.numeric(up_mult$smallh)
print(sum(up_mult$smallh_numeric))

print(range(up_mult$smallh_numeric))
print(table(up_mult$smallh_numeric))


# print(length(up_mult$smallh[up_mult$smallh == 1]))

print(range(up_mult$nst))
print(table(up_mult$nst))

# print(length(up_mult$nst[up_mult$nst == 1]))

print(table(up_mult$totsd))

tab_strm = table(up_mult$strm)
tab_sstrm = table(up_mult$sstrm)
tab_panel = table(up_mult$panel)
tab_ss = table(up_mult$ss)
tab_fsu = table(up_mult$fsu)
tab_sss = table(up_mult$sss)
tab_totsd = table(up_mult$totsd)

#---
tab_strm_df = as.data.frame(tab_strm)
View(tab_strm_df)
tab_strm_codes_freq = tab_strm_df[,  c("Var1", "Freq")]
colnames(tab_strm_codes_freq) = c("code", "freq")
print(tab_strm_codes_freq)
#---

#---
tab_sstrm_df = as.data.frame(tab_sstrm)
View(tab_sstrm_df)
tab_sstrm_codes_freq = tab_sstrm_df[,  c("Var1", "Freq")]
colnames(tab_sstrm_codes_freq) = c("code", "freq")
print(tab_sstrm_codes_freq)
#---

#---
tab_panel_df = as.data.frame(tab_panel)
View(tab_panel_df)
tab_panel_codes_freq = tab_panel_df[,  c("Var1", "Freq")]
colnames(tab_panel_codes_freq) = c("code", "freq")
print(tab_panel_codes_freq)
#---

#---
tab_ss_df = as.data.frame(tab_ss)
View(tab_ss_df)
tab_ss_codes_freq = tab_ss_df[,  c("Var1", "Freq")]
colnames(tab_ss_codes_freq) = c("code", "freq")
print(tab_ss_codes_freq)
#---

#---
tab_fsu_df = as.data.frame(tab_fsu)
View(tab_fsu_df)
tab_fsu_codes_freq = tab_fsu_df[,  c("Var1", "Freq")]
colnames(tab_fsu_codes_freq) = c("code", "freq")
print(tab_fsu_codes_freq)
nrow(tab_fsu_codes_freq)
#---

#---
tab_sss_df = as.data.frame(tab_sss)
View(tab_sss_df)
tab_sss_codes_freq = tab_sss_df[,  c("Var1", "Freq")]
colnames(tab_sss_codes_freq) = c("code", "freq")
print(tab_sss_codes_freq)
#---

#---
tab_totsd_df = as.data.frame(tab_totsd)
View(tab_totsd_df)
tab_totsd_codes_freq = tab_totsd_df[,  c("Var1", "Freq")]
colnames(tab_totsd_codes_freq) = c("code", "freq")
print(tab_totsd_codes_freq)



###############################
str(my_common_data)
a<-  my_common_data[my_common_data$Sector == 1 & my_common_data$my_district ==57  & my_common_data$my_fsu_ser=="66395" & my_common_data$my_strm=="13" & my_common_data$my_sstrm=="1" & my_common_data$my_panel=="1" & my_common_data$my_ss=="1" & my_common_data$my_fod_sub_region=="0923" & my_common_data$my_sss=="2"]
print(a)

######################################## Merging the the both files ########################################

rm(list=ls())
library(readxl)
setwd("C:/Users/SOSU/Downloads/SPSS File")
my_common_data = read.csv("my_common_data_HCES(2022-23)_DISTRICT.csv")

my_common_id = my_common_data$Common_ID

my_fsu_ser = rep("", length(my_common_id))
my_fsu_ser = substr(my_common_id, 9, 13)  

my_nss_region = rep("", length(my_common_id))
my_nss_region = substr(my_common_id, 17, 19)  

my_strm = rep("", length(my_common_id))
my_strm = substr(my_common_id, 22, 23)  

my_sstrm = rep("", length(my_common_id))
my_sstrm = substr(my_common_id, 24, 25)

my_panel = rep("", length(my_common_id))
my_panel = substr(my_common_id, 26, 27)

my_ss = rep("", length(my_common_id))
my_ss = substr(my_common_id, 28, 28)

my_fod_sub_region = rep("", length(my_common_id))
my_fod_sub_region = substr(my_common_id, 29, 32)

my_su = rep("", length(my_common_id))
my_su = substr(my_common_id, 33, 34)

my_sub_division = rep("", length(my_common_id))
my_sub_division = substr(my_common_id, 35, 35)

my_sss = rep("", length(my_common_id))
my_sss = substr(my_common_id, 36, 36)

my_sample_hhid = rep("", length(my_common_id))
my_sample_hhid = substr(my_common_id, 37, 38)

View(cbind(my_fsu_ser, my_nss_region, my_strm, my_sstrm, my_panel, my_ss, my_fod_sub_region, my_su, my_sub_division, my_sss, my_sample_hhid))

my_common_data = cbind(my_common_data, my_fsu_ser, my_nss_region, my_strm, my_sstrm, my_panel, my_ss, my_fod_sub_region, my_su, my_sub_division, my_sss, my_sample_hhid)
View(my_common_data)

my_common_data_new <- my_common_data[my_common_data$State == 9, ]
View(my_common_data_new)

setwd("C:/Users/SOSU/Downloads")
up_mult = read_excel("hces1_multfile_UP.xlsx")
View(up_mult)

library(dplyr)
str(my_common_data_new)

my_common_data_new$Sector <- as.numeric(my_common_data_new$Sector)
#my_common_data_new$Common_ID <- as.numeric(my_common_data_new$Common_ID)
my_common_data_new$my_district <- as.numeric(my_common_data_new$my_district)
my_common_data_new$my_fsu_ser <- as.numeric(my_common_data_new$my_fsu_ser)
my_common_data_new$my_nss_region  <- as.numeric(my_common_data_new$my_nss_region )
my_common_data_new$my_strm <- as.numeric(my_common_data_new$my_strm)
my_common_data_new$my_sstrm <- as.numeric(my_common_data_new$my_sstrm)
my_common_data_new$my_panel <- as.numeric(my_common_data_new$my_panel)
my_common_data_new$my_ss <- as.numeric(my_common_data_new$my_ss)
my_common_data_new$my_fod_sub_region <- as.numeric(my_common_data_new$my_fod_sub_region)
my_common_data_new$my_su<- as.numeric(my_common_data_new$my_su)
my_common_data_new$my_sub_division <- as.numeric(my_common_data_new$my_sub_division)
my_common_data_new$my_sss <- as.numeric(my_common_data_new$my_sss)
my_common_data_new$my_sample_hhid <- as.numeric(my_common_data_new$my_sample_hhid)



str(up_mult)
up_mult$sro <- as.numeric(up_mult$sro)
up_mult$st <- as.numeric(up_mult$st)
up_mult$stnew <- as.numeric(up_mult$stnew)
up_mult$sector <- as.numeric(up_mult$sector)
up_mult$dc <- as.numeric(up_mult$dc)
#up_mult$dist_name <- as.numeric(up_mult$dist_name)
up_mult$strm <- as.numeric(up_mult$strm)
up_mult$sstrm <- as.numeric(up_mult$sstrm)
up_mult$panel <- as.numeric(up_mult$panel)
up_mult$ss <- as.numeric(up_mult$ss)
up_mult$framepop <- as.numeric(up_mult$framepop)
up_mult$apop <- as.numeric(up_mult$apop)
up_mult$totsu <- as.numeric(up_mult$totsu)
up_mult$supop <- as.numeric(up_mult$supop)
up_mult$listpop <- as.numeric(up_mult$listpop)
up_mult$svc <- as.numeric(up_mult$svc)
up_mult$svcnew <- as.numeric(up_mult$svcnew)
up_mult$fsu <- as.numeric(up_mult$fsu)
up_mult$sss <- as.numeric(up_mult$sss)
up_mult$Nst <- as.numeric(up_mult$Nst)
up_mult$nst <- as.numeric(up_mult$nst)
up_mult$totsd <- as.numeric(up_mult$totsd)
up_mult$caph <- as.numeric(up_mult$caph)
up_mult$smallh <- as.numeric(up_mult$smallh)
up_mult$mult <- as.numeric(up_mult$mult)


merged <- left_join(my_common_data_new, up_mult, 
                    by = c("Sector" = "sector", 
                           "my_district" = "dc", 
                           "my_fsu_ser" = "fsu", 
                           "my_strm" = "strm", 
                           "my_sstrm" = "sstrm", 
                           "my_panel" = "panel", 
                           "my_ss" = "ss", 
                           "my_fod_sub_region" = "sro", 
                           "my_sss" = "sss"))

str(merged)

View(merged)

write.csv(merged, "Merged_mycommondata_mult.csv")

#unique(merged$Nst)


#################################### Calculating y_sum,y_bar,x_sum, x_bar, sx^2, sy^2, #############################################


rm(list = ls())
library(readxl)
library(dplyr)

# Load the data
data <- read.csv("C:/Users/SOSU/Downloads/Merged.csv")

up_mult <-  read_excel("C:/Users/SOSU/Downloads/hces1_multfile_UP.xlsx", sheet ="multnew09") 

str(up_mult)
up_mult$sro <- as.numeric(up_mult$sro)
up_mult$st <- as.numeric(up_mult$st)
up_mult$stnew <- as.numeric(up_mult$stnew)
up_mult$sector <- as.numeric(up_mult$sector)
up_mult$dc <- as.numeric(up_mult$dc)
#up_mult$dist_name <- as.numeric(up_mult$dist_name)
up_mult$strm <- as.numeric(up_mult$strm)
up_mult$sstrm <- as.numeric(up_mult$sstrm)
up_mult$panel <- as.numeric(up_mult$panel)
up_mult$ss <- as.numeric(up_mult$ss)
up_mult$framepop <- as.numeric(up_mult$framepop)
up_mult$apop <- as.numeric(up_mult$apop)
up_mult$totsu <- as.numeric(up_mult$totsu)
up_mult$supop <- as.numeric(up_mult$supop)
up_mult$listpop <- as.numeric(up_mult$listpop)
up_mult$svc <- as.numeric(up_mult$svc)
up_mult$svcnew <- as.numeric(up_mult$svcnew)
up_mult$fsu <- as.numeric(up_mult$fsu)
up_mult$sss <- as.numeric(up_mult$sss)
up_mult$Nst <- as.numeric(up_mult$Nst)
up_mult$nst <- as.numeric(up_mult$nst)
up_mult$totsd <- as.numeric(up_mult$totsd)
up_mult$caph <- as.numeric(up_mult$caph)
up_mult$smallh <- as.numeric(up_mult$smallh)
up_mult$mult <- as.numeric(up_mult$mult)


y_sum = data %>% group_by(my_fod_sub_region,Sector,my_district,
                                     my_strm, my_sstrm, my_panel,
                                     my_ss,my_fsu_ser,
                                     my_sss) %>% summarise(
                                       sum_y = sum(total_lvl14))


y_sum = y_sum %>% left_join(select(up_mult,sro,sector,dc,strm,sstrm,
                                       panel,ss, fsu, sss,Nst,nst,
                                       totsd,caph,smallh,mult), 
                                by = c("my_fod_sub_region" = "sro", "Sector" = "sector",
                                       "my_district" = "dc", "my_strm" = "strm", 
                                       "my_sstrm" = "sstrm", "my_panel" = "panel","my_ss" = "ss", "my_fsu_ser" = "fsu", "my_sss" = "sss"))




#View(y_sum)

y_bar <- y_sum %>%
 mutate(y_bar = sum_y / smallh)



# Join y_sum and y_bar
combined_y <- y_sum %>%
  left_join(y_bar, 
            by = c("my_fod_sub_region", "Sector", "my_district", 
                   "my_strm", "my_sstrm", "my_panel", 
                   "my_ss", "my_fsu_ser", "my_sss"))

View(combined_y)


x_sum = data %>% group_by(my_fod_sub_region,Sector,my_district,
                          my_strm, my_sstrm, my_panel,
                          my_ss,my_fsu_ser,
                          my_sss) %>% summarise(
                            sum_x = sum(hh_size))

final_result <- combined_y %>%
  left_join(x_sum, 
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

# View the final result
View(final_result)

x_bar <- final_result %>%
  mutate(x_bar = sum_x / smallh.x)

final_result_with_x_bar <- final_result %>%
  left_join(x_bar %>% select(my_fod_sub_region, Sector, my_district, 
                             my_strm, my_sstrm, my_panel, 
                             my_ss, my_fsu_ser, my_sss, x_bar), 
            by = c("my_fod_sub_region", "Sector", "my_district", 
                   "my_strm", "my_sstrm", "my_panel", 
                   "my_ss", "my_fsu_ser", "my_sss"))

# View the final result with x_bar
View(final_result_with_x_bar)

#write.csv(final_result_with_x_bar,"final_result_with_x_bar.csv")

# Assuming you have already calculated y_bar in the previous steps
# Assuming you have already calculated y_bar in the previous steps
y_bar <- y_sum %>% 
  mutate(y_bar = sum_y / smallh)

# Join y_sum and y_bar
combined_y <- y_sum %>% 
  left_join(y_bar, 
            by = c("my_fod_sub_region", "Sector", "my_district", 
                   "my_strm", "my_sstrm", "my_panel", 
                   "my_ss", "my_fsu_ser", "my_sss"))

# Calculate y_var
combined_y <- combined_y %>%
  mutate(y_var = (sum_y - y_bar) / (smallh - 1))

# Calculate x_sum
x_sum = data %>% 
  group_by(my_fod_sub_region, Sector, my_district, 
           my_strm, my_sstrm, my_panel, 
           my_ss, my_fsu_ser, my_sss) %>% 
  summarise(sum_x = sum(hh_size))

# Join combined_y with x_sum
final_result <- combined_y %>% 
  left_join(x_sum, 
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

# Calculate x_bar
final_result <- final_result %>% 
  mutate(x_bar = sum_x / smallh.x)

# Calculate x_var
final_result <- final_result %>%
  mutate(x_var = (sum_x - x_bar) / (smallh.x - 1))

# View the final result
View(final_result)

# Write the final result to a CSV file
write.csv(final_result, "final_result_with_y_var_x_var.csv")





#######################################################Variance calculation ######################################################

rm(list=ls())
# Load necessary libraries
library(readr)
library(dplyr)

# Read the datasets
data_combined <- read_csv("C:/Users/SOSU/Documents/final_result_with_y& x_sum & bar.csv")
data_merged <- read_csv("C:/Users/SOSU/Downloads/Merged.csv")

names(data_combined)
names(data_merged)


# Optional: View unique combinations to debug
combined_keys <- data_combined %>% select(Sector, my_district, my_strm, my_sstrm, my_panel, my_ss, my_fsu_ser, my_sss,smallh) %>% distinct()
merged_keys <- data_merged %>% select(Sector, my_district, my_strm, my_sstrm, my_panel, my_ss, my_fsu_ser, my_sss,smallh) %>% distinct()

# Try anti_join to find non-matching records
anti_join(merged_keys, combined_keys,
          by = c("Sector", "my_district", "my_strm", "my_sstrm", "my_panel", "my_ss", "my_fsu_ser", "my_sss","smallh"))

merged_data <- data_merged %>%
  inner_join(data_combined, by = c("Sector", "my_district", "my_strm", "my_sstrm", "my_panel", "my_ss", "my_fsu_ser", "my_sss","smallh"))

nrow(merged_data)  # Check if any rows are returned

library(dplyr)

# Compute y_var and x_var per group
var_by_group <- merged_data %>%
  group_by(Sector, my_district, my_strm, my_sstrm, my_panel, my_ss, my_fsu_ser, my_sss, smallh) %>%
  summarise(
    y_var = sum((total_lvl14 - y_bar)^2, na.rm = TRUE) / (unique(smallh) - 1),
    x_var = sum((hh_size - x_bar)^2, na.rm = TRUE) / (unique(smallh) - 1),
    .groups = "drop"
  )

# View result
View(var_by_group)

write.csv(var_by_group, "var_by_group.csv")


#### Merging the 2 files 

var_by_group <- read.csv("C:/Users/SOSU/Documents/var_by_group.csv")
final_result_with_y_var_x_var <- read.csv("C:/Users/SOSU/Documents/final_result_with_y& x_sum & bar.csv")

final_result_final <- 
left_join(final_result_with_y_var_x_var, var_by_group,by= c("Sector", "my_district" , "my_strm" ,"my_sstrm", "my_panel" ,"my_ss", "my_fsu_ser" ,"my_sss", "smallh"))

View(final_result_final)
write.csv(final_result_final,"final_result_final.csv")


final_result_final_R = final_result_final[final_result_final$Sector == 1, ]
final_result_final_U = final_result_final[final_result_final$Sector == 2, ]

Yhat = sum(final_result_final_R$sum_y *(final_result_final_R$caph/final_result_final_R$smallh)*final_result_final_R$totsd *(final_result_final_R$Nst/final_result_final_R$nst))
Xhat = sum(final_result_final_R$sum_x *(final_result_final_R$caph/final_result_final_R$smallh)*final_result_final_R$totsd *(final_result_final_R$Nst/final_result_final_R$nst))
Yhat
Xhat
Rhat = Yhat / Xhat
Rhat

Yhat = sum(final_result_final_U$sum_y *(final_result_final_U$caph/final_result_final_U$smallh)*final_result_final_U$totsd *(final_result_final_U$Nst/final_result_final_U$nst))
Xhat = sum(final_result_final_U$sum_x *(final_result_final_U$caph/final_result_final_U$smallh)*final_result_final_U$totsd *(final_result_final_U$Nst/final_result_final_U$nst))
Yhat
Xhat
Rhat = Yhat / Xhat
Rhat

################################################     Cor Matrix trial  ##########################################################################################

rm(list=ls())
library(readxl)
library(dplyr)

#data_1_R <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_RATION CARD_codes.xlsx", sheet = "22_23_R")
#View(data_1_R )

data_1_R <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_RATION CARD_codes.xlsx", sheet = "22_23_R_n")
View(data_1_R )



data_1_U <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_RATION CARD_codes.xlsx", sheet = "22-23_U")
View(data_1_U )

data_2_R <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_DIVYANGJAN PENSION_codes.xlsx", sheet ="2022-23")
View(data_2_R)

data_2_U <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_DIVYANGJAN PENSION_codes.xlsx", sheet ="2022-23-U")
View(data_2_U)

#sector<-ifelse(survey_data$sector=="Rural",1,0)
#print(sector)

#data_2_U <- data2[data2$URBAN>0, ]
#View(data_2_U)

data3 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_Districtwise Consumed load_codes.xlsx", sheet = "2022-23_R")
View(data3)

print(colnames(data3))
colnames(data3) = c("Sr. No."  , "District Name" ,"District_code" ,
                      "R_Number of Domestic Electricity connection having different consumption load 1 KW"  ,"R_Number of Domestic Electricity connection having different consumption load 2 KW"   ,  "R_Number of Domestic Electricity connection having different consumption load 3 KW",  "R_Number of Domestic Electricity connection having different consumption load 4 KW", "R_Number of Domestic Electricity connection having different consumption load 5 KW or more than 5 KW")


print(colnames(data3))

#names(data3)[is.na(names(data3))] <- "new_name"  # Replace NA names
#names(data3)[names(data3) == ""] <- "new_name"   # Replace empty names

data3 <- data3 %>% mutate(
  rural_elec = `R_Number of Domestic Electricity connection having different consumption load 1 KW` +
    `R_Number of Domestic Electricity connection having different consumption load 2 KW` +
    `R_Number of Domestic Electricity connection having different consumption load 3 KW` +
    `R_Number of Domestic Electricity connection having different consumption load 4 KW` +
    `R_Number of Domestic Electricity connection having different consumption load 5 KW or more than 5 KW`
)

# View the updated data frame
View(data3)


#data3_U <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_Districtwise Consumed load_codes.xlsx", sheet = "2022-23_U")
#print(colnames(data3_U))
#colnames(data3_U) = c("Sr. No."  , "District Name" ,"District_code" ,
                    #"U_Number of Domestic Electricity connection having different consumption load 1 KW"  ,"U_Number of Domestic Electricity connection having different consumption load 2 KW"   ,  "U_Number of Domestic Electricity connection having different consumption load 3 KW","U_Number of Domestic Electricity connection having different consumption load 5 KW or more than 5 KW")


#print(colnames(data3_U))

#names(data3_U)[is.na(names(data3_U))] <- "new_name"  # Replace NA names
#names(data3_U)[names(data3_U) == ""] <- "new_name"   # Replace empty names

#data3_U <- data3_U %>% mutate(
  #urban_elec = `U_Number of Domestic Electricity connection having different consumption load 1 KW` +
    #`U_Number of Domestic Electricity connection having different consumption load 2 KW` +
    #`U_Number of Domestic Electricity connection having different consumption load 3 KW` +
    #`new_name` +
    #`U_Number of Domestic Electricity connection having different consumption load 5 KW or more than 5 KW`
#)

# View the updated data frame
#View(data3_U)



#View(data3$data_3_R)
#View(data3)

# Print the column names in data3
#print(colnames(data3))

# Create a vector of the column names you want to sum (update based on actual names)
#columns_to_sum_U <- c(
  #"U_Number of Domestic Electricity connection having different consumption load 1 KW",
  #"U_Number of Domestic Electricity connection having different consumption load 2 KW",
  #"U_Number of Domestic Electricity connection having different consumption load 3 KW",
  #"U_Number of Domestic Electricity connection having different consumption load 4 KW",  # Ensure this matches exactly
  #"U_Number of Domestic Electricity connection having different consumption load 5 KW or more than 5 KW"  # Ensure this matches exactly
#)

# Check if all columns exist
#if (all(columns_to_sum_U %in% colnames(data3))) {
  # Sum the specified columns across rows
#  data_3_U <- rowSums(data3[columns_to_sum_U], na.rm = TRUE)  # na.rm = TRUE to ignore NA values
#} else {
 # stop("One or more specified columns do not exist in data3.")
#}

# If you want to see the result
#print(data_3_U)


data4 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx")

data4 <- subset(data4,select=c(1,2,3,5,4))

data4_U <- subset(data4,select=c(1,2,3,4,8))

View(data4_U)

# merge them for Rural   

merged_data_1_2 <- left_join(data_1_R, data_2_R, by = "District_code")
View(merged_data_1_2)

merged_data_1_2_3 <- left_join(merged_data_1_2, data3, by = "District_code")
View(merged_data_1_2_3)

merged_data_all_R <- left_join(merged_data_1_2_3,data4, by = "District_code")
View(merged_data_all_R)

merged_data_all_R <-  subset(merged_data_all_R,select=c(1,2,3,4,5,8,16,19))
View(merged_data_all_R)

write.csv(merged_data_all_R, "merged_data_all_R.csv")

#merged_data_all_R <-  subset(merged_data_all_R,select=c(4,5,8,16,19))
#View(merged_data_all_R)

#write.csv(merged_data_all_R, "merged_data_all_R.csv")

# Calculate the correlation matrix
correlation_matrix <- cor(merged_data_all_R)  # Use complete observations to handle NA values

# Print the correlation matrix
print(correlation_matrix)

write.csv(correlation_matrix, "correlation_matrix_R_now.csv")



# merge them for Urban   

merged_data_1_2 <- left_join(data_1_U, data_2_U, by = "District_code")
View(merged_data_1_2)

#merged_data_1_2_3 <- left_join(merged_data_1_2, data3_U, by = "District_code")
#View(merged_data_1_2_3)

merged_data_all_U <- left_join(merged_data_1_2,data4_U, by = "District_code")
View(merged_data_all_U)

merged_data_all_U <-  subset(merged_data_all_U,select=c(1,2,3,4,7,11))
View(merged_data_all_U)

write.csv(merged_data_all_U, "merged_data_all_U.csv")

#merged_data_all_U <-  subset(merged_data_all_U,select=c(4,7,11))
#View(merged_data_all_U)

str(merged_data_all_U)

sum(is.na(merged_data_all_U$Urban_MPCE)) 

merged_data_all_U$Urban_MPCE <- as.numeric(merged_data_all_U$Urban_MPCE) 

correlation_matrix_U <- cor(merged_data_all_U, use = "complete.obs")  # Use complete observations to handle NA values

# Print the correlation matrix
print(correlation_matrix_U)

write.csv(correlation_matrix_U, "correlation_matrix_U_now.csv")



##############################  code by sr ##########################################

library(readxl)
library(dplyr)
library(writexl)

# Load and preprocess the first dataset
data_1 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx") %>%
  select(-c(1, 2)) %>%
  mutate(District_names = tolower(District_names))

# Load and preprocess the second dataset
data_2 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_DIVYANGJAN PENSION_codes.xlsx", sheet ="2022-23_all") %>%
  select(-1) %>%
  mutate(Dist_names = tolower(Dist_names))

# Merge the first two datasets
m1 <- data_2 %>%
  left_join(data_1, by = c("Dist_names" = "District_names")) %>%
  arrange(District_code.x) %>%
  select(District_code.x, Dist_names, everything())
View(m1)

# Load and preprocess the third dataset
data_3 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_Districtwise Consumed load_codes.xlsx", sheet = "2022-23",col_names = FALSE) %>%
  
colnames(data_3)
  slice(-76) %>%
  select(2:8) 
  colnames(data_3)= c("district_names","district_code","kw1",
                       "kw2","kw3", "kw4", "kw5")
  mutate(rural_elec = 'R_Number of Domestic Electricity connection having different consumption load 1 KW' + 'R_Number of Domestic Electricity connection having different consumption load 2 KW' + 'R_Number of Domestic Electricity connection having different consumption load 3 KW' + 'R_Number of Domestic Electricity connection having different consumption load 4 KW' + 'R_Number of Domestic Electricity connection having different consumption load 5 KW')
view(data_3)

data_3 <- data_3 %>%
  mutate(across(starts_with("R_Number"), as.numeric)) %>%
  mutate(rural_elec = 
           `R_Number of Domestic Electricity connection having different consumption load 1 KW` + 
           `R_Number of Domestic Electricity connection having different consumption load 2 KW` + 
           `R_Number of Domestic Electricity connection having different consumption load 3 KW` + 
           `R_Number of Domestic Electricity connection having different consumption load 4 KW` + 
           `R_Number of Domestic Electricity connection having different consumption load 5 KW`
  )

# Merge the third dataset
m2 <- m1 %>%
  left_join(data_3, by = c("District_code.x" = "district_code")) %>%
  select(District_code.x, Dist_names, rural_elec, everything())

# Load and preprocess the fourth dataset
data_4 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_RATION CARD_codes.xlsx", sheet = "Sheet1") %>%
  select(3, 2, 4:9) %>%
  select(district_code = 1, district_names = 2, rural_tot_ration = 7)

# Merge the fourth dataset
m3 <- m2 %>%
  left_join(data_4, by = c("District_code.x" = "district_code")) %>%
  select(-6) %>%
  slice(-9)

# Calculate correlation matrix and save to Excel
y_x <- m3 %>% select(3:6)
corr_mat <- cor(y_x) %>% as.data.frame()
write_xlsx(corr_mat, 'C:/Users/SOSU/Documents/MPC_Proj/eblups/rural_correlation_matrix.xlsx')



                          ######################################## Rural ###############################################


rm(list=ls())
# Load necessary libraries
library(sae)
library(dplyr)
library(readxl)

data <- read.csv("~/merged_data_all_R.csv")

View(data)

data_2 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx")

View(data_2)

data_var_est_all = cbind(data, data_2)

write.csv(data_var_est_all, "data_var_est_all.csv")

data_var_est_all <- read.csv("~/data_var_est_all_R_U.csv")

View(data_var_est_all)

names(data_var_est_all)

eblup_est <- eblupFH(data_var_est_all$Rural_MPCE ~ data_var_est_all$RURAL_Divyang_Pension, data_var_est_all$Rural_MPSE_Var, method = "ML", MAXITER = 100, PRECISION = 0.0001)

eblup_est

mse_eblup_est <- mseFH(data_var_est_all$Rural_MPCE ~data_var_est_all$RURAL_Divyang_Pension, data_var_est_all$Rural_MPSE_Var, method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_eblup_est 

mse_eblup_est$est$eblup

mse_eblup_est$mse

eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
eblup_cv

#names()

data_mat <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP dist Longitude and Latitude (in degrees).xlsx")

View(data_mat)

longitudes_radians <- data_mat$Longitude * (pi / 180)
latitudes_radians <- data_mat$Latitude * (pi / 180)

# Print the converted values
print(longitudes_radians)
print(latitudes_radians)

# Radius of the Earth in kilometers
r <- 6371

# Initialize a matrix to store distances
distance_matrix <- matrix(0, nrow = 75, ncol = 75)

# Calculate distances using the Haversine formula
for (i in 1:75) {
  for (j in 1:75) {
    if (i != j) {  # Skip the distance for the same point
      phi_i <- latitudes_radians[i]
      lambda_i <- longitudes_radians[i]
      phi_j <- latitudes_radians[j]
      lambda_j <- longitudes_radians[j]
      
      # Haversine formula
      d_ij <- 2 * r * asin(sqrt(sin((phi_j - phi_i) / 2)^2 + 
                                  cos(phi_i) * cos(phi_j) * 
                                  sin((lambda_j - lambda_i) / 2)^2))
      
      # Store the distance in the matrix
      distance_matrix[i, j] <- d_ij
    }
  }
}

# Print the distance matrix
print(distance_matrix)



#View(distmat_data)
########## Making rowsum 1  ##################
wmat = matrix(rep(0,75*75), ncol=75, byrow=T)
#View(wmat)

for (i in c(1:75))
{
  for(j in c(1:75))
  {
    if (i != j) {
      wmat[i,j] = 1/distance_matrix[i,j]
    }   
  }
}

View(wmat)


#wmat = distance_matrix


#proxmat = wmat
myproxmat =  wmat/rowSums(wmat)
rowSums(myproxmat)

View(myproxmat)
det(myproxmat)
myproxmat = data.frame(myproxmat)

#rural_corout = cor(cbind(data_var_est$Rural_MPCE, data_var_est$rural_elec, data_var_est$R_Total_beneficiaries, data_var_est$R_Total_Rationcard, data_var_est$RURAL_Divyang_Pension))

#rural_corout

#write.csv(rural_corout,"rural_corout_5by5.csv")

#library(car)

#lm1 = lm(data_var_est$Rural_MPCE ~  data_var_est$rural_elec+ data_var_est$R_Total_beneficiaries+ data_var_est$R_Total_Rationcard+ data_var_est$RURAL_Divyang_Pension)

#summary(lm1)

#vif(lm1)

#lm2 = lm(data_var_est$Rural_MPCE ~  data_var_est$rural_elec + data_var_est$RURAL_Divyang_Pension)

#summary(lm2)

#vif(lm2)

#lm3 = lm(data_var_est$Rural_MPCE ~ data_var_est$RURAL_Divyang_Pension)

#summary(lm3)






seblup_est <- eblupSFH(data_var_est_all$Rural_MPCE ~ data_var_est_all$RURAL_Divyang_Pension, data_var_est_all$Rural_MPSE_Var, proxmat=myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

seblup_est


mse_seblup_est <- mseSFH(data_var_est_all$Rural_MPCE ~ data_var_est_all$RURAL_Divyang_Pension, data_var_est_all$Rural_MPSE_Var, proxmat=myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_seblup_est 


mse_seblup_est$est$eblup

mse_seblup_est$mse

seblup_cv = (sqrt(mse_seblup_est$mse)/(mse_seblup_est$est$eblup))*100.0
seblup_cv

#names(merged)

Rural_all_out  = cbind(data_var_est_all$District_names, data_var_est_all$District_code, data_var_est_all$Rural_MPCE, mse_eblup_est$est$eblup, mse_seblup_est$est$eblup, data_var_est_all$Rural_MPCE_RSE, eblup_cv, seblup_cv)

View(Rural_all_out)

write.csv(Rural_all_out, "Rural_all_out_allML.csv")

#matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")


eblup_rural_out  = cbind(data$District_names.x, data$District_code,  merged$Rural_MPCE, mse_eblup_est$est$eblup, merged$Rural_MPSE_Var, mse_eblup_est$mse, merged$Rural_MPCE_RSE, eblup_cv)

View(eblup_rural_out)

write.csv(eblup_rural_out , "Rural_MPCE_eblup.csv")


matplot(merged$District_code, 
        cbind(merged$Rural_MPCE_RSE, eblup_cv), 
        col = c(1, 2), 
        type = "b", 
        pch = c("*", "#"),  # Different point types for each line
        lty = 1:2,  # Different line types for each line
        xlab = "District Code", 
        ylab = "Values", 
        main = "Rural MPCE RSE and EBLUP CV by District Code")



####################################################  Urban  ################################################

#a= 1.104926e+04 + ((1.289117e-01)*125)
#a



rm(list=ls())
# Load necessary libraries
library(sae)
library(dplyr)
library(readxl)

data <- read.csv("~/merged_data_all_U.csv")


View(data)

data_2 <- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx", sheet ="Method 1")

View(data_2)

data_var_est = cbind(data, data_2)
View(data_var_est)

#write.csv(data_var_est, "data_var_est.csv")

dt <- read.csv("C:/Users/SOSU/Documents/data_var_est_Urban.csv")



names(data_var_est)

sum(is.na(dt$Urban_MPCE))
sum(is.na(dt$URBAN_Divyang_Pension)) 

# Check for NA values
#sum(is.na(data_var_est$Urban_MPCE))  # Count of NA values in Urban_MPCE
#sum(is.na(data_var_est$URBAN_Divyang_Pension))  # Count of NA values in URBAN_Divyang_Pension
#sum(is.na(data_var_est$Urban_MPCE_RSE))  # Count of NA values in Urban_MPCE_RSE

#data_var_est_clean <- data_var_est[!is.na(data_var_est$Urban_MPCE), ]

#data_var_est_clean$Urban_MPCE <- as.numeric(as.character(data_var_est_clean$Urban_MPCE))
#data_var_est_clean$URBAN_Divyang_Pension <- as.numeric(as.character(data_var_est_clean$URBAN_Divyang_Pension))
#data_var_est_clean$Urban_MPCE_RSE <- as.numeric(as.character(data_var_est_clean$Urban_MPCE_RSE))

eblup_est <- eblupFH(dt$Urban_MPCE[-50] ~ dt$URBAN_Divyang_Pension[-50], dt$Urban_MPCE_Var[-50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

eblup_est

mse_eblup_est <- mseFH(dt$Urban_MPCE[-50] ~ dt$URBAN_Divyang_Pension[-50], dt$Urban_MPCE_Var[-50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_eblup_est 

mse_eblup_est$est$eblup

mse_eblup_est$mse

eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
eblup_cv

a <- 4094.5259537  +( 0.1547305*125)
a


#eblup

#names()

data_mat <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP dist Longitude and Latitude (in degrees).xlsx")

#View(data_mat)

longitudes_radians <- data_mat$Longitude * (pi / 180)
latitudes_radians <- data_mat$Latitude * (pi / 180)

# Print the converted values
print(longitudes_radians)
print(latitudes_radians)

# Radius of the Earth in kilometers
r <- 6371

# Initialize a matrix to store distances
distance_matrix <- matrix(0, nrow = 75, ncol = 75)

# Calculate distances using the Haversine formula
for (i in 1:75) {
  for (j in 1:75) {
    if (i != j) {  # Skip the distance for the same point
      phi_i <- latitudes_radians[i]
      lambda_i <- longitudes_radians[i]
      phi_j <- latitudes_radians[j]
      lambda_j <- longitudes_radians[j]
      
      # Haversine formula
      d_ij <- 2 * r * asin(sqrt(sin((phi_j - phi_i) / 2)^2 + 
                                  cos(phi_i) * cos(phi_j) * 
                                  sin((lambda_j - lambda_i) / 2)^2))
      
      # Store the distance in the matrix
      distance_matrix[i, j] <- d_ij
    }
  }
}

# Print the distance matrix
print(distance_matrix)



#View(distmat_data)
########## Making rowsum 1  ##################
wmat = matrix(rep(0,75*75), ncol=75, byrow=T)
#View(wmat)

for (i in c(1:75))
{
  for(j in c(1:75))
  {
    if (i != j) {
      wmat[i,j] = 1/distance_matrix[i,j]
    }   
  }
}

View(wmat)


#wmat = distance_matrix


#proxmat = wmat
myproxmat =  wmat/rowSums(wmat)
rowSums(myproxmat)

View(myproxmat)
det(myproxmat)
myproxmat = data.frame(myproxmat)

#rural_corout = cor(cbind(data_var_est$Rural_MPCE, data_var_est$rural_elec, data_var_est$R_Total_beneficiaries, data_var_est$R_Total_Rationcard, data_var_est$RURAL_Divyang_Pension))

#rural_corout

#write.csv(rural_corout,"rural_corout_5by5.csv")

#library(car)

#lm1 = lm(data_var_est$Rural_MPCE ~  data_var_est$rural_elec+ data_var_est$R_Total_beneficiaries+ data_var_est$R_Total_Rationcard+ data_var_est$RURAL_Divyang_Pension)

#summary(lm1)

#vif(lm1)

#lm2 = lm(data_var_est$Rural_MPCE ~  data_var_est$rural_elec + data_var_est$RURAL_Divyang_Pension)

#summary(lm2)

#vif(lm2)

#lm3 = lm(data_var_est$Rural_MPCE ~ data_var_est$RURAL_Divyang_Pension)

#summary(lm3)






seblup_est <- eblupSFH(dt$Urban_MPCE[-50] ~ dt$URBAN_Divyang_Pension[-50], dt$Urban_MPCE_Var[-50], proxmat=myproxmat[-50, -50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

seblup_est


mse_seblup_est <- mseSFH(dt$Urban_MPCE[-50] ~ dt$URBAN_Divyang_Pension[-50], dt$Urban_MPCE_Var[-50], proxmat=myproxmat[-50, -50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_seblup_est 


mse_seblup_est$est$eblu

mse_seblup_est$mse

seblup_cv = (sqrt(mse_seblup_est$mse)/(mse_seblup_est$est$eblup))*100.0
seblup_cv

Urban_all_out  = cbind(dt$District_names[-50], dt$District_code[-50], dt$Urban_MPCE[-50], mse_eblup_est$est$eblup[-50], mse_seblup_est$est$eblup[-50], eblup_cv[-50], seblup_cv[-50])

View(Urban_all_out)

b <-  4196.9324951 + (0.1306973*125)
b



write.csv(Urban_all_out, "Urban_all_out_allML.csv")

#names(merged)


######################  calculating cv for Srawasti_Urban  #######################

x_bar_pension <- mean(dt$URBAN_Divyang_Pension[-50])
x_bar_pension

sigma_sq_pension <-  var(dt$URBAN_Divyang_Pension[-50])
sigma_sq_pension

sigma_sq_MPCE <- var(dt$Urban_MPCE[-50])
sigma_sq_MPCE

cov_term <- 2* (dt$URBAN_Divyang_Pension[50])* (-(x_bar_pension*sigma_sq_MPCE)/ (length(dt$Urban_MPCE[-50]) * sigma_sq_pension))
cov_term

first_term_FH <- (204.46591722)^2
second_term_FH <- (dt$URBAN_Divyang_Pension[50])^2 * ((0.06986752)^2)

v_hat_srawasti_FH <- first_term_FH  + second_term_FH + cov_term
v_hat_srawasti_FH

cv_shrawasti_FH = (sqrt(v_hat_srawasti_FH)/(4113.867)) *100
cv_shrawasti_FH



first_term_SFH <- (519.36057118)^2
second_term_SFH <- (dt$URBAN_Divyang_Pension[50])^2 * ((0.06685303)^2)

v_hat_srawasti_SFH <- first_term_SFH  + second_term_SFH + cov_term
v_hat_srawasti_SFH

cv_shrawasti_SFH = (sqrt(v_hat_srawasti_SFH)/(4213.27)) *100

cv_shrawasti_SFH



                ############################################  U,RSE, MSE Calculation #######################################


########## U #############

rm(list=ls())
library(readxl)
library(dplyr)
library(writexl)
data_R <- read_excel("C:/Users/SOSU/Downloads/Merged_mycommondata_mult.xlsx", sheet= "Rural")
y_hat_R <-data_R$total_lvl14
x_hat_R <- data_R$hh_size
R_hat_R <- 3191
U_Rural <- y_hat_R- (R_hat_R*x_hat_R)
U_Rural

result <- cbind(data_R, U_Rural)
write_xlsx(result, "C:/Users/SOSU/Downloads/Merged_mycommondata_mult_U_Rural.xlsx")


data_U <- read_excel("C:/Users/SOSU/Downloads/Merged_mycommondata_mult.xlsx", sheet= "Urban")
y_hat_U <-data_U$total_lvl14
x_hat_U <- data_U$hh_size
R_hat_U <- 5040 
U_Urban <- y_hat_U- (R_hat_U*x_hat_U)
U_Urban

result <- cbind(data_U, U_Urban)
write_xlsx(result, "C:/Users/SOSU/Downloads/Merged_mycommondata_mult_U_Urban.xlsx")


############  MSE for Rural #####################


rm(list=ls())
library(readxl)
library(writexl)
library(dplyr)

data_R <- read_excel("C:/Users/SOSU/Downloads/Merged_mycommondata_mult_U_Rural.xlsx", sheet= "Rural")
View(data_R)
names(data_R)

u_Rural_sum = data_R %>% group_by(my_fod_sub_region,Sector,my_district,
                          my_strm, my_sstrm, my_panel,
                          my_ss,my_fsu_ser,
                          my_sss) %>% summarise(
                            sum_U_R = sum(U_Rural))
View(u_Rural_sum)


u_Rural_bar = data_R %>% group_by(my_fod_sub_region,Sector,my_district,
                                  my_strm, my_sstrm, my_panel,
                                  my_ss,my_fsu_ser,
                                  my_sss) %>% summarise(
                                    Ubar_R = mean(U_Rural))
View(u_Rural_bar)

u_Rural_var = data_R %>% group_by(my_fod_sub_region,Sector,my_district,
                                  my_strm, my_sstrm, my_panel,
                                  my_ss,my_fsu_ser,
                                  my_sss) %>% summarise(
                                    Uvar_R = var(U_Rural))
View(u_Rural_var)


final_result_final <- read.csv("~/final_result_final.csv")
final_result_final_R <- final_result_final[final_result_final$Sector=="1", ] 
final_result_final_U <- final_result_final[final_result_final$Sector=="2", ] 



final_result_u_sum_R <-  final_result_final_R %>%
  left_join(u_Rural_sum,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))


View(final_result_u_sum_R)

final_result_u_bar_R <-  final_result_u_sum_R %>%
  left_join(u_Rural_bar,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

View(final_result_u_bar_R)

final_result_u_combined_R <-  final_result_u_bar_R %>%
  left_join(u_Rural_var,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

View (final_result_u_combined_R)
write.csv(final_result_u_combined_R, "final_result_final_u_X_Y_combined_Rural.csv")


############  MSE for Urban #####################


rm(list=ls())
library(readxl)
library(writexl)
library(dplyr)

data_U <- read_excel("C:/Users/SOSU/Downloads/Merged_mycommondata_mult_U_Urban.xlsx")
View(data_U)
names(data_U)

u_Urban_sum = data_U %>% group_by(my_fod_sub_region,Sector,my_district,
                                  my_strm, my_sstrm, my_panel,
                                  my_ss,my_fsu_ser,
                                  my_sss) %>% summarise(
                                    sum_U_U = sum(U_Urban))
View(u_Urban_sum)


u_Urban_bar = data_U %>% group_by(my_fod_sub_region,Sector,my_district,
                                  my_strm, my_sstrm, my_panel,
                                  my_ss,my_fsu_ser,
                                  my_sss) %>% summarise(
                                    Ubar_U = mean(U_Urban))
View(u_Urban_bar)

u_Urban_var = data_U %>% group_by(my_fod_sub_region,Sector,my_district,
                                  my_strm, my_sstrm, my_panel,
                                  my_ss,my_fsu_ser,
                                  my_sss) %>% summarise(
                                    Uvar_U = var(U_Urban))
View(u_Urban_var)


final_result_final <- read.csv("~/final_result_final.csv")
final_result_final_R <- final_result_final[final_result_final$Sector=="1", ] 
final_result_final_U <- final_result_final[final_result_final$Sector=="2", ] 



final_result_u_sum_U <-  final_result_final_U %>%
  left_join(u_Urban_sum,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))


View(final_result_u_sum_U)

final_result_u_bar_U <-  final_result_u_sum_U %>%
  left_join(u_Urban_bar,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

View(final_result_u_bar_U)

final_result_u_combined_U <-  final_result_u_bar_U %>%
  left_join(u_Urban_var,   
            by = c("my_fod_sub_region" = "my_fod_sub_region", 
                   "Sector" = "Sector", 
                   "my_district" = "my_district", 
                   "my_strm" = "my_strm", 
                   "my_sstrm" = "my_sstrm", 
                   "my_panel" = "my_panel", 
                   "my_ss" = "my_ss", 
                   "my_fsu_ser" = "my_fsu_ser", 
                   "my_sss" = "my_sss"))

View (final_result_u_combined_U)
write.csv(final_result_u_combined_U, "final_result_final_u_X_Y_combined_Urban.csv")


##############  2nd term calculation  ###################

data_R <- read.csv("~/final_result_final_u_X_Y_combined_Rural.csv")
#data_U <- read.csv("~/final_result_final_u_X_Y_combined_Urban.csv") 

names(data_R)

Nst_R <- data_R$Nst
#Nst_U <- data_U$Nst

nst_R <- data_R$nst
#nst_U <- data_U$nst

View(Nst_R)

H_R <- data_R$caph
#H_U <- data_U$caph

h_R <- data_R$smallh
#h_U <- data_U$smallh

View(H_R)

D1_R <- data_R$totsd
#D1_U <- data_U$totsd

View(D1_R)

uvar_R <- data_R$uvar_R

term2 <- sum((Nst_R/nst_R)* ((H_R*D1_R)^2)* ((1/h_R)-(1/(H_R*D1_R)))* (uvar_R))
term2

X_hat <- sum(data_R$sum_x* data_R$mult)
X_hat 

term1 <- 





















# Sample data (you need to replace these with your actual data)
N <- 10  # Number of nst
nst <- 1:N  # Example nst values
nstj <- runif(N)  # Example nstj vector
H2ij <- matrix(runif(N * N), nrow = N)  # Example H2ij matrix
D1si <- runif(N)  # Example D1si vector
hij <- matrix(runif(N * N), nrow = N)  # Example hij matrix
sui <- matrix(runif(N * N), nrow = N)  # Example sui matrix

# Initialize the result
result <- 0

# Calculate the expression
for (j in 1:N) {
  for (i in 1:N) {
    term1 <- (1 / hij[i, j]) - (1 / (H2ij[i, j] * D1si[i]))
    result <- result + (nst[i] / nstj[j]) * H2ij[i, j] * D1si[i]^2 * term1 * sui[i, j]^2
  }
}

# Print the result
print(result)

















































#matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")