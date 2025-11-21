                         


                             ##################################### MPCE  ##########################################################

rm(list = ls())

# Load libraries
library(haven)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(boot)                            

data2 <- read_dta("~/HCES 2022-23/HCES DTA FILE/LEVEL - 03.dta")
data3 <- read_dta("~/HCES 2022-23/HCES DTA FILE/LEVEL - 14 (Section  A1,B1 & C1).dta")
data4 <- read_dta("~/HCES 2022-23/HCES DTA FILE/LEVEL - 15 (Section 1.dta")

#View(data3)

create_hhid_c_mult <- function(df, multiplier_col) {
  df %>%
    mutate(
      common_ID = str_c(survey_name, year, fsu,sector,state,nss_region,district,stratum,sub_stratum,panel,sub_sample,fod_subregion,b1q1pt7,b1q1pt10,b1q1pt11, b1q1pt12, sep = ""),
      c_mult = as.numeric(get(multiplier_col)) / 100
    )
}

data2 <- create_hhid_c_mult(data2, "mult")
data3 <- create_hhid_c_mult(data3, "mult")
data4 <- create_hhid_c_mult(data4, "mult")

common_data <- data2 %>%
  select(common_ID, b2q2pt1, c_mult) %>%
  rename(hh_size = b2q2pt1)

cd_data <- data4 %>%
  filter(questionaire_no %in% c("C", "D")) %>%
  select(common_ID, ba2b2c2q9, questionaire_no) %>%
  pivot_wider(
    names_from = questionaire_no,
    values_from = ba2b2c2q9,
    names_prefix = "hh_size_"
  )

common_data <- common_data %>%
  left_join(cd_data, by = "common_ID")
names(common_data)

visit3_data <- data4 %>%
  filter(questionaire_no == "D") %>%
  distinct(common_ID, .keep_all = TRUE) %>%
  select(common_ID, c_mult)


common_data <- common_data %>%
  left_join(visit3_data, by = "common_ID", suffix = c("", "_visit3"))


lvl14 <- data3 %>%
  left_join(
    common_data %>% select(common_ID, hh_size, hh_size_C, hh_size_D),
    by = "common_ID"
  ) %>%
  mutate(
    Value = as.numeric(ba1b1c1_3),
    Value = case_when(
      ba1b1c1_2 == "539" ~ 0,
      ba1b1c1_2 %in% c(409, 419, 899, 379, 399, 389, 629, 609, 619,
                       599, 579, 559, 569, 639, 649, "099") ~ Value * (30 / 365),
      ba1b1c1_2 %in% c(309, 319, 329, 169, 219, 239, 249,
                       199, 189, 269, 279, 289, 299) ~ Value * (30 / 7),
      TRUE ~ Value
    )
  )

lvl14 <- lvl14 %>%
  mutate(
    hh_size = as.numeric(hh_size),
    hh_size_C = as.numeric(hh_size_C),
    hh_size_D = as.numeric(hh_size_D),
    Value = case_when(
      questionaire_no == "C" ~ Value * hh_size / hh_size_C,
      questionaire_no == "D" ~ Value * hh_size / hh_size_D,
      TRUE ~ Value
    )
  )

combined_data14 <- lvl14 %>%
  group_by(common_ID) %>%
  summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")

common_data <- common_data %>%
  left_join(combined_data14, by = "common_ID") %>%
  mutate(
    total_lvl14 = as.numeric(Value),
    MPCE_lvl14 = total_lvl14 / hh_size
  )

print(sum(common_data$hh_size * common_data$c_mult_visit3, na.rm = TRUE))
print(sum(common_data$total_lvl14 * common_data$c_mult_visit3, na.rm = TRUE))

common_data <- common_data %>%
  left_join(data4 %>% select(common_ID, state), by = "common_ID") %>%
  left_join(data4 %>% select(common_ID, sector), by = "common_ID")

#my_common_id = common_data$Common_ID
district = rep("", length(common_data$common_ID))
district = substr(common_data$common_ID, 20, 21)
district = substr(common_data$common_ID, 20, 21)              #-- district code appears in 20-21 column
# my_district = as.numeric(my_district)
# View(my_district)
#length(my_district)
#View(my_district)
common_data = data.frame(cbind(common_data, district))
View(common_data_t)


common_data_t <- common_data %>% filter(state == "09")

print(sum(common_data_t$hh_size_D[common_data_t$sector == "1"] *
            common_data_t$c_mult_visit3[common_data_t$sector == "1"], na.rm = TRUE))

print(sum(common_data_t$hh_size_D[common_data_t$sector == "2"] *
            common_data_t$c_mult_visit3[common_data_t$sector == "2"], na.rm = TRUE))

df_r <- common_data %>% filter(sector == "1")
df_u <- common_data %>% filter(sector == "2")

df_r_up <- common_data_t %>% filter(sector == "1")
df_u_up <- common_data_t %>% filter(sector == "2")

print(sum(df_r$total_lvl14 * df_r$c_mult_visit3, na.rm = TRUE))
print(sum(df_u$total_lvl14 * df_u$c_mult_visit3, na.rm = TRUE))

print(sum(df_r$total_lvl14 * df_r$c_mult_visit3, na.rm = TRUE) /
        sum(df_r$hh_size * df_r$c_mult_visit3, na.rm = TRUE))

print(sum(df_u$total_lvl14 * df_u$c_mult_visit3, na.rm = TRUE) /
        sum(df_u$hh_size * df_u$c_mult_visit3, na.rm = TRUE))

print(sum(df_r_up$total_lvl14 * df_r_up$c_mult_visit3, na.rm = TRUE) /
        sum(df_r_up$hh_size * df_r_up$c_mult_visit3, na.rm = TRUE))

print(sum(df_u_up$total_lvl14 * df_u_up$c_mult_visit3, na.rm = TRUE) /
        sum(df_u_up$hh_size * df_u_up$c_mult_visit3, na.rm = TRUE))



              

            