
########################################  Method 2    ###################################################

rm(list=ls())
library(readxl)
data<- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx", sheet = 'Method 1')
w1 <- 3097564    ####Rural
w2 <-3597201
#w1 <- 307995     ####Urban
#w2 <- 199916

RB_V_hat <-  as.numeric(data$Rural_MPSE_Var[data$District_code == 27])
View(RB_V_hat)
SP_V_hat <- as.numeric(data$Rural_MPSE_Var[data$District_code == 48])
View(SP_V_hat )
result <- (((w1^2) *RB_V_hat) +((w2^2) * SP_V_hat)) / (w1 + w2)^2
print(result)

#####################################    Method 3   #####################################################

rm(list=ls())
library(readxl)
data<- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx", sheet = 'Method 1')
RB_MPCE <- as.numeric(data$Rural_MPCE[data$District_code == 27])
SP_MPCE <- as.numeric(data$Rural_MPCE[data$District_code == 48]) 
RB_V_hat <-  as.numeric(data$Rural_MPSE_Var[data$District_code == 27])
View(RB_V_hat )
SP_V_hat <- as.numeric(data$Rural_MPSE_Var[data$District_code == 48])
View(SP_V_hat )
w1 <- (RB_V_hat)/(RB_V_hat+SP_V_hat)
w2 <- (SP_V_hat)/(RB_V_hat+SP_V_hat)
result <- (w1*SP_MPCE)+(w2*RB_V_hat)
print(result)
