rm(list=ls())
library(dplyr)
library(readxl)
library(writexl)
library(sae)
#install.packages("sae")
#install.packages("glmnet")
#install.packages("car")
data_pension <- read_excel("C:/Users/TANISHA/Downloads/UP_DIVYANGJAN PENSION_codes.xlsx", sheet ="2022-23")
data_directestimates <- read_excel("C:/Users/TANISHA/Downloads/Original_New_UP_districts_mpce_rse_output.xlsx", sheet ="Method 1")
View(data_pension)
View(data_directestimates)

#names(data_ration)
#names(data_directestimates)
#colnames(data_ration) <- trimws(colnames(data_ration))
#colnames(data_estimates) <- trimws(colnames(data_estimates))

merged<-inner_join(data_directestimates, data_pension, by=c("District_code"))
View(merged)
#print(merged)

summary(merged)

#names(merged)

############################## eblup ############################

######## Rural  ########

#cor(merged$Rural_MPCE, merged$`R_Total_Ration card`)

eblup_est <- eblupFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)

eblup_est

mse_eblup_est <- mseFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)

mse_eblup_est 

mse_eblup_est$est$eblup

mse_eblup_est$mse

eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
eblup_cv

names(merged)

eblup_rural_out  = cbind(merged$District_code, merged$District_names, merged$Rural_MPCE, mse_eblup_est$est$eblup, merged$Rural_MPSE_Var, mse_eblup_est$mse, merged$Rural_MPCE_RSE, eblup_cv)

View(eblup_rural_out)
setwd("C:/Users/SOSU/Downloads/SPSS File")
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


######## Urban  ########

cor(merged$Rural_MPCE, merged$`R_Total_Ration card`)

merged$Urban_MPCE <- as.numeric(as.character(merged$Urban_MPCE))            # To transform char to num
merged$Urban_MPCE_Var <- as.numeric(as.character(merged$Urban_MPCE_Var)) # To transform char to num

# Check for NA values in Urban_MPCE and URBAN
sum(is.na(merged$Urban_MPCE))  # Count NA values in Urban_MPCE
sum(is.na(merged$URBAN))        # Count NA values in URBAN
sum(is.na(merged$Urban_MPCE_Var))  # Count NA values in Urban_MPCE_Var

# Remove rows with NA values in Urban_MPCE, URBAN, or Urban_MPCE_Var
merged_clean <- merged[!is.na(merged$Urban_MPCE) & !is.na(merged$URBAN) & !is.na(merged$Urban_MPCE_Var), ]

eblup_est <- eblupFH(merged_clean$Urban_MPCE ~ merged_clean$URBAN, merged_clean$Urban_MPCE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)

eblup_est


mse_eblup_est <- mseFH(merged_clean$Rural_MPCE ~ merged_clean$RURAL, merged_clean$Rural_MPSE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)

mse_eblup_est$est$eblup

mse_eblup_est$mse

eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
eblup_cv

names(merged_clean)

eblup_rural_out  = cbind(merged_clean$District_code, merged_clean$District_names, merged_clean$Urban_MPCE, mse_eblup_est$est$eblup, merged_clean$Urban_MPCE_Var, mse_eblup_est$mse, merged_clean$Urban_MPCE_RSE, eblup_cv)

View(eblup_rural_out)
setwd("C:/Users/SOSU/Downloads/SPSS File")
write.csv(eblup_rural_out , "Urban_MPCE_eblup_pension.csv")

#############################  Spatial eblup ####################


#################################  Dist calculation    ########################  Trial 
#rm(list=ls())
library(readxl)

# Read the data
data <- read_excel("C:/Users/TANISHA/Downloads/UP dist Longitude and Latitude (in degrees) (1).xlsx")

# Convert degrees to radians
longitudes_radians <- data$Longitude * (pi / 180)
latitudes_radians <- data$Latitude * (pi / 180)

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

######################### Spatial ######################

#####################################  Rural #################################

seblup_est <- eblupSFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var,proxmat=myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

seblup_est


mse_seblup_est <- mseSFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var, proxmat=myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_seblup_est 


mse_seblup_est$est$eblup

mse_seblup_est$mse

seblup_cv = (sqrt(mse_seblup_est$mse)/(mse_seblup_est$est$eblup))*100.0
seblup_cv

names(merged)

seblup_rural_out  = cbind(merged$District_code, merged$District_names, merged$Rural_MPCE, mse_seblup_est$est$eblup, merged$Rural_MPSE_Var, mse_seblup_est$mse, merged$Rural_MPCE_RSE, seblup_cv)

View(seblup_rural_out)
setwd("C:/Users/TANISHA/Downloads")
write.csv(seblup_rural_out , "Rural_MPCE_eblup_spatial.csv")
#matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")



# Add a legend to the plot
legend("topright", 
       legend = c("Rural MPCE RSE", "EBLUP CV"), 
       col = c(1, 2), 
       pch = c("*", "#"), 
       lty = 1:2)

###################################  Urban #################################

#cor(merged$Urban_MPCE, merged$`U_Total_Ration card`)
#plot(merged$Urban_MPCE, merged$`U_Total_Ration card`)

# Check the structure of the merged dataframe
str(merged)


#seblup_est <- eblupSFH(merged_clean$Urban_MPCE ~ merged_clean$URBAN, merged_clean$Urban_MPCE_Var, proxmat=temp_myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

merged$Urban_MPCE <- as.numeric(as.character(merged$Urban_MPCE))
merged$URBAN <- as.numeric(as.character(merged$URBAN))
merged$Urban_MPCE_Var <- as.numeric(as.character(merged$Urban_MPCE_Var))


seblup_est <- eblupSFH(merged$Urban_MPCE[-50] ~ merged$URBAN[-50], merged$Urban_MPCE_Var[-50],proxmat=myproxmat[-50,-50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

seblup_est


mse_seblup_est <- mseSFH(merged$Urban_MPCE[-50] ~ merged$URBAN[-50], merged$Urban_MPCE_Var[-50],proxmat=myproxmat[-50,-50], method = "ML", MAXITER = 100, PRECISION = 0.0001)

mse_seblup_est 


mse_seblup_est$est$eblup

mse_seblup_est$mse

seblup_cv = (sqrt(mse_seblup_est$mse)/(mse_seblup_est$est$eblup))*100.0
seblup_cv

names(merged)

seblup_urban_out  = cbind(merged$District_code[-50], merged$District_names[-50], merged$Urban_MPCE[-50], mse_seblup_est$est$eblup[-50], merged$Urban_MPCE_Var[-50], mse_seblup_est$mse[-50], merged$Urban_MPCE_RSE[-50], seblup_cv[-50])

View(seblup_urban_out)
setwd("C:/Users/TANISHA/Downloads")
write.csv(seblup_urban_out , "Urban_MPCE_eblup_spatial.csv")
#matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")



# Add a legend to the plot
legend("topright", 
       legend = c("Rural MPCE RSE", "EBLUP CV"), 
       col = c(1, 2), 
       pch = c("*", "#"), 
       lty = 1:2)
                     Moran.I(x, w, scaled = TRUE) # usualy the same











                     
