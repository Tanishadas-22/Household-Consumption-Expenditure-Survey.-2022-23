rm(list=ls())
library(dplyr)
library(readxl)
library(writexl)
library(sae)
#install.packages("glmnet")
#install.packages("car")
data_pension <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP_DIVYANGJAN PENSION_codes.xlsx", sheet ="2022-23")
data_directestimates <- read_excel("C:/Users/SOSU/Downloads/SPSS File/Original_New_UP_districts_mpce_rse_output.xlsx", sheet ="Method 1")
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

#################################  Dist calculation    ########################

#data <- read_excel("C:/Users/SOSU/Downloads/UP dist X,Y coordinates.xlsx")
#View(data)

# Print the data to check if it is read correctly
#print("Districts Data:")
#print(data)

# Assuming the data frame has columns named 'X' and 'Y'
# Extract the coordinates
#coordinates <- data[, c("xcord", "ycord")]


# Calculate the Euclidean distance between the districts
#euclidean_distance <- dist(coordinates, method = "euclidean")

# Convert the distance object to a matrix
#distance_matrix <- as.matrix(euclidean_distance)

# Print the Euclidean distance matrix
#print("Euclidean Distance Matrix:")
#print(distance_matrix)
#View(distance_matrix)

#distmat_data <- read_excel("C:/Users/SOSU/Downloads/Distance_new.xlsx")
#View(distmat_data)

#ncol(distance_matrix)
#nrow(distance_matrix)
#dim(distance_matrix)


################ ######################

#################################  Dist calculation    ########################  Trial 
rm(list=ls())
library(readxl)

# Read the data
data <- read_excel("C:/Users/SOSU/Downloads/SPSS File/UP dist Longitude and Latitude (in degrees).xlsx")

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
wmat = matrix(rep(1,75*75), ncol=75, byrow=T)
#View(wmat)

for (i in c(1:75))
{
  for(j in c(1:75))
  {
    if (i != j) {
      wmat[i,j] = distance_matrix[i,j]
    }   
  }
}

View(wmat)


wmat = distance_matrix


#proxmat = wmat
myproxmat =  wmat/rowSums(wmat)
rowSums(myproxmat)

View(myproxmat)
det(myproxmat)
myproxmat = data.frame(myproxmat)

        ######################### Spatial ######################

#temp_proxmat = myproxmat[29:75,29:75] ; temp_proxmat = temp_proxmat/ rowSums(temp_proxmat); rowSums(temp_proxmat)


seblup_est <- eblupSFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var,proxmat=myproxmat, method = "ML", MAXITER = 100, PRECISION = 0.0001)

seblup_est


mse_eblup_est <- mseFH(merged$Rural_MPCE ~ merged$RURAL, merged$Rural_MPSE_Var, proxmat=myproxmat, method = "FH", MAXITER = 100, PRECISION = 0.0001)

mse_eblup_est 



# Check the rank of the matrix
rank_matrix <- qr(wmat)$rank
print(rank_matrix)


install.packages("sf")
?sf()

det(wmat)

str(merged$Urban_MPCE)
str(merged$Urban_MPCE_Var)
str(merged$U_Antyodoya_beneficiaries)

merged$Urban_MPCE <- as.numeric(as.character(merged$Urban_MPCE))            # To transform char to num
merged$Urban_MPCE_Var <- as.numeric(as.character(merged$Urban_MPCE_Var))    # To transform char to num

# Remove rows with NA values in the relevant columns
cleaned_data <- merged %>%
  filter(!is.na(U_Antyodoya_beneficiaries) & !is.na(Urban_MPCE) & !is.na(Urban_MPCE_Var))

# Recalculate the proximity matrix
#wmat_cleaned <- wmat[cleaned_data$District_code, cleaned_data$District_code]

# Fit the model with cleaned data
seblup_est <- eblupSFH(cleaned_data$Urban_MPCE[21:49] ~ cleaned_data$U_Antyodoya_beneficiaries[21:49], 
                       cleaned_data$Urban_MPCE_Var[21:49], 
                       proxmat=myproxmat[21:49,21:49], 
                       method = "ML",  
                       MAXITER = 1000, 
                       PRECISION = 0.0001)
seblup_est


seblup_est <- eblupSFH(merged$Urban_MPCE[-50] ~ merged$U_Antyodoya_beneficiaries[-50], merged$Urban_MPCE_Var[-50], proxmat=wmat[-50,-50], method = "ML",  MAXITER = 1000, PRECISION = 0.0001)

seblup_est

seblup_est <- eblupSFH(merged$Rural_MPCE[1:75] ~  merged$R_Total_Rationcard[1:75], merged$Rural_MPSE_Var[1:75], proxmat=wmat[1:75, 1:75], method = "ML",  MAXITER = 1000, PRECISION = 0.0001)

seblup_est

det(wmat[1:21, 1:21])
det(wmat[22:75, 22:75])

det(wmat[-21,-21])

seblup_est <- eblupSFH(merged$Rural_MPCE[-21] ~  merged$R_Antyodoya_beneficiaries[-21], merged$Rural_MPSE_Var[-21], proxmat=wmat[-21, -21], method = "ML",  MAXITER = 1000, PRECISION = 0.0001)

# Check for zero distances
if (any(distance_matrix == 0)) {
  print("There are zero distances in the distance matrix.")
}

# Check eigenvalues of the proximity matrix
eigen_values <- eigen(myproxmat)$values
print(eigen_values)

library(glmnet)

# Prepare the data
x <- model.matrix(Rural_MPCE ~ ., data = merged)[, -1]  # Exclude intercept
y <- merged$Rural_MPCE

# Fit Ridge regression model
ridge_model <- glmnet(x, y, alpha = 0)  # alpha = 0 for Ridge

# Cross-validation to find the best lambda
cv_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_ridge$lambda.min

# Make predictions
predictions <- predict(ridge_model, s = best_lambda, newx = x)


mse_seblup_est <- mseFH(merged$Rural_MPCE ~ merged$R_Antyodoya_beneficiaries, merged$Rural_MPSE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)

mse_seblup_est 

mse_eblup_est$est$eblup

mse_eblup_est$mse

eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
eblup_cv

names(merged)

eblup_rural_out  = cbind(merged$District_code, merged$District_names.x, merged$Rural_MPCE, mse_eblup_est$est$eblup, merged$Rural_MPSE_Var, mse_eblup_est$mse, merged$Rural_MPCE_RSE, eblup_cv)

View(eblup_rural_out)
setwd("C:/Users/SOSU/Downloads/SPSS File")
write.csv(eblup_rural_out , "Rural_MPCE_eblup.csv")
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

# Convert columns to numeric if they are not already
merged$Urban_MPCE <- as.numeric(as.character(merged$Urban_MPCE))
merged$U_Total_Rationcard <- as.numeric(as.character(merged$U_Total_Rationcard))
merged$Urban_MPCE_Var <- as.numeric(as.character(merged$Urban_MPCE_Var))
merged$Urban_MPCE_RSE <- as.numeric(as.character(merged$Urban_MPCE_RSE))

# Check for NA values
na_count_urban_mpce <- sum(is.na(merged$Urban_MPCE))
na_count_ration_card <- sum(is.na(merged$U_Total_Rationcard))

# Print NA counts
cat("NA count in Urban_MPCE:", na_count_urban_mpce, "\n")
cat("NA count in U_Total_beneficiaries:", na_count_ration_card, "\n")

# Calculate correlation, using complete cases only
correlation <- cor(merged$Urban_MPCE, merged$U_Total_Rationcard, use = "complete.obs")
print(correlation)


eblup_est <- eblupFH(direct_female ~ all_atleasthh + wall_strong_oldp,
                     #                    se_direct^2, method = “REML”, MAXITER = 100, PRECISION = 0.0001)
                     
                     #eblup_est <- eblupFH(merged$Urban_MPCE ~ merged$U_Total_beneficiaries, merged$Urban_MPCE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001, na.rm = T)
                     
                     #eblup_est
                     
                     
                     cleaned_merged = na.omit(merged)
                     nrow(cleaned_merged)
                     View(cleaned_merged)
                     cleaned_merged = data.frame(cleaned_merged)
                     View(cleaned_merged)
                     
                     names(cleaned_merged)
                     
                     summary(cleaned_merged)
                     
                     
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Patra_Rationcard)
                     #[1] 0.2298984
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Antyodaya_Rationcard)
                     #[1] -0.1132839
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Patra_beneficiaries)
                     #[1][1] 0.2298148
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Antyodoya_beneficiaries)
                     #[1] [1] -0.1210248
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Total_beneficiaries)
                     #[1] 0.2237486
                     cor(cleaned_merged$Urban_MPCE, cleaned_merged$U_Total_Rationcard)
                     #[1]0.2211851
                     
                     eblup_est <- eblupFH(cleaned_merged$Urban_MPCE ~ cleaned_merged$U_Patra_Rationcard, cleaned_merged$Urban_MPCE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)
                     
                     eblup_est
                     
                     eblup_est$fit$estcoef
                     
                     eblup_est$fit$estcoef$beta
                     
                     eblup_est$fit$estcoef$beta[1]  + eblup_est$fit$estcoef$beta[2] * 5279  #--- For Shravasti : U_Patra_Rationcard = 5279 ----
                     #[1] 4137.514
                     
                     mse_eblup_est <- mseFH(cleaned_merged$Urban_MPCE ~ cleaned_merged$U_Patra_Rationcard, cleaned_merged$Urban_MPCE_Var, method = "FH", MAXITER = 100, PRECISION = 0.0001)
                     
                     mse_eblup_est
                     
                     
                     mse_eblup_est$est$eblup
                     
                     mse_eblup_est$mse
                     
                     eblup_cv = (sqrt(mse_eblup_est$mse)/(mse_eblup_est$est$eblup))*100.0
                     eblup_cv
                     
                     names(cleaned_merged)
                     
                     cbind(cleaned_merged$Urban_MPCE_RSE, eblup_cv)
                     
                     eblup_urban_out  = cbind(cleaned_merged$District_code, cleaned_merged$District_names.x, cleaned_merged$Urban_MPCE, mse_eblup_est$est$eblup, cleaned_merged$Urban_MPCE_Var, mse_eblup_est$mse, cleaned_merged$Urban_MPCE_RSE, eblup_cv)
                     
                     View(eblup_urban_out)
                     setwd("C:/Users/SOSU/Downloads/SPSS File")
                     write.csv(eblup_urban_out , "Urban_MPCE_eblup.csv")
                     #matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")
                     
                     #matplot(merged$District_code, cbind(merged$Rural_MPCE_RSE, eblup_cv), col=c(1,2), type="b")
                     
                     matplot(cleaned_merged$District_code, 
                             cbind(cleaned_merged$Urban_MPCE_RSE, eblup_cv), 
                             col = c(1, 2), 
                             type = "b", 
                             pch = c("*", "#"),  # Different point types for each line
                             lty = 1:2,  # Different line types for each line
                             xlab = "District Code", 
                             ylab = "Values", 
                             main = "Urban MPCE RSE and EBLUP CV by District Code")
                     
                     # Add a legend to the plot
                     legend("topright", 
                            legend = c("Urban MPCE RSE", "EBLUP CV"), 
                            col = c(1, 2), 
                            pch = c("*", "#"), 
                            lty = 1:2)
                     
                     ?eblupSFH()  
                     
                     
                     
                     if(!require('ape')) {
                       install.packages('ape')
                       library('ape')
                     }     
                     Moran.I(merged$Rural_MPCE, wmat, alt = "g")
                     
                     
                     
                     tr <- rtree(30)
                     x <- rnorm(30)
                     ## weights w[i,j] = 1/d[i,j]:
                     w <- 1/cophenetic(tr)
                     ## set the diagonal w[i,i] = 0 (instead of Inf...):
                     diag(w) <- 0
                     Moran.I(x, w)
                     Moran.I(x, w, alt = "l")
                     Moran.I(x, w, alt = "g")
                     Moran.I(x, w, scaled = TRUE) # usualy the same