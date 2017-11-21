library(tidyverse)
library(caret)
library(plspm)

annual_flux<-read_csv("../data/ars_annual_N2O.csv")

######Getting data into required formats for preprocessing ###########

#Predictor variables
flux_x<-select(annual_flux, -Unit_ID, -site, -series, -town, -avg_N2O, -log_avg_N2O,-year)

#Response variable, previously determined log transformation was better than none
flux_y<-select(annual_flux, log_avg_N2O)

#Not everything likes tibbles
flux_xish<-as.data.frame(unclass(flux_x))

#Centering and scaling, removing rows with NA
flux_x_processed<-preProcess(flux_xish, method = c("BoxCox", "center", "scale"), na.remove = TRUE)
flux_x_pro_mat<-predict(flux_x_processed, flux_xish) 

#Put the x and y back together for PLS
flux_preproc<-cbind(flux_y, flux_x_pro_mat)


#########  PSLPM-specific work ##################
#reorder dataframe in blocks 
flux_inblocks<-flux_preproc[c("theta_s", "clay", "silt_f_psa", "sand", "Ks",
               "silt_c_psa", "silt", "theta_r", "oc", "estimated_om",
               "n_tot", "estimated_c_to_n", "cold_sum", "sum_rad", 
               "sum_tmax", "precip_sum", "snow_sum", "ex_mg", 
               "cec7", "bs7", "ex_k_saturation", "cec82", "ex_k",
               "ph_h2o", "ex_ca", "log_avg_N2O")]  

#Extra set with no NAs in case they are a problem
flux_inblocks_nona<-flux_inblocks%>%select(-cec7, -cec82, -bs7, 
                        -ex_ca, -ex_mg, -ex_k, -n_tot, -estimated_c_to_n, -ex_k_saturation)

# rows of inner model matrix  
chem <- c(0,0,0,0,0)
water<- c(0,0,0,0,0)
sub  <- c(1,1,0,0,0)
weath<- c(0,0,0,0,0)
n2o  <- c(1,1,1,1,0)

flux_path<-rbind(chem, water, sub, weath, n2o)
colnames(flux_path) = rownames(flux_path)

innerplot(flux_path) #Sweet visual

#Blocks with a few NAs
flux_blocks <- list(1:8, 9:12, 13:17, 18:25, 26)

#Blocks with no NAs
flux_blocks_nona <- list(1:8, 9:10, 11:15, 16, 17)

flux_modes2 <- c("A", "A", "A", "A", "B")  

my_pls <- plspm(flux_inblocks, flux_path, flux_blocks, modes = flux_modes2)
