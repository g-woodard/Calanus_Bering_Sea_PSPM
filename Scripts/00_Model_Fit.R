library(PSPManalysis)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(lubridate)
library(here)

root <- here()


EQ_Function = function(Delta, Mopt)
  
{
  
  DefaultParameters <- c(Delta = Delta, #turnover rate is 1 divided by the per capita growth rate
                         # Turnover is 1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                         Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                         
                         A_hat = 0.096, #0.096 liters per day filtering rate AKA volume swept clear via frost 1972. Units should be liters per day.
                         # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                         
                         
                         Temp = 273.15,  #20 C is 293.15 K 
                         E_mu = .57,  #.57 eV (McCoy et al. 2008) 
                         E_M = .55, #55, #.55 (Maps et al. 2014)
                         E_I = .46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                         #Average of .6 Savage et al. 2004, as cited in (Crossier 1926; Raven and Geider 1988; Vetter 1995; Gillooly et al. 2001)
                         E_Delta = 0.5, #average activation energy of phytoplankton growth from Barton and Yvon-Durocher (2019) 
                        
                         Im = 11.26, #Im ended up not being used in the final ingestion formulation, but left here to prevent breaking numeric indices
                         #mean of just calanus at 15 C is 22.48333.  Mean of all species at 15 C is 17.74286
                       
                         t0_Im = 285.65, #average of Saiz Calbet data restricted to 10-15 C
                         
                         #286.803, #mean temp of saiz and calbet dataframe when restricted to experiments between 10 and 15 degrees C
                         cI = 0, #Ohlberger assumes a value of 0 in Roach model 
                         cM = 0.0,# Ohlberger tests the Roach model with values of -.02, 0, and .02 
                         Lambda1 = 2, #(Petersen, 1986)
                         Lamda2 = 3.94, #(Petersen, 1986)
                         k = 8.617e-5, #boltzmann constant
                         alpha = 0.75, #guess
                         t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                         sigma = 0.7 , #0.6 (Kiørboe, 2008.) Converts ingested energy to biomass
                         #0.66 works well
                         Mopt = Mopt, #exp(-3.18)*exp(.73*12), 
                         
                         gamma1 = exp(-3.211e-06), #from Saiz and Calbet max ingestion data at 15 C
                         gamma2 = 9.683e-03,
                         gamma3 = 9.894e-01,
                         t0_gamma = 288.15,
                         
                         theta1 = exp(-3.6374366),
                         theta2 = 0.2492352,
                         theta3 = 0.8211947,
                         t0_theta = 286.953, #mean of Saiz and Calbet ingestion data when restricted to specimens at between 10 and 15 C
                         
                         epsi1 = exp(0.83445), #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                         epsi2 = 0.70310 , #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                         t0_epsi = 288.15,
                         
                         epsilon1 = exp(-8.68361),
                         epsilon2 = 0.35064, #from Saiz and Calbet max ingestion data at 15 C
                         epsilon3 = 0.88576,
                         epsilon4 = 0.01603,
                         t0_epsilon = 288.15,
                         
                         omega1 = 3.6577,
                         omega2 = 0.3307,
                         t0_omega = 286.953, #mean of Saiz and Calbet ingestion data when restricted to specimens at between 10 and 15 C
                         
                         phi1 = 1.336596, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                         phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  Might need to change my reference temp?
                         t0_phi = 288.15,
                         
                         rho1 = exp(2.45033), #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                         rho2 = 0.75816, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                         t0_rho = 275.15,
                         
                         mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                         mj = exp(-3.18)*exp(.73*12),   # ug (micrograms) (Petersen, 1986) pg 66
                         
                         t0_Delta = 281.15 #8 degrees celsius from Maranon et al. at an intermediate nutrient level had turnover rate of about 1 day
                         #Turnover_Time = 2.19907e-7 #converted from ms to days from Falkowski et al 1981
                         
                         #z = 0.002694034 #juvenile to adult ratio 
  )
  

  Bering_Calanus_Data = read.csv(here::here("Data/Calanus_BSMS.csv"))
  
  
  Calanus_Juvenile = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] != "ADULT"),c(12, 33, 42)]
  
  Juvenile_Months = month(Calanus_Juvenile[,1])
  
  Calanus_Adult = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] == "ADULT"), c(12, 33, 42)]
  
  Adult_Months = month(Calanus_Adult[,1])
  
  Calanus_Juvenile = cbind(Calanus_Juvenile, Juvenile_Months)
  Calanus_Juvenile[,"Stage"] = rep("Juvenile", nrow(Calanus_Juvenile))
  colnames(Calanus_Juvenile) = c("Date","Year","Weight","Month","Stage")
  
  Calanus_Adult = cbind(Calanus_Adult,Adult_Months)
  Calanus_Adult[,"Stage"] = rep("Adult", nrow(Calanus_Adult))
  colnames(Calanus_Adult) = c("Date","Year","Weight","Month","Stage")
  
  Calanus_Biomass_DF = rbind(Calanus_Juvenile, Calanus_Adult)


  Bering_SST_Summer= read.csv(paste0(root,"/Data/Bering_SST_12_06.csv"))[c(49:65,67:69),7:9] + 273.15 #1996 through 2012, 2014 through 2016 #This matches summer zooplankton sample availability
  
  
  year_sequence = as.numeric(c(seq(1996, 2012, 1),seq(2014,2016,1)))
  
  Bering_SST_Summer <- Bering_SST_Summer  %>%
    mutate("Year" = year_sequence, .before = June)
  

  month_sequence = as.numeric(6:8)
  
  Juvenile_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
  colnames(Juvenile_Mean_Weights) = c("June","July","August")
  
  
  Adult_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
  colnames(Adult_Mean_Weights) = c("June","July","August")
  
  
  for(i in 1:length(year_sequence))
  {
    for(j in 1:length(month_sequence))
    {
      Juvenile_Mean_Weights[i,j] = mean(Calanus_Juvenile[which(Calanus_Juvenile$Year == year_sequence[i] & Calanus_Juvenile$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
      Adult_Mean_Weights[i,j] = mean(Calanus_Adult[which(Calanus_Adult$Year == year_sequence[i] & Calanus_Adult$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
    }
  }
  
  Juvenile_Mean_Weights <- Juvenile_Mean_Weights %>%
    mutate("Year" = year_sequence, .before = June)
  
  Adult_Mean_Weights <- Adult_Mean_Weights %>%
    mutate("Year" = year_sequence, .before = June)
  
  
  
Modified_Parameters_A_hat_.096 = DefaultParameters
  
output1_1_A_hat_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), biftype = "EQ", startpoint = c(1, 1), 
                                stepsize = 1.5,
                                parbnds = c(1, 1, 5000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                                clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


df = data.frame(R_max = output1_1_A_hat_.096$curvepoints[, 1],
                R = output1_1_A_hat_.096$curvepoints[, 2],
                J =output1_1_A_hat_.096$curvepoints[, 5],
                A = output1_1_A_hat_.096$curvepoints[, 6],
                A_Hat = as.factor(rep(.096, nrow(output1_1_A_hat_.096$curvepoints))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.25, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 5000), parameters = Modified_Parameters_A_hat_.096, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)




t1 = output1_1_non_trivial_.096$curvepoints


start_.096 = median(which(t1[,"Rmax"] < (Modified_Parameters_A_hat_.096[2]+500) & t1[,"Rmax"] > (Modified_Parameters_A_hat_.096[2]-500)))

start_.096 = round(start_.096, 0)


output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                                                         output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = 0.1,
                                                          parbnds = c(3, 273.15, 305), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)



Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])


Temp_A_hat = Temp_A_hat_.096

R_Biomass = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
J_Biomass = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
A_Biomass = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]

#Negative biomass is biologically impossible, but the PSPM package stops after the first negative value (so it may include a negative value as the last value)
#Manually replace the negative values with 0

for(i in 1:length(J_Biomass))
{
if(J_Biomass[i] < 0 )
{
  J_Biomass[i] = 0
}
  if(A_Biomass[i] < 0 )
  {
    A_Biomass[i] = 0
  }
  
}


df3 <- data.frame(Temperature = Temp_A_hat,
                  R = R_Biomass,
                  J = J_Biomass,
                  A = A_Biomass
) %>% 
  pivot_longer(c(R, J, A))

          
  
df3_J = df3[which(df3[,"name"] == "J"),]
df3_A = df3[which(df3[,"name"] == "A"),]


rows_to_extract_J = c()
rows_to_extract_A = c()
Bering_SST_Summer_Long <- Bering_SST_Summer %>%
  pivot_longer(c(
                 "June",
                 "July",
                 "August"))
colnames(Bering_SST_Summer_Long) = c("Year","Month","Temperature")

Juvenile_Mean_Weights <- Juvenile_Mean_Weights  %>%
  pivot_longer(c(
                 "June",
                 "July",
                 "August"))
colnames(Juvenile_Mean_Weights) = c("Year","Month","Weight")

Adult_Mean_Weights <- Adult_Mean_Weights  %>%
  pivot_longer(c(
                 "June",
                 "July",
                 "August"))
colnames(Adult_Mean_Weights) = c("Year","Month","Weight")



months = c(
           "June",
           "July",
           "August")

rows_to_extract_J = c()
rows_to_extract_A = c()

for(i in 1:nrow(Bering_SST_Summer_Long))
{
  model_temp_vector_J = as.vector(df3_J[,1])
  rows_to_extract_J[i] = min(which(abs(model_temp_vector_J - Bering_SST_Summer_Long[i,"Temperature"]) == min(abs(model_temp_vector_J - Bering_SST_Summer_Long[i,"Temperature"]))) )
  
  model_temp_vector_A = as.vector(df3_A[,1])
  rows_to_extract_A[i] = min(which(abs(model_temp_vector_A - Bering_SST_Summer_Long[i,"Temperature"]) == min(abs(model_temp_vector_A - Bering_SST_Summer_Long[i,"Temperature"]))) )
  
}

Model_J_Biomass_Selected = df3_J[rows_to_extract_J,] #Model Predicted Juvenile Biomass
colnames(Model_J_Biomass_Selected) = c("Temperature",
                                       "Stage",
                                       "Predicted")
Model_A_Biomass_Selected = df3_A[rows_to_extract_A,] #Model Predicted Adult Biomass
colnames(Model_A_Biomass_Selected) = c("Temperature",
                                       "Stage",
                                       "Predicted")

Observed_Predicted_DF_J = Model_J_Biomass_Selected
Observed_Predicted_DF_J[,"Observed"]  = Juvenile_Mean_Weights[,"Weight"]
Observed_Predicted_DF_J = na.omit(Observed_Predicted_DF_J)
Observed_Predicted_DF_A = Model_A_Biomass_Selected
Observed_Predicted_DF_A[,"Observed"]  = Adult_Mean_Weights[,"Weight"]
Observed_Predicted_DF_A = na.omit(Observed_Predicted_DF_A)

DF_Observed_Predicted = rbind(Observed_Predicted_DF_J, Observed_Predicted_DF_A)
DF_Observed_Predicted <- DF_Observed_Predicted %>%
  pivot_longer(c("Observed", "Predicted"))

#Gives the observed year/month combination with the closest temperature to the temperatures in the model
#we need to extract from the model_temp_vector



DF_Observed = DF_Observed_Predicted[which(DF_Observed_Predicted[,"name"] =="Observed"),]




return(DF_Observed_Predicted)
  
}

Delta_Vector = seq(0.001,.002,.0001) #rows
Mopt_Vector = seq(83,94,1) #columns
  
Parameter_combo_diff_A <- array(dim = c(length(Delta_Vector), length(Mopt_Vector)))
Parameter_combo_diff_J <- array(dim = c(length(Delta_Vector), length(Mopt_Vector)))


n = length(Delta_Vector)*length(Mopt_Vector)


step = 0 

output = c()

for(i in 1:length(Delta_Vector))
{
      for(z in 1:length(Mopt_Vector))
        
      {

  cat("########################################")
step = step + 1 
print(step)
  
  output = EQ_Function(Delta = Delta_Vector[i], Mopt = Mopt_Vector[z])
  
  

  Obs_J = rbind(data.frame(cbind(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Obs_A = rbind(data.frame(cbind(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,]))
  )))
           
       
  Pred_J = rbind(data.frame(cbind(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Pred_A = rbind(data.frame(cbind(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,]))
  )))

  #Calculate sum of squared residuals
  Diff_A = sum((log(Obs_A[,"value"]+.000000000001) - log(Pred_A[,"value"]+.000000000001))^2)
  Diff_J = sum((log(Obs_J[,"value"]+.000000000001) - log(Pred_J[,"value"]+.000000000001))^2)
  
  #Calculate MAPE
  #Diff_A = mean(abs(Obs_A[,"value"]-Pred_A[,"value"])/Obs_A[,"value"], na.rm = TRUE)
  #Diff_J = mean(abs(Obs_J[,"value"]-Pred_J[,"value"])/Obs_J[,"value"], na.rm = TRUE)
  

  Parameter_combo_diff_A[i,z] <- Diff_A
  Parameter_combo_diff_J[i,z] <- Diff_J
  
        }
    }

parameter_combo_mean_diff <- Parameter_combo_diff_A + Parameter_combo_diff_J

rownames(parameter_combo_mean_diff) = Delta_Vector
colnames(parameter_combo_mean_diff) = Mopt_Vector

min(parameter_combo_mean_diff)

apply(parameter_combo_mean_diff, 1, min)# row minimums
Delta_Vector[which(apply(parameter_combo_mean_diff, 1, min) == min(apply(parameter_combo_mean_diff, 1, min)))]

#.0014 is minimized delta

apply(parameter_combo_mean_diff, 2, min)# column minimums
Mopt_Vector[which(apply(parameter_combo_mean_diff, 2, min) == min(apply(parameter_combo_mean_diff, 2, min)))]
#87 is minimized Mopt

plot(parameter_combo_mean_diff[5,]~Mopt_Vector)
plot(parameter_combo_mean_diff[,7]~Delta_Vector)
