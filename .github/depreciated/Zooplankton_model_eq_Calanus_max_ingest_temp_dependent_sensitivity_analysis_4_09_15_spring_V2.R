library(PSPManalysis)
library(tidyverse)

get.NLL = function(Nt,Ntpred,sigma)
  
  
{
  
  
  
  
  NLL = c()
  
  for (i in 1:length(years_index)) 
  {
    NLL[i] <- log(1/sqrt(2*3.1415*sigma^2)) - ((Nt[i])-(Ntpred[i]))^2/(2*(sigma[i])^2)
  }
  
  NLL <- sum(NLL)
  return(NLL)
  
} 

#Plot ingestion at an intermediate temperature over a range of resource levels
#Add in temp dependence to turnover?
#Look at critical resource density
#assess competitive dominance of the different life stages
#sensitivity analysis of Mopt
#Plot total production in addition to net production
#Total production as a function of temperature is what we want to compare to productivity metric in bering sea
#Nonlinear attack rate vs temperature
#celsius to fahrenheit is *9/5 + 32  


  
  Bering_Calanus_Data = read.csv("Data/Calanus_BSMS.csv")
  
  library(lubridate)
  
  
  Negative_to_zero = function(value)
  {
    if(value < 0)
    {
      value = 0
    }
    
    else
      value = value
    
    return(value)
  }
  
  

  labeller_A_J_R <- function(variable,value){
    return(Life_Stages_A_J_R[value])
  }
  

EQ_Function = function(Delta, Rmax, alpha, Mopt, sigma)
  
{
  
  DefaultParameters <- c(Delta =  Delta, #turnover rate is 1 divided by the per capita growth rate
                         # Turnover is 1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                         Rmax = Rmax, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                         
                         A_hat = 0.096, #0.096 liters per day filtering rate AKA volume swept clear via frost 1972. Units should be liters per day.
                         # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                         
                         
                         Temp = 273.15,  #20 C is 293.15 K 
                         E_mu = .57,  #.57 eV (McCoy et al. 2008) 
                         E_M = .55, #.55 (Maps et al. 2014)
                         E_I = 0.46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                         #Average of .6 Savage et al. 2004, as cited in (Crossier 1926; Raven and Geider 1988; Vetter 1995; Gillooly et al. 2001)
                         E_Delta = 0.5, #average activation energy of phytoplankton growth from Barton and Yvon-Durocher (2019) 
                         #Im = 29.89, #when n = A_hat*R
                         Im = 11.26, #fit from kiorbe et al 2018 formulation of ingestion with alpha = .75 when n = A_hat*Size^alpha*R
                         #mean of just calanus at 15 C is 22.48333.  Mean of all species at 15 C is 17.74286
                         #Im = 17.74286,
                         t0_Im = 285.65, #average of Saiz Calbet data restricted to 10-15 C
                         
                         #286.803, #mean temp of saiz and calbet dataframe when restricted to experiments between 10 and 15 degrees C
                         cI = 0, #Jan assumes a value of 0 in Roach model 
                         cM = 0.0,# Jan tests the Roach model with values of -.02, 0, and .02 
                         Lambda1 = 2, #(Petersen, 1986)
                         Lamda2 = 3.94, #(Petersen, 1986)
                         k = 8.617e-5, #boltzmann constant
                         alpha = alpha, #guess
                         t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                         sigma = sigma , # .6 (Kiørboe, 2008.) Converts ingested energy to biomass
                         #assimilation efficiencies between .685 and .854 Landry et al. 1984
                         Mopt = Mopt, #???????????
                         
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
  
  Bering_SST = read.csv("Data/Bering_Sea_SST.csv")[45:71,] #1992 through 2018
  Bering_SST_Spring = (Bering_SST[,4:9]+ 273.15)  %>%
    mutate("Year" = c(1992:2018), .before = March)
  
  year_sequence = as.numeric(seq(1992, 2018, 1))
  
  month_sequence = as.numeric(3:8)
  
  Juvenile_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
  colnames(Juvenile_Mean_Weights) = c("March","April","May","June","July","August")
  
  
  Adult_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
  colnames(Adult_Mean_Weights) = c("March","April","May","June","July","August")
  
  
  for(i in 1:length(year_sequence))
  {
    for(j in 1:length(month_sequence))
    {
      Juvenile_Mean_Weights[i,j] = mean(Calanus_Juvenile[which(Calanus_Juvenile$Year == year_sequence[i] & Calanus_Juvenile$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
      Adult_Mean_Weights[i,j] = mean(Calanus_Adult[which(Calanus_Adult$Year == year_sequence[i] & Calanus_Adult$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
    }
  }
  
  Juvenile_Mean_Weights <- Juvenile_Mean_Weights %>%
    mutate("Year" = c(1992:2018), .before = March)
  
  Adult_Mean_Weights <- Adult_Mean_Weights %>%
    mutate("Year" = c(1992:2018), .before = March)
  
  
  
Modified_Parameters_A_hat_.096 = DefaultParameters
  
output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", biftype = "EQ", startpoint = c(1, 1), 
                                stepsize = 1.5,
                                parbnds = c(1, 1, 5000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                                clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


df = data.frame(R_max = output1_1_A_hat_.096$curvepoints[, 1],
                R = output1_1_A_hat_.096$curvepoints[, 2],
                J =output1_1_A_hat_.096$curvepoints[, 5],
                A = output1_1_A_hat_.096$curvepoints[, 6],
                A_Hat = as.factor(rep(.096, nrow(output1_1_A_hat_.096$curvepoints))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.25, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 5000), parameters = Modified_Parameters_A_hat_.096, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


ingestion_.096 = output1_1_non_trivial_.096$curvepoints[,4]#/output1_1_non_trivial_.096$curvepoints[,5]+output1_1_non_trivial_.096$curvepoints[,6]
Rmax_.096 = output1_1_non_trivial_.096$curvepoints[,1]
max_ingestion_.096 = output1_1_non_trivial_.096$curvepoints[,9]

Ingestion_DF = data.frame(
  Ingestion = ingestion_.096,
  Imax = max_ingestion_.096,
  
  Rmax = Rmax_.096
  
)

Ingestion_DF2 = Ingestion_DF  %>%
  pivot_longer(c(Ingestion,Imax))

ggplot(Ingestion_DF2, aes(x = Rmax, y = value, color = name))+
  geom_line()+
  #guides(linetype=guide_legend(title = "Â"))+
  labs(x= "Rmax (µg/L)", y="Ingestion rate (µg/day)") +
  theme_light()


t1 = output1_1_non_trivial_.096$curvepoints


start_.096 = median(which(t1[,"Rmax"] < (Modified_Parameters_A_hat_.096[2]+500) & t1[,"Rmax"] > (Modified_Parameters_A_hat_.096[2]-500)))

start_.096 = round(start_.096, 0)

df2 <- data.frame(R_max = output1_1_non_trivial_.096$curvepoints[, 1], 
                  
                  
                  R = output1_1_non_trivial_.096$curvepoints[, 2], 
                  
                  J = output1_1_non_trivial_.096$curvepoints[, 5], 
                  
                  A = output1_1_non_trivial_.096$curvepoints[, 6])

df2 <- df2 %>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name)) + 
  geom_line()+ 
  theme_light()

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                                                         output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = 0.5,
                                                          parbnds = c(3, 273.15, 305), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)



Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])


Temp_A_hat =Temp_A_hat_.096

R_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
J_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
A_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]
print(A_A_hat/J_A_hat)
df3 <- data.frame(Temperature = Temp_A_hat,
                  R = R_A_hat,
                  J = J_A_hat,
                  A = A_A_hat
) %>% 
  pivot_longer(c(R, J, A))

          
  
df3_J = df3[which(df3[,"name"] == "J"),]
df3_A = df3[which(df3[,"name"] == "A"),]


rows_to_extract_J = c()
rows_to_extract_A = c()
Bering_SST_Spring_Long <- Bering_SST_Spring %>%
  pivot_longer(c("March",
                 "April",
                 "May",
                 "June",
                 "July",
                 "August"))
colnames(Bering_SST_Spring_Long) = c("Year","Month","Temperature")

Juvenile_Mean_Weights <- Juvenile_Mean_Weights  %>%
  pivot_longer(c("March",
                 "April",
                 "May",
                 "June",
                 "July",
                 "August"))
colnames(Juvenile_Mean_Weights) = c("Year","Month","Weight")

Adult_Mean_Weights <- Adult_Mean_Weights  %>%
  pivot_longer(c("March",
                 "April",
                 "May",
                 "June",
                 "July",
                 "August"))
colnames(Adult_Mean_Weights) = c("Year","Month","Weight")



months = c("March",
           "April",
           "May",
           "June",
           "July",
           "August")

rows_to_extract_J = c()
rows_to_extract_A = c()

for(i in 1:nrow(Bering_SST_Spring_Long))
{
  model_temp_vector_J = as.vector(df3_J[,1])
  rows_to_extract_J[i] = min(which(abs(model_temp_vector_J - Bering_SST_Spring_Long[i,"Temperature"]) == min(abs(model_temp_vector_J - Bering_SST_Spring_Long[i,"Temperature"]))) )
  
  model_temp_vector_A = as.vector(df3_A[,1])
  rows_to_extract_A[i] = min(which(abs(model_temp_vector_A - Bering_SST_Spring_Long[i,"Temperature"]) == min(abs(model_temp_vector_A - Bering_SST_Spring_Long[i,"Temperature"]))) )
  
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

#Gives the observed year/month combination with the closes temperature to the temperatures in the model
#we need to extract from the model_temp_vector



DF_Observed = DF_Observed_Predicted[which(DF_Observed_Predicted[,"name"] =="Observed"),]




return(DF_Observed_Predicted)
  
}
  
# 
# A_Hat_Vector = seq(.04, .1, .0025)
# #Delta_Vector = seq(.1,3.1,.5)
# 
# sensitivity_array = array(dim = c(length(A_Hat_Vector), 2) )
# Pred_J = array(dim = c(23, length(A_Hat_Vector)))
# Obs_J =  array(dim = c(23, length(A_Hat_Vector)))
# Pred_A =  array(dim = c(21, length(A_Hat_Vector)))
# Obs_A =  array(dim = c(21, length(A_Hat_Vector)))
# 
# 
# 
# output = c()
#   
# for(i in 1:length(A_Hat_Vector))
# {
#     cat("########################################")
#     print(i)
#     output = EQ_Function(A_Hat_Vector[i])
#     
#     # squared_residuals_J = c()
#     # squared_residuals_A = c()
#     
#    
#     # for(k in 1:length(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"]))
#     # {
#       #squared_residuals_J[k] = ((output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"][k]) - (output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,"value"][k]) )^2
#       Obs_J[,i] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"]
#       Pred_J[,i] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,"value"]
#     #}
#     
#     # for(a in 1:length(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]))
#     # {
#       # squared_residuals_A[a] = ((output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"][a]) - (output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"][a]) )^2   
#       Obs_A[,i] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]
#       Pred_A[,i] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"]
#     #}
#     
#     # J_sum_squares = sum(squared_residuals_J)
#     # A_sum_squares = sum(squared_residuals_A)
#     # 
#     # print(J_sum_squares)
#     # print(A_sum_squares)
#     # 
#     # sensitivity_array[i,1] = J_sum_squares
#     # sensitivity_array[i,2] = A_sum_squares
# 
# }
# 
# # min(sensitivity_array[,1]) # 2,1 .6 and 2000
# # 
# # min(sensitivity_array[,2]) # 2,1
# # 
# # sensitivity_array_total = sensitivity_array[,1] + sensitivity_array[,2]
# # min(sensitivity_array_total, na.rm = TRUE) # 1, 11
# # 
# # Rmax_Vector[2]
# #Delta_Vector[1]
# 
# MAPE_J = colMeans(abs(Pred_J - Obs_J)/Obs_J)
# MAPE_A = colMeans(abs(Pred_A - Obs_A)/Obs_A)
# min(MAPE_J)
# min(MAPE_A)
# 
# which(MAPE_J == min(MAPE_J))
# which(MAPE_A == min(MAPE_A))
# 
# A_Hat_Vector[8] 

Delta_Vector =  seq(0.2,1,.2) 

Rmax_Vector = seq(1400,2000,200) 
#A_Hat_Vector = seq(0.035,.065,.01) 
A_Hat_Vector = 0.096

alpha_Vector = .75 #seq(0.70,0.78,0.02)

Mopt_Vector = 200 #seq(150,250,50)

sigma_Vector = seq(0.6,.68,.02) 


Pred_J = data.frame()
Obs_J =  data.frame()
Pred_A =  data.frame()
Obs_A = data.frame()


output = c()

for(i in 1:length(Delta_Vector))
{
  for(j in 1:length(Rmax_Vector))
  {
    for(k in 1:length(alpha_Vector))
        {
      for(z in 1:length(Mopt_Vector))
        
      {
        for(a in 1:length(sigma_Vector))
            {
  cat("########################################")
  print(i)
  print(j)
  print(k)
  print(z)
  print(a)
  
  output = EQ_Function(Delta_Vector[i], Rmax_Vector[j], alpha_Vector[k], Mopt_Vector[z], sigma_Vector[a])
  

  Obs_J = rbind(Obs_J, data.frame(cbind(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Obs_A = rbind(Obs_A, data.frame(cbind(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"name"] == "Observed" & output[,"Stage"] == "A" ) ,]))
  )))
           
       
  Pred_J = rbind(Pred_J, data.frame(cbind(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Pred_A = rbind(Pred_A, data.frame(cbind(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"name"] == "Predicted" & output[,"Stage"] == "A" ) ,]))
  )))
           
  # Obs_A[,i,j,k] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]
  # Pred_A[,i,j,k] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"]
  # Error_array_A[y,i,j,k] = Obs_A[y,i,j,k]-Pred_A[y,i,j,k]
  # #}
  # 
  # }
  # 

        }
    }
  }
}

}

Temp_A = unique(Obs_A[,"Temperature"])
Temp_J = unique(Obs_J[,"Temperature"])



Diff_A = abs((Obs_A[,"value"]-Pred_A[,"value"])/Obs_A[,"value"])
Diff_J = abs((Obs_J[,"value"]-Pred_J[,"value"])/Obs_J[,"value"])

Parameter_combo_mean_diff_A = c()

Delta_A = c()
Rmax_A = c()
alpha_A = c()
Mopt_A = c()
sigma_A = c()

iterations = length(seq(1,length(Diff_A)/(length(Temp_A)),1))

start_A = seq(1, (length(Diff_A)-length(Temp_A)+1),length(Temp_A))
end_A = seq(length(Temp_A), length(Diff_A),length(Temp_A))

for(i in 1:iterations)
{


  Parameter_combo_mean_diff_A[i] = mean(Diff_A[start_A[i]:end_A[i]])
  Delta_A[i] = Obs_A[(start_A[i]),6]
  Rmax_A[i] = Obs_A[(start_A[i]),7]
  alpha_A[i] = Obs_A[(start_A[i]),8]
  Mopt_A[i] = Obs_A[(start_A[i]),9]
  sigma_A[i] = Obs_A[(start_A[i]),10]
}

Parameter_combo_mean_diff_J = c()

Delta_J = c()
Rmax_J = c()
alpha_J = c()
Mopt_J = c()
sigma_J = c()

iterations = length(seq(1,length(Diff_J)/length(Temp_J),1))

start_J = seq(1, (length(Diff_J)-length(Temp_J)+1),length(Temp_J))
end_J = seq(length(Temp_J), length(Diff_J),length(Temp_J))

for(i in 1:length(start_J))
{
  
 
  Parameter_combo_mean_diff_J[i] = mean(Diff_J[start_J[i]:end_J[i]])
  Delta_J[i] = Obs_J[start_J[i],5]
  Rmax_J[i] = Obs_J[start_J[i],6]
  alpha_J[i] = Obs_J[start_J[i],7]
  Mopt_J[i] = Obs_J[start_J[i],8]
  sigma_J[i] = Obs_J[start_J[i],9]
 
  
}

nrow(Obs_J)

rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))
min(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J)))
which(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J)) == min(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))))
results = data.frame(cbind(Delta_J,Rmax_J,alpha_J,Mopt_J,sigma_J,rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))))
Mopt_J[1]
Rmax_J[1]
alpha_J[1]
# min(sensitivity_array[,1]) # 2,1 .6 and 2000
# 
# min(sensitivity_array[,2]) # 2,1
# 
# sensitivity_array_total = sensitivity_array[,1] + sensitivity_array[,2]
# min(sensitivity_array_total, na.rm = TRUE) # 1, 11
# 
# Rmax_Vector[2]
#Delta_Vector[1]

# Avg_Obs_J = apply(Obs_J, c(2,3), mean)
# Avg_Obs_A = apply(Obs_A, c(2,3), mean)
# Avg_Pred_J = apply(Pred_J, c(2,3), mean)
# Avg_Pred_A = apply(Pred_A, c(2,3), mean)
# 
# 
# MAPE_J = abs((Avg_Pred_J - Avg_Obs_J)/(Avg_Obs_J))
# MAPE_A = abs((Avg_Pred_A - Avg_Obs_A)/(Avg_Obs_A))
# min(MAPE_J)
# min(MAPE_A)
# 
# MAPE_combined = array(dim = c(nrow(MAPE_J), ncol(MAPE_J)))
# 
# for(j in 1:ncol(MAPE_J))
# {
#   for(i in 1:nrow(MAPE_J))
#   {
#     MAPE_combined[i,j] = mean(c(MAPE_J[i,j],MAPE_A[i,j]))
#     
#   }
#   
# }
# which(MAPE_J == min(MAPE_J))
# which(MAPE_A == min(MAPE_A))
# which(MAPE_combined == min(MAPE_combined, na.rm = TRUE))
# A_Hat_Vector[1]
 Rmax_Vector[36]
# Mopt_Vector[6]
# Delta_Vector[2]
# #.092 and 125 work well MAPE of .2365
# #.097 and 200 work well MAPE of .2335
# #0.0965 and 205 work well MAPE of 0.232986
# 
# #0.0968 and 200 work well MAPE of 0.2185869
# 
# #.0968 and 200 work well MAPE of 0.2103425
# 
# #.096 and 193 work well MAPE of 0.1994644
# #MAPE of 0.002398885 at delta of .06 and Rmax of 1550
# #MAPE of 0.004878781 at delta of .055 and Rmax of 1560
# #Rmax = 1597 Delta = 0.295 MAPE. = 0.00832193
# #MAPE of 0.0687 at delta = 0.025 Rmax = 1640 and Emu = .57
# #MAPE of .0441 Rmax = 1650 delta = .21, A_hat = 1.3344
 
 # 0.1
 # 1350
 # 0.75
 # 200
 # 0.61
 # 2.346221
 
 # 0.1
 # 1375
 # 0.75
 # 175
 # 0.60
 # 1.893780
 
 # 0.1
 # 1340
 # 0.75
 # 140
 # 0.60
 # 0.9887325
 

 # 0.1
 # 1340
 # 0.75
 # 140
 # 0.60
 # 0.9887325
 
 # 0.1
 # 1350
 # 0.75
 # 125
 # 0.60
 # 9.059749