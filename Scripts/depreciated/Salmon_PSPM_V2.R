duration = NULL
Shared_Resource = c()

salmonParameters <- c(Rho =  .25, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 40000000, #Rmax is a density grams of carbon per liter (in thousands).  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat_freshwater = 0.90, #Johansen et al 2020 found that rainbow trout attack almost all encountered prey in low flows.  Similary they found that attacks were almost always successful in low flows
                                #Attacks and success at velocity of 0 in Johansen et al were nearly 100%.  I will choose a value of 90% of Cmax to represent the freshwater successful attack rate given the lentic environments most sockeye rear in
                       A_hat_saltwater = 0.90 #Hebard 1961 shows mean currents in southeat bering sea from 0.05 to 0.11 knots.  .08 knots is about 4 cm per second.  I will use same successful attack rate as in freshwater
                       Temp = 275.15,  #20 C is 293.15 K (reference temp)
                       E_mu = .85, 
                       E_M = .4,
                       E_I = 0.46, 
                       
                       E_delta = 0.43, 
                       cI = 0, 
                       cM = 0.02,
                       Lambda1 = 2, 
                       Lamda2 = 3.94, 
                       k= 8.617e-5, #boltzmann constant
                       alpha = .75, 
                       t0 = 285.65, 
                       sigma = .6 ,
                       Mopt = 150, #???????????
                       
                       U = exp(3),  #swimming speed in Body length per second #middle value from Trudel and Welch 2011
                      
                       P_BW_Even = .0357, #Percent body weight à la Davies et al 1989
                       P_BW_Odd = .01, #Percent body weight à la Davies et al 1989
                       Cmax = 0.0551, # Brett 1969 as cited in Beauchamp 1989.  original units of g/g/day.  THIS IS UNITS PER BODY WEIGHT SO NEED TO MULTIPLY BY BODY WEIGHT FOR TOTAL
                       PCmax = 0.69, # Brett 1969 as cited in Beauchamp 1989  g/g/day
                       alpha1 = 0.303, # Beauchamp 1989  g/g/day
                       alpha2 = -0.275, #Beauchamp 1989  g/g/day
                       t0_P_BW = 278.15,
                       epsilon1 = 2.902, #3.8 - 6.6 C, Elliott 1976 for brown trout fed at maximum rations
                       epsilon2 = 0.762, #Elliott 1976
                       t0_epsilon = 278.35, #Elliott 1976 mean if range for now
                       inst_mort = 0.038, #units of per month, Ricker 1961
                       Alevin_Weight = .120, #converted from mg to grams, at 5 C, (278.15 K) Murray and Mcphail 1987
                       rho1 = 0.951*(1/1000)*24, #Brett and Glass, 1973 at 15 C. converted from mg O2 per hour to grams of O2 per day
                       rho2 =  0.963, #Brett and Glass, 1973 at 15 C. Exponent stays the same when converting units
                       t0_rho = 288.15,
                       
                      PF1 = .5,
                      PF2 = .5,
                      Egg_Survival = .1,
                      F1_survival = .35,
                      F2_survival = .6,
                      
                       mh = .75, 
                       mj = exp(-3.18)*exp(.73*12)  
                      
)                 

Alevin_Size = salmonParameters["Alevin_Weight"] #Here we calcuate an average freshwater growth for F1 and F2 smolts

juvenile_Size = Alevin_Size

Freshwater_Resource = 50000000

for(day in 1:365)
{
#NEED TO ADD SOME SORT OF RESOURCE DEPENDENCE AND FUNCTIONAL RESPONSE BECAUSE JUVENILE (AND ADULT) GROWTH IS DENSITY DEPENDENT
#THIS DENSITY DEPENDENT CAN BE ADDED BY Lindmark 2017
Max_Ingest_Freshwater = exp(salmonParameters["E_I"]*(salmonParameters["Temp"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*juvenile_Size^salmonParameters["alpha2"]*juvenile_Size #max consumption rate in grams per gram of body mass per day at 5 C from Beauchamp et al. 1989.  Right now salmon size is in wet weight
Encounter_Rate_Freshwater = salmonParameters["A_hat_freshwater"]*Freshwater_Resource
Ingestion_Freshwater = Encounter_Rate_Freshwater/(1+Encounter_Rate_Freshwater/Max_Ingest_Freshwater)
Metabolic_rate = exp(-1.06 +.79*log(juvenile_Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
netproduction_juvenile = (salmonParameters["sigma"]*(Ingest_Freshwater)*1107*4.2 - Metabolic_rate*13560)/(1665*4.2) #1107 calories per gram of wet weight euphasiid prey (davies et al 1989) CONSIDER CHANGING TO A FRESHWATER PREY FOR THIS STAGE BUT SHOULD BE SIMILAR SO OK FOR NOW
#1 gram of sockeye is 1665 calories davies et al 1998
juvenile_Size = juvenile_Size + netproduction_juvenile
}



salmon_pop = 3000
duration = 60

reproduction_event = 0
Fecundity_total = 0
R = 40000

salmon_list = list()

salmon_Matrix = array(dim = c(salmon_pop,4))
colnames(salmon_Matrix) = c("Age","Size","Status")
salmon_Matrix[,"Age"] = rep(0, salmon_pop)

salmon_Matrix[,"Size"] = rep(as.numeric(salmonParameters["mj"]), salmon_pop)
salmon_Matrix[,"Status"] = rep(1, salmon_pop) #1 is alive, 0 is dead
salmon_Matrix[,"Stage"] = rep("Adult", salmon_pop) #For initial population total number of
salmon_list[[1]] = salmon_Matrix

F1_smolt_total = 0
F2_smolt_total = 0
for(t in 1:duration)
  
{
  print(t)
  reproduction_event = reproduction_event +1
  salmon_Matrix = salmon_list[[t]] #Age, Size, Dead or Alive Status
  
  colnames(salmon_Matrix) = c("Age","Size","Status")
  
  for(i in 1:nrow(salmon_Matrix))
  {
    
    Size = salmon_Matrix[i,2]
    Max_Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Size^salmonParameters["alpha2"]*Size #max consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
    #alpha*Size^alpha2 calculates Cmax, PCmax calculates proportion of Cmax fed at t0
    ###NEED TO ADD AN ASYMPTOTIC PARAMETER TO FUNCTIONAL RESPONSE FOR THE RESOURCE VS FEEDING RATE RELATIONSHIP
    
    # if(Even_Odd = Even)
    # {
    # Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_P_BW"]))*salmonParameters["P_BW_Even"]*Size #consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
    # #Right now Ingestion is resource independent, the choice of proportion of body weight consumed can be linked to resource density later
    # }
    # 
    # if(Even_Odd = Odd)
    # {
    #   Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_P_BW"]))*salmonParameters["P_BW_Odd"]*Size #consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
    #   #Right now Ingestion is resource independent, the choice of proportion of body weight consumed can be linked to resource density later
    # }
    # 
      
    Metabolic_rate = exp(-1.06 +.79*log(Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
    Metabolic_rate = Metabolic_rate
    
    #IF I WANT TO CONVERT TO MONTHLY RATES FROM DAYS MULTIPLY BY 30.41667
    
    #Metabolic_rate = exp(salmonParameters["E_M"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_rho"]))*salmonParameters["rho1"]*Size^(salmonParameters["rho2"] + salmonParameters["cM"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])) 
    
  
    netproduction = (salmonParameters["sigma"]*(Ingest)*1107*4.2 - Metabolic_rate*13560)/(1665*4.2) #1107 calories per gram of wet weight euphasiid prey (davies et al 1989)
    #1 gram of sockeye is 1665 calories davies et al 1998
    #Later take the average of all prey species or weighted average of most important prey species
    #1 calorie is 4.2 Joules
    #oxycalorific coefficient 13560 J per g of O2; Stewart et al. 1983
    
    
     if(netproduction < 0)
    {
      netproduction = 0
    }
    if(Size < salmonParameters["mj"])
    {
    Size = Size+netproduction
    }
    if(Size == salmonParameters["mj"])
      
    {
      Size = Size
      
    }
   
    
    salmon_Matrix[i,2] = Size
    salmon_Matrix[i,1] = salmon_Matrix[i,1] + 1/12 #increase age by 1/12 of a year
    
    draw = runif(1,0,1)
      
    if(draw<=inst_mort)
    {
      salmon_Matrix[i,3] = 0
    }
    else if(draw > inst_mort)
    {
      salmon_Matrix[i,3] = 1
      
    }
    if(Size == salmonParameters["mj"] & draw>inst_mort)
      {
      Egg_Fecundity = 3700/2 # 3700 is eggs per female in Lessard et al. 2008, divide by two to assume 50% sex ratio
      F1_Smolts = salmonParameters["PF1"]*Egg_Fecundity*salmonParameters["Egg_Survival"]*salmonParameters["F1_survival"]
      F2_Smolts = salmonParameters["PF2"]*Egg_Fecundity*salmonParameters["Egg_Survival"]*salmonParameters["F1_survival"]*salmonParameters["F2_survival"]
      }
   
   
    Egg_Size = salmonParameters["Egg_Weight"] #Here we calcuate an average freshwater growth for F1 and F2 smolts
    
    Ingest_Freshwater = exp(salmonParameters["E_I"]*(salmonParameters["Temp"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Egg_Size^salmonParameters["alpha2"]*salmonParameters["PCmax"] #consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
    
    Metabolic_rate = exp(-1.06 +.79*log(Egg_Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
    Metabolic_rate = Metabolic_rate
    #Metabolic_rate = exp(salmonParameters["E_M"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_rho"]))*salmonParameters["rho1"]*Size^(salmonParameters["rho2"] + salmonParameters["cM"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])) 
    
    
    netproduction = (salmonParameters["sigma"]*(Ingest)*1107*4.2 - Metabolic_rate*13560)/(1665*4.2) #1107 calories per gram of wet weight euphasiid prey (davies et al 1989)
    
    
    F1_smolt_total = F1_smolt_total + F1_Smolts
    F2_smolt_total = F2_smolt_total + F2_Smolts
    #Need to get mean F1 smolt size and F2 smolt size (Size at ocean entry for each age class since I am not explicitly modeling freshwater growth)
    
    #egg to fry survival of .1 Lessard Hilborn and Chasco 2008
    #fry to F1 survival of .35 Lessard Hilborn and Chasco 2008
    #surivival in second year of freshwater was pretty variable, mode was .6 in Lessard Hilborn and Chasco 2008
    
    #switch over to recruitment to smolt stage
  }
  
 
  
  salmon_Matrix = salmon_Matrix[which(salmon_Matrix[,3] == 1),] #remove dead individuals

  
  #Births
  
  if(reproduction_event == 12 & Fecundity_total > 0)
  {
    
    fecundity_Matrix = array(dim = c(Fecundity_total,3))
  
    colnames(fecundity_Matrix) = c("Age","Size","Status")
    
    #print(fecundity_Matrix)
    
    
    fecundity_Matrix[,1] =  rep(0, nrow(fecundity_Matrix))
    fecundity_Matrix[,2] = rep(, nrow(fecundity_Matrix))
    fecundity_Matrix[,3] = rep(1, nrow(fecundity_Matrix))
    salmon_Matrix = rbind(salmon_Matrix, fecundity_Matrix)
    #Need to bind fecundity matrix to salmon matrix in either year t+1 or t+2 depending on freshwater residency
    reproduction_event = 0
    Fecundity_total = 0
  }
  
  #Add fecundity to zoop dataframe

  
  
  salmon_list[[t+1]] = salmon_Matrix
  
  }
  

#"adult salmon consume between 0.04% and 0.10% of available annual zooplansalmonParameters["k"]ton production (Brodeur et al., 1999)"

# n = exp(E_I*(salmonParameters["Temp"]-salmonParameters["t0"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0"]))*(salmonParameters["A_hat"]*((Size/salmonParameters["Mopt"])*exp(1-Size/salmonParameters["Mopt"]))^(salmonParameters["alpha"]+salmonParameters["cI"]*(salmonParameters["Temp"]-salmonParameters["t0"])))*R 
# 
# Imax = salmonParameters["epsilon1"]*Size^(salmonParameters["epsilon2"]) #based on dry weight for zooplansalmonParameters["k"]ton and grams of Carbon for phytoplansalmonParameters["k"]ton
# 
# Ingest = n/(1+(n/Imax)) #in units of micro grams of carbon per day
# Metabolic_rate = exp(salmonParameters["E_M"]*(salmonParameters["Temp"]-salmonParameters["t0"]_rho)/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0"]_rho))*salmonParameters["rho1"]*Size^(salmonParameters["rho2"] + salmonParameters["cM"]*(salmonParameters["Temp"]-salmonParameters["t0"]_rho)) #parameters estimated from dry weight.  Units of micrograms of O2 per day
# 
# 
# 
# netproduction = (salmonParameters["sigma"]*(Ingest))/.455 - Metabolic_rate*.014196*(1/21)*(1000/1) #need to convert ingestion parameter from micrograms of carbon to micrograms of dry weight of zooplansalmonParameters["k"]ton with conversion factor of .455 (Uye 1982)
# 
# mortality = exp(salmonParameters["E_M"]u*(salmonParameters["Temp"]-salmonParameters["t0_phi"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_phi"]))*salmonParameters["phi1"]*Size^(salmonParameters["phi2"] + salmonParameters["cI"]*(salmonParameters["Temp"]-salmonParameters["t0_phi"]))