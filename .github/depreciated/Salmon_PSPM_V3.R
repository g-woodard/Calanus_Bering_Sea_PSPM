duration = NULL
Resource = c()

salmonParameters <- c(Rho =  .25, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 100, #grams per cubic meter
                       
                       # A_hat_freshwater = 0.90, #Johansen et al 2020 found that rainbow trout attack almost all encountered prey in low flows.  Similary they found that attacks were almost always successful in low flows
                       #          #Attacks and success at velocity of 0 in Johansen et al were nearly 100%.  I will choose a value of 90% of Cmax to represent the freshwater successful attack rate given the lentic environments most sockeye rear in
                       # A_hat_saltwater = 0.90 #Hebard 1961 shows mean currents in southeat bering sea from 0.05 to 0.11 knots.  .08 knots is about 4 cm per second.  I will use same successful attack rate as in freshwater
                       
                       A_hat = 300, #cubic meters per day for Roach (Hjelm and Persson 2001) as presented in Lindmark et al. 2017
                       Temp_Freshwater = 280.15,  #20 C is 293.15 K (reference temp)
                       Temp_Ocean = 280.15,
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
                       
                       U = 50,  #swimming speed in cm per second #approximate value from Trudel and Welch 2011
                      
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
                      Egg_Survival = .1, #Lessard et al 2008
                      F1_mort = .65, #Lessard et al 2008
                      F2_mort = .4, #Lessard et al 2008
                      Prob_O2_Spawn = 0.80, #Blair et al 1993
                      Prob_F1_Stay = .5  #JUST A RANDOM NUMBER RIGHT NOW, HAVENT FOUND ACTUAL PROPORTION
                      
                      
)                 


Freshwater_Resource = 100 #grams per cubic meter
Saltwater_Resource = 100

#Need to create a stage structured initial salmon population each year based on harvest and escapement
#can back calculate with estimated survival parameters
#Populations in thousands
#May not need to include pink salmon directly if I can use zooplankton data (density dependence will be observed by reduced resource)
Egg_Pop = (nrow(Escapement_Matrix)/2)*3700*Egg_Survival #This is a combination of the egg and alevin stage
salmon_pop = 3000
duration = 60

reproduction_event = 0
Fecundity_total = 0
R = 40000 #Need to get 

salmon_list = list()

salmon_Matrix = array(dim = c(salmon_pop,5))
colnames(salmon_Matrix) = c("Age","Size","Status","Stage","Spawning")
salmon_Matrix[,"Age"] = rep(0, salmon_pop)

salmon_Matrix[,"Size"] = rep(as.numeric(salmonParameters["mj"]), salmon_pop)
salmon_Matrix[,"Status"] = rep(1, salmon_pop) #1 is alive, 0 is dead
#salmon_Matrix[,"Stage"] = rep("Adult", salmon_pop) #For initial population total number of
salmon_list[[1]] = salmon_Matrix

#Lets do the salmon population in 1000s

for(t in 1:duration)
  
{
  print(t)
  reproduction_event = reproduction_event +1
  salmon_Matrix = salmon_list[[t]] #Age, Size, Dead or Alive Status
  
  colnames(salmon_Matrix) = c("Age","Size","Status")
  
  for(i in 1:nrow(salmon_Matrix))
  {
    
   
    Size = salmon_Matrix[i,2]
    #Remember eggs stay in the gravel a year so need to lag them (can just deposit them as alevins in year t+2
    
    if(salmon_Matrix[,"Stage"] == "F1" | salmon_Matrix[,"Stage"] == "F2")
    {
      
    Max_Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp_Freshwater"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp_Freshwater"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Size^salmonParameters["alpha2"]*Size #max consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
    
    
    Encounter_Rate = salmonParameters["A_hat"]*Freshwater_Resource
    Ingest = Encounter_Rate/(1+Encounter_Rate/Max_Ingest)
     
    Metabolic_rate = exp(-1.06 +.79*log(Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp_Freshwater"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
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
   
    
    
    salmon_Matrix[i,2] = Size
    salmon_Matrix[i,1] = salmon_Matrix[i,1] + 1/12 #increase age by 1/12 of a year
    
    draw = runif(1,0,1)
    if(salmon_Matrix[,"Stage"] == "F2" & draw < salmonParameters["F2_Mort"]) #Death
    {
      salmon_Matrix[i,3] = 0
    }
    if(salmon_Matrix[,"Stage"] == "F2" & draw > salmonParameters["F2_Mort"]) #Survival
    {
      salmon_Matrix[i,3] = 1
      if(reproduction_event == 10)
      {
      salmon_Matrix[i,"Stage"] = "O1"
      }
    }
    
    if(salmon_Matrix[,"Stage"] == "F1" & draw < salmonParameters["F1_Mort"]) #Death
    {
      salmon_Matrix[i,3] = 0
    }
    
    draw_for_stay = runif(1,0,1)
    
    if(salmon_Matrix[,"Stage"] == "F1" & draw > salmonParameters["F1_Mort"]) #Survival
    {
      salmon_Matrix[i,3] = 1
      
      if(reproduction_event == 10 & draw_for_stay < salmonParameters["Prob_F1_Stay"])
         {
      salmon_Matrix[i,"Stage"] = "F2"
         }
      if(reproduction_event == 10 & draw_for_stay > salmonParameters["Prob_F1_Stay"])
      {
        salmon_Matrix[i,"Stage"] = "O1"
      }
    }
    
    
    
    }
    
    
    if(salmon_Matrix[,"Stage"] == "O1")
    {
      
      Max_Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp_Saltwater"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp_Saltwater"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Size^salmonParameters["alpha2"]*Size #max consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
      
      
      Encounter_Rate = salmonParameters["A_hat"]*Saltwater_Resource
      Ingest = Encounter_Rate/(1+Encounter_Rate/Max_Ingest)
 
      Metabolic_rate = exp(-1.06 +.79*log(Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp_Saltwater"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
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
      
     
      
    }
    
    if(salmon_Matrix[,"Stage"] == "O2")
    {
      
      Max_Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp_Saltwater"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp_Saltwater"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Size^salmonParameters["alpha2"]*Size #max consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
      
      
      Encounter_Rate = salmonParameters["A_hat"]*Saltwater_Resource
      Ingest = Encounter_Rate/(1+Encounter_Rate/Max_Ingest)

      
      Metabolic_rate = exp(-1.06 +.79*log(Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp_Saltwater"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
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
      
      if(reproduction_event == 12)
      {
      draw_prob_spawn = runif(1,0,1)
      if(runif(1,0,1) < salmonParameters["Prob_O2_Spawn"] & draw>inst_mort)
      {
     
        salmon_Matrix[i,"Spawning"] = 1
      }
      }
    }
    
    if(salmon_Matrix[,"Stage"] == "O3")
    {
      
      Max_Ingest = exp(salmonParameters["E_I"]*(salmonParameters["Temp_Saltwater"]-salmonParameters["t0_P_BW"])/((salmonParameters["k"])*salmonParameters["Temp_Saltwater"]*salmonParameters["t0_P_BW"]))*salmonParameters["alpha1"]*Size^salmonParameters["alpha2"]*Size #max consumption rate in grams per day at 5 C from Davies et al. 1998.  Right now salmon size is in wet weight
      
      
      Encounter_Rate = salmonParameters["A_hat"]*Saltwater_Resource
      Ingest = Encounter_Rate/(1+Encounter_Rate/Max_Ingest)
      
      
      Metabolic_rate = exp(-1.06 +.79*log(Size)+1.12*log(salmonParameters["U"])-.95/salmonParameters["Temp_Saltwater"])/(1000)*24 #Trudel and Welch 2011, originally in mg of O2 per hour, here I have converted it to grams of O2 per day.
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
      
      
      if(draw > inst_mort & reproduction_event == 12)
      {
        
        salmon_Matrix[i,"Spawning"] = 1
        
      }
      
      
    }
   
    #Metabolic_rate = exp(salmonParameters["E_M"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])/((salmonParameters["k"])*salmonParameters["Temp"]*salmonParameters["t0_rho"]))*salmonParameters["rho1"]*Size^(salmonParameters["rho2"] + salmonParameters["cM"]*(salmonParameters["Temp"]-salmonParameters["t0_rho"])) 
    
   # Alevin_total = Alevin_total + Alevins
   
    #egg to fry survival of .1 Lessard Hilborn and Chasco 2008
    #fry to F1 survival of .35 Lessard Hilborn and Chasco 2008
    #surivival in second year of freshwater was pretty variable, mode was .6 in Lessard Hilborn and Chasco 2008
    
    #switch over to recruitment to smolt stage
  }
  
 
  
  salmon_Matrix = salmon_Matrix[which(salmon_Matrix[,3] == 1),] #remove dead individuals
  
  #Births
  
  if(reproduction_event == 6 & salmon_Matrix[,"Age"])
  {
    salmon_Matrix[which(salmon_Matrix[,"Stage"] == "Egg"),"Stage"] = "F1" #First graduate existing eggs to F1 in february
  }
  
  if(reproduction_event == 12) #Assuming Reproduction month is August, so first month is september
  {
    spawner_Matrix = salmon_Matrix[which(salmon_Matrix[,"Spawning"] == 1),] #spawners
    
    harvested_fish = runif(harvest,0, nrow(spawner_Matrix))
    
    Escapement_Matrix = spawner_Matrix[-harvested_fish,]
    
    salmon_Matrix = salmon_Matrix[which(salmon_Matrix[,"Spawning"] == 0),] #remove spawned escapement salmon from salmon matrix
    
    
    
    # 3700 is eggs per female in Lessard et al. 2008, divide by two to assume 50% sex ratio
    Fecundity_alevin = (nrow(Escapement_Matrix)/2)*3700*Egg_Survival
    
    fecundity_Matrix = array(dim = c(Fecundity_Alevin,5))
  
    colnames(fecundity_Matrix) = c("Age","Size","Status","Stage","Spawning")
    
   
    
    
    fecundity_Matrix[,1] =  rep(0, nrow(fecundity_Matrix))
    fecundity_Matrix[,2] = rep(Alevin_Weight, nrow(fecundity_Matrix))
    fecundity_Matrix[,3] = rep(1, nrow(fecundity_Matrix)) #Start Alive
    fecundity_Matrix[,4] = rep("Egg", nrow(fecundity_Matrix)) #Start First freshwater year as "Egg" - im using the alevin weight here as im assuming no growth as egg, just have to wait about 6 months before hatching
    fecundity_Matrix[,5] = rep(0, nrow(fecundity_Matrix)) #haven't spawned
    salmon_Matrix = rbind(salmon_Matrix, fecundity_Matrix)
    
   
    reproduction_event = 0
    
  }
  
  salmon_list[[t+1]] = salmon_Matrix
  
  }
  
