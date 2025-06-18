duration = NULL
Shared_Resource = c()

DefaultParameters <- c(Rho =  .25, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.096, #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
                       # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                       
                       #bifurcation with Rho = 3 and A hat = 1 min Rmax was 8.69
                       Temp = 275.15,  #20 C is 293.15 K (reference temp)
                       E_mu = .85,  #.85 eV (Savage et al. 2004) 
                       E_M = .4, #.4 for C. glacialis (Tande 1988)
                       E_I = 0.46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                       
                       E_delta = 0.43, 
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.02,# Jan tests the Roach model with values of -.02, 0, and .02 
                       Lambda1 = 2, #(Petersen, 1986)
                       Lamda2 = 3.94, #(Petersen, 1986)
                       k= 8.617e-5, #boltzmann constant
                       alpha = .75, #guess
                       t0 = 285.65, #Frost expreiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = .6 , # (Kiørboe, 2008.) Converts ingested energy to biomass
                       Mopt = 150, #???????????
                       
                       #one gram is 1 million micrograms
                       epsilon1 = 1.678804, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  Saiz and calbert found maximum ingestion rate was temp independent.
                       epsilon2 = 0.703, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  Saiz and calbert found maximum ingestion rate was temp independent.
                       
                       phi1 = 1.336596, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                       phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  Might need to change my reference temp?
                       t0_phi = 288.15,
                       
                       rho1 = 0.02310296  , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       rho2 =  0.7581552   , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       t0_rho = 275.15,
                       
                       mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                       mj = exp(-3.18)*exp(.73*12)   # ug (micrograms) (Petersen, 1986) pg 66
                       
                       
                       #z = 0.002694034 #juvenile to adult ratio 
)                 


zoop_pop = 300
duration = 12
zoop_list = list()
reproduction_event = 0
Fecundity_total = 0

for(t in 1:duration)
  
{
  reproduction_event = reproduction_event +1
  zoop_Matrix = array(dim = c(zoop_pop,3)) #Age, Size, Dead or Alive Status
  
  colnames(zoop_Matrix) = c("Age","Size","Status")
  
  for(i in 1:nrow(zoop_Matrix))
  {
    zoop_Matrix = data.frame(zoop_list[[i]])
    Size = zoop_Matrix[,2]
    
    n = exp(zoopParameters["E_I"]*(zoopParameters["Temp"]-zoopParameters["t0"])/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0"]))*(zoopParameters["A_hat"]*((Size/zoopParameters["Mopt"])*exp(1-Size/zoopParameters["Mopt"]))^(zoopParameters["alpha"]+zoopParameters["cI"]*(zoopParameters["Temp"]-zoopParameters["t0"])))*R
    
    Imax = zoopParameters["epsilon1"]*Size^(zoopParameters["epsilon2"]) #based on dry weight for zooplanzoopParameters["k"]ton and grams of Carbon for phytoplanzoopParameters["k"]ton
    
    Ingest = n/(1+(n/Imax)) #in units of micro grams of carbon per day
    Metabolic_rate = exp(zoopParameters["E_M"]*(zoopParameters["Temp"]-zoopParameters["t0_rho"])/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0_rho"]))*zoopParameters["rho1"]*Size^(zoopParameters["rho2"] + zoopParameters["cM"]*(zoopParameters["Temp"]-zoopParameters["t0_rho"])) #parameters estimated from dry weight.  Units of micrograms of O2 per day
    
  
    
    
    netproduction = (zoopParameters["sigma"]*(Ingest))/.455 - Metabolic_rate*.014196*(1/21)*(1000/1) #need to convert ingestion parameter from micrograms of carbon to micrograms of dry weight of zooplanzoopParameters["k"]ton with conversion factor of .455 (Uye 1982)
    
    if(Size < zooParameters["mj"])
    {
    Size = Size+netproduction
    }
    else if(Size = zooParameters["mj"])
      
    {
      Size = Size
      
     
    }
   
    
    zoop_Matrix[i,2] = Size
    zoop_Matrix[i,1] = zoop_Matrix[t,1,i] + 1/12 #increase age by 1/12 of a year
    
    draw = runif(1,0,1)
    
    mortality = exp(zoopParameters["E_M"]*(zoopParameters["Temp"]-zoopParameters["t0_phi"])/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0_phi"]))*zoopParameters["phi1"]*Size^(zoopParameters["phi2"] + zoopParameters["cI"]*(zoopParameters["Temp"]-zoopParameters["t0_phi"])) #need to convert this to a probability draw for an ibm
  
    if(draw<=mortality)
    {
      zoop_Matrix[i,3] = "Dead"
    }
    else if(draw > mortality)
    {
      zoop_Matrix[i,3] = "Alive"
      
    }
    if(Size = zooParameters["mj"] && draw>mortality)
      {
      Fecundity = netproduction/zooParameters["mh"]
      }
    
    Fecundity_total = Fecundity_total + Fecundity
    }
  zoop_Matrix = zoop_Matrix[which(zoop_Matrix[i,3] = "Alive"),] #remove dead individuals
  
  #Births
  
  if(reproduction_event = 12)
  {
    fecundity_Matrix = data.frame(array(dim = c(lengthFecundity_total),3))
    colnames(fecundity_Matrix) = c("Age","Size","Status")
    fecundity_Matrix[1,] =  rep(0, nrow(fecundity_Matrix))
    fecundity_Matrix[2,] = rep(zoopParameters["mh"], nrow(fecundity_Matrix))
    fecundity_Matrix[3,] = rep("Alive", nrow(fecundity_Matrix))
    reproduction_event = 0
    Fecundity_total = 0
  }
  
  #Add fecundity to zoop dataframe
  zoop_Matrix = rbind(zoop_Matrix, fecundity_Matrix)
  zooplist[[t+1]] = zoop_Matrix
  
  }
  

#"adult salmon consume between 0.04% and 0.10% of available annual zooplanzoopParameters["k"]ton production (Brodeur et al., 1999)"

# n = exp(E_I*(zoopParameters["Temp"]-zoopParameters["t0"])/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0"]))*(zoopParameters["A_hat"]*((Size/zoopParameters["Mopt"])*exp(1-Size/zoopParameters["Mopt"]))^(zoopParameters["alpha"]+zoopParameters["cI"]*(zoopParameters["Temp"]-zoopParameters["t0"])))*R 
# 
# Imax = zoopParameters["epsilon1"]*Size^(zoopParameters["epsilon2"]) #based on dry weight for zooplanzoopParameters["k"]ton and grams of Carbon for phytoplanzoopParameters["k"]ton
# 
# Ingest = n/(1+(n/Imax)) #in units of micro grams of carbon per day
# Metabolic_rate = exp(zoopParameters["E_M"]*(zoopParameters["Temp"]-zoopParameters["t0"]_rho)/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0"]_rho))*zoopParameters["rho1"]*Size^(zoopParameters["rho2"] + zoopParameters["cM"]*(zoopParameters["Temp"]-zoopParameters["t0"]_rho)) #parameters estimated from dry weight.  Units of micrograms of O2 per day
# 
# 
# 
# netproduction = (zoopParameters["sigma"]*(Ingest))/.455 - Metabolic_rate*.014196*(1/21)*(1000/1) #need to convert ingestion parameter from micrograms of carbon to micrograms of dry weight of zooplanzoopParameters["k"]ton with conversion factor of .455 (Uye 1982)
# 
# mortality = exp(zoopParameters["E_M"]u*(zoopParameters["Temp"]-zoopParameters["t0_phi"])/((zoopParameters["k"])*zoopParameters["Temp"]*zoopParameters["t0_phi"]))*zoopParameters["phi1"]*Size^(zoopParameters["phi2"] + zoopParameters["cI"]*(zoopParameters["Temp"]-zoopParameters["t0_phi"]))