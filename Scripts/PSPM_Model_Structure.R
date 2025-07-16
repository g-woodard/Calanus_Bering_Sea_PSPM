

# StageStructuredBiomass.R -  R file specifying the elementary life-history functions
#                   of the size-structured consumer-unstructured resource model from
#
#                   A.M. de Roos et al., 2008. Simplifying a physiologically structured
#                   population model to a stage-structured biomass model.
#                   Theor. Pop. Biol. 73: 47–62.
#
#                   This model forms the basis for the stage-structured biomass model
#                   analyzed in:
#
#                   A.M. de Roos et al., 2007. Food-dependent growth leads to
#                   overcompensation in stage- specific biomass when mortality
#                   increases: the influence of maturation versus reproduction
#                   regulation. American Naturalist 170: E59-E76.
#
#                   Consumers hence follow a net-production DEB model with scaling
#                   functions of ingestion and maintenance proportional to body size.
#                   Juveniles and adults use all their net-production for growth and
#                   reproduction, respectively.
#
#                   The variable that is solved for is the biomass of the resource.
#
#   Model i-state variables:
#     1 : Age
#     2 : Size
#
#   Model of environmental state variables:
#     1 : Resource
#
#   Model interaction (and output) variables:
#     1 : Total resource ingestion by the consumer population
#     2 : Total biomass of juveniles
#     3 : Total biomass of adults
#     4 : Net Production

# Model dimension variables: PSPMdimensions (required)
#
# Define a numerical (integer) vector called 'PSPMdimensions' that specifies the
# dimensions of the model. The vector should include the following named vecctor
# elements:
#
# PopulationNr:       The number of populations in the model that are structured,
#                     that is, of which the life history of individuals is explicitly
#                     modelled
# IStateDimension:    The number of individual state variables that characterise the
#                     individuals in the structured populations. This number should be
#                     the same for all structured populations in the model
# LifeHistoryStages:  The number of distinct and discrete stages in the life history
#                     of the individuals. A part of the individual life history is
#                     considered a stage, when at the boundary of this part one of the
#                     life history processes (development, fecundity, mortality or
#                     impact) changes discontinuously
# ImpactDimension:    The number of functions that represent the impact of an individual
#                     on its environment. At the population level these impacts
#                     affect the dynamics of the environemtal state variables and hence
#                     determine their equilibrium values. Impact functions can, however,
#                     also be used to extract certain output information on the
#                     structured populations (such as number of biomass of juveniles and
#                     adults) as all population-level values of the impact functions
#                     are reported as output.
#
# An error will occur when one of the above named elements is missing.
#
PSPMdimensions <- c(PopulationNr = 1, IStateDimension = 2, LifeHistoryStages = 2, ImpactDimension = 8)


#
# Variable name: NumericalOptions (optional)
#
# Define a numerical vector called 'NumericalOptions' the elements of which specify
# the optional numerical settings to tweak the computations. The elements of the
# vector should have names corresponding to one of the possible numerical settings
# (see the vignette). Examples of such numerical settings are 'MIN_SURVIVAL = 1.0E-9'
# and 'RHSTOL = 1.0E-6', which set the survivial at which the individual is considered
# dead and the tolerance value determining when a solution has been found, respectively.
#

NumericalOptions <- c(MIN_SURVIVAL  = 1.0E-9,                                       # Survival at which individual is considered dead
                      MAX_AGE       = 100000,                                       # Give some absolute maximum for individual age
                      DYTOL         = 1.0E-6,                                       # Variable tolerance
                      RHSTOL        = 1.0E-6,
                      ALLOWNEGATIVE = 0)                                       # Function tolerance

#Default Allow negative is set to 0 (false) in numerical options so not listed here
# Variable name: EnvironmentState (required)
#
# Define a vector called 'EnvironmentState' with a length equal to the number of
# environmental state variables in the problem. Each element of this vector should be
# one of the strings "PERCAPITARATE", "GENERALODE" or "POPULATIONINTEGRAL", defining
# the nature or type of environmental state variable. The 3 different types are defined
# as follows:
#
# Set an entry to "PERCAPITARATE" if the dynamics of E[j] follow an ODE and 0
# is a possible equilibrium state of E[j]. The ODE is then of the form
# dE[j]/dt = P(E,I)*E[j], with P(E,I) the per capita growth rate of E[j].
# Specify the equilibrium condition as condition[j] = P(E,I), do not include
# the multiplication with E[j] to allow for detecting and continuing the
# transcritical bifurcation between the trivial and non-trivial equilibrium.
#
# Set an entry to "GENERALODE" if the dynamics of E[j] follow an ODE and 0 is
# NOT an equilibrium state of E. The ODE then has a form dE[j]/dt = G(E,I).
# Specify the equilibrium condition as condition[j] = G(E,I).
#
# Set an entry to "POPULATIONINTEGRAL" if E[j] is a (weighted) integral of the
# population distribution, representing for example the total population
# biomass. E[j] then can be expressed as E[j] = I[p][i]. Specify the
# equilibrium condition in this case as condition[j] = I[p][i].
#
# If the members of the vector 'EnvironmentState' are given names, these names can be
# used in the functions below that define the life history processes.

EnvironmentState <- c(R = "GENERALODE")

#
# Variable name: DefaultParameters (required)
#
# Define a vector called 'DefaultParameters' with a length equal to the number of
# parameters in the model. Each element of this vector should be given the default
# for the particualr parameter. If the members of the vector 'DefaultParameters' are
# given names, these names can be used conveniently in the functions below that define
# the life history processes.

DefaultParameters <- c(Delta = 0.003, #turnover rate is 1 divided by the per capita growth rate
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
                       
                       
                       k = 8.617e-5, #boltzmann constant
                       alpha = 0.75, #(Hjelm and Persson 2001)
                       t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = 0.7, #(de Roos et al. 2007; Peters 1983; Yodzis and Innes 1992)
                       
                       Mopt = 39, #exp(-3.18)*exp(.73*12), #???????????
                       
                       epsi1 = 0.9902766, #Approximated from saiz and calbert 2007 On marine calanoid species. 15 C.
                       epsi2 = 0.002102, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                       epsi3 = 0.998725, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                       t0_epsi = 288.15,
                       
                       #Kiorbe, Mohlenberg and Nicolajsen (2012) maximum rate equal to 85 % body C · d−1 at 15 °C
                       
                       phi1 = 1.3365966, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                       phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  
                       t0_phi = 288.15,
                       
                       rho1 = 0.02310296, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       rho2 = 0.75816, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       t0_rho = 275.15,
                       
                       mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                       mj = exp(-3.18)*exp(.73*12),   # ug (micrograms) (Petersen, 1986) pg 66
                       
                       t0_Delta = 281.15, #8 degrees celsius from Maranon et al. at an intermediate nutrient level had turnover rate of about 1 day
                       #Turnover_Time = 2.19907e-7 #converted from ms to days from Falkowski et al 1981
                       
                       #z = 0.002694034 #juvenile to adult ratio 
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.0 # Jan tests the Roach model with values of -.02, 0, and .02 
)

# Function name: StateAtBirth  (required)
#
# Specify for all structured populations in the problem all possible values of the individual state
# at birth.
#
# Function arguments:
#
#   E:    Vector with the current values of the environmental state variables.
#   pars: Vector with the model parameters
#
# Required return:
#
# A vector of length equal to the number of i-state variables. The biological interpretation of
# each of the i-state variables is completely up to the user. Each element should specify the
# numeric value of the particular i-state variable with which the individual is born.
# at birth for the particular i-state variable. If the members of the vector are given
# meaningful names, these names can be used conveniently in the functions below that define
# the life history processes.
#
# If individuals can differ in their individual state at birth this function should return a
# matrix with the number of rows equal to the number of possible states at birth and the number
# of columns equal to the number of i-state variables. Each row then specifies the value of the
# individual state variable of the particualr state at birth.
#
# In case the model accounts for multiple, structured populations this function should return a
# a matrix with the number of rows equal to the number of structured populations in the problem
# and the number of columns equal to the number of i-state variables.
#
# In case the model accounts for multiple, structured populations and individuals can differ in
# their individual state at birth this function should return a 3-dimensional array with the
# first dimension having a length equal to the number of structured populations in the problem,
# the second dimension equal to the number of possible states at birth and the third dimension
# equal to the number of i-state variables.

StateAtBirth <- function(E, pars)
{
  with(as.list(c(E, pars)),{
    # We model a single structured population with two i-state variables:
    # 1: age (initial value 0); 2: length (initial value equal to parameter z)
    c(Age = 0.0, Size = mh) #mh/mj = z = #juvenile to adult ratio
  })
}

#
# Function name: LifeStageEndings  (required)
#
# Specify for all structured populations in the problem the threshold value at which the current life
# stage of the individual ends and the individual matures to the next life history stage. The threshold
# value may depend on the current i-state variables of the individual, its state at birth and the life
# stage that it currently is in.
#
# Function arguments:
#
#  lifestage:     Integer value specifying the life stage that the individual is currently in.
#                 These stages are numbered 1 (youngest) and up.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a vector of integer values.
#  istate:        Vector of length equal to the number of i-state variables that charaterize
#                 the state of an individual. The biological interpretation of the i-state
#                 variables is up to the user. Each element specifies the current value of the
#                 particular i-state variable of the individual.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a matrix with the number of rows equal to the number of structured populations
#                 in the problem and the number of columns equal to the number of i-state variables.
#  birthstate:    Vector of length equal to the number of i-state variables that charaterize
#                 the state of an individual. Each element specifies the value of the particular
#                 i-state variables at which the individual was born.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a matrix with the number of rows equal to the number of structured populations
#                 in the problem and the number of columns equal to the number of i-state variables.
#  BirthStateNr:  The integer index of the state of birth to be specified, ranging from 1 and up.
#  E:             Vector with the current values of the environmental state variables.
#  pars:          Vector with the model parameters
#
# Required return:
#
# maturation:   A single value specifying when the current life stage of the individual ends and
#               the individual matures to the next life history stage. The end of the current life
#               history stage occurs when this threshold value becomes 0 and switches sign from
#               negative to  positive. For the final life stage (which never ends) return a constant
#               negative value (for example, -1)
#               In case the model accounts for multiple, structured populations this argument
#               is a vector with the number of elements equal to the number of structured populations
#               in the problem.

LifeStageEndings <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {
  with(as.list(c(E, pars, istate)),{
    maturation  = switch(lifestage, Size - mj, -1) #Scaled sizes so saying maturity is at size 1
  })
}

#
# Function name: LifeHistoryRates  (required)
#
# Specify for all structured populations in the problem the rates of the various life history
# processes (development, fecundity, mortality and impact on the environment)  of an individual
# as a function of its i-state variables, the individual's state at birth and the life
# stage that the individual is currently in.
#
# Function arguments:
#
#  lifestage:     Integer value specifying the life stage that the individual is currently in.
#                 These stages are numbered 1 (youngest) and up.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a vector of integer values.
#  istate:        Vector of length equal to the number of i-state variables that charaterize
#                 the state of an individual. The biological interpretation of the i-state
#                 variables is up to the user. Each element specifies the current value of the
#                 particular i-state variable of the individual.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a matrix with the number of rows equal to the number of structured populations
#                 in the problem and the number of columns equal to the number of i-state variables.
#  birthstate:    Vector of length equal to the number of i-state variables that charaterize
#                 the state of an individual. Each element specifies the value of the particular
#                 i-state variables at which the individual was born.
#                 In case the model accounts for multiple, structured populations this argument
#                 is a matrix with the number of rows equal to the number of structured populations
#                 in the problem and the number of columns equal to the number of i-state variables.
#  BirthStateNr:  The integer index of the state of birth to be specified, ranging from 1 and up.
#  E:             Vector with the current values of the environmental state variables.
#  pars:          Vector with the model parameters
#
# Required return:
#
# A list with 4 components, named "development", "fecundity", "mortality" and "impact". The
# components should have the following structure:
#
# development:  A vector of length equal to the number of i-state variables. Each element
#               specifies the rate of development for the particular i-state variable.
#               In case the model accounts for multiple, structured populations this component
#               should be a matrix with the number of rows equal to the number of structured
#               populations in the problem and the number of columns equal to the number of
#               i-state variables.
# fecundity:    The value of the current fecundity of the individual.
#               In case the model accounts for multiple, structured populations this argument
#               is a matrix of fecundities with the number of rows equal to the number
#               of structured populations in the problem and a single column.
#               In case individuals can be born with different states at birth the component
#               should have a number of columns equal to the number of states at birth. Each
#               column should specify the number of offspring produced with the particular
#               state at birth.
# mortality:    A single value specifying the current mortality rate that the individual experiences.
#               In case the model accounts for multiple, structured populations this argument
#               is a vector of mortality rates with the number of elements equal to the number
#               of structured populations in the problem.
# impact:       A single value or a vector of a length equal to the number of impact functions that
#               need to be monitored for the individual. The value (or the values of the vector)
#               should specify the current contribution of the individual to this population-level
#               impact.
#               In case the model accounts for multiple, structured populations this component
#               should be a matrix with the number of rows equal to the number of structured
#               populations in the problem and the number of columns equal to the number of impact
#               functions.




LifeHistoryRates <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {
  with(as.list(c(E, pars, istate)),{
    n = exp(E_I*(Temp-t0)/((k)*Temp*t0))*(A_hat*((Size/Mopt)*exp(1-Size/Mopt))^(alpha+cI*(Temp-t0)))*R 
    
    #Original formulation for Imax
    #Imax = exp(E_I*(Temp-t0_epsi)/((k)*Temp*t0_epsi))*epsi1*Size^(epsi2+cI*(Temp-t0_epsi))
    
    Imax = exp(E_I*(Temp-t0_epsi)/((k)*Temp*t0_epsi))* epsi1 * Size^(epsi2 + cI*(Temp-t0_epsi))* R^(epsi3 + cI*(Temp-t0_epsi))
    #Imax = exp(E_I*(Temp-t0_epsi)/((k)*Temp*t0_epsi))* epsi1 * Size^(epsi2 + cI*(Temp-t0_epsi))
    
    Ingest = Imax*((n)/(n+Imax)) #Formula from Kiorbe et al 2018
    
    Metabolic_rate = exp(E_M*(Temp-t0_rho)/((k)*Temp*t0_rho))*rho1*Size^(rho2 + cM*(Temp-t0_rho)) #parameters estimated from dry weight. Units of micrograms of O2 per day

    #Should assimilation efficiency be used here (sigma) with carbon? or is it redundant because of the conversion from carbon weight to dry weight??
    netproduction = sigma*Ingest/.455 - (Metabolic_rate*(.014196/0.0279)) #need to convert ingestion from carbon to micrograms of copepod, and metabolism from oxygen consumed to micrograms of copepod
 
    #0.455 is conversion factor from Uye 1982 where COPEPOD dry weight is 45.5% Carbon
       
    #0.014196 is oxycalorific coefficient where 1 ug of O2 = 0.014196 J (Elliot and Davison 1975).
      #1 calorie is 4.2 joules
      #3.38 Calories per mg O2 (Elliot and Davison 1975)
      #1 mg O2 = 14.196 J so 1 ug of O2 = .014196 J (Elliot and Davison 1975)
   
    #mean copepod energy density was 27.9 kJ/gram AKA 0.0279 J/ug dw (Davies et al. 2012)
    
    #Ratio of oxycalorific coefficent to copepod energy density = .014196/0.0279 = 0.5088172
    
    mortality_rate = exp(E_mu*(Temp-t0_phi)/((k)*Temp*t0_phi))*phi1*Size^(phi2 + cI*(Temp-t0_phi)) 

    
    list(
      
      development = switch(lifestage, c(1.0, (netproduction)), c(1.0, 0.0)),
      
      fecundity   = switch(lifestage, 0, (netproduction)/mh), #Number of eggs produced per unit time
      
      mortality   = switch(lifestage, mortality_rate, mortality_rate),  #Added temperature dependence to allometric exponent (Jan's paper doesn't include this)
      
      impact      = switch(lifestage, c(Ingest, Size, 0, netproduction, 0, Imax, Metabolic_rate, mortality_rate), c(Ingest, 0, Size, 0, netproduction, Imax, Metabolic_rate, mortality_rate))
    )
  })
}



EnvEqui <- function(I, E, pars) {
  with(as.list(c(E, pars)),{
    (exp(E_Delta*(Temp-t0_Delta)/((k)*Temp*t0_Delta))*Delta*(Rmax - R) - I[1])
  })
}


