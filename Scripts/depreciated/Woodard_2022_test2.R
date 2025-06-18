

#
#   Model i-state variables:
#     1 : Age
#     2 : Mass
#
#   Model environmental state variables:

#     1 : Resource biomass  [0]

#_________________


#     2 : Juvenile zoop biomass [1]
#     3 : Adult zoop biomass [2]
# 
#     
#     4 : Temperature [3] may just need to be a parameter and not an environmental state variable since we are not keeping track of it.
#   
#  Where is the resource dynamics defined? (impact dimensions)
#   Model interaction (and output) variables:

#     1 : Total resource ingestion by the consumer population [0]
#     2 : Total biomass of zoop juveniles [1]
#     3 : Total biomass of zoop Adults [2]


#
# Model dimension variables: PSPMdimensions (required)
#
# Define a numerical (integer) vector called 'PSPMdimensions' that specifies the
# dimensions of the model. The vector should include the following named vector
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

#     1 : Total resource ingestion by the consumer population [0]
#     2 : Total biomass of zoop juveniles [1]
#     3 : Total biomass of zoop Adults [2]


# An error will occur when one of the above named elements is missing.
#
PSPMdimensions <- c(PopulationNr = 1, IStateDimension = 2, LifeHistoryStages = 2, ImpactDimension = 3)

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
                      DYTOL         = 1.0E-7,                                       # Variable tolerance
                      RHSTOL        = 1.0E-6,                                       # Function tolerance
                      ALLOWNEGATIVE = 0,                                            # Negative solution values allowed?
                      COHORT_NR     = 100)                                          # Number of cohorts in population state output

#
# Variable name: EnvironmentState  (required)
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


EnvironmentState <- c(R = "GENERALODE") #No predator so prey pops may not be environmental variables Bj = "POPULATIONINTEGRAL", Ba = "POPULATIONINTEGRAL"

#Ba equals adult zoop density
#Bj equals juvenile zoop density
#R equals resource

# Variable name: DefaultParameters  (required)
#
# Define a vector called 'DefaultParameters' with a length equal to the number of
# parameters in the model. Each element of this vector should be given the default
# for the particular parameter. If the members of the vector 'DefaultParameters' are
# given names, these names can be used conveniently in the functions below that define
# the life history processes.

DefaultParameters <- c(Delta = .1,
                    
                       Rmax = 1,
                       A_hat = 3, #max zooplankton attack rate                                                                
                       Temp = 20, 
                       E_mu = .45,
                       E_M = .594,
                       E_I = 1.21,
                       
                       Omega = 9.0E-6,
                       Gamma = 0.006,
                       Rh = 1.5E-5,
                       Rm = 0.003,
                       Mub = 0.01, A = 5000.0, Th = 0.1, Imax = 1.0E-4,
                       
                       sigma = .5,
                       T_0 = 295.15,
                       k = 1.380649*10^(-23), #boltzman constant
                      
                       Mopt = 41,
                 
                       alpha = .75,
                       epsilon1 = .25,
                       epsilon2 = .77,
                       phi1 = .0015,
                       phi2 = .75,
                       rho1 = .0123,
                       rho2 = .77,
                       mj = .00794*140^3.15, #Mass when zoop changes from juvenile to adult
                       mb = .00794*12^3.15, #Mass of zoop at birth
                       z = (.00794*12^3.15)/(.00794*140^3.15) #Z is used for the maturation rate using the equation from the proposal. z = ratio of juvenile to adult individual mass 
                       ) 


#
# Function name: StateAtBirth  (required)
#
# Specify for all structured populations in the problem all possible values of the individual state
# at birth.
#
# Function arguments:
#
#  E:     Vector with the current values of the environmental state variables.
#  pars:  Vector with the model parameters
#
# Required return:
#
# A vector of a length equal to the number of i-state variables. The biological interpretation of
# each of the i-state variables is completely up to the user. Each element should specify the
# numeric value of the particular i-state variable with which the individual is born. If the
# members of the vector are given meaningful names, these names can be used conveniently in the
# functions below that define the life history processes.
#
# If individuals can differ in their individual state at birth this function should return a
# matrix with the number of rows equal to the number of possible states at birth and the number
# of columns equal to the number of i-state variables. Each row then specifies the value of the
# individual state variable of the particular state at birth.
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
    # 1: age (initial value 0); 2: length (initial value equal to parameter Lb)
    
    c(Age = 0, Mass = mb)
   # matrix(c(Stage = 1, Stage = 1, Mass = mb, Mass = mzb), nrow = 2, ncol = 2) #columns are the i state variables so stage and size at birth, rows are the two populations (Fish and Zooplankton)
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
    maturation  = switch(lifestage, Mass - mj, -1)
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


#     1 : Total resource ingestion by the consumer population [0]


#These impact functions may not be needed since there is no predator population
#     2 : Total biomass of zoop juveniles [1]
#     3 : Total biomass of zoop Adults [2]


LifeHistoryRates <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {

  # rI = exp(E_I*(Temp-T_0)/k*Temp*T_0)*phi1*Mass^phi2
  # rM = exp(E_M*(Temp-T_0)/k*Temp*T_0)*phi1*Mass^phi2
  # ai = rI*A_hat*((Mass/Mopt)*exp(1-Mass/Mopt))^(alpha)
  # ni = ai*R #Encounter rate
  # Imax = rI*epsilon1*Mass^(epsilon2)
  # I = ni/(1+(ni/Imax)) #ingestion 
  # Metabolic_rate = rM*rho1*Mass^rho
  # I = (((exp(E_I*(Temp-T_0)/k*Temp*T_0)*phi1*Mass^phi2)*A_hat*((Mass/Mopt)*exp(1-Mass/Mopt))^(alpha))*R)/(1+((((exp(E_I*(Temp-T_0)/k*Temp*T_0)*phi1*Mass^phi2)*A_hat*((Mass/Mopt)*exp(1-Mass/Mopt))^(alpha))*R)/((exp(E_I*(Temp-T_0)/k*Temp*T_0)*phi1*Mass^phi2)*epsilon1*Mass^(epsilon2))))
  with(as.list(c(E, pars, istate)),{
    list(
      # We model a single structured population (nrow=1) with two i-state variables:
      # 1: stage (developmental rate 1.0); 2: Biomass production
      
      development = c(      1.0, Gamma*(mj*R/(R + Rh) - Mass) ), #Denom
                             #Net production But currently I think there is no weight loss from starvation
      
      fecundity   = switch(lifestage, 0, Rm*R/(R + Rh)*Mass^2),
      
      mortality   = switch(lifestage, Mub, Mub),
      
      impact      = switch(lifestage,c(Imax*R/(R + Rh)*Mass^2,
                                        Mass, #Juvenile biomass
                                        0), # Adult biomass
                           
                           c(Imax*R/(R + Rh)*Length^2,
                             0, #Juvenile biomass
                             Mass) #Adult biomass
      )    
      #Ingestion [0], juvenile growth [1], adult growth [2]
    )
  })
}

#
# Function name: DiscreteChanges  (optional)
#
# If discrete changes in the individual state occur at the moment individuals mature to the next
# life history stage, specify for all structured populations in the problem these discrete changes
# (jumps) in the individual state variables on ENTERING a particular life stage. The life stage that is
# entered is specified by the vector 'lifestage'.
#
# NOTE: LEAVE THIS FUNCTION UNSPECIFIED IF THERE ARE NO DISCRETE CHANGES, AS THIS SAVES COMPUTATION TIME
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
# A vector of length equal to the number of i-state variables. Each element should specify the value
# of the particular i-state variable after the transition to the current state, as given in
# the vector 'lifestage'
# In case the model accounts for multiple, structured populations this function should return
# a matrix with the number of rows equal to the number of structured populations in the problem
# and the number of columns equal to the number of i-state variables.

DiscreteChanges <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {
  with(as.list(c(E, pars)),{
       # No discrete changes in this problem, function is commented out, which
       # would be equivalent to returning a copy of the input argument 'istate'
       istate
     })
}

# Specify for each of the environmental state variables the condition that determines its
# equilibrium value, dependent on the values of the environmental state variables themselves,
# the population impact values and the paramenters.
#
#  I:     Matrix of population impact values. The number of rows of this matrix equals
#         the number of structured populations in the problem, while the number of columns
#         equals the number of impact factors of an individual on its environment, as specified
#         by the output variable impact of the function LifeHistoryRates().
#         The interpretation of the latter is up to the user.
#  E:     List with named current values of the environmental state variables
#  pars:  List with named model parameters
#
# Required return:
#
#  A vector with a length equal to the number of environmental state variables in the problem.
#  Each entry i of this vector should specify the equilibrium condition for the particular environmental
#  state variable (i).

#Model environmental state variables:
  
  #     1 : Resource biomass  [0]
  #     2 : Juvenile zoop biomass [1]
  #     3 : Adult zoop biomass [2]
  
EnvEqui <- function(I, E, pars) {
  with(as.list(c(E, pars)),{
    c(Delta*(Rmax - R) - I[1], I[2], I[3]) #resource biomass, juvenile zoop biomass, adult zoop biomass
  })#Somthing is wrong with the turnover rate because presently it is not temperature dependent
  
}



