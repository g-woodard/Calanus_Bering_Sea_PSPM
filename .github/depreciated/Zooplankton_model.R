#Individuals of copepodite stages C1-C5 do not reproduce and allocate all 
#energy to somatic growth (C1-C4) and lipid storage (primarily C5), while 
#individuals of the final life-stage (C6) do not grow and allocate energy to 
#reproduction. Importantly, reproduction in C. marshallae (and other marine 
#copepods such as C. finmarchicus and C. glacialis) is food- dependent such 
#that a sustained food supply is required to fuel egg production by females during the
#reproductive season

#The model consists of an unstructured phytoplankton resource (R), and a 
#stage-structured zooplankton consumer with two critical life stages 
#representing juvenile copepod stages (C1-5, here J) and the adult copepod 
#stage (C6, here A). The dynamics of the consumer and the resource are given 
#by a set of ordinary differential equations describing changes in 
#stage-specific biomass density based on food (R)-, temperature- (T) and 
#mass- (m) dependent rates (see Equations)

#3rd dimension is a vector 
Ba0 = .2 #starting biomass
sm = .01 #size at maturity
sb = .0001 #Size at birth
#sb/sm scale size at birth set = to .01

z = sb/sm  #scaled size at birth

J1_Number_Vector = c()
J1_Number_Vector[1] = 0 #first time step has no juveniles
J1_Size_Vector = c() #starting juvenile size vector
Reproducing_Adult_Number_Vector = c()
Reproducing_Adult_Number_Vector[1] = Ba0/sm #Set inital adult biomass to .2, Ba = C*sm 

Adult_Size_Vector = c(rep(1,Ba0/sm)) #starting adult size vector (everything scaled to adult size of 1)

t = 340
H= 1
Imax = 10 #ingestion rate  (de Roos et al. 2008) referencing (Hansen et al. (1997))
q = 1 #proportionality for adult ingestion TEST over variety of q values as in de Roos et al. 2008

sigma = .5 #Assimilation_efficiency
T_ = 1 #prop_constant (mass specific metabolic rate parameter)  (de Roos et al. 2008) referencing Yodzis and Innes (1992) and Brown et al. (2004)

uj = .1 #juvenile background mortality (de Roos et al. 2008)
ua = .1 #adult background mortality (de Roos et al. 2008)

#Dont forget to add starvation mortality when NBP is negative

#Semi Chemostat resource dynamics

Rmax = 2 #resource density in absence of consumers
R = c()
R[1] = 1 #set initial density
dj = NULL #juvenile death rate = background mortality plus starvation mortality
da = NULL #Adult death rate = background mortality plus starvation mortality
dt = 1 #temperature dependent turnover rate
G_R = dt*(Rmax - R) #Growth of resource


  
  
t_prime = t*T_

#Scale body size by s' = s/sm


#juvenile_NBP = (sigma*Imax*((R)/(H+R))-T_ ) #per unit of body mass (so need to multiply by mass)

#Adult_NBP = (sigma*q*Imax*((R)/(H+R))-T_ ) #per unit of body mass (so need to multiply by mass)

#If net Biomass is less than 0, growth and reproduction is 0.  Additionally there is an additional starvation 
#mortaltiy of vj(R) and va(R)

juvenile_NBP = c()

Adult_NBP = c()

for(t in 1:(t_prime))
  
{
  
  #change in juvenile and adult population
  #juvenile population size distribution changes due to growth, mortality, and reproduction
  #adult population size distribution changes due to and mortality
  
  
  
  juvenile_NBP = (sigma*Imax*((R)/(H+R))-T_ ) #This is per unit of mass
  
  Adult_NBP = (sigma*q*Imax*((R)/(H+R))-T_ ) #per unit mass (so need to multiply by individual's size)
  

  Updated_J1_Size_Vector = c()
  #calculating net biomass production and growth
 
 
  if(juvenile_NBP < 0 )
  {
    
    J1_Size_Vector = J1_Size_Vector #If juvenile NBP is negative, they don't grow (but they also dont decrease in size)
    
    dj = uj + juvenile_NBP #juvenile mortality rate is background plus starvation
    
    
    for(j in 1:length(J1_Size_Vector))
    {
      
      num = runif(n=1, min = 0, max = 1)
   
    #so if random number is dj or less, the individual dies
    
    if(num > dj) #if the individual survives
    {
      Updated_J1_Size_Vector = c(Updated_J1_Size_Vector,J1_Size_Vector[j])
    }
    else if(num<= dj)
    {
      Updated_J1_Size_Vector = c(Updated_J1_Size_Vector)
    }
    
  }
  
}
  
  if(juvenile_NBP >= 0 )
  {
    J1_Size_Vector = J1_Size_Vector + J1_Size_Vector*juvenile_NBP
    
    dj = uj 
    
    for(j in 1:length(J1_Size_Vector))
    {
      
      num = runif(n=1, min = 0, max = 1)
    
    
    #so if random number is dj or less, the individual dies
    
    if(num > dj) #if the individual survives
    {
      Updated_J1_Size_Vector = c(Updated_J1_Size_Vector,J1_Size_Vector[j])
    }
    else if(num<= dj)
    {
      Updated_J1_Size_Vector = c(Updated_J1_Size_Vector)
    }
  }
  
    
  }
  

  
  Updated_Adult_Size_Vector = c()
  
  
    
  if(Adult_NBP < 0 )
  {
    Adult_Size_Vector = Adult_Size_Vector #In De Roos et al 2008 model adults dont grow
    
    da = ua + Adult_NBP
    
    
    #so if random number is dj or less, the individual dies
    
    for(l in 1:length(Adult_Size_Vector))
    {
      
      num = runif(n=1, min = 0, max = 1)
      
    
    if(num > da) #if the individual survives
    {
      Updated_Adult_Size_Vector = c(Updated_Adult_Size_Vector,Adult_Size_Vector[l])
    }
    else if(num <= da)
    {
      Updated_Adult_Size_Vector = c(Updated_Adult_Size_Vector)
    }
  
      }
  
  }
  
  if(Adult_NBP >= 0 )
  {
    
    
    Adult_Size_Vector = Adult_Size_Vector #In De Roos et al 2008 model adults dont grow
    
    da = ua
    
    for(l in 1:length(Adult_Size_Vector))
    {
      
      num = runif(n=1, min = 0, max = 1)
      
    
    if(num > da) #if the individual survives
    {
      Updated_Adult_Size_Vector = c(Updated_Adult_Size_Vector,Adult_Size_Vector[l])
    }
    else if(num<= da)
    {
      Updated_Adult_Size_Vector = c(Updated_Adult_Size_Vector)
    }
  }
  
    
  }
  
  
  New_Adults = Updated_J1_Size_Vector[which(Updated_J1_Size_Vector >= 1)] 
  
  Updated_J1_Size_Vector = Updated_J1_Size_Vector[which(Updated_J1_Size_Vector < 1)] 
  
  J1_Size_Vector = Updated_J1_Size_Vector
  
  Updated_Adult_Size_Vector = c(Updated_Adult_Size_Vector,New_Adults)
  
  Adult_Size_Vector = Updated_Adult_Size_Vector
  
  
  #calculate reproduction
  if(Adult_NBP >= 0)
  {
    Offspring_Number =  sum(Adult_NBP*Adult_Size_Vector)/z
  }else if(Adult_NBP < 0)
  {
    Offspring_Number =  0
  }

  J1_Size_Vector = c(J1_Size_Vector,c(rep(z,Offspring_Number)))

Adult_foraging = sum(Adult_Size_Vector)*Imax*q
Juvenile_foraging = sum(J1_Size_Vector)*Imax
resource_density = dt*(Rmax-R)
Holling = R/(H+R)

change_in_resource_density = resource_density - Holling*(Juvenile_foraging+Adult_foraging)
R = R + change_in_resource_density

  print(t)
}



