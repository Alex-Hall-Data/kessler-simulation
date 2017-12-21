#to do:

#outer loop for different simulation runs
for(seed in 8:8){
  
  #set seed
  set.seed(seed)


#distribuion of fragmented objects from collisions and explosions. How to model these? Could use data from nasa paper for explosions

total.impact.probability<-c(0)


#impact flux constant (to be changed according to monte carlo result). Target sum of flux is 0.00027 - ie one collision in 10 years
K<-0.00000000024




tle1<-read.table("tle text.txt",header=TRUE)

tle<-as.data.frame(tle1)

attach(tle)

#calculate period for each body and add to data frame
period<-((24*3600)/tle$rpd)
tle$period<-period

#calculate periapsis for each body
per<-((1-tle$ecc)*(399000000000000*(tle$period/6.28)^2)^(1/3))-6371000
tle$periapsis<-per


#remove objects with periapsis<2000km
TLE<-subset(tle,periapsis<2000000)

TLESTART<-TLE
detach(tle)
attach(TLE)


#assign mass for each body - weibull distribution with sum of all masses = 6000T. Shape parameter found by trial and error to give realistic min and max masses
mass<-rweibull(length(TLE$ecc),0.71,scale=5000000/length(TLE$ecc))
TLE$mass<-mass
#M<-density(mass)



#assign XSA for each body - mass to XSA is from empirical data
area<-(((TLE$mass^(1/3))^2)/16)
TLE$xsa<-area
A<-density(area)


attach(TLE)

#calculate density of periapses
dper<-density(TLE$periapsis)
#periapsis function
perfun<-approxfun(dper$x,dper$y)

#calculate density of eccentricities
decc<-density(TLE$ecc)
eccfun<-approxfun(decc$x,decc$y)

#calculate density of inclinations
dinc<-density(TLE$inc)
#plot(dinc)
incfun<-approxfun(dinc$x,dinc$y)


#Add enhancement factor for inclination
#vector of inclinations
I<-c(0,10,20,30,40,50,60,70,75,80,85,90,95,100,105,110,120,130,140,150)

#vector of flux enhancement factors
fi<-c(0.8,0.83,0.87,0.9,0.92,0.97,1,1.1,1.35,1.7,1.5,1.4,1.35,1.55,1.8,1.6,1.2,1.1,0.9,0.8)



Fi<-approxfun(I,fi)
incenhance<-Fi(inc)
TLE$incenhance<-incenhance


attach(TLE)



#function to calculate peiapsis for each day
percalc<-function(period,ecc){
  per<-((1-ecc)*(399000000000000*(period/6.28)^2)^(1/3))-6371000
  return(per)
}


#function to calculate orbital decay due to drag (specifically reduction in period per day)
dpdtcalc<-function(periapsis,xsa,m,day){
  #solar flux (10.7cm)
  S<-40*(cos(2*(pi/11)*(5000+day/365.25)))+110
  
  #Ap index - roughly 1/10 of the solar flux (10.7cm)
  Ap<-S/10
  
  #Temperature
  T<-900+(2.5*(S-70))+(1.5*Ap)
  
  
  #m
  M<-27-0.012*((periapsis/1000)-200)
  
  #scale height
  H<-T/M
  
  #atmospheric density
  rho<-0.0000000006*exp(-1*(((periapsis/1000)-175)/H))
  
  #change in orbital period with time - seconds per day
  dpdt<-ifelse(periapsis<1200000, -3*3.14*(periapsis+6371000)*rho*((2*xsa)/m)*60*60*24,0)
  #DRAG MODEL ONLY COVERS ORBITS TO 1200km (only really accurate to 500km) - need to create model for orbits over 1200km (just add this model to end of ifelse statement)

  return(dpdt)
  #return(S)
  
}

#Function to calculate solar flux
solarcalc<-function(day){
  #solar flux (10.7cm)
  S<-40*(cos(2*(pi/11)*(5000+day/365.25)))+110
  
  #Ap index - roughly 1/10 of the solar flux (10.7cm)
  Ap<-S/10
  
  return(S)
}



days<-c(0)
alt<-c(0)
total<-c(0) #total number of bodies
periapsisdensity<-c(0)
inclinationdensity<-c(0)
decay<-c(0)
globalimpactprobability<-c(0)
solarflux<-c(0)
iflux<-c(0)
total.impacts<-0
impact.days<-0
collision.list<-data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
impacter.mass.list<-c(0)


TLE$impactno<-10
TLE$impactrandom<-10

days<-c(1:36500)

for(day in 1:1){

  
  solarflux[day]<-solarcalc(day)
  
  #remove decayed objects
  TLEp<-c(TLE$periapsis>180000)
  TLE<-TLE[TLEp,]
  
  TLEq<-c(TLE$periapsis<1999999)
  TLE<-TLE[TLEq,]
  
  
  #calculate new periapsis density
  histper<-hist(TLE$periapsis,breaks=seq(0,2000000,by=1000),plot=FALSE,main=day,ylim=c(0,380))

  #save histogram to file every day (CODE RUNS FAR SLOWER IF UNCOMMENTED)
  mypath <- file.path("C:","Users","Alex","Documents","R","HISTOGRAMS",paste("myplot_", days[day], ".jpg", sep = ""))
    jpeg(file=mypath)
# mytitle = paste("day", days[day])
  hist(TLE$periapsis,breaks=seq(0,2000000,by=1000),plot=TRUE,main=day,ylim=c(0,380))
  dev.off()
  
  #calculate new periapsis from period
 TLE$periapsis<-percalc(TLE$period,TLE$ecc)
  #calculate loss of period for next day
TLE$dpdt<-dpdtcalc(TLE$periapsis,TLE$xsa,TLE$mass,day)
  #calculate period for next day
  TLE$period<-TLE$period+TLE$dpdt
  
  
#Launch new satellite with probability 222/1000 per day
  launch.number<-runif(1,1,1000)
  #launch if number generated is less than 222
  if(launch.number<222){
    
    launchsatno<-runif(1,1,1000)
    launchinc<-sample(TLESTART$inc,1)
    launchra<-runif(1,1,360)
    launchecc<-sample(TLESTART$ecc,1)
    launchaop<-runif(1,1,360)
    launchma<-sample(TLESTART$ma,1)
    launchrpd<-sample(TLESTART$rpd,1)
    launchperiod<-((24*3600)/launchrpd)
    launchperiapsis<-((1-launchecc)*(399000000000000*(launchperiod/6.28)^2)^(1/3))-6371000
    launchmass<-sample(mass,1)
    launchxsa<-(((launchmass^(1/3))^2)/16)
    launchincenhance<-Fi(launchinc)
    #generate row for new satellite
    newsatellite<-c(launchsatno,launchinc,launchra,launchecc,launchaop,launchmass,launchrpd,launchperiod,launchperiapsis,launchmass,launchxsa,launchincenhance,0.00000000001,0.00000000001)
    
    #append new satellite onto TLE
    TLE<-rbind(TLE,newsatellite)
  }

  
  #Decombobulate satellite
  explosion.number<-runif(1,1,1000)
  #decombobulate if explosion number<15 (ie rate of 5 explosions per year)
  if(explosion.number<15){
    
    #pick random satelite to explode
    exploding.satellite<-sample(1:(nrow(TLE)),1)
    
    exploding.mass<-TLE$mass[exploding.satellite]
    
    #only explode satellites above 10kg
    if(exploding.mass>10){
      #explode mass into 50 fragments (exponential distribution) - multiplying by mass/50 preserves total mass
      exploded.masses<-(exploding.mass/50)*rweibull(50,1,1)
      
      explodedsatno<-runif(50,1,1000)
      explodeinc<-TLE$inc[exploding.satellite]+rnorm(50,0,2)##SD IS A GUESS - MAY CHANGE LATER
      explodera<-runif(50,1,360)
      explodeecc<-rep(TLE$ecc[exploding.satellite],50)#LEAVE AS SAME AS FOR EXPLODED SATELLITE FOR NOW
      explodeaop<-runif(50,1,360)
      explodema<-sample(tle$ma,50)
      exploderpd<-TLE$rpd[exploding.satellite]+rnorm(50,0,2)##SD IS A GUESS - MAY CHANGE LATER
      explodeperiod<-((24*3600)/exploderpd)
      explodeperiapsis<-((1-explodeecc)*(399000000000000*(explodeperiod/6.28)^2)^(1/3))-6371000
      explodexsa<-(((exploded.masses^(1/3))^2)/16)
      explodeincenhance<-Fi(explodeinc)
      
      exploded.objects<-as.data.frame(cbind(explodedsatno,explodeinc,explodera,explodeecc,explodeaop,explodema,exploderpd,explodeperiod,explodeperiapsis,exploded.masses,explodexsa,explodeincenhance,0.00000000001,0.00000000001,10,10))
      
      names(exploded.objects)<-names(TLE)
      

      
      TLE<-TLE[-exploding.satellite,]
      TLE<-rbind(TLE,exploded.objects)
      
      
      
    }
  }

  #remove decayed objects
  TLEp<-c(TLE$periapsis>180000)
  TLE<-TLE[TLEp,]
  
  TLEq<-c(TLE$periapsis<1999999)
  TLE<-TLE[TLEq,]
  
  
  
  #calculate new inclination density
  dinc<-density(TLE$inc)
#  incfun<-approxfun(dinc$x,dinc$y)
  
  #calculate debris flux for each object (collisions per time period) - NEED TO INCLUDE INCLINATION
  flux<-ifelse(histper$counts[TLE$periapsis/1000]>0, histper$counts[TLE$periapsis/1000]*K*TLE$xsa*Fi(TLE$inc),0.0000000000001)
  TLE$flux<-flux
  
  
  
#Determine collision occurance and result
  #chance of impact (1 in X)
  TLE$impactno<-1/TLE$flux
  
  #generate random no for each object
  TLE$impactrandom<-runif(nrow(TLE),1,TLE$impactno)
  
  #BASE THIS OFF EXPLOSION BUT INTRODUCE SECOND DEBRIS FIELD.  IF MASS<50 USE DIFFERENT MODEL (IN NOTEBOOK - IF MASS<50 (M/50)rexp(50,1))
  #if (i in chance of impact), collision = true
  
  #total number of satellites with impact=true
  if(length(which(TLE$impactrandom<=2))>=1){
   
    #record total collisions
    total.impacts<-total.impacts+1
    #record day of collision
    impact.days[total.impacts]<-day
    
    #returns row numer of any collision satellites
    collider.satellite<-which(TLE$impactrandom<=2)
    
    
    #if 2 satellites collide (extremely unlikely), reduce the number to one (to stop program from crashing)
    if(length(collider.satellite)>1){
      collider.satellite<-collider.satellite[1]
    }
    
    #record collider satellite
    names(collision.list)<-names(TLE)
    collision.list<-rbind(collision.list,TLE[collider.satellite,])
    
    #mass of collided satellite
    collider.mass<-TLE$mass[collider.satellite]
    
    #case 1 - 50kg+ collider
    if(collider.mass>=50){
      
      #masses of fragmented objects
      fragmented.masses<-rexp(collider.mass,1)
      
      fragmentedsatno<-runif(50,1,1000)
      fragmentedinc<-TLE$inc[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
      fragmentedra<-runif(50,1,360)
      fragmentedecc<-rep(TLE$ecc[collider.satellite],50)#LEAVE AS SAME AS FOR EXPLODED SATELLITE FOR NOW
      fragmentedaop<-runif(50,1,360)
      fragmentedma<-sample(tle$ma,50)
      fragmentedrpd<-TLE$rpd[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
      fragmentedperiod<-((24*3600)/fragmentedrpd)
      fragmentedperiapsis<-((1-fragmentedecc)*(399000000000000*(fragmentedperiod/6.28)^2)^(1/3))-6371000
      fragmentedxsa<-(((fragmented.masses^(1/3))^2)/16)
      fragmentedincenhance<-Fi(fragmentedinc)
      
      #data frame of fragmented objects
      fragmented.objects<-as.data.frame(cbind(fragmentedsatno,fragmentedinc,fragmentedra,fragmentedecc,fragmentedaop,fragmentedma,fragmentedrpd,fragmentedperiod,fragmentedperiapsis,fragmented.masses,fragmentedxsa,fragmentedincenhance,0,0,0,0.0000000000001))
      
      names(fragmented.objects)<-names(TLE)
      
      TLE<-TLE[-collider.satellite,]
      TLE<-rbind(TLE,fragmented.objects)
      
    }
    
    if(collider.mass<50){
      
      #masses of fragmented objects - generate 50 objects of total mass=collider mass
      fragmented.masses<-(collider.mass/50)*rexp(50,1)
      
      fragmentedsatno<-runif(50,1,1000)
      fragmentedinc<-TLE$inc[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
      fragmentedra<-runif(50,1,360)
      fragmentedecc<-rep(TLE$ecc[collider.satellite],50)#LEAVE AS SAME AS FOR EXPLODED SATELLITE FOR NOW
      fragmentedaop<-runif(50,1,360)
      fragmentedma<-sample(tle$ma,50)
      fragmentedrpd<-TLE$rpd[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
      fragmentedperiod<-((24*3600)/fragmentedrpd)
      fragmentedperiapsis<-((1-fragmentedecc)*(399000000000000*(fragmentedperiod/6.28)^2)^(1/3))-6371000
      fragmentedxsa<-(((fragmented.masses^(1/3))^2)/16)
      fragmentedincenhance<-Fi(fragmentedinc)
      
      #data frame of fragmented objects
      fragmented.objects<-as.data.frame(cbind(fragmentedsatno,fragmentedinc,fragmentedra,fragmentedecc,fragmentedaop,fragmentedma,fragmentedrpd,fragmentedperiod,fragmentedperiapsis,fragmented.masses,fragmentedxsa,fragmentedincenhance,0,0,0,0.0000000000001))
      
      names(fragmented.objects)<-names(TLE)
      
      TLE<-TLE[-collider.satellite,]
      TLE<-rbind(TLE,fragmented.objects)
      
      
    }
    
    #determine mass of impacter - pick a random object
    impacter.mass<-sample(TLE$mass,1)
    
    if(impacter.mass>=50){
    #masses of fragmented objects 
    fragmentedimpacter.masses<-rexp(impacter.mass,1)
    
    fragmentedimpactersatno<-runif(50,1,1000)
    fragmentedimpacterinc<-sample(TLE$inc,1)#sample from existing distribution
    fragmentedimpacterra<-runif(50,1,360)
    fragmentedimpacterecc<-sample(TLE$ecc,1)#sample from existing distribution
    fragmentedimpacteraop<-runif(50,1,360)
    fragmentedimpacterma<-sample(tle$ma,50)
    fragmentedimpacterrpd<-TLE$rpd[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
    fragmentedimpacterperiod<-((24*3600)/fragmentedimpacterrpd)
    fragmentedimpacterperiapsis<-((1-fragmentedimpacterecc)*(399000000000000*(fragmentedimpacterperiod/6.28)^2)^(1/3))-6371000
    fragmentedimpacterxsa<-(((fragmentedimpacter.masses^(1/3))^2)/16)
    fragmentedimpacterincenhance<-Fi(fragmentedimpacterinc)
    
    #data frame of fragmented objects
    fragmentedimpacter.objects<-as.data.frame(cbind(fragmentedimpactersatno,fragmentedimpacterinc,fragmentedimpacterra,fragmentedimpacterecc,fragmentedimpacteraop,fragmentedimpacterma,fragmentedimpacterrpd,fragmentedimpacterperiod,fragmentedimpacterperiapsis,fragmentedimpacter.masses,fragmentedimpacterxsa,fragmentedimpacterincenhance,0,0,0,0.0000000000001))
    
    names(fragmentedimpacter.objects)<-names(TLE)
    
   #add fragmented objects to total list
    TLE<-rbind(TLE,fragmentedimpacter.objects)
    
    #record mass of impacter
    impacter.mass.list[total.impacts]<-impacter.mass
    }
    
    
    
    if(impacter.mass<50){
      
      #masses of fragmented objects - generate 50 objects of total mass=collider mass
      fragmentedimpacter.masses<-(impacter.mass/50)*rexp(50,1)
      

      fragmentedimpactersatno<-runif(50,1,1000)
      fragmentedimpacterinc<-sample(TLE$inc,1)#sample from existing distribution
      fragmentedimpacterra<-runif(50,1,360)
      fragmentedimpacterecc<-sample(TLE$ecc,1)#sample from existing distribution
      fragmentedimpacteraop<-runif(50,1,360)
      fragmentedimpacterma<-sample(tle$ma,50)
      fragmentedimpacterrpd<-TLE$rpd[collider.satellite]+rnorm(50,0,0.1)##SD IS A GUESS - MAY CHANGE LATER
      fragmentedimpacterperiod<-((24*3600)/fragmentedimpacterrpd)
      fragmentedimpacterperiapsis<-((1-fragmentedimpacterecc)*(399000000000000*(fragmentedimpacterperiod/6.28)^2)^(1/3))-6371000
      fragmentedimpacterxsa<-(((fragmentedimpacter.masses^(1/3))^2)/16)
      fragmentedimpacterincenhance<-Fi(fragmentedimpacterinc)
      
      #data frame of fragmented objects
      fragmentedimpacter.objects<-as.data.frame(cbind(fragmentedimpactersatno,fragmentedimpacterinc,fragmentedimpacterra,fragmentedimpacterecc,fragmentedimpacteraop,fragmentedimpacterma,fragmentedimpacterrpd,fragmentedimpacterperiod,fragmentedimpacterperiapsis,fragmentedimpacter.masses,fragmentedimpacterxsa,fragmentedimpacterincenhance,0,0,0,0.0000000000001))
      
      names(fragmentedimpacter.objects)<-names(TLE)
      
      
      TLE<-rbind(TLE,fragmentedimpacter.objects)
      
      #record mass of impacter
      impacter.mass.list[total.impacts]<-impacter.mass
      
    }
    
    
  }
  
  

  
  
  
  
  #UNCOMMENT TO RECORD DAYS VS ALTITUDE FOR A GIVEN OBJECT
  days[day]<-day
  alt[day]<-TLE$periapsis[15]
  
#outputs for each day
  total[day]<-nrow(TLE)
  
  #break loop if total number of objects is greater than 100000
  if(nrow(TLE)>100000){
    break
  }
  
  #change numbers to get specific arguments
  periapsisdensity[day]<-histper$counts[800]
  
  inclinationdensity[day]<-incfun(70)
  
  #probability of a collision on given day
  globalimpactprobability[day]<-sum(TLE$flux,na.rm=TRUE)
  
  #decay rate per day for given object
  decay[day]<-TLE$dpdt[15]
  
  #impact flux for given object
  iflux[day]<-TLE$flux[15]
  

}



#Uncomment below for plots

x<-c(1:day)

#total probability of impact/day
total.impact.probability[seed]<-sum(TLE$flux,na.rm=TRUE)

#histogram of debris by periapsis at start
hist(TLESTART$periapsis,breaks=seq(0,2000000,by=1000))

#save histogram of periapsis every 1000 days
#write.table(histogram.end$counts,"periapsis.histogram",append=TRUE)

#Plots of densities for original configuration (periapsis, area, eccentricity, mass, inclination enhancement factor)
#plot(dper)
#plot(A)
#plot(decc)
#plot(M)
#plot(I,fi)

#plot density at given periapsis
plot(c(1:day),periapsisdensity)

#plot density at given inclination
#plot(c(1:day),inclinationdensity)

#plot of total objects by day over entire simulation
plot(x,total)
write.table(total,"total",append=TRUE)

#plot probability of impact by day
plot(x,globalimpactprobability)
write.table(globalimpactprobability,"global.impact.probability",append=TRUE)

#plot solar flux cycle
plot(x,solarflux)

#plot decay rate for selected object
#plot(x,decay)

#plot impact flux for given objet
#plot(x,iflux)

#plot altitude by day for given object
#plot(x,alt)


#histogram of debris by periapsis at end
histogram.end<-hist(TLE$periapsis,breaks=seq(0,2000000,by=1000))

#write histogram to file
#write.table(histogram.end$counts,"periapsis.histogram",append=TRUE)


#other outputs are:
#impacter.mass.list - list of masses of impacters
write.table(impacter.mass.list,"impacter.mass.list",append=TRUE)
#collision.list - list of target objects from collisions
write.table(collision.list,"collision.list",append=TRUE)
}

write.table(total.impact.probability,"total.impact.probability")
