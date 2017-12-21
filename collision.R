#chance of impact (1 in X)
TLE$impactno<-1/TLE$flux

#generate random no for each object
TLE$impactrandom<-runif(length(TLE$flux),1,TLE$impactno)

#BASE THIS OFF EXPLOSION BUT INTRODUCE SECOND DEBRIS FIELD.  IF MASS<50 USE DIFFERENT MODEL (IN NOTEBOOK - IF MASS<50 (M/50)rexp(50,1))
#if (i in chance of impact), collision = true

#total number of satellites with impact=true
if(length(which(TLE$impactrandom<=1))>=1){

#returns row numer of any collision satellites
collider.satellite<-which(TLE$impactrandom<=1)

#if 2 satellites collide (extremely unlikely), reduce the number to one (to stop program from crashing)
if(length(collider.satellite)>1){
  collider.satellite<-collider.satellite[1]
}
  
#mass of collided satellite
collider.mass<-TLE$mass[collider.satellite]

#case 1 - 50kg+ collider
if(collider.mass>=50){
  
  #masses of fragmented objects
  fragmented.masses<-rexp(collider.mass,1)
  
  fragmentedsatno<-runif(50,1,1000)
  fragmentedinc<-TLE$inc[collider.satellite]+rnorm(50,0,2)##SD IS A GUESS - MAY CHANGE LATER
  fragmentedra<-runif(50,1,360)
  fragmentedecc<-rep(TLE$ecc[collider.satellite],50)#LEAVE AS SAME AS FOR EXPLODED SATELLITE FOR NOW
  fragmentedaop<-runif(50,1,360)
  fragmentedma<-sample(tle$ma,50)
  fragmentedrpd<-TLE$rpd[collider.satellite]+rnorm(50,0,2)##SD IS A GUESS - MAY CHANGE LATER
  fragmentedperiod<-((24*3600)/fragmentedrpd)
  fragmentedperiapsis<-((1-fragmentedecc)*(399000000000000*(fragmentedperiod/6.28)^2)^(1/3))-6371000
  fragmentedxsa<-(((fragmented.masses^(1/3))^2)/16)
  fragmentedincenhance<-Fi(fragmentedinc)
  
  #data frame of fragmented objects
  fragmented.objects<-as.data.frame(cbind(fragmentedsatno,fragmentedinc,fragmentedra,fragmentedecc,fragmentedaop,fragmentedma,fragmentedrpd,fragmentedperiod,fragmentedperiapsis,fragmented.masses,fragmentedxsa,fragmentedincenhance,0,0,0,0))
  
  names(exploded.objects)<-names(TLE)
  
  TLE<-TLE[-collider.satellite,]
  TLE<-rbind(TLE,fragmented.objects)
  
}
  
  
}