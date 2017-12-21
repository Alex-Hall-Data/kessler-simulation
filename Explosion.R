explosion.number<-runif(1,1,1000)

if(explosion.number<15){
  
  exploding.satellite<-sample(1:(nrow(TLE)),1)
  
  exploding.mass<-TLE$mass[exploding.satellite]
  
  #only explode satellites above 10kg
  if(exploding.mass>10){
    #explode mass into 50 fragments (exponential distribution)
    exploded.masses<-(exploding.mass/50)*rexp(50,1)
    
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
    
    exploded.objects<-as.data.frame(cbind(explodedsatno,explodeinc,explodera,explodeecc,explodeaop,explodema,exploderpd,explodeperiod,explodeperiapsis,exploded.masses,explodexsa,explodeincenhance,0,0))
    
    names(exploded.objects)<-names(TLE)
    
    TLE<-TLE[-exploding.satellite,]
    TLE<-rbind(TLE,exploded.objects)
  
    
  
  }
}