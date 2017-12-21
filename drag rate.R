
#m<-862
#xsa<-5.6
#periapsis<-400000
#day=1

data1<-read.table("tle sample.txt",header=TRUE)

data<-as.data.frame(data1)


percalc<-function(period,ecc){
  per<-((1-ecc)*(399000000000000*(period/6.28)^2)^(1/3))-6371000
  return(per)
}



dpdtcalc<-function(periapsis,xsa,m,day){
  #solar flux (10.7cm)
  S<-40*(cos(2*(pi/11)*(day/365.25)))+110
  
  #Ap index - roughly 1/10 of the solar flux (10.7cm)
  Ap<-S/10
  
  #Temperature
  T<-900+(2.5*(S-70))+(1.5*Ap)
  
  
  #m
  m<-27-0.012*((periapsis/1000)-200)
  
  #scale height
  H<-T/m
  
  #atmospheric density
  rho<-0.0000000006*exp(-1*(((periapsis/1000)-175)/H))
  
  #change in orbital period with time - seconds per day
  dpdt<- -3*3.14*(periapsis+6371000)*rho*((2*xsa)/m)*60*60*24
  
  return(dpdt)
  
}

days<-c(0)
alt<-c(0)

for(day in 1:10){
  #calculate new periapsis from period
data$periapsis<-percalc(data$period,data$ecc)
#calculate loss of period for next day
data$dpdt<-dpdtcalc(data$periapsis,data$xsa,data$mass,day)
#calculate period for next day
data$period<-data$period+data$dpdt

#UNCOMMENT TO RECORD DAYS VS ALTITUDE FOR A GIVEN OBJECT
days[day]<-day
alt[day]<-data$periapsis[1]

}