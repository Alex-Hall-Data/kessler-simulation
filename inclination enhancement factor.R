#Add enhancement factor for inclination
#vector of inclinations
I<-c(0,10,20,30,40,50,60,70,75,80,85,90,95,100,105,110,120,130,140,150)

#vector of flux enhancement factors
fi<-c(0.8,0.83,0.87,0.9,0.92,0.97,1,1.1,1.35,1.7,1.5,1.4,1.35,1.55,1.8,1.6,1.2,1.1,0.9,0.8)

plot(I,fi)

Fi<-approxfun(I,fi)
