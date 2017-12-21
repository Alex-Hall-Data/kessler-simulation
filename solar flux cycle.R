
#day will be used as loop counter
day<-seq(from=1,to=10000,by=1)


solar.flux<-function(day){
S<-40*(cos(2*(3.141/11)*(day/365.25)))+110
return(S)
}

flux.vector<-lapply(day,solar.flux)