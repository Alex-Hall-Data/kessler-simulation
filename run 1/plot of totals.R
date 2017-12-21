total<-read.table("total table.txt")

run1<-total$V2[2:36501]
run2<-total$V2[36503:73002]
run3<-total$V2[73004:109503]
run4<-total$V2[109505:146004]
run5<-total$V2[146006:182505]
run6<-total$V2[182507:219006]
run7<-total$V2[219008:255507]
run8<-total$V2[255509:292008]
run9<-total$V2[292010:328509]
run10<-total$V2[328511:365010]

x<-c(0:36499)

plot(x,run1,type="l",col="red",main="total obects in orbit - 10 seperate simulation runs",xlab="day",ylab="total objects")
points(x,run2,type="l",col="red")
points(x,run3,type="l",col="red")
points(x,run4,type="l",col="red")
points(x,run5,type="l",col="red")
points(x,run6,type="l",col="red")
points(x,run7,type="l",col="red")
points(x,run8,type="l",col="red")
points(x,run9,type="l",col="red")
points(x,run10,type="l",col="red")