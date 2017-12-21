collisions<-read.table("collision.list table.txt",header=TRUE)

collisions.run1<-collisions[2:47,]
collisions.run2<-collisions[49:73,]
collisions.run3<-collisions[75:85,]
collisions.run4<-collisions[87:106,]
collisions.run5<-collisions[108:126,]
collisions.run6<-collisions[125:136,]#this is correct
collisions.run7<-collisions[141:163,]
collisions.run8<-collisions[162:187,]#this is correct
collisions.run9<-collisions[193:212,]
collisions.run10<-collisions[214:223,]

colliders<-read.table("impacter.mass.list table.txt")
colliders.run1<-colliders$V2[2:47]
colliders.run2<-colliders$V2[49:73]
colliders.run3<-colliders$V2[75:85]
colliders.run4<-colliders$V2[87:106]
colliders.run5<-colliders$V2[108:126]
colliders.run6<-colliders$V2[126:137]#this is correct
colliders.run7<-colliders$V2[141:163]
colliders.run8<-colliders$V2[163:188]#this is correct
colliders.run9<-colliders$V2[193:212]
colliders.run10<-colliders$V2[214:223]

mass.run.6<-as.numeric(as.vector(collisions.run6$xsa))#using xsa as this is the heading for the mass colume - headings hae shifted by 1 somehow
hist(mass.run.6,col=rgb(1,0,0,0.5),xlab="object mass",ylim=c(0,25),main="distribution of impactor and target masses for run 6")
hist(colliders.run6,col=rgb(0,0,1,0.5),add=T)
legend("topright", c("target", "impacter"), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5) ), lwd=5)

box()
