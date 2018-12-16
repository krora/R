library(UsingR)
data(father.son)


plot(father.son$fheight, father.son$sheight, xlab="Father's height (in)", 
  ylab="Son's height (in)", xlim=c(58,70), ylim=c(58,80), pch=20);

abline(lm(father.son$sheight~father.son$fheight));

father<-father.son$fheight

son<-father.son$sheight

sdf<-sd(father)
sds<-sd(son)


family<-sds/sdf
family

mf<-mean(father)
ms<-mean(son)


C<- ms-(family*mf)

abline(C,family,col="blue", lty=4, lwd=3)

abline(h=ms, v=mf, col= "green")

summary(lm(son~father))


