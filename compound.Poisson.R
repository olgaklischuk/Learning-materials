poisson=1
if(poisson==1){
#Linear#####
CogPois<-setPoisson(intensity="alpha+lambda*t",
                    df=list("dnorm(z,mu,sigma)"),dimension=1)
set.seed(123)
  Cogparpoisson11 <- list(alpha=1,lambda=.5,mu=0, sigma=2)
CogPois.2<-setPoisson(intensity="alpha+lambda*t",
                      df=list("dnorm(z,mu,sigma)"),dimension=1)
CogPois.3<-setPoisson(intensity="alpha+lambda*t",
                      df=list("dnorm(z,mu,sigma)"),dimension=1)
CogPois.4<-setPoisson(intensity="alpha+lambda*t",
                      df=list("dnorm(z,mu,sigma)"),dimension=1)
CogPois.5<-setPoisson(intensity="alpha+lambda*t",
                      df=list("dnorm(z,mu,sigma)"),dimension=1)}else{if(poisson==2){  Cogparpoisson11 <-list(a=2,omega=0.5,phi=3.14,lambda=5,mu=0,sigma=1)
#Periodical####
CogPois<-setPoisson(intensity="0.5*a*(1+cos(omega*t+phi))+lambda",
df=list("dnorm(z,mu,sigma)"))
CogPois.2<-setPoisson(intensity="0.5*a*(1+cos(omega*t+phi))+lambda",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.3<-setPoisson(intensity="0.5*a*(1+cos(omega*t+phi))+lambda",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.4<-setPoisson(intensity="0.5*a*(1+cos(omega*t+phi))+lambda",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.5<-setPoisson(intensity="0.5*a*(1+cos(omega*t+phi))+lambda",
                    df=list("dnorm(z,mu,sigma)"))
                      }else{if(poisson==3){Cogparpoisson11<-list(lambda=2,mu=0, sigma=2)
###Weibul#####

CogPois<-setPoisson(intensity="lambda*t^(lambda-1)",
           df=list("dnorm(z,mu,sigma)"))
CogPois.2<-setPoisson(intensity="lambda*t^(lambda-1)",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.3<-setPoisson(intensity="lambda*t^(lambda-1)",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.4<-setPoisson(intensity="lambda*t^(lambda-1)",
                    df=list("dnorm(z,mu,sigma)"))
CogPois.5<-setPoisson(intensity="lambda*t^(lambda-1)",
                    df=list("dnorm(z,mu,sigma)"))}}}


sim1.cp <- simulate(CogPois, true.parameter = Cogparpoisson11, sampling = setSampling(0,Terminal = 9.2529,135),
                    method = "mixed")
s1<-sim1.cp@data@original.data[,3]
G1<-sim1.cp@data@original.data[,2]
varianceCP1<-data.frame(sim1.cp@data@zoo.data)[,1]
incr.poisson1<-G1/sqrt(varianceCP1)

sim2.cp <- simulate(CogPois.2, true.parameter = Cogparpoisson11, sampling = setSampling(0,Terminal = 8.4270,110),
                    method = "mixed")
s2<-sim2.cp@data@zoo.data[,3]
G2<-sim2.cp@data@original.data[,2]
varianceCP2<-data.frame(sim2.cp@data@zoo.data)[,1]
incr.poisson2<-G2/sqrt(varianceCP2)

sim3.cp <- simulate(CogPois.3, true.parameter = Cogparpoisson11, sampling = setSampling(0,Terminal =  8.3711,104),
                    method = "mixed")
s3<-sim3.cp@data@original.data[,3]
G3<-sim3.cp@data@original.data[,2]
varianceCP3<-data.frame(sim3.cp@data@zoo.data[,1])
incr.poisson3<-G3/sqrt(varianceCP53)

sim4.cp <- simulate(CogPois.4, true.parameter = Cogparpoisson11, sampling = setSampling(0,Terminal = 8.2684,104),
                    method = "mixed")
s4<-sim4.cp@data@zoo.data[,3]
G4<-sim4.cp@data@original.data[,2]
varianceCP4<-data.frame(sim4.cp@data@zoo.data)[,1]
incr.poisson4<-G4/sqrt(varianceCP4)

sim5.cp <- simulate(CogPois.5, true.parameter = Cogparpoisson11, sampling = setSampling(0,Terminal = 2.1334,135),nsim = 135,
                    method = "code")
# s5<-sim5.cp@data@original.data[,3]
# G5<-sim5.cp@data@original.data[,2]
varianceCP5<-data.frame(sim5.cp@data@zoo.data)[,1]
# incr.poisson5<-G5/sqrt(varianceCP5)
dir.create("H:/GARCH code/data/Poisson process")
setwd("Poisson"<-"H:/GARCH code/data/Poisson process")
# par(mfrow=c(3,2),mai=c(1.7,1,2.5,.5),oma=c(2,2,2.5,1),mar=c(2,1.5,2.5,1))
# yuima::plot(s1,main=FER.U[1],ylab="часова серія",xlab="alpha=1,lambda=.5,mu=0, sigma=2")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
# title(main=c("alpha=1,lambda=0.5, mu=0, sigma=2","0.5*a*(1+cos(omega*t+phi))+lambda","lambda*t^(lambda-1)")[poisson],outer=TRUE)
# yuima::plot(s2,main=FER.U[2],ylab="часова серія",xlab="alpha=1,lambda=.5,mu=0, sigma=2")+grid(col="lightgray",lty="dotted",lwd=par("lwd"))+title();yuima::plot(s3,main=FER.U[3],xlab="alpha=1,lambda=.5,mu=0, sigma=2")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(s4,main=FER.U[4],xlab="alpha=1,lambda=.5,mu=0, sigma=2")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(s5,main=FER.U[5],xlab="alpha=1,lambda=.5,mu=0, sigma=2")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
# grDevices::savePlot("Рис. 3.5.2 Симуляція трендів валютних курсів за Пуасонівським розподілом шоків.png",type="png")
#
# par(mfrow=c(3,2),mai=c(1.7,1,2.5,.5),oma=c(2,2,2.5,1),mar=c(2,1.5,2.5,1))
# yuima::plot(G1,main=FER.U[1],ylab="середньоквадратичне відхилення",xlab="t")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
# title(main=c("alpha=1,lambda=0.5, mu=0, sigma=2","0.5*a*(1+cos(omega*t+phi))+lambda","lambda*t^(lambda-1)")[poisson],outer=TRUE)
# yuima::plot(G2,main=FER.U[2],ylab="середньоквадратичне відхилення")+grid(col="lightgray",lty="dotted",lwd=par("lwd"))+title();yuima::plot(G3,main=FER.U[3])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(G4,main=FER.U[4])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(G5,main=FER.U[5])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
# grDevices::savePlot("Рис. 3.5.2 Симуляція середньоквадратичних відхилень валютних курсів за Пуасонівським розподілом шоків.png",type="png")

par(mfrow=c(3,2),mai=c(1.7,1,2.5,.5),oma=c(2,2,2.5,1),mar=c(2,1.5,2.5,1))
yuima::plot(varianceCP1,main=FER.U[1],ylab="дисперсія",xlab="t")+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
title(main=c("alpha=1,lambda=0.5, mu=0, sigma=2","0.5*a*(1+cos(omega*t+phi))+lambda","lambda*t^(lambda-1)")[poisson],outer=TRUE)
yuima::plot(varianceCP2,main=FER.U[2],ylab="дисперсія")+grid(col="lightgray",lty="dotted",lwd=par("lwd"))+title();yuima::plot(varianceCP3,main=FER.U[3])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(varianceCP4,main=FER.U[4])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"));yuima::plot(varianceCP5,main=FER.U[5])+title()+grid(col="lightgray",lty="dotted",lwd=par("lwd"))
grDevices::savePlot(paste("Рис. 3.5.",poisson+1, "Симуляція дисперсій валютних курсів за Пуасонівським розподілом шоків.png"),type="png")


if(poisson==1){
lower=list(alpha = 0.1,lambda = 0.1, mu = -1, sigma = 0.1)
upper=list(alpha = 10,lambda = 10, mu = 3, sigma =4)
}else{if(poisson==2){
lower=list(a=.1, omega=0.1, phi=.1, lambda=.1, mu=-2, sigma=.1)
upper=list(a=5, omega=1, phi=5, lambda=10, mu=2, sigma=3)
}else{if(poisson==3){
lower=list(lambda = 0.1, mu = -1,sigma = 0.1)
upper=list(lambda = 10, mu = 3, sigma = 4)}}}
param.pois<-qmle(sim1.cp,start=Cogparpoisson11,lower=lower,upper=upper,method="L-BFGS-B")#Est.Incr = "Incr.par")
param.pois2<-qmle(sim2.cp,start=Cogparpoisson11,lower=lower,upper=upper,method="L-BFGS-B")#Est.Incr = "Incr.par")
param.pois3<-qmle(sim3.cp,start=Cogparpoisson11,lower=lower,upper=upper,method="L-BFGS-B")
param.pois4<-qmle(sim4.cp,start=Cogparpoisson11,lower=lower,upper=upper,method="L-BFGS-B")
param.pois5<-qmle(sim5.cp,start=Cogparpoisson11,lower=lower,upper=upper,method="L-BFGS-B")



##############
# arch.test.1.15<-ArchTest(incr.poisson1, lags = 5)
# arch.test.1.16<-ArchTest(incr.poisson2, lags = 5)
# arch.test.1.17<-ArchTest(incr.poisson3, lags = 5)
# arch.test.1.18<-ArchTest(incr.poisson4, lags = 5)
# arch.test.1.19<-ArchTest(incr.poisson5, lags = 5)
# arch.report.2<-data.frame(matrix(c(arch.test.1.15$statistic,arch.test.1.16$statistic,arch.test.1.17$statistic,arch.test.1.18$statistic,arch.test.1.19$statistic,
#                  arch.test.1.15$p.value,arch.test.1.16$p.value,arch.test.1.17$p.value,arch.test.1.18$p.value,arch.test.1.19$p.value),ncol=2))
# colnames(arch.report.2)<-c("statistics","p.value")
# rownames(arch.report.2)<-c("NBU rate","Interbank rate","BdE buy","BdE sell","REER")
arch.test.1.15<-ArchTest(varianceCP1, lags = 5)
arch.test.1.16<-ArchTest(varianceCP2, lags = 5)
arch.test.1.17<-ArchTest(varianceCP3, lags = 5)
arch.test.1.18<-ArchTest(varianceCP4, lags = 5)
arch.test.1.19<-ArchTest(varianceCP5, lags = 5)
arch.report.3<-data.frame(matrix(c(arch.test.1.15$statistic,arch.test.1.16$statistic,arch.test.1.17$statistic,arch.test.1.18$statistic,arch.test.1.19$statistic,
                                   arch.test.1.15$p.value,arch.test.1.16$p.value,arch.test.1.17$p.value,arch.test.1.18$p.value,arch.test.1.19$p.value),ncol=2))
colnames(arch.report.3)<-c("statistics","p.value")
rownames(arch.report.3)<-c("NBU rate","Interbank rate","BdE buy","BdE sell","REER")

# arch.test.1.15<-ArchTest(G1, lags = 5)
# arch.test.1.16<-ArchTest(G2, lags = 5)
# arch.test.1.17<-ArchTest(G3, lags = 5)
# arch.test.1.18<-ArchTest(G4, lags = 5)
# arch.test.1.19<-ArchTest(G5, lags = 5)
# arch.report.4<-data.frame(matrix(c(arch.test.1.15$statistic,arch.test.1.16$statistic,arch.test.1.17$statistic,arch.test.1.18$statistic,arch.test.1.19$statistic,
#                                    arch.test.1.15$p.value,arch.test.1.16$p.value,arch.test.1.17$p.value,arch.test.1.18$p.value,arch.test.1.19$p.value),ncol=2))
# colnames(arch.report.4)<-c("statistics","p.value")
# rownames(arch.report.4)<-c("NBU rate","Interbank rate","BdE buy","BdE sell","REER")
