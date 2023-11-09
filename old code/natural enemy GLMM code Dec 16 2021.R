setwd("/Users/zhajianf/desktop/July 2021 desktop/Oct 30 desktop/Research 2019/Puerto Rico 25 farm data final July 26 2019")

plant_data <- read.csv("2021_11_1_PR_data_with_landscape_classification_all_scales_all_farms.csv")

library(lme4)

data.all <- plant_data

forest.est <-numeric(15) ## a model for each spatial scale 
forest.error <-numeric(15)
forest.z <-numeric(15)
forest.p <-numeric(15)

agri.est <-numeric(15)
agri.error <-numeric(15)
agri.z <-numeric(15)
agri.p <-numeric(15)

past.est <-numeric(15)
past.error <-numeric(15)
past.z <-numeric(15)
past.p <-numeric(15)

roya.est <-numeric(15)
roya.error <-numeric(15)
roya.z <-numeric(15)
roya.p <-numeric(15)

model.intercept <-numeric(15)
indexer <- 1
AIC.vals <- numeric(15)
for(x in 24:38){

	## Here is a model excluding developed land 
	model<- glmer(data.all$myco ~ data.all[,x] + data.all[,x + 15] +data.all[,x + 30] + data.all$roya  +(1| data.all$site_name) + (1|data.all$visit),family="poisson")
  summary(model)
  ### Here we can check for overdisperson and zero inflation
	
#   simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
#	plot(simulationOutput)
#   testDispersion(simulationOutput)
#   testZeroInflation(simulationOutput)

	
	AIC.vals[indexer] <- summary(model)$AIC[1]
	model.intercept[indexer] <- summary(model)$coefficients[1,1]
	
	forest.est[indexer] <- summary(model)$coefficients[2,1]
	forest.error[indexer] <-  summary(model)$coefficients[2,2]
	forest.z[indexer] <- summary(model)$coefficients[2,3]
	forest.p[indexer] <- summary(model)$coefficients[2,4]
	
	agri.est[indexer] <- summary(model)$coefficients[3,1]
	agri.error[indexer] <- summary(model)$coefficients[3,2]
	agri.z[indexer] <- summary(model)$coefficients[3,3]
	agri.p[indexer] <- summary(model)$coefficients[3,4]
	
	past.est[indexer] <- summary(model)$coefficients[4,1]
	past.error[indexer] <- summary(model)$coefficients[4,2]
	past.z[indexer] <- summary(model)$coefficients[4,3]
	past.p[indexer] <- summary(model)$coefficients[4,4]
	
	roya.est[indexer] <- summary(model)$coefficients[5,1]
	roya.error[indexer] <-summary(model)$coefficients[5,2] 
	roya.z[indexer] <- summary(model)$coefficients[5,3]
	roya.p[indexer] <- summary(model)$coefficients[5,4]
	
	indexer <- indexer +1
}

### Mycodiplosis plots

par(mfrow=c(1,3),mai=c(0.5,0.05,0.2,0),oma=c(1,5,1,1))
col.for <- rep("black",length(forest.p))
col.for[forest.p <= 0.05] <- "dark red"
col.for[forest.p <= 0.01] <- "red"

scales.vec <- seq(100,1500,100)
plot(NA, NA,xlim=c(-20,20),ylim=c(100,1500),xlab="",ylab="",yaxt="n")
axis(2,las=2) 
abline(v=0,lty=3,col="red")
mtext("Estimate",side=1,line=2.5,cex=0.8)
#mtext("Spatial scale(m)",side=2,line=3,cex=0.8)
points(forest.est, scales.vec,pch=17,col=col.for)
arrows(forest.est + forest.error, scales.vec,forest.est- forest.error, scales.vec,code=3,angle=90,length=0.01,col=col.for)
mtext("% forest in landscape",cex=0.8,line=0.3)

#### agriculture plot 
col.agr <- rep("black",length(agri.p))
col.agr[agri.p <= 0.05] <- "dark red"
col.agr[agri.p <= 0.01] <- "red"
scales.vec <- seq(100,1500,100)
plot(NA, NA,xlim=c(-20,20),ylim=c(100,1500),xlab="",ylab="",yaxt="n")
#axis(2,las=2) 
abline(v=0,lty=3,col="red")
mtext("Estimate",side=1,line=2.5,cex=0.8)
#mtext("Spatial scale(m)",side=2,line=3,cex=0.8)
points(agri.est, scales.vec,pch=18,col= col.agr,cex=1.3)
arrows(agri.est + agri.error, scales.vec, agri.est- agri.error, scales.vec,code=3,angle=90,length=0.01,col= col.agr)
mtext("% agriculture in landscape",cex=0.8,line=0.3)

#### pasture plot 
col.past <- rep("black",length(past.p))
col.past[past.p <= 0.05] <- "dark red"
col.past[past.p <= 0.01] <- "red"
scales.vec <- seq(100,1500,100)
plot(NA, NA,xlim=c(-50,100),ylim=c(100,1500),xlab="",ylab="",yaxt="n")
#axis(2,las=2) 
abline(v=0,lty=3,col="red")
mtext("Estimate",side=1,line=2.5,cex=0.8)
#mtext("Spatial scale(m)",side=2,line=3,cex=0.8)
points(past.est, scales.vec,pch= 19,col= col.past,cex=1)
arrows(past.est + past.error, scales.vec, past.est- past.error, scales.vec,code=3,angle=90,length=0.01,col= col.past)
mtext("% pasture in landscape",cex=0.8,line=0.3)

mtext("Spatial scale (m)",outer=T,side=2,line=3)


par(mfrow=c(1,1),mai=c(0.9,0.7,0.2,0),oma=c(1,2,1,1))
plot(scales.vec,AIC.vals,yaxt="n",xlab="",ylab="",xaxt="n",pch=19)
axis(2,las=2)
axis(1,las=2)
mtext("Spatial scale (m)",side=1,line=3)
mtext("AIC",side=2,line=4)
abline(h=min(AIC.vals),col="red",lty=3,lwd=2)
abline(h=min(AIC.vals) + 2,col="red",lty=3,lwd=2)



