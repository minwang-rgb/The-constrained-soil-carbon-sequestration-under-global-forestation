# Core data manipulation and I/O
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(janitor)
library(tidyr)  # Often used with dplyr, not in your list but recommended

# Meta-analysis
library(metafor)
library(metagear)
library(Rmisc)

# Multiple imputation
library(mice)
library(brms)

# Statistical modeling
library(MASS)
library(nnet)
library(boot)
library(glmulti)

# Visualization
library(ggplot2)
library(lattice)

# Performance and parallel computing
library(tictoc)
library(parallel)

setwd("/Users/min/Desktop/NEE")

#Part one: compute SOC effect size#########
#step one: compute individual effect size
 
dat1 <- read.csv("2025S0318KOoutli.csv")
dat3 <- escalc(data=dat1,measure="ROM", m1i=Xe, sd1i=Sde,
                 n1i=Ne, m2i=Xc, sd2i=Sdc, n2i=Nc)
 dat3$se <- sqrt(dat3$vi)
write.csv(dat3, file="2025SInew.csv")

#step two: compute general mean effect size
#total groups#####
#Cropland#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 


df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]




df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("active","passive","SOC")

names(df)[1]="croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]




df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("active","passive","SOC")

names(df1)[1]="croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"



# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"

data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/cropland.csv")

rm(list=ls())











#Natural forest#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)



write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/Natural.csv")



rm(list=ls())



#Abandoned land#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Abandoned",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Abandonedtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Abandoned",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Abandonedtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)

write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/Abandoned.csv")



rm(list=ls())

#Bare land#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Bareland",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Barelandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Bareland",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Barelandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)

write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/Bareland.csv")



rm(list=ls())



#Grassland#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)

write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/Grassland.csv")



rm(list=ls())


#Desert & semi-desert#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Sandy",] #Sandy refers to Desert & semi-desert
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Sandytop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Sandy",] 
dat6 <- dat2[dat2$Soil.layer=="subsoil",] 

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Sandytop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
as.data.frame(df1)
data_msd8 <- rbind.fill(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)

write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/total/Sandy.csv")



rm(list=ls())




#Age group#############
#######################CroplandP1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$Age=="P1",] # P1 refers to age 0-20
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$Age=="P1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/CroplandP1.csv")



rm(list=ls())



#######################CroplandP2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$Age=="P2",] # P1 refers to age 20-40
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$Age=="P2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/CroplandP2.csv")



rm(list=ls())


######################CroplandP3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$Age=="P3",] # P1 refers to age 40+
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$Age=="P3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/CroplandP3.csv")



rm(list=ls())

#######################NaturalP1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$Age=="P1",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$Age=="P1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]
r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/NaturalP1.csv")



rm(list=ls())



#######################NaturalP2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$Age=="P2",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$Age=="P2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]
r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/NaturalP2.csv")



rm(list=ls())


######################NaturalP3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$Age=="P3",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$Age=="P3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]
r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/NaturalP3.csv")



rm(list=ls())

#######################GrasslandP1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$Age=="P1",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$Age=="P1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/GrasslandP1.csv")



rm(list=ls())



#######################GrasslandP2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$Age=="P2",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$Age=="P2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/GrasslandP2.csv")



rm(list=ls())


######################GrasslandP3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$Age=="P3",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$Age=="P3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/age/GrasslandP3.csv")



rm(list=ls())


#Biome group ###################
#######################Grassland1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="1",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland1.csv")



rm(list=ls())

#######################Grassland2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="2",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland2.csv")



rm(list=ls())

#######################Grassland3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="3",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland3.csv")



rm(list=ls())

#######################Grassland4#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="4",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="4",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland4.csv")



rm(list=ls())

#######################Grassland5#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="5",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="5",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland5.csv")



rm(list=ls())

#######################Grassland6#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="6",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"




data_msdfinal1 <- rbind(data_msd7)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland6.csv")



rm(list=ls())

#######################Grassland7#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat3 <- dat2[dat2$biome=="9",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Grasslandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat4 <- dat2[dat2$biome=="9",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Grasslandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Grassland9.csv")



rm(list=ls())



#######################Cropland1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="1",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$biome=="1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland1.csv")



rm(list=ls())

#######################Cropland2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="2",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$biome=="2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland2.csv")



rm(list=ls())

#######################Cropland3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="3",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$biome=="3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland3.csv")



rm(list=ls())

#######################Cropland4#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="4",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


data_msdfinal1 <- rbind(data_msd7)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland4.csv")



rm(list=ls())

#######################Cropland5#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="5",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat4 <- dat2[dat2$biome=="5",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Croplandtop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland5.csv")



rm(list=ls())

#######################Cropland6#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat3 <- dat2[dat2$biome=="6",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Croplandtop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"





# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"



data_msdfinal1 <- rbind(data_msd7)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Cropland6.csv")



rm(list=ls())

#######################Natural1#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="1",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$biome=="1",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural1.csv")



rm(list=ls())

#######################Natural2#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="2",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$biome=="2",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural2.csv")



rm(list=ls())

#######################Natural3#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="3",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$biome=="3",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural3.csv")



rm(list=ls())

#######################Natural4#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="4",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"




# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


data_msdfinal1 <- rbind(data_msd7)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural4.csv")



rm(list=ls())

#######################Natural5#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="5",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"



#########################################################subsoil
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat4 <- dat2[dat2$biome=="5",] 
dat6 <- dat4[dat4$Soil.layer=="subsoil",] 
dat6 <- dat6[!is.na(dat6$Ref), ]

r5<-rma.mv(yi,vi,mods = ~Var-1,data=dat6, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r5)


df1=data.frame() 

df1[1,1]=r5$b[1]
df1[2,1]=r5$b[2]
df1[3,1]=r5$b[3]


df1[1,2]=r5$b[1]
df1[2,2]=r5$b[2]
df1[3,2]=r5$b[3]


df1[1,3]=r5$ci.lb[1]
df1[2,3]=r5$ci.lb[2]
df1[3,3]=r5$ci.lb[3]


df1[1,4]=r5$ci.ub[1]
df1[2,4]=r5$ci.ub[2]
df1[3,4]=r5$ci.ub[3]


df1[1,5]=r5$pval[1]
df1[2,5]=r5$pval[2]
df1[3,5]=r5$pval[3]


df1[1,6]=r5$zval[1]
df1[2,6]=r5$zval[2]
df1[3,6]=r5$zval[3]


df1[1,7]=r5$se[1]
df1[2,7]=r5$se[2]
df1[3,7]=r5$se[3]



df1[1,8]=r5$sigma2[1]
df1[1,9]=r5$sigma2[2]
df1[1,10]=r5$sigma2[3]
df1[1,11]=r5$QM 
df1[1,12]=r5$QMp 
df1[1,13]=r5$k


rownames(df1) <- c("Active","Passive","SOC")

names(df1)[1]="Naturaltop"
names(df1)[2]="estimate"
names(df1)[3]="ci.lb"
names(df1)[4]="ci.ub"
names(df1)[5]="pval"
names(df1)[6]="zval"
names(df1)[7]="se"
names(df1)[8]="sigma1"
names(df1)[9]="sigma2"
names(df1)[10]="sigma3"
names(df1)[11]="Qm"
names(df1)[12]="QmP"
names(df1)[13]="K"







# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


# Ensure the Var column is a factor with the correct order
dat6$Var <- factor(dat6$Var, levels = c("active", "passive", "SOC"))
dat10<-tabyl(dat6,Var)
data_msd8<- cbind(df1,dat10)
names(data_msd8)[15]="number"



data_msdfinal1 <- rbind(data_msd7,data_msd8)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural5.csv")



rm(list=ls())

#######################Natural6#################
dat1 <- read.csv("2025S0318KOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat3 <- dat2[dat2$biome=="6",] 
dat5 <- dat3[dat3$Soil.layer=="topsoil",] 
dat5 <- dat5[!is.na(dat5$Ref), ]
r4<-rma.mv(yi,vi,mods = ~Var-1,data=dat5, random=~1|Ref/studysite/Case, method="REML",sparse = T)
summary(r4)

df=data.frame() 



df[1,1]=r4$b[1]
df[2,1]=r4$b[2]
df[3,1]=r4$b[3]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]
df[3,2]=r4$b[3]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]
df[3,3]=r4$ci.lb[3]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]
df[3,4]=r4$ci.ub[3]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]
df[3,5]=r4$pval[3]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]
df[3,6]=r4$zval[3]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]
df[3,7]=r4$se[3]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k



rownames(df) <- c("Active","Passive","SOC")

names(df)[1]="Naturaltop"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"

# Ensure the Var column is a factor with the correct order
dat5$Var <- factor(dat5$Var, levels = c("active", "passive", "SOC"))
# Now create the tabyl (it will respect the factor order)
dat9 <- tabyl(dat5, Var)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"


data_msdfinal1 <- rbind(data_msd7)
write.csv(data_msdfinal1,file="/Users/min/Desktop/NEE/biome/Natural6.csv")







#Part two: compute Lability Index (LI) effect size#####
setwd("/Users/min/Desktop/NEE/SImeta")
###############total##############################################################
RRcal###########
# 
 dat1 <- read.csv("sensitivitySD.csv")
 dat3 <- escalc(data=dat1,measure="ROM", m1i=Xe, sd1i=Sde,
                n1i=Ne, m2i=Xc, sd2i=Sdc, n2i=Nc)
 dat3$se <- sqrt(dat3$vi)

#######################cropland#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="cropland"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Cropland.csv")

rm(list=ls())

#######################Natural#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Natural",] 
#dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Natural"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Natural.csv")

rm(list=ls())


#######################Grassland#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Grassland",] 
#dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Grassland"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Grassland.csv")

rm(list=ls())


#######################Sandy#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Sandy",] 
#dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Sandy"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Sandy.csv")

rm(list=ls())


#######################Bareland#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Bareland",] 
#dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Bareland"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Bareland.csv")

rm(list=ls())


#######################Abandoned#################
dat1 <- read.csv("20205SIKOoutli.csv")
dat5 <- dat1[dat1$Control=="Abandoned",] 
#dat5 <- dat2[dat2$Soil.layer=="topsoil",] 
r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"

write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/total/Abandoned.csv")

rm(list=ls())


#Biome group###################
####croplandbiome###################
#######################Cropland1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$biome=="1",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Cropland1.csv")

rm(list=ls())

#######################Cropland2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$biome=="2",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Cropland2.csv")

rm(list=ls())


#######################Cropland3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$biome=="3",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Cropland3.csv")

rm(list=ls())



#######################Cropland4#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$biome=="4",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Cropland4.csv")

rm(list=ls())


#######################Cropland5#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$biome=="5",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Cropland5.csv")

rm(list=ls())



####Naturalbiome###################
#######################Natural1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$biome=="1",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Natural1.csv")

rm(list=ls())

#######################Natural2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$biome=="2",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Natural2.csv")

rm(list=ls())


#######################Natural3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$biome=="3",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Natural3.csv")

rm(list=ls())


#######################Natural5#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$biome=="5",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Natural5.csv")

rm(list=ls())



####Grasslandbiome###################
#######################Grassland1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$biome=="1",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Grassland1.csv")

rm(list=ls())

#######################Grassland2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$biome=="2",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Grassland2.csv")

rm(list=ls())


#######################Grassland3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$biome=="3",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Grassland3.csv")

rm(list=ls())



#######################Grassland4#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$biome=="4",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Grassland4.csv")

rm(list=ls())


#######################Grassland5#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$biome=="5",] 


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"
dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/biome/Grassland5.csv")
rm(list=ls())


#Age group#############
#######################CroplandP1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat1 <- dat1[!is.na(dat1$Ref), ]
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$Age=="P1",] 
dat5 <- dat5[!is.na(dat5$Ref), ]


r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/croplandP1.csv")

rm(list=ls())

#######################CroplandP2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$Age=="P2",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/croplandP2.csv")

rm(list=ls())

#######################CroplandP3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Cropland",] 
dat5 <- dat2[dat2$Age=="P3",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/croplandP3.csv")

rm(list=ls())
#######################NaturalP1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$Age=="P1",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/NaturalP1.csv")

rm(list=ls())

#######################NaturalP2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$Age=="P2",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/NaturalP2.csv")

rm(list=ls())

#######################NaturalP3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Natural",] 
dat5 <- dat2[dat2$Age=="P3",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/NaturalP3.csv")

rm(list=ls())

#######################GrasslandP1#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$Age=="P1",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/GrasslandP1.csv")

rm(list=ls())

#######################GrasslandP2#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$Age=="P2",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/GrasslandP2.csv")

rm(list=ls())

#######################GrasslandP3#################

dat1 <- read.csv("20205SIKOoutli.csv")
dat2 <- dat1[dat1$Control=="Grassland",] 
dat5 <- dat2[dat2$Age=="P3",] 
dat5 <- dat5[!is.na(dat5$Ref), ]

r4<-rma.mv(yi,vi,mods = ~Soil.layer-1,data=dat5, random=~1|Ref, method="REML",sparse = T)
summary(r4)

df=data.frame() 

df[1,1]=r4$b[1]
df[2,1]=r4$b[2]


df[1,2]=r4$b[1]
df[2,2]=r4$b[2]


df[1,3]=r4$ci.lb[1]
df[2,3]=r4$ci.lb[2]


df[1,4]=r4$ci.ub[1]
df[2,4]=r4$ci.ub[2]


df[1,5]=r4$pval[1]
df[2,5]=r4$pval[2]


df[1,6]=r4$zval[1]
df[2,6]=r4$zval[2]


df[1,7]=r4$se[1]
df[2,7]=r4$se[2]


df[1,8]=r4$sigma2[1]
df[1,9]=r4$sigma2[2]
df[1,10]=r4$sigma2[3]
df[1,11]=r4$QM 
df[1,12]=r4$QMp 
df[1,13]=r4$k

rownames(df) <- c("Soil.layersubsoil","Soil.layertopsoil")

names(df)[1]="Abandoned"
names(df)[2]="estimate"
names(df)[3]="ci.lb"
names(df)[4]="ci.ub"
names(df)[5]="pval"
names(df)[6]="zval"
names(df)[7]="se"
names(df)[8]="sigma1"
names(df)[9]="sigma2"
names(df)[10]="sigma3"
names(df)[11]="Qm"
names(df)[12]="QmP"
names(df)[13]="K"


dat9<-tabyl(dat5,Soil.layer)
data_msd7<- cbind(df,dat9)
names(data_msd7)[15]="number"
write.csv(data_msd7,file="/Users/min/Desktop/NEE/SImeta/age/GrasslandP3.csv")

rm(list=ls())

#Part three: ploting#####

setwd("/Users/min/Desktop/NEE/total")

#Load data
dat <- read.csv("filled_total_combined_data.csv")
create_plot <- function(landuse) {
  dat1 <- dat %>%
    filter(Landuse == landuse) %>%
    mutate(
      group = dplyr::recode(group, "Sensitivity" = "LI")
    )
  dat1$group <- factor(dat1$group, levels = c("SOC","LI","Active","Passive"))
  dat1$facet <- factor(dat1$facet,
                       levels = c("0-30cm", "30+cm"),
                       labels = c("Topsoil", "Subsoil"))
  if (landuse == "Sandy") {
    y_limits <- c(-50, 600)
    y_breaks <- seq(-50, 600, by = 100)
  } else if (landuse == "Bareland") {
    y_limits <- c(-50, 250)
    y_breaks <- seq(-50, 250, by = 50)
  } else {
    y_limits <- c(-50, 100)
    y_breaks <- seq(-50, 100, by = 50)
  }
  
  y_labels <- as.character(y_breaks)
  soil_cols <- c("Topsoil" = "#008fd5", "Subsoil" = "#FFC125")
  dat1$N_label <- paste0(
    "(", 
    format(dat1$N, big.mark = ",", scientific = FALSE, trim = TRUE), 
    ")"
  )
  
  pos_dodge <- position_dodge(width = 0.8)
  x_label_expressions <- c(
    "SOC",
    expression(italic("LI")), 
    "Active",
    "Passive"
  )
  
  p <- ggplot(dat1, aes(x = group, y = OR, color = facet, fill = facet)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.5, linewidth = 1.5, position = pos_dodge) +
    geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
    geom_point(size = 8, shape = 21, colour = "white", stroke = 1, position = pos_dodge) +
    geom_text(
      aes(
        y = ifelse(landuse == "Sandy", 580, ifelse(landuse == "Bareland", 230, 90)),
        label = N_label
      ),
      position = pos_dodge,
      size = 10,
      vjust = 0.9,
      angle = 90,
      fontface = "plain",
      color = "black",  # Solid black for N labels
      family = "arial"
    ) +
    scale_fill_manual(values = soil_cols) +
    scale_color_manual(values = soil_cols) +
    scale_x_discrete(name = "", labels = x_label_expressions) +
    scale_y_continuous(
      name = "",
      limits = y_limits,
      breaks = y_breaks,
      labels = y_labels,
      position = "right"
    ) +
    theme_minimal(base_family = "arial", base_size = 30) +
    theme(
      plot.background  = element_blank(),
      panel.background = element_blank(),
      panel.grid       = element_blank(),
      # --- Force X-axis text to black ---
      axis.text.x      = element_text(color = "black", face = "plain", size = 30, angle = 90, vjust = 0.5, hjust = 1),
      # --- Force Y-axis text to black ---
      axis.text.y      = element_text(color = "black", face = "plain", size = 30, angle = 90, hjust = 0, vjust = 0),
      axis.line        = element_line(color = "black", linewidth = 1),
      axis.ticks.y     = element_line(color = "black", linewidth = 1),
      axis.ticks.length.y = unit(0.3, "cm"),
      legend.position  = "none"
    )
  
  return(p)
}

landuse_types <- c("Natural", "cropland", "Grassland", "Abandoned", "Bareland", "Sandy")
for (landuse in landuse_types) {
  p <- create_plot(landuse)
  filename <- paste0(tolower(landuse), "_topsoil_subsoil_same_metric_withN.tiff")
  ggsave(filename, p, width = 18, height = 22, units = "cm", dpi = 500, device = "tiff")
  cat("已保存 (全黑字体):", filename, "\n")
}



#######total#########
library(metafor)
library(dplyr)
library(writexl)
library(Rmisc)
library(readxl)
library(ggplot2)
library(dplyr)
library(metafor)


#Cropland#################
dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="cropland",] 

#dotCOLS = c("#a6d8f0","#f9b282","#AB82FF","#FFA500","#96CDCD","#FFD700")#bar
#barCOLS = c("#008fd5","#de6b35","#8073ac","#CD8500","#668B8B","#FFC125")#dot

dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p



ggsave("cropland.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())


#################Abandoned#################

dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="Abandoned",] 

dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("Abandoned.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())

############### ##Grassland#################

dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="Grassland",] 
#dotCOLS = c("#a6d8f0","#f9b282","#AB82FF","#FFA500","#96CDCD","#FFD700")#bar
#barCOLS = c("#008fd5","#de6b35","#8073ac","#CD8500","#668B8B","#FFC125")#dot
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p



ggsave("Grassland.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())



#Natural forest#################

dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="Natural",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("Natural.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())


#Bare land#################

dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="Bareland",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-200,200, by=50), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-200, 200),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("barren.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())


#Desert & semi desert#################

dat <- read.csv("filled_total_combined_data.csv")
dat1 <- dat[dat$Landuse=="Sandy",] #Sandy refers to Desert & semi desert
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,600, by=100), 7)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 600),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("Sandy.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#Age group##############
#croplandP1##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="CroplandP1",] 


dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p

ggsave("croplandP1.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#






#########croplandP2##############

dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="CroplandP2",] 

dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p



ggsave("croplandP2.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#


#########croplandP3##############

dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="CroplandP3",] 

dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,150, by=50), 5)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 150),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p




ggsave("croplandP3.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())





#########grasslandP1##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="GrasslandP1",] 

dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("grasslandP1.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#########grasslandP2##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="GrasslandP2",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("grasslandP2.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#




#########grasslandP3##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="GrasslandP3",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,150, by=50), 5)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 150),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("grasslandP3.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#




#########naturalP1##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="NaturalP1",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p



ggsave("naturalP1.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#########naturalP2##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="NaturalP2",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("naturalP2.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#




#########naturalP3##############
setwd("/Users/min/Desktop/NEE/total")
dat <- read.csv("filled_age_combined_data.csv")
dat1 <- dat[dat$Landuse=="NaturalP3",] 
dotCOLS = c("#FFA500","#96CDCD","#FFD700","#a6d8f0")#bar
barCOLS = c("#CD8500","#668B8B","#FFC125","#008fd5")#dot
dat1$group <- factor(dat1$group,levels = c("SOC","Sensitivity","Active","Passive")) 
breaks = c(seq(-100,100, by=25), 4)
# and labels
labels = as.character(breaks)

p <- dat1 %>%
  mutate(facet = factor(facet, levels = c("0-30cm", "30+cm"))) %>%
  ggplot(aes(x = group,
             y = OR, ymin = Lower, ymax = Upper, col = group, fill = group)) + 
  facet_grid(~ facet) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = group), width = 0.5, linewidth = 1) +
  geom_hline(yintercept = 0, lty = 2, linewidth = 1) +
  geom_point(size = 5, shape = 21, colour = "white", stroke = 1,
             position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS) +
  scale_color_manual(values = dotCOLS) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", limits = c(-100, 100),
                     breaks = breaks, labels = labels, position = "right") +
  theme_minimal(base_family = "arial", base_size = 20) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 25),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.title.y = element_text(face = "bold"),
    axis.title.y.right = element_text(angle = 90, margin = unit(c(2, 2, 2, 0.5), "cm")),
    axis.line = element_line(color = "black", size = 1),
    strip.text.x = element_blank(),
    legend.position = "none"
  )
p


ggsave("naturalP3.tiff", 
       p, 
       width = 19, height = 16, units = "cm", dpi = 500, device = "tiff")

rm(list=ls())
#




