###Title: Analysis of LGBTQIA+ data
###Author: Abbey Feuka (she/her)
###Date: 30032023
###Notes: fitting beta and logistic regressions to lgbtqia+ safety, belonging, and outness survey results
# Setup -----------------------------------------------------------------
library(tidyverse)
library(betareg)
library(grid)
library(pROC)
library(boot)
library(rstatix)

load("./Data/lgbtqia_survresults_20032023_cleanheadings_bin.Rdata") #cleaned
total_respond <- nrow(dat)
text.size=15

#Inclusion -----------------------------
dat$Conservation_inclusive <- factor(dat$Conservation_inclusive,
                                     levels=c("Strongly disagree",
                                              "Somewhat disagree",
                                              "Neither agree nor disagree",
                                              "Somewhat agree",
                                              "Strongly agree",
                                              ""))
levels(dat$Conservation_inclusive)[levels(dat$Conservation_inclusive)==""] <- "No Response"

dat$Conservation_inclusive_no <- as.numeric(dat$Conservation_inclusive)

dat$lgbtqia <- ifelse(dat$cis==0 | dat$hetero==0,1,0)

dat %>% identify_outliers(Conservation_inclusive_no)
dat %>% shapiro_test(Conservation_inclusive_no)

wilcox.test(Conservation_inclusive_no ~ lgbtqia, data=dat) 
wilcox.test(Conservation_inclusive_no ~ cis, data=dat) 
wilcox.test(Conservation_inclusive_no ~ hetero, data=dat) 

# Outness -----------------------------------------------------------------
## sexual orientation -----------------------------------------------------------------
out.who.names <- c("Colleagues at work/school",
                   "Colleagues outside of work/school",
                   "Members of the public at work/school",
                   "Mentor at work/school",
                   "Office or campus as a whole",
                   "Work/school friends",
                   "Friends outside of work/school",
                   "Chosen family","Biological family",
                   "Prefer not to respond",
                   "Other",
                   "None of these")

outlocs <- c("Colleagues at work/school",
             "Colleagues outside work/school",
             "Public outside of work",
             "Mentor",
             "Office/campus as whole",
             "Work/school friends",
             "Non-work/school friends",
             "Chosen family",
             "Biological family")
dat$Age.sc <- scale(dat$Age)
out.so <- dat %>% select(SexOrientation_Out_who,queer_so,queer_gi,
                         Age.sc,poc,republican,academ.bin,vote.dem.cont) %>%
  filter(queer_so==1)
out.so$colleaguesInside <- as.numeric(grepl("Certain colleagues at work or school",
                                 out.so$SexOrientation_Out_who))
out.so$colleaugesOutside <- as.numeric(grepl("Colleagues outside",out.so$SexOrientation_Out_who))
out.so$public <- as.numeric(grepl("public",out.so$SexOrientation_Out_who))
out.so$mentor <- as.numeric(grepl("Mentor at work or school",out.so$SexOrientation_Out_who))
out.so$officeCampus <- as.numeric(grepl("The office or campus as a whole",out.so$SexOrientation_Out_who))
out.so$workfriends <- as.numeric(grepl("Work or school friends",out.so$SexOrientation_Out_who))
out.so$nonworkfriends <- as.numeric(grepl("Friends outside of work or school",out.so$SexOrientation_Out_who))
out.so$chosenFamily <- as.numeric(grepl("Chosen family",out.so$SexOrientation_Out_who))
out.so$bioFamily <- as.numeric(grepl("Biological family",out.so$SexOrientation_Out_who))

so.out.glm <- so.out.modsum <-list()
start<- which(colnames(out.so)=="colleaguesInside")
for(i in start:ncol(out.so)){
  so.out.glm[[i-(start-1)]] <- glm(out.so[,i] ~ 
                             out.so$queer_gi + 
                             out.so$Age.sc + 
                             out.so$poc + 
                             out.so$republican +
                             out.so$academ.bin ,family="binomial")
  so.out.modsum[[i-(start-1)]] <- cbind.data.frame(so.out.glm[[i-(start-1)]]$coefficients,
                                confint(so.out.glm[[i-(start-1)]] ))
  colnames(so.out.modsum[[i-(start-1)]]) <- c("estimate","lci","uci")
  so.out.modsum[[i-(start-1)]]$out_who <- outlocs[i-(start-1)]
  so.out.modsum[[i-(start-1)]]$coeff<- c("Intercept",#
                                 "Gender Expansive",
                                 "Age",
                                 "POC",
                                 "Republican State",
                                 "Academia")
  rownames(so.out.modsum[[i-(start-1)]]) <- NULL
}

so.out <- do.call("rbind.data.frame",so.out.modsum)
so.out$signif <- (so.out$lci>0 & so.out$uci>0) | (so.out$lci<0 & so.out$uci<0) 

so.out <- so.out %>% select(out_who,coeff,estimate,lci,uci,signif)
# write.csv(so.out,"./Plots/Out/so_out_tab.csv")

## gender identity -----------------------------------------------------------------
out.gi <- dat %>% select(GenderID_Outwho,queer_so,queer_gi,
                         Age.sc,poc,republican,academ.bin,vote.dem.cont) %>% 
  filter(queer_gi==1)
out.gi$colleaguesInside <- as.numeric(grepl("Certain colleagues at work or school",
                                            out.gi$GenderID_Outwho))
out.gi$colleaugesOutside <- as.numeric(grepl("Colleagues outside",out.gi$GenderID_Outwho))
out.gi$public <- as.numeric(grepl("public",out.gi$GenderID_Outwho))
out.gi$mentor <- as.numeric(grepl("Mentor at work or school",out.gi$GenderID_Outwho))
out.gi$officeCampus <- as.numeric(grepl("The office or campus as a whole",out.gi$GenderID_Outwho))
out.gi$workfriends <- as.numeric(grepl("Work or school friends",out.gi$GenderID_Outwho))
out.gi$nonworkfriends <- as.numeric(grepl("Friends outside of work or school",out.gi$GenderID_Outwho))
out.gi$chosenFamily <- as.numeric(grepl("Chosen family",out.gi$GenderID_Outwho))
out.gi$bioFamily <- as.numeric(grepl("Biological family",out.gi$GenderID_Outwho))

# plot(out.gi[,8],out.gi$queer_gi)
gi.out.glm <- gi.out.modsum <-list()
start <- which(colnames(out.gi)=="colleaguesInside")
for(i in start:ncol(out.gi)){
  gi.out.glm[[i-(start-1)]] <- glm(out.gi[,i] ~ out.gi$queer_so + 
                             out.gi$Age.sc + 
                             out.gi$poc +
                             out.gi$republican +
                             out.gi$academ.bin ,family="binomial")
  gi.out.modsum[[i-(start-1)]] <- cbind.data.frame(gi.out.glm[[i-(start-1)]]$coefficients,
                                           confint(gi.out.glm[[i-(start-1)]]))
  colnames(gi.out.modsum[[i-(start-1)]]) <- c("estimate","lci","uci")
  gi.out.modsum[[i-(start-1)]]$out_who <- outlocs[i-(start-1)]
  gi.out.modsum[[i-(start-1)]]$coeff<- c("Intercept",
                                 "Queer+",
                                 "Age",
                                 "POC",
                                 "Republican State",
                                 "Academia")
  rownames(gi.out.modsum[[i-(start-1)]]) <- NULL
}
gi.out <- do.call("rbind.data.frame",gi.out.modsum)
gi.out$signif <- (gi.out$lci>0 & gi.out$uci>0) | (gi.out$lci<0 & gi.out$uci<0) 

gi.out <- gi.out %>% select(out_who,coeff,estimate,lci,uci,signif)
# write.csv(gi.out,"./Plots/Out/gi_out_tab.csv")

# Safety -----------------------------------------------------------------
## sexual orientation -----------------------------------------------------------------
locs <- c("Personal life","Conferences","Public",
          "Outside","In person","Remote")

### safety ~ queer+ + GE -----------------------------------------------------------------
safe.so.het <- dat %>% select(SexOrientation_safe_personal,
                          SexOrientation_safe_conferences,
                          SexOrientation_safe_Publicnonwork,
                          SexOrientation_safe_Fieldwork,
                          SexOrientation_safe_Workplaceinperson,
                          SexOrientation_safe_Workplaceremote,
                          queer_so,queer_gi,woman_cisonly)

safe.so.het[,1:6] <- safe.so.het[,1:6]/100

safe.so.het.glm <- safe.so.het.modsum <-list()
for(i in 1:6){
  samp <- safe.so.het[!is.na(safe.so.het[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  safe.so.het.glm[[i]] <- betareg(samp[,i] ~ queer_so + queer_gi, data=samp)
  safe.so.het.modsum[[i]] <- cbind.data.frame(c(safe.so.het.glm[[i]]$coefficients$mean,
                                            safe.so.het.glm[[i]]$coefficients$precision),
                                          confint(safe.so.het.glm[[i]]))
  colnames(safe.so.het.modsum[[i]]) <- c("estimate","lci","uci")
  safe.so.het.modsum[[i]]$safe_where <- locs[i]
  safe.so.het.modsum[[i]]$coeff<- c("Intercept","Queer+","Gender Expansive","Phi")
  rownames(safe.so.het.modsum[[i]]) <- NULL
}
so.het.safe <- do.call("rbind.data.frame",safe.so.het.modsum)
so.het.safe$signif <- (so.het.safe$lci>0 & so.het.safe$uci>0) | (so.het.safe$lci<0 & so.het.safe$uci<0) 

so.het.safe <- so.het.safe %>% select(safe_where,coeff,estimate,lci,uci,signif)
# write.csv(so.het.safe,"./Plots/Safety/so_het_safety_tab.csv")

### queer only -----------------------------------------------------------------
safe.so <- dat %>% select(SexOrientation_safe_personal,
                          SexOrientation_safe_conferences,
                          SexOrientation_safe_Publicnonwork,
                          SexOrientation_safe_Fieldwork,
                          SexOrientation_safe_Workplaceinperson,
                          SexOrientation_safe_Workplaceremote,
                          queer_so,queer_gi,Age.sc,poc,republican,academ.bin,
                          woman_cistrans,vote.dem.cont) %>% 
  filter(queer_so==1)
safe.so[,1:6] <- safe.so[,1:6]/100

safe.so.glm <- safe.so.modsum <-list()
for(i in 1:6){
  samp <- safe.so[!is.na(safe.so[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  safe.so.glm[[i]] <- betareg(samp[,i] ~ 
                                queer_gi+ 
                                Age.sc + 
                                poc +
                                republican +
                                academ.bin +
                                woman_cistrans, data=samp)
  safe.so.modsum[[i]] <- cbind.data.frame(c(safe.so.glm[[i]]$coefficients$mean,
                                            safe.so.glm[[i]]$coefficients$precision),
                                           confint(safe.so.glm[[i]]))
  colnames(safe.so.modsum[[i]]) <- c("estimate","lci","uci")
  safe.so.modsum[[i]]$safe_where <- locs[i]
  safe.so.modsum[[i]]$coeff<- c("Intercept",
                                "Gender Expansive",
                                "Age",
                                "POC",
                                "Republican State",
                                "Academia", "Woman",
                                "Phi")
  rownames(safe.so.modsum[[i]]) <- NULL
}
so.safe <- do.call("rbind.data.frame",safe.so.modsum)
so.safe$signif <- (so.safe$lci>0 & so.safe$uci>0) | (so.safe$lci<0 & so.safe$uci<0) 

so.safe <- so.safe %>% select(safe_where,coeff,estimate,lci,uci,signif)
# write.csv(so.safe,"./Plots/Safety/so_queer+_safety_tab.csv")

## gender identity -----------------------------------------------------------------
safe.gi.cis <- dat %>% select(GenderID_safe_personal,
                              GenderID_safe_conferences,
                              GenderID_safe_Publicnonwork,
                              GenderID_safe_Fieldwork,
                              GenderID_safe_Workplaceinperson,
                              GenderID_safe_Workplaceremote,
                              queer_so,queer_gi,woman_cisonly)
safe.gi.cis[,1:6] <- safe.gi.cis[,1:6]/100

### safety ~ queer+ + GE  -----------------------------------------------------------------
safe.gi.cis.glm <- safe.gi.cis.modsum <-list()
for(i in 1:6){
  samp <- safe.gi.cis[!is.na(safe.gi.cis[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  safe.gi.cis.glm[[i]] <- betareg(samp[,i] ~ queer_so + queer_gi, data=samp)
  safe.gi.cis.modsum[[i]] <- cbind.data.frame(c(safe.gi.cis.glm[[i]]$coefficients$mean,
                                                safe.gi.cis.glm[[i]]$coefficients$precision),
                                              confint(safe.gi.cis.glm[[i]]))
  colnames(safe.gi.cis.modsum[[i]]) <- c("estimate","lci","uci")
  safe.gi.cis.modsum[[i]]$safe_where <- locs[i]
  safe.gi.cis.modsum[[i]]$coeff<- c("Intercept","Queer+","Gender Expansive","Phi")
  rownames(safe.gi.cis.modsum[[i]]) <- NULL
}
gi.cis.safe <- do.call("rbind.data.frame",safe.gi.cis.modsum)
gi.cis.safe$signif <- (gi.cis.safe$lci>0 & gi.cis.safe$uci>0) | (gi.cis.safe$lci<0 & gi.cis.safe$uci<0) 

gi.cis.safe <- gi.cis.safe %>% select(safe_where,coeff,estimate,lci,uci,signif)
# write.csv(gi.cis.safe,"./Plots/Safety/gi_cis_safety_tab.csv")

### gender expansive only -----------------------------------------------------------------
safe.gi <- dat %>% select(GenderID_safe_personal,
                          GenderID_safe_conferences,
                          GenderID_safe_Publicnonwork,
                          GenderID_safe_Fieldwork,
                          GenderID_safe_Workplaceinperson,
                          GenderID_safe_Workplaceremote,
                          queer_so,queer_gi,Age.sc,poc,republican,academ.bin,
                          woman_cistrans,vote.dem.cont) %>% 
  filter(queer_gi==1)
safe.gi[,1:6] <- safe.gi[,1:6]/100
safe.gi.glm <- safe.gi.modsum <-list()
for(i in 1:6){
  samp <- safe.gi[!is.na(safe.gi[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  safe.gi.glm[[i]] <- betareg(samp[,i] ~ queer_so +
                                Age.sc + 
                                poc +
                                republican +
                                academ.bin +
                                woman_cistrans, data=samp)
  safe.gi.modsum[[i]] <- cbind.data.frame(c(safe.gi.glm[[i]]$coefficients$mean,
                                            safe.gi.glm[[i]]$coefficients$precision),
                                          confint(safe.gi.glm[[i]]))
  colnames(safe.gi.modsum[[i]]) <- c("estimate","lci","uci")
  safe.gi.modsum[[i]]$safe_where <- locs[i]
  safe.gi.modsum[[i]]$coeff<-  c("Intercept",
                                 "Queer+",
                                 "Age",
                                 "POC",
                                 "Republican State",
                                 "Academia", "Woman",
                                 "Phi")
  rownames(safe.gi.modsum[[i]]) <- NULL
}
gi.safe <- do.call("rbind.data.frame",safe.gi.modsum)
gi.safe$signif <- (gi.safe$lci>0 & gi.safe$uci>0) | (gi.safe$lci<0 & gi.safe$uci<0) 

gi.safe <- gi.safe %>% select(safe_where,coeff,estimate,lci,uci,signif)
# write.csv(gi.safe,"./Plots/Safety/gi_ge_safety_tab.csv")

# Belonging -----------------------------------------------------------------
## sexual orientation -----------------------------------------------------------------
### belong ~ queer+ + GE -----------------------------------------------------------------
belong.so.het <- dat %>% select(SexOrientation_belong_personal,
                        SexOrientation_belong_conferences,
                        SexOrientation_belong_Publicnonwork,
                        SexOrientation_belong_fieldwork,
                        SexOrientation_belong_Workinperson,
                        SexOrientation_belong_Workremote,
                        queer_so,queer_gi,woman_cisonly)
belong.so.het[,1:6] <- belong.so.het[,1:6]/100

belong.so.het.glm <- belong.so.het.modsum <-list()
for(i in 1:6){
  samp <- belong.so.het[!is.na(belong.so.het[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  belong.so.het.glm[[i]] <- betareg(samp[,i] ~ queer_so + queer_gi, data=samp)
  belong.so.het.modsum[[i]] <- cbind.data.frame(c(belong.so.het.glm[[i]]$coefficients$mean,
                                             belong.so.het.glm[[i]]$coefficients$precision),
                                           confint(belong.so.het.glm[[i]]))
  colnames(belong.so.het.modsum[[i]]) <- c("estimate","lci","uci")
  belong.so.het.modsum[[i]]$belong_where <- locs[i]
  belong.so.het.modsum[[i]]$coeff<- c("Intercept","Queer+","Gender Expansive","Phi")
  rownames(belong.so.het.modsum[[i]]) <- NULL
}
so.het.belong <- do.call("rbind.data.frame",belong.so.het.modsum)
so.het.belong$signif <- (so.het.belong$lci>0 & so.het.belong$uci>0) | (so.het.belong$lci<0 & so.het.belong$uci<0) 

so.het.belong <- so.het.belong %>% select(belong_where,coeff,estimate,lci,uci,signif)
# write.csv(so.het.belong,"./Plots/Belonging/so_het_belonging_tab.csv")

### queer only -----------------------------------------------------------------
belong.so <- dat %>% select(SexOrientation_belong_personal,
                          SexOrientation_belong_conferences,
                          SexOrientation_belong_Publicnonwork,
                          SexOrientation_belong_fieldwork,
                          SexOrientation_belong_Workinperson,
                          SexOrientation_belong_Workremote,
                          queer_so,queer_gi,Age.sc,poc,republican,academ.bin,
                          woman_cistrans) %>% 
filter(queer_so==1)
belong.so[,1:6] <- belong.so[,1:6]/100
belong.so.glm <- belong.so.modsum <-list()

for(i in 1:6){
  samp <- belong.so[!is.na(belong.so[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  belong.so.glm[[i]] <- betareg(samp[,i] ~  
                                  queer_gi  + 
                                  Age.sc + 
                                  poc +
                                  republican + 
                                  academ.bin +
                                  woman_cistrans, data=samp)
  belong.so.modsum[[i]] <- cbind.data.frame(c(belong.so.glm[[i]]$coefficients$mean,
                                              belong.so.glm[[i]]$coefficients$precision),
                                            confint(belong.so.glm[[i]]))
  colnames(belong.so.modsum[[i]]) <- c("estimate","lci","uci")
  belong.so.modsum[[i]]$belong_where <- locs[i]
  belong.so.modsum[[i]]$coeff<- c("Intercept",
                                  "Gender Expansive",
                                  "Age",
                                  "POC",
                                  "Republican State",
                                  "Academia", "Woman",
                                  "Phi")
  rownames(belong.so.modsum[[i]]) <- NULL
}
so.belong <- do.call("rbind.data.frame",belong.so.modsum)
so.belong$signif <- (so.belong$lci>0 & so.belong$uci>0) | (so.belong$lci<0 & so.belong$uci<0) 

so.belong <- so.belong %>% select(belong_where,coeff,estimate,lci,uci,signif)
# write.csv(so.belong,"./Plots/Belonging/so_queer+_belong_tab.csv")

## gender identity -----------------------------------------------------------------
### belong ~ queer+ + GE -----------------------------------------------------------------
belong.gi.cis <- dat %>% select(GenderID_belong_personal,
                                GenderID_belong_conferences,
                                GenderID_belong_Publicnonwork,
                                GenderID_belong_fieldwork,
                                GenderID_belong_Workinperson,
                                GenderID_belong_Workremote,
                                queer_so,queer_gi,woman_cisonly)
belong.gi.cis[,1:6] <- belong.gi.cis[,1:6]/100

belong.gi.cis.glm <- belong.gi.cis.modsum <-list()
for(i in 1:6){
  samp <- belong.gi.cis[!is.na(belong.gi.cis[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  belong.gi.cis.glm[[i]] <- betareg(samp[,i] ~ queer_so + queer_gi, data=samp)
  belong.gi.cis.modsum[[i]] <- cbind.data.frame(c(belong.gi.cis.glm[[i]]$coefficients$mean,
                                                  belong.gi.cis.glm[[i]]$coefficients$precision),
                                                confint(belong.gi.cis.glm[[i]]))
  colnames(belong.gi.cis.modsum[[i]]) <- c("estimate","lci","uci")
  belong.gi.cis.modsum[[i]]$belong_where <- locs[i]
  belong.gi.cis.modsum[[i]]$coeff<- c("Intercept","Queer+","Gender Expansive","Phi")
  rownames(belong.gi.cis.modsum[[i]]) <- NULL
}
gi.cis.belong <- do.call("rbind.data.frame",belong.gi.cis.modsum)
gi.cis.belong$signif <- (gi.cis.belong$lci>0 & gi.cis.belong$uci>0) | (gi.cis.belong$lci<0 & gi.cis.belong$uci<0) 

gi.cis.belong <- gi.cis.belong %>% select(belong_where,coeff,estimate,lci,uci,signif)
# write.csv(gi.cis.belong,"./Plots/Belonging/gi_cis_belong_tab.csv")

### gender expansive only -----------------------------------------------------------------
belong.gi <- dat %>% select(GenderID_belong_personal,
                          GenderID_belong_conferences,
                          GenderID_belong_Publicnonwork,
                          GenderID_belong_fieldwork,
                          GenderID_belong_Workinperson,
                          GenderID_belong_Workremote,
                          queer_so,queer_gi,Age.sc,poc,republican,academ.bin,
                          woman_cistrans) %>% 
  filter(queer_gi==1)
belong.gi[,1:6] <- belong.gi[,1:6]/100
belong.gi.glm <- belong.gi.modsum <-list()
for(i in 1:6){
  samp <- belong.gi[!is.na(belong.gi[,i]),]
  samp[samp[,i]==1,i] <- 0.99999
  samp[samp[,i]==0,i] <- 0.00001
  belong.gi.glm[[i]] <- betareg(samp[,i] ~ queer_so +
                                  Age.sc + 
                                  poc +
                                  republican + 
                                  academ.bin +
                                  woman_cistrans, data=samp)
  belong.gi.modsum[[i]] <- cbind.data.frame(c(belong.gi.glm[[i]]$coefficients$mean,
                                              belong.gi.glm[[i]]$coefficients$precision),
                                            confint(belong.gi.glm[[i]]))
  colnames(belong.gi.modsum[[i]]) <- c("estimate","lci","uci")
  belong.gi.modsum[[i]]$belong_where <- locs[i]
  belong.gi.modsum[[i]]$coeff<- c("Intercept",
                                  "Queer+",
                                  "Age",
                                  "POC",
                                  "Republican State",
                                  "Academia", "Woman",
                                  "Phi")
  rownames(belong.gi.modsum[[i]]) <- NULL
}
gi.belong <- do.call("rbind.data.frame",belong.gi.modsum)
gi.belong$signif <- (gi.belong$lci>0 & gi.belong$uci>0) | (gi.belong$lci<0 & gi.belong$uci<0) 

gi.belong <- gi.belong %>% select(belong_where,coeff,estimate,lci,uci,signif)
# write.csv(gi.belong,"./Plots/Belonging/gi_ge_belong_tab.csv")

# Goodness of fit -----------------------------------------------------------------
## beta regression  -----------------------------------------------------------------
###lgbtqia+ ------------------------------------------
beta.reg.r2 <- list()
beta.reg.r2[[1]] <- data.frame(location = locs,
                               pseudoR2 = rep(NA,length(locs)))
beta.reg.r2[[1]]$model <- "Belonging"
beta.reg.r2[[1]]$type <- "gi"

for(i in 1:6){
  beta.reg.r2[[1]]$pseudoR2[i] <- summary(belong.gi.glm[[i]])$pseudo.r.squared
}

beta.reg.r2[[2]] <- data.frame(location = locs,
                               pseudoR2 = rep(NA,length(locs)))
beta.reg.r2[[2]]$model <- "Safety"
beta.reg.r2[[2]]$type <- "gi"
for(i in 1:6){
  beta.reg.r2[[2]]$pseudoR2[i] <- summary(safe.gi.glm[[i]])$pseudo.r.squared
}
beta.reg.r2[[3]] <- data.frame(location = locs,
                               pseudoR2 = rep(NA,length(locs)))
beta.reg.r2[[3]]$model <- "Safety"
beta.reg.r2[[3]]$type <- "so"
for(i in 1:6){
  beta.reg.r2[[3]]$pseudoR2[i] <- summary(safe.so.glm[[i]])$pseudo.r.squared
}

beta.reg.r2[[4]] <- data.frame(location = locs,
                               pseudoR2 = rep(NA,length(locs)))
beta.reg.r2[[4]]$model <- "Belonging"
beta.reg.r2[[4]]$type <- "so"
for(i in 1:6){
  beta.reg.r2[[4]]$pseudoR2[i] <- summary(belong.so.glm[[i]])$pseudo.r.squared
}
beta.reg.r2 <- do.call("rbind.data.frame",beta.reg.r2)
beta.reg.r2$type[beta.reg.r2$type=="so"] <- "Sexual Orientation"
beta.reg.r2$type[beta.reg.r2$type=="gi"] <- "Gender Identity"

ggplot()+geom_bar(data=beta.reg.r2,aes(x=location,y=pseudoR2,fill= type),
                  stat="identity",position="dodge")+
  facet_wrap(.~model)+
  scale_fill_viridis_d(name="Identity Focus",
                       begin=0,end=0.5)+
  ylab("Pseudo R2 value")+
  xlab("Location")+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1),
        text=element_text(size=20),plot.margin = margin(0,0,0,2,"cm"))
# write.csv(beta.reg.r2,"./PLots/Final/beta_reg_pseudo_r2_lgbt_tab.csv")

###cis and het ------------------------------
beta.reg.r2.cishet <- list()
beta.reg.r2.cishet[[1]] <- data.frame(location = locs,
                                      pseudoR2 = rep(NA,length(locs)))
beta.reg.r2.cishet[[1]]$model <- "Belonging"
beta.reg.r2.cishet[[1]]$type <- "gi"

for(i in 1:6){
  beta.reg.r2.cishet[[1]]$pseudoR2[i] <- summary(belong.gi.cis.glm[[i]])$pseudo.r.squared
}

beta.reg.r2.cishet[[2]] <- data.frame(location = locs,
                                      pseudoR2 = rep(NA,length(locs)))
beta.reg.r2.cishet[[2]]$model <- "Safety"
beta.reg.r2.cishet[[2]]$type <- "gi"
for(i in 1:6){
  beta.reg.r2.cishet[[2]]$pseudoR2[i] <- summary(safe.gi.cis.glm[[i]])$pseudo.r.squared
}
beta.reg.r2.cishet[[3]] <- data.frame(location = locs,
                                      pseudoR2 = rep(NA,length(locs)))
beta.reg.r2.cishet[[3]]$model <- "Safety"
beta.reg.r2.cishet[[3]]$type <- "so"
for(i in 1:6){
  beta.reg.r2.cishet[[3]]$pseudoR2[i] <- summary(safe.so.het.glm[[i]])$pseudo.r.squared
}

beta.reg.r2.cishet[[4]] <- data.frame(location = locs,
                                      pseudoR2 = rep(NA,length(locs)))
beta.reg.r2.cishet[[4]]$model <- "Belonging"
beta.reg.r2.cishet[[4]]$type <- "so"
for(i in 1:6){
  beta.reg.r2.cishet[[4]]$pseudoR2[i] <- summary(belong.so.het.glm[[i]])$pseudo.r.squared
}
beta.reg.r2.cishet <- do.call("rbind.data.frame",beta.reg.r2.cishet)
beta.reg.r2.cishet$type[beta.reg.r2.cishet$type=="so"] <- "Sexual Orientation"
beta.reg.r2.cishet$type[beta.reg.r2.cishet$type=="gi"] <- "Gender Identity"
# write.csv(beta.reg.r2.cishet,"./PLots/Final/beta_reg_pseudo_r2_cishet_tab.csv")

## logistic regression -----------------------------------------------------------------
samp.so <- out.so %>% drop_na()
roc.out.so <- numeric(9)
for(i in 8:16){
  roc.out.so[i-7] <- as.numeric(roc(response = samp.so[,i+1],
                                    predictor = inv.logit(predict(so.out.glm[[i-7]])))$auc)
}
roc.so <- cbind.data.frame(out.who.names[1:9],roc.out.so)
colnames(roc.so) <- c("group","ROC")

samp.gi <- out.gi %>% drop_na()
roc.out.gi <- numeric(9)

for(i in 8:16){
  roc.out.gi[i-7] <- as.numeric(roc(response = samp.gi[,i+1],
                                    predictor = inv.logit(predict(gi.out.glm[[i-7]])))$auc)
}
roc.gi <- cbind.data.frame(out.who.names[1:9],roc.out.gi)
colnames(roc.gi) <- c("group","ROC")

roc.all <- rbind.data.frame(roc.so,roc.gi)
roc.all <- cbind.data.frame(roc.all,identity=c(rep("SO",9),rep("GI",9)))
write.csv(roc.all,"./Plots/Final/roc_tab.csv")

# safety and belonging coeffs together ---------------------------
# filtering for goodness of fit scores
### sexual orientation --------------------------------------
so.safe1 <- so.safe %>% rename(where=safe_where)
so.belong1 <- so.belong %>% rename(where=belong_where)
so.safe1$type <- rep("Safety",nrow(so.safe1))
so.belong1$type <- rep("Belonging",nrow(so.belong1))
so.both <- rbind.data.frame(so.safe1,so.belong1)

beta.reg.r2.so <- beta.reg.r2 %>% filter(type=="Sexual Orientation") %>% 
  select(location,pseudoR2,model) %>% 
  rename(where=location,type=model)

so.both <- left_join(so.both,beta.reg.r2.so)
so.both$goodfit <- ifelse(round(so.both$pseudoR2,2)>=0.05,TRUE,FALSE)
so.both$estimate[so.both$goodfit==FALSE]<- NA
so.both$lci[so.both$goodfit==FALSE]<- NA
so.both$uci[so.both$goodfit==FALSE]<- NA

g2 <-
  so.both %>% 
  filter(coeff!="Intercept" & coeff!="Phi") %>%
  ggplot()+
  geom_jitter(aes(y=coeff,x=estimate,col=type,alpha=signif),size=3.5,
              position=position_dodge(width=-0.5))+
  geom_linerange(aes(y=coeff,xmin=lci,xmax=uci,col=type,alpha=signif),size=2,
                 position=position_dodge(width=-0.5))+
  geom_vline(xintercept=0,col="grey",lty=2)+
  scale_color_manual(name="",values=c("magenta","mediumorchid4"))+
  scale_alpha_discrete(range=c(0.4,1),name="",
                       labels=c("Not significant","Significant"))+
  facet_wrap(.~where,nrow=1)+
  ylab("")+xlab("Coefficient estimate") +
  ggtitle("Sexual orientation")+
  theme(text=element_text(size=19),
        axis.text.x = element_text(size=15))
gt <- ggplotGrob(g2)
gt <- editGrob(grid.force(gt), gPath("key-[3,4]-1-[1,2]"), 
               grep = TRUE, global = TRUE,
               x0 = unit(0, "npc"), y0 = unit(0.5, "npc"), 
               x1 = unit(1, "npc"), y1 = unit(0.5, "npc")) 
grid.newpage()
grid.draw(gt)
# png("./Plots/Final/so_coeffs_all_goodfit_only.png",width=12,height=5,units="in",res=900)
grid.draw(gt)
dev.off()

### gender identity --------------------------------------
gi.safe1 <- gi.safe %>% rename(where=safe_where)
gi.belong1 <- gi.belong %>% rename(where=belong_where)
gi.safe1$type <- rep("Safety",nrow(gi.safe1))
gi.belong1$type <- rep("Belonging",nrow(gi.belong1))
gi.both <- rbind.data.frame(gi.safe1,gi.belong1)

beta.reg.r2.gi <- beta.reg.r2 %>% filter(type=="Gender Identity") %>% 
  select(location,pseudoR2,model) %>% 
  rename(where=location,type=model)

gi.both <- left_join(gi.both,beta.reg.r2.gi)
gi.both$goodfit <- ifelse(round(gi.both$pseudoR2,2)>=0.05,TRUE,FALSE)
gi.both$estimate[gi.both$goodfit==FALSE]<- NA
gi.both$lci[gi.both$goodfit==FALSE]<- NA
gi.both$uci[gi.both$goodfit==FALSE]<- NA
                       
g2 <-
  gi.both %>% 
  filter(coeff!="Intercept" & coeff!="Phi") %>%
  ggplot()+
  geom_jitter(aes(y=coeff,x=estimate,col=type,alpha=signif),size=3.5,
              position=position_dodge(width=-0.5))+
  geom_linerange(aes(y=coeff,xmin=lci,xmax=uci,col=type,alpha=signif),size=2,
                 position=position_dodge(width=-0.5))+
  geom_vline(xintercept=0,col="grey",lty=2)+
  scale_color_manual(name="",values=c("magenta","mediumorchid4"))+
  scale_alpha_discrete(range=c(0.4,1),name="",
                       labels=c("Not significant","Significant"))+
  facet_wrap(.~where,nrow=1)+
  ylab("")+xlab("Coefficient estimate") +
  ggtitle("Gender identity")+
  theme(text=element_text(size=19),
        axis.text.x = element_text(size=15))
gt <- ggplotGrob(g2)
gt <- editGrob(grid.force(gt), gPath("key-[3,4]-1-[1,2]"), 
               grep = TRUE, global = TRUE,
               x0 = unit(0, "npc"), y0 = unit(0.5, "npc"), 
               x1 = unit(1, "npc"), y1 = unit(0.5, "npc")) 
grid.newpage()
grid.draw(gt)
# png("./Plots/Final/gi_coeffs_all_goodfit_only.png",width=12,height=5,units="in",res=900)
grid.draw(gt)
dev.off()
