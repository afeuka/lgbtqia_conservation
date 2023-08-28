###Title: Summaries of LGBTQIA+ data
###Author: Abbey Feuka (she/her)
###Date: 30032023
###Notes: summary plots and statistics for lgbtqia+ survey data on safety, belonging, outness, and inclusion
###no model fitting
#data -------------------------
library(tidyverse)
library(ggrepel)
library(RColorBrewer)

dat <- read.csv("./Data/lgbtqia_survresults_20032023_cleanheadings.csv")
dat <- subset(dat,Response_type!="Survey Preview")

total_respond <- nrow(dat)
text.size=15

# demographics  ---------------------------------
## gender identity -------------------------
dat$cis <- rep(NA,nrow(dat))
dat$cis[grep("Cisgender",dat$GenderID)] <- 1
dat$cis[grepl("Cisgender",dat$GenderID) & nchar(dat$GenderID)>16] <- 0 #mixed responses = expansive
dat$cis[!(grepl("Cisgender",dat$GenderID))] <- 0
dat[grepl("I don't understand the question",dat$GenderID),]$cis <- NA
dat[dat$GenderID=="",]$cis <- NA
dat[grepl("Prefer not to respond",dat$GenderID),]$cis <- NA

### gender table - binary -------------------------------
gen.bin <- dat %>% group_by(cis) %>% 
  summarise(n = n(),
            prop=n/total_respond) 
colnames(gen.bin) <- c("GenderID","n","Proportion")
gen.bin$GenderID <- c("Gender Expansive","Cisgender","No Response")

gen.id.names <- c("Agender","Cisgender female","Cisgender male","Genderfluid","Genderqueer or non-binary","Gender non-conforming","I don't identify with a specific identity","Intersex","Questioning","Transgender female","Transgender male","Two-spirit/Other traditional Indigenous gender","Self-identified","Prefer not to respond","Don't understand question")
gen.all <- dat %>% 
  summarise(Agender=sum(grepl("Agender",GenderID)),
            CisFemale=sum(grepl("Cisgender female",GenderID)),
            CisMale=sum(grepl("Cisgender male",GenderID)),
            Genderfluid=sum(grepl("Genderfluid",GenderID)),
            Genderqueer=sum(grepl("Genderqueer or non-binary",GenderID)),
            GemderNonconforming=sum(grepl("Gender non-conforming",GenderID)),
            Nospecific=sum(grepl("I don't identify with a specific identity",GenderID)),
            Intersex=sum(grepl("Intersex",GenderID)),
            Questioning=sum(grepl("Questioning or figuring it out",GenderID)),
            TransgenderFemale=sum(grepl("Transgender female",GenderID)),
            TransgenderMale=sum(grepl("Transgender male",GenderID)),
            TwoSpiritIndigenous=sum(grepl("Two-spirit or other traditional Indigenous gender",GenderID)),
            SelfID=sum(grepl("Prefer to self-identiy",GenderID)),
            PreferNoResponse=sum(grepl("Prefer not to respond",GenderID)),
            DontUnderstand=sum(grepl("I don't understand the question",GenderID))) %>% 
  pivot_longer(cols=1:15,names_to="genderid",values_to="n") %>% 
  mutate(prop=n/total_respond)

### gender table - all --------------------------------
gen.all$genderid <- gen.id.names
gen.tab <- gen.all[order(gen.all$prop,decreasing = T),]
gen.tab$prop <- round(gen.tab$prop,3)
colnames(gen.tab)<-c("Gender_Identity","n","Proportion")

##sexual orientation ----------------------------
dat$hetero <- rep(NA,nrow(dat))
dat$hetero[!grepl("Straight or heterosexual",dat$SexOrientation)] <- 0
dat$hetero[grepl("Straight or heterosexual",dat$SexOrientation)] <- 1
dat[grepl("I don't understand the question",dat$SexOrientation),]$hetero <- NA
dat[dat$SexOrientation=="",]$hetero <- NA
dat[grepl("Prefer not to respond",dat$SexOrientation),]$hetero <- NA

###sex orientation table - binary --------------------------
het.bin <- dat %>%  group_by(hetero) %>% 
  summarise(n = n(),
            prop=n/total_respond)
colnames(het.bin) <- c("Sexual_Orientation","n","Proportion")
het.bin$Sexual_Orientation <- c("Queer+","Heterosexual","No response")

sex.orient.names <- c("Asexual","Bisexual","Gay","Lesbian","Not Heterosexual","Pansexual/Omnisexual","Questioning","Queer","Prefer to self-identify","Heterosexual","Prefer not to respond","Don't understand question")
sex.all <- dat %>% 
  summarise(Asexual=sum(grepl("Asexual or ace spectrum",SexOrientation)),
            Bisexual=sum(grepl("Bisexual",SexOrientation)),
            Gay=sum(grepl("Gay",SexOrientation)),
            Lesbian=sum(grepl("Lesbian",SexOrientation)),
            NotHetero=sum(grepl("Not heterosexual, but don't identify with a specific identity",SexOrientation)),
            PanOmni=sum(grepl("Pansexual or omnisexual",SexOrientation)),
            Questioning=sum(grepl("Questioning or figuring it out",SexOrientation)),
            Queer=sum(grepl("Queer",SexOrientation)),
            SelfID=sum(grepl("Prefer to self-identiy",SexOrientation)),
            Heterosexual=sum(grepl("Straight or heterosexual",SexOrientation)),
            PreferNoResponse=sum(grepl("Prefer not to respond",SexOrientation)),
            DontUnderstand=sum(grepl("I don't understand the question",SexOrientation))) %>% 
  pivot_longer(cols=1:12,names_to="sexorientation",values_to="n")%>% 
  mutate(prop=n/total_respond)

###sexual orientation table -all ----------------------------------------
sex.all$sexorientation <- sex.orient.names
sex.tab <- sex.all[order(sex.all$prop,decreasing = T),]
sex.tab$prop <- round(sex.tab$prop,2)
colnames(sex.tab)<-c("Sexual_Orientation","n","Proportion")

##race -------------------------------
race.names <- c("American Indian/Native American/Alaska Native/First Nations",
                "Asian",
                "Black/African American",
                "Hispanic/LatinX",
                "Middle Eastern/North African",
                "Native Hawaiian/Pacific Islander",
                "White/Caucasian",
                "Prefer to self-describe",
                "Prefer not to answer")
race.all <- dat %>% 
  summarise(Native=sum(grepl("American Indian/Native American/Alaska Native/First Nations",Race_ethnicity)),
            Asian=sum(grepl("Asian",Race_ethnicity)),
            Black=sum(grepl("Black/African American",Race_ethnicity)),
            LatinX=sum(grepl("Hispanic/LatinX",Race_ethnicity)),
            MiddleEastern=sum(grepl("Middle Eastern/North African",Race_ethnicity)),
            PacificIslander=sum(grepl("Native Hawaiian/Pacific Islander",Race_ethnicity)),
            White=sum(grepl("White/Caucasian",Race_ethnicity)),
            SelfID=sum(grepl("Prefer to self-describe",Race_ethnicity)),
            PreferNoResponse=sum(grepl("Prefer not to answer",Race_ethnicity)))%>% 
  pivot_longer(cols=1:9,names_to="Race_ethnicity",values_to="n")%>% 
  mutate(prop=n/total_respond)

race.all$Race_ethnicity <- race.names
race.tab <- race.all[order(race.all$prop,decreasing = T),]
race.tab$prop <- round(race.tab$prop,2)
colnames(race.tab)<-c("Race/Ethnicity","n","Proportion")

dat$white <- rep(0,nrow(dat))
dat$white[grep("White",dat$Race_ethnicity)] <- 1
dat$white[grepl("White",dat$Race_ethnicity) & nchar(dat$Race_ethnicity)>15] <- 0 #mixed race w/ white

##age -------------------------------------------
summary(dat$Age)
age.all <- dat %>% filter(!is.na(Age)) %>% 
  summarise("18-25"=sum(Age>=18 & Age <=25),
            "26-35"=sum(Age>=26 & Age <=35),
            "36-45"=sum(Age>=36 & Age <=45),
            "46-55"=sum(Age>=46 & Age <=55),
            "56-65"=sum(Age>=56 & Age <=65),
            "66+"=sum(Age>=66)) %>% 
  pivot_longer(cols=1:6,names_to="Age",values_to="n")%>%
  mutate(prop=n/total_respond)

dat$under40 <- rep(NA,nrow(dat))
dat$under40[dat$Age<40] <- 1
dat$under40[dat$Age>=40] <- 0

##job sector ------------------------------------------
job.names <- c("Academia/College/University",
               "Consultant",
               "Unemployed",
               "State agency",
               "Federal agency",
               "Non-governmental/non-profit organization",
               "Private sector",
               "Prefer not to answer",
               "Other")
job.all <- dat %>% 
  summarise(Academia=sum(grepl("Academia/College/University",Work_sector)),
            Consultant=sum(grepl("Consultant",Work_sector)),
            Unemployed=sum(grepl("Unemployed",Work_sector)),
            State=sum(grepl("State agency",Work_sector)),
            Federal=sum(grepl("Federal agency",Work_sector)),
            NGO=sum(grepl("Non-governmental/non-profit organization",Work_sector)),
            Private=sum(grepl("Private sector",Work_sector)),
            NoResponse=sum(grepl("Prefer not to answer",Work_sector)),
            Other=sum(grepl("Other",Work_sector)),)%>% 
  pivot_longer(cols=1:9,names_to="Work_sector",values_to="n")%>% 
  mutate(prop=n/total_respond)

job.all$Work_sector <- job.names
job.tab <- job.all[order(job.all$prop,decreasing = T),]
job.tab$prop <- round(job.tab$prop,2)
colnames(job.tab)<-c("Work Sector","n","Proportion")

dat$academ.bin <- rep(NA,nrow(dat))
dat$academ.bin[grep("Academia",dat$Work_sector)] <- 1
dat$academ.bin[!grepl("Academia",dat$Work_sector)] <- 0
dat$academ.bin[grep("Prefer not to answer",dat$Work_sector)] <- NA

##state / political ---------------------------------------------
states <- read.csv("./Data/states_vote.csv")
dat <- left_join(dat,states,by="State")
dat$democrat <- rep(NA,nrow(dat))
dat$democrat[dat$Vote=="Democrat"] <- 1
dat$democrat[dat$Vote=="Republican"] <- 0

vote.all <- dat %>% group_by(Vote) %>% 
  summarise(n= n(),
            prop=n/total_respond)

##voting - continuous -------------------------------
biden <- read.csv("./Data/Popular_vote.csv")
biden <- biden %>% select(state,dem_this_margin) %>% 
  rename(State=state,
         vote.dem.cont=dem_this_margin)
biden$vote.dem.cont[biden$vote.dem.cont==""] <- NA
biden$State[biden$State=="Hawaii"] <- "Hawai'i"
biden$vote.dem.cont <- as.numeric(unlist(sapply(1:nrow(biden),function(i)strsplit(biden$vote.dem.cont[i],"%"))))/100

dat <- dat %>% left_join(biden)
vote_tab <- dat %>% group_by(State) %>% 
  summarise(n=n(),
            prop=n/total_respond) %>% 
  left_join(biden) 
vote_tab$State[vote_tab$State==""] <- "No response"

##disability status ----------------------
disabled.all <- dat %>% summarise(
  Mental=sum(grepl("Mental impairment/disability",Disabilities)),
  Physical=sum(grepl("Physical impairment/disability",Disabilities)),
  None=sum(grepl("None",Disabilities)),
  PreferNot=sum(grepl("Prefer not to answer",Disabilities))) %>% 
pivot_longer(cols=1:4,names_to="Disability",values_to="n")%>% 
  mutate(prop=n/total_respond)
disabled.all$Disability <- c("Mental Disability","Physical Disability","None","Prefer not to answer")

##woman ------------------------------
dat$woman_cistrans <- rep(0,nrow(dat))
dat$woman_cistrans[grep("female",dat$GenderID)] <- 1

## binaries ----------------------------
dat$queer_so <- abs(1-dat$hetero)
dat$queer_gi <- abs(1-dat$cis)
dat$poc <- abs(1-dat$white)
dat$over40 <- abs(1-dat$under40)
dat$agency <- abs(1-dat$academ.bin)
dat$republican <- abs(1-dat$democrat)

# conservation inclusive ---------------------------------
##sexual orientation --------------------------------
dat$Conservation_inclusive <- factor(dat$Conservation_inclusive,
                                     levels=c("Strongly disagree",
                                              "Somewhat disagree",
                                              "Neither agree nor disagree",
                                              "Somewhat agree",
                                              "Strongly agree",
                                              ""))
levels(dat$Conservation_inclusive)[levels(dat$Conservation_inclusive)==""] <- "No Response"

inc.so <- dat %>% group_by(hetero) %>% 
  filter(!is.na(hetero)) %>% 
  mutate(n=n()) %>% #proportion calculated out of het/queer+
  group_by(hetero,Conservation_inclusive) %>% 
  filter(Conservation_inclusive!="No Response") %>% 
  summarise(prop=n()/n) %>% 
  distinct()

###stacked ------------------------------------------
incl.so <- dat %>% 
  filter(Conservation_inclusive!="No Response") %>% 
  filter(!is.na(hetero)) %>% 
  group_by(Conservation_inclusive) %>% 
  mutate(n=n()) %>% 
  group_by(hetero,Conservation_inclusive) %>% 
  summarise(prop=n()/n) %>% 
  distinct()

ggplot(incl.so)+geom_bar(aes(x=Conservation_inclusive,y=prop,fill=factor(hetero)),
                         stat="identity")+
  ylab("Proportion of respondents")+
  xlab("Is conservation inclusive for LGBTQIA+ identities?")+
  scale_fill_manual(name="Sexual Orientation",
                    labels=c("Queer+","Heterosexual"),
                    values=brewer.pal(n = 10, name = "Paired")[c(1,2)])+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        text = element_text(size=15))

## gender identity ----------------------------------------
inc.gi <- dat %>% 
  group_by(cis) %>% 
  filter(!is.na(cis)) %>% 
  mutate(n=n()) %>% #proportion calculated by identity (cis/GE)
  group_by(cis,Conservation_inclusive) %>% 
  filter(Conservation_inclusive!="No Response") %>% 
  summarise(prop=n()/n) %>% 
  distinct()
incl.gi<- rbind(inc.gi,data.frame(cis=0,Conservation_inclusive="Strongly agree",prop=0))
incl.gi$Conservation_inclusive <- factor(incl.gi$Conservation_inclusive,
                                         levels=c("Strongly disagree","Somewhat disagree",
                                                  "Neither agree nor disagree", "Somewhat agree",
                                                  "Strongly agree"))
### stacked --------------------------------------------
incl.gi <- dat %>% 
  filter(Conservation_inclusive!="No Response") %>% 
  filter(!is.na(cis)) %>% 
  group_by(Conservation_inclusive) %>% 
  mutate(n=n()) %>% #proportion calculated by answer
  group_by(cis,Conservation_inclusive) %>% 
  summarise(prop=n()/n) %>% 
  distinct()

ggplot(incl.gi)+geom_bar(aes(x=Conservation_inclusive,y=prop,fill=factor(cis)),
                      stat="identity")+
  ylab("Proportion of respondents")+
  xlab("Is conservation inclusive for LGBTQIA+ identities?")+
  scale_fill_manual(name="Gender Identity",
                    labels=c("Gender expansive","Cisgender"),
                    values=brewer.pal(n = 10, name = "Paired")[c(9,10)])+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        text = element_text(size=15))

#safety bar plot --------------------------------------------
locs <- c("Conference","Public work","Personal life","Public non-work",
          "In person workplace","Remote workplace")

## queer + vs straight -----------------------------------
suppressMessages(
  dat %>% group_by(hetero) %>% 
    mutate(n=n()) %>% 
    filter(!is.na(hetero)) %>% 
    summarise(personal=mean(SexOrientation_safe_personal,na.rm=T),
              conferences=mean(SexOrientation_safe_conferences,na.rm=T),
              public_nonwork=mean(SexOrientation_safe_Publicnonwork,na.rm=T),
              fieldwork=mean(SexOrientation_safe_Fieldwork,na.rm=T),
              work_inperson=mean(SexOrientation_safe_Workplaceinperson,na.rm=T),
              work_remote=mean(SexOrientation_safe_Workplaceremote,na.rm=T)) %>% 
    pivot_longer(cols=2:7,names_to="Space",values_to="prop") %>% 
    ggplot(aes(y=prop,x=Space,fill=as.factor(hetero)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_manual(name="Sexual Orientation",
                      labels=c("Queer+","Heterosexual","No Response"),
                      values=brewer.pal(n = 10, name = "Paired")[c(1,2)])+
    scale_x_discrete(labels=locs)+
    ylab("Mean percent of time felt safe")+
    theme(axis.text.x=element_text(angle=45,hjust=1),
          text = element_text(size=20)))

safe.so <- dat %>% group_by(hetero) %>% 
  mutate(n=n()) %>% 
  filter(!is.na(hetero)) %>% 
  summarise(personal=mean(SexOrientation_safe_personal,na.rm=T),
            conferences=mean(SexOrientation_safe_conferences,na.rm=T),
            public_nonwork=mean(SexOrientation_safe_Publicnonwork,na.rm=T),
            fieldwork=mean(SexOrientation_safe_Fieldwork,na.rm=T),
            work_inperson=mean(SexOrientation_safe_Workplaceinperson,na.rm=T),
            work_remote=mean(SexOrientation_safe_Workplaceremote,na.rm=T))

safe.so.df <-safe.so[1,]-safe.so[2,]

## gender expansive vs cisgender ------------------------------
dat %>% group_by(cis) %>% 
  filter(!is.na(cis)) %>% 
  summarise(personal=mean(GenderID_safe_personal,na.rm=T),
            conferences=mean(GenderID_safe_conferences,na.rm=T),
            public_nonwork=mean(GenderID_safe_Publicnonwork,na.rm=T),
            fieldwork=mean(GenderID_safe_Fieldwork,na.rm=T),
            work_inperson=mean(GenderID_safe_Workplaceinperson,na.rm=T),
            work_remote=mean(GenderID_safe_Workplaceremote,na.rm=T)) %>% 
  pivot_longer(cols=2:7,names_to="Space",values_to="prop") %>% 
  ggplot(aes(y=prop,x=Space,fill=as.factor(cis)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(name="Gender Identity",
                      labels=c("Gender Expansive","Cisgender","No Response"),
                    values=brewer.pal(n = 10, name = "Paired")[c(9,10)])+
  scale_x_discrete(labels=locs)+
  ylab("Mean percent of time felt safe")+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        text = element_text(size=20))

safe.gi <- dat %>% group_by(cis) %>% 
  filter(!is.na(cis)) %>% 
  summarise(personal=mean(GenderID_safe_personal,na.rm=T),
            conferences=mean(GenderID_safe_conferences,na.rm=T),
            public_nonwork=mean(GenderID_safe_Publicnonwork,na.rm=T),
            fieldwork=mean(GenderID_safe_Fieldwork,na.rm=T),
            work_inperson=mean(GenderID_safe_Workplaceinperson,na.rm=T),
            work_remote=mean(GenderID_safe_Workplaceremote,na.rm=T))
safe.gi.df <-safe.gi[1,]-safe.gi[2,]

#belonging bar plot -------------------------------
##queer+ vs straight --------------------------
dat %>% group_by(hetero) %>% 
  filter(!is.na(hetero)) %>% 
  summarise(personal=mean(SexOrientation_belong_personal,na.rm=T),
            conferences=mean(SexOrientation_belong_conferences,na.rm=T),
            public_nonwork=mean(SexOrientation_belong_Publicnonwork,na.rm=T),
            fieldwork=mean(SexOrientation_belong_fieldwork,na.rm=T),
            work_inperson=mean(SexOrientation_belong_Workinperson,na.rm=T),
            work_remote=mean(SexOrientation_belong_Workremote,na.rm=T)) %>% 
  pivot_longer(cols=2:7,names_to="Space",values_to="prop") %>% 
  ggplot(aes(y=prop,x=Space,fill=as.factor(hetero)))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(labels=locs)+
  scale_fill_manual(name="Sexual Orientation",
                    labels=c("Queer+","Heterosexual","No Response"),
                    values=brewer.pal(n = 10, name = "Paired")[c(1,2)])+
  ylab("Mean percent of time felt belonging")+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        text = element_text(size=20))

belong.so <- dat %>% group_by(hetero) %>% 
  filter(!is.na(hetero)) %>% 
  summarise(personal=mean(SexOrientation_belong_personal,na.rm=T),
            conferences=mean(SexOrientation_belong_conferences,na.rm=T),
            public_nonwork=mean(SexOrientation_belong_Publicnonwork,na.rm=T),
            fieldwork=mean(SexOrientation_belong_fieldwork,na.rm=T),
            work_inperson=mean(SexOrientation_belong_Workinperson,na.rm=T),
            work_remote=mean(SexOrientation_belong_Workremote,na.rm=T))
belong.so.df <-belong.so[1,]-belong.so[2,]

## gender expansive vs cisgender -----------------------------
dat %>% group_by(cis) %>% 
  filter(!is.na(cis)) %>% 
  summarise(personal=mean(GenderID_belong_personal,na.rm=T),
            conferences=mean(GenderID_belong_conferences,na.rm=T),
            public_nonwork=mean(GenderID_belong_Publicnonwork,na.rm=T),
            fieldwork=mean(GenderID_belong_fieldwork,na.rm=T),
            work_inperson=mean(GenderID_belong_Workinperson,na.rm=T),
            work_remote=mean(GenderID_belong_Workremote,na.rm=T)) %>% 
  pivot_longer(cols=2:7,names_to="Space",values_to="prop") %>% 
  ggplot(aes(y=prop,x=Space,fill=as.factor(cis)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(name="Gender Identity",
                    labels=c("Gender Expansive","Cisgender","No Response"),
                    values=brewer.pal(n = 10, name = "Paired")[c(9,10)])+
  scale_x_discrete(labels=locs)+
  ylab("Mean percent of time felt belonging")+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        text = element_text(size=20))

belong.gi <- dat %>% group_by(cis) %>% 
  filter(!is.na(cis)) %>% 
  summarise(personal=mean(GenderID_belong_personal,na.rm=T),
            conferences=mean(GenderID_belong_conferences,na.rm=T),
            public_nonwork=mean(GenderID_belong_Publicnonwork,na.rm=T),
            fieldwork=mean(GenderID_belong_fieldwork,na.rm=T),
            work_inperson=mean(GenderID_belong_Workinperson,na.rm=T),
            work_remote=mean(GenderID_belong_Workremote,na.rm=T))
belong.gi.df <-belong.gi[1,]-belong.gi[2,]

safe.so.df$typ <- "safe"
safe.gi.df$typ <-"safe"
belong.so.df$typ <-"belong"
belong.gi.df$typ <-"belong"
safe.so.df$id <- "so"
safe.gi.df$id <-"gi"
belong.so.df$id <-"so"
belong.gi.df$id <-"gi"

diff.all <- rbind(belong.gi.df[,-1],belong.so.df[,-1],safe.gi.df[,-1],safe.so.df[,-1])

#out where barplot----------------------------------
out.who.names <- c("Colleagues at work/school",
                   "Colleagues outside of work/school",
                   "Members of the public at work/school",
                   "Mentor at work/school",
                   "Office or campus as a whole",
                   "Work/school friends",
                   "Friends outside of work/school",
                   "Chosen family",
                   "Biological family",
                   "Prefer not to respond",
                   "Other","None of these")

##sexual orientation -------------------------
tot_nonhet <- nrow(subset(dat,hetero==0))
out.sex <- dat %>% filter(hetero==0)%>% 
  summarise(colleaugesInside=sum(grepl("Certain colleagues at work or school",SexOrientation_Out_who))/tot_nonhet,
            colleaugesOutside=sum(grepl("Colleagues outside",SexOrientation_Out_who))/tot_nonhet,
            public=sum(grepl("public",SexOrientation_Out_who))/tot_nonhet,
            mentor=sum(grepl("Mentor at work or school",SexOrientation_Out_who))/tot_nonhet,
            officeCampus=sum(grepl("The office or campus as a whole",SexOrientation_Out_who))/tot_nonhet,
            workfriends=sum(grepl("Work or school friends",SexOrientation_Out_who))/tot_nonhet,
            nonworkfriends=sum(grepl("Friends outside of work or school",SexOrientation_Out_who))/tot_nonhet,
            chosenFamily=sum(grepl("Chosen family",SexOrientation_Out_who))/tot_nonhet,
            bioFamily=sum(grepl("Biological family",SexOrientation_Out_who))/tot_nonhet,
            noResponse=sum(grepl("Prefer not to say",SexOrientation_Out_who))/tot_nonhet,
            other=sum(grepl("Other",SexOrientation_Out_who))/tot_nonhet,
            noneofthese=sum(grepl("I am not out to any of these groups.",SexOrientation_Out_who))/tot_nonhet) %>% 
  pivot_longer(cols=1:12,names_to="out_to",values_to="prop")

out.sex$out_to <- out.who.names

##gender identity ------------------------------------
tot_noncis <- nrow(subset(dat,cis==0))
out.gen <- dat %>% filter(cis==0)%>% 
  summarise(colleaugesInside=sum(grepl("Certain colleagues at work or school",GenderID_Outwho))/tot_noncis,
            colleaugesOutside=sum(grepl("Colleagues outside",GenderID_Outwho))/tot_noncis,
            public=sum(grepl("public",GenderID_Outwho))/tot_noncis,
            mentor=sum(grepl("Mentor at work or school",GenderID_Outwho))/tot_noncis,
            officeCampus=sum(grepl("The office or campus as a whole",GenderID_Outwho))/tot_noncis,
            workfriends=sum(grepl("Work or school friends",GenderID_Outwho))/tot_noncis,
            nonworkfriends=sum(grepl("Friends outside of work or school",GenderID_Outwho))/tot_noncis,
            chosenFamily=sum(grepl("Chosen family",GenderID_Outwho))/tot_noncis,
            bioFamily=sum(grepl("Biological family",GenderID_Outwho))/tot_noncis,
            noResponse=sum(grepl("Prefer not to say",GenderID_Outwho))/tot_noncis,
            other=sum(grepl("Other",GenderID_Outwho))/tot_noncis,
            noneofthese=sum(grepl("I am not out to any of these groups.",GenderID_Outwho))/tot_noncis) %>% 
  pivot_longer(cols=1:12,names_to="out_to",values_to="prop")

out.gen$out_to <- out.who.names

## together -----------------------------------------
out.gen$type <- rep("gi",nrow(out.gen))
out.sex$type <- rep("so",nrow(out.sex))
out.all <- rbind(out.gen,out.sex)
ggplot(data=out.all, aes(y=reorder(out_to,prop,decreasing=F),
                             x=prop,fill=type))+
  geom_bar(stat="identity",position = "dodge")+
  xlab("Proportion of respondents out to given group \nwith regard to given identity")+
  ylab("Group")+
  scale_fill_manual(name="LGBTQIA+ Identity",labels=c("Gender expansive","Queer+"),
                    values=brewer.pal(n = 10, name = "Paired")[c(9,1)])+
  theme(#axis.text.x=element_text(angle=45,vjust=1,hjust=1),
        text=element_text(size=20),
        plot.margin = margin(0, 0, 0, 4, "cm"))

#support resources -----------------------------------------------
support.names <- c("External collaborators at work/school",
                   "Leadership resources",
                   "Listervs",
                   "Members of the public at work/school",
                   "Mentor at work/school",
                   "Peers at work/school",
                   "Support groups within work/school",
                   "Support groups outside work/school",
                   "Support groups within a professional society",
                   "Webinars",
                   "Wellness/counseling services through work/school",
                   "Wellness/counseling services outside work/school",
                   "Webistes or forums",
                   "Other",
                   "None availabile at work/school",
                   "None availabile anywhere")

##sexual orientation ---------------------------------------------
support.sex <- dat %>% filter(hetero==0)%>% 
  summarise(externalCollab=sum(grepl("External",Support_resources)),
            leadership=sum(grepl("Leadership",Support_resources)),
            listervs=sum(grepl("Listervs",Support_resources)),
            public=sum(grepl("public",Support_resources)),
            mentors=sum(grepl("Mentor",Support_resources)),
            peers=sum(grepl("Peers",Support_resources)),
            groupsWithin=sum(grepl("Support groups within my organization",Support_resources)),
            groupsOutside=sum(grepl("Support groups outside my organization",Support_resources)),
            groupsSociety=sum(grepl("Support groups within a professional society",Support_resources)),
            webinars=sum(grepl("Webinars",Support_resources)),
            counselingWithin=sum(grepl("counseling services provided through work or school",Support_resources)),
            counselingOutside=sum(grepl("counseling services outside of work or school",Support_resources)),
            websites=sum(grepl("Websites",Support_resources)),
            other=sum(grepl("Other",Support_resources)),
            noneInside=sum(grepl("but I do outside of these",Support_resources)),
            none=sum(grepl("I don't have support resources available to me at or outside work or school",Support_resources))) %>% 
  pivot_longer(cols=1:16,names_to="support_resource",values_to="n") %>%  
  mutate(prop = n/tot_nonhet)

support.sex$support_resource <- support.names

##gender id ---------------------------
support.gen <- dat %>% filter(cis==0)%>% 
  summarise(externalCollab=sum(grepl("External",Support_resources)),
            leadership=sum(grepl("Leadership",Support_resources)),
            listervs=sum(grepl("Listervs",Support_resources)),
            public=sum(grepl("public",Support_resources)),
            mentors=sum(grepl("Mentor",Support_resources)),
            peers=sum(grepl("Peers",Support_resources)),
            groupsWithin=sum(grepl("Support groups within my organization",Support_resources)),
            groupsOutside=sum(grepl("Support groups outside my organization",Support_resources)),
            groupsSociety=sum(grepl("Support groups within a professional society",Support_resources)),
            webinars=sum(grepl("Webinars",Support_resources)),
            counselingWithin=sum(grepl("counseling services provided through work or school",Support_resources)),
            counselingOutside=sum(grepl("counseling services outside of work or school",Support_resources)),
            websites=sum(grepl("Websites",Support_resources)),
            other=sum(grepl("Other",Support_resources)),
            noneInside=sum(grepl("but I do outside of these",Support_resources)),
            none=sum(grepl("I don't have support resources available to me at or outside work or school",Support_resources))) %>% 
  pivot_longer(cols=1:16,names_to="support_resource",values_to="n") %>%  
  mutate(prop = n/tot_noncis)

support.gen$support_resource <- support.names

#cultural practices -----------------------------------------
cultural.names <- c("Anonymous feedback systems",
                   "Community norms",
                   "Non-anonymous feedback systems",
                   "Roundtable discussions",
                   "Trainings",
                   "Work or school socials",
                   "Other")

##sexual orientation --------------------------------------
cultural.sex <- dat %>% filter(hetero==0)%>% 
  summarise(anonymous=sum(grepl("Anonymous feedback systems",Support_cultural)),
            communityNorms=sum(grepl("Community norms",Support_cultural)),
            nonAnonymous=sum(grepl("Non-anonymous feedback systems",Support_cultural)),
            roundtables=sum(grepl("Roundtable discussions",Support_cultural)),
            trainings=sum(grepl("Trainings",Support_cultural)),
            socials=sum(grepl("socials",Support_cultural)),
            other=sum(grepl("Other",Support_cultural))) %>% 
  pivot_longer(cols=1:7,names_to="cultural_practice",values_to="n") %>%  
  mutate(prop = n/tot_nonhet)

cultural.sex$cultural_practice <- cultural.names

##gender id --------------------------------------
cultural.gen <- dat %>% filter(cis==0)%>% 
  summarise(anonymous=sum(grepl("Anonymous feedback systems",Support_cultural)),
            communityNorms=sum(grepl("Community norms",Support_cultural)),
            nonAnonymous=sum(grepl("Non-anonymous feedback systems",Support_cultural)),
            roundtables=sum(grepl("Roundtable discussions",Support_cultural)),
            trainings=sum(grepl("Trainings",Support_cultural)),
            socials=sum(grepl("socials",Support_cultural)),
            other=sum(grepl("Other",Support_cultural))) %>% 
  pivot_longer(cols=1:7,names_to="cultural_practice",values_to="n") %>%  
  mutate(prop = n/tot_noncis)

cultural.gen$cultural_practice <- cultural.names
  