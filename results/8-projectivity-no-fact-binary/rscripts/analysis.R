# Prior probability work
# 8-projectivity-no-fact-binary

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

# load subject information
# s = read.csv("../data/subject-information.csv")
#View(s)

# load raw data
d = read.csv("../data/experiment_noreps.csv")
head(d)

# bind the two files by workerid
# d = left_join(d,s)
# head(d)

nrow(d) #13650 = 525 participants x 26 items
names(d)
length(unique(d$workerid)) #525 participants

# tmp <- d %>%
  # group_by(workerid) %>%
  # summarize(Answer.time = sum(rt, na.rm = TRUE)) %>%
  # mutate(Answer.time_in_seconds = Answer.time/1000) %>%
  # mutate(Answer.time_in_minutes = Answer.time_in_seconds/60)
# as.data.frame(tmp)
# head(tmp)
summary(d$Answer.time_in_minutes)

ggplot(d, aes(x=Answer.time_in_minutes)) +
  geom_histogram()

summary(d)

d = d %>%
  select(workerid,rt,content,subjectGender,speakerGender,verb,utterance,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #13650

d = d %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"),
         nResponse = ifelse(response == "Yes",1,0))

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #78 missing values (3 Turkers)
table(d$age) #18-81 (plus two nonsense values)
median(d$age,na.rm=TRUE) #37 (nonsense values not excluded)
table(d$gender)
#272 female, 318 male, 2 undeclared
ages = unique(d[,c("workerid","age"),])

ggplot(ages %>% filter(age < 130),aes(x=age)) +
  geom_histogram()

### exclude non-American English speakers
length(unique(d$workerid)) #600
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- droplevels(subset(d, (d$language != "United States" & d$language != "African" & d$language != "" & d$language != "Spanish" & d$language != "0" & d$language != "Arabic" & d$language != "1" & d$language != "British English" & d$language != "german" & d$language != "Vietnamese" & d$language != "4" & d$language != "chinese" & d$language != "Italian" & d$language != "Korean")))
length(unique(d$workerid)) #506 (19 Turkers excluded)

# American English
length(which(is.na(d$american))) #0 (0 Turkers didn't respond)
table(d$american) # 24 declared non-American English
d <- droplevels(subset(d, d$american == 0))
length(unique(d$workerid)) #482 (24 Turkers excluded)

## exclude turkers who completed the experiment too quickly?

times = d %>%
  select(Answer.time_in_minutes,workerid) %>%
  unique() %>%
  mutate(VerySlow = Answer.time_in_minutes > mean(Answer.time_in_minutes) + 2*sd(Answer.time_in_minutes)) %>%
  filter(!VerySlow) %>%
  mutate(TooFast = Answer.time_in_minutes < mean(Answer.time_in_minutes) - 2*sd(Answer.time_in_minutes))
summary(times)
# nobody excluded

# exclude participants who did the experiment in under 1 minute
# table(d$Answer.time_in_minutes) 
# unique(d[d$Answer.time_in_minutes < 1.5,c("workerid","Answer.time_in_minutes")])
# d <- droplevels(subset(d, d$Answer.time_in_minutes >= 1))
# length(unique(d$workerid)) # 530 participants (11 Turkers excluded)

min(d$Answer.time_in_minutes)

## exclude Turkers based on non-projecting controls
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "MC")
c <- droplevels(c)
head(c)
nrow(c) #2892 / 6 controls = 482 Turkers

# proportion of "no" responses to controls
c %>%
  count(response) %>%
  mutate(prop =  n / sum(n)) %>%
  filter(response == "No")

# proportion of "no" responses to controls by participant
controlresponses = c %>%
  count(workerid, response) %>%
  group_by(workerid) %>%
  filter(response == "No")

ggplot(controlresponses, aes(x=n)) +
  geom_histogram()

table(controlresponses$n)

# identify outlier Turkers, defined as Turkers who responded "yes/1" to at least one control
outlier_Turkers = c %>%
  group_by(workerid) %>%
  summarize(Prop = mean(nResponse)) %>%
  filter(Prop != 0)
outlier_Turkers

# remove participants who gave "yes/1" response to at least one control
d <- droplevels(subset(d, !(d$workerid %in% outlier_Turkers$workerid)))
length(unique(d$workerid)) #426 Turkers (482-426 = 56 excluded)

# exclude turkers who always clicked "No"

# noclickers = d %>%
#   filter(verb != "MC") %>%
#   group_by(workerid) %>%
#   summarize(Proportion=mean(nResponse))
# 
# ggplot(noclickers, aes(x=Proportion)) +
#   geom_histogram()

# # people who always said No:
# noclickers %>%
#   filter(Proportion == 0)
# 
# d[d$workerid == 495,c("verb","response")]
# 
# 

# exclude workers with factive mean smaller than or equal to control mean
# given that control mean is 0, this means that they only responded "no/0" to factives

# fcmeans = d %>%
#   filter(verb %in% c("MC","be_annoyed","see","discover","reveal","know")) %>%
#   mutate(ItemType = ifelse(verb == "MC","MC","factive")) %>%
#   group_by(workerid,ItemType) %>%
#   summarize(Mean = mean(nResponse)) %>%
#   spread(ItemType,Mean) %>%
#   mutate(FCDiff = factive - MC)
# fcmeans
# 
# negfcdiffworkers = fcmeans[fcmeans$FCDiff <= 0,]$workerid
# length(negfcdiffworkers) # 10 turkers
# 
# # exclude the 10 Turkers identified above
# d <- droplevels(subset(d, !(d$workerid %in% negfcdiffworkers)))
# length(unique(d$workerid)) #471 Turkers remain (482-11 = 471)
# table(d$nResponse)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #11076 / 26 items = 426 participants

# load clean data for analysis ----
cd = read.csv("../data/cd.csv")
nrow(cd) #

# age info
table(cd$age) #18-81
length(which(is.na(cd$age))) # 26 missing values = 1 Turker
median(cd$age,na.rm=TRUE) #37
table(cd$gender)
#225 female, 240 male, 2 other

# define colors for the predicates
cols = data.frame(V=levels(cd$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))

# mean projectivity by predicate, including the main clause controls
prop = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
  # ungroup() %>% 
  # mutate(Verb = fct_reorder(verb,-prop))
prop

# plot of proportions

prop$VeridicalityGroup = as.factor(
  ifelse(prop$Verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(prop$Verb  %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(prop$Verb  %in% c("be_right","demonstrate"),"VNF",
                       ifelse(prop$Verb  %in% c("MC"),"MC","V")))))

levels(prop$VeridicalityGroup)
cols$Colors

ggplot(prop, aes(x=Verb, y=Mean, fill=VeridicalityGroup)) +
  #geom_point(color="black", size=4) +
  #geom_point(shape=21,fill="gray60",data=subjmeans, alpha=.1, color="gray40") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="top") +
  ylab("Proportion of 'yes' answers") +
  xlab("Predicate") 
ggsave("../graphs/proportion-by-predicate-variability.pdf",height=4,width=7)


# are the responses bi-modal, as suspected by Dotlacil and wondered by Katsos?
names(cd)

means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means

cd$verb <-factor(cd$verb, levels=levels(means$Verb))

ggplot(cd, aes(x=verb, y=response)) +
  geom_point(position="jitter") +
  #geom_boxplot(colour = "grey50") +
  scale_alpha(range = c(.3,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  ylab("Certainty ratings") +
  xlab("Predicate") 
ggsave("../graphs/raw-ratings-by-predicate.pdf",height=6,width=10)


# do the responses for each predicate-clause combination differ a lot?
names(cd)
t <- droplevels(subset(cd, cd$verb != "MC"))
nrow(t) #5660 / 20 items = 283 Turkers
names(t)
table(t$content)

means = t %>%
  group_by(verb,content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
options(tibble.print_max = Inf)
means

t$verb <-factor(t$verb, levels=levels(means$Verb))

ggplot(means, aes(x=content, y=Mean)) +
  geom_point(position="jitter") +
  #geom_boxplot(colour = "grey50") +
  scale_alpha(range = c(.3,1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  facet_grid(. ~ verb) +
  ylab("Certainty ratings") +
  xlab("Predicate") 
ggsave("../graphs/raw-ratings-by-predicate-and-content.pdf",height=6,width=20)

# median rating by predicates
medians = cd %>%
  group_by(verb) %>%
  summarize(Median = median(response),Quantile1 = quantile(response,1),Quantile2 = quantile(response,0.75),Quantile4 = quantile(response,0.25),Quantile5 = quantile(response,0))
options(tibble.print_max = Inf)
options(tibble.width = Inf)
medians

# target data (20 items per Turker) ---- 
names(cd)
table(cd$verb)
t <- droplevels(subset(cd, cd$verb != "MC"))
nrow(t) #5280 / 20 items = 264 Turkers

# how many ratings per predicate and per predicate-clause combination?
names(t)
tmp <- as.data.frame(table(t$verb))
min(tmp$Freq) #264
max(tmp$Freq) #264
mean(tmp$Freq) #264
# 264 because 264 Turkers and each Turker saw each predicate once

table(t$content)
t$predicateClause <- interaction(t$verb,t$content)
tmp <- as.data.frame(table(t$predicateClause))
head(tmp)
min(tmp$Freq) #4
max(tmp$Freq) #25
mean(tmp$Freq) #13.2

# plot mean projectivity by mean veridicality (2 measures) ----
head(t)
means = t %>%
  group_by(verb) %>%
  summarize(mean_proj = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(projMin = mean_proj - CILow, projMax = mean_proj + CIHigh)
#View(means)

# get veridicality means (2 measures)
infMeans <- read.csv(file="../../4-veridicality3/data/inference_means.csv", header=T, sep=",")
colnames(infMeans) <- c("verb","mean_inf","infMin","infMax")
infMeans <- droplevels(subset(infMeans, infMeans$verb != "entailing C" & infMeans$verb != "non-ent. C"))
infMeans
#View(infMeans)

contrMeans <- read.csv(file="../../2-veridicality2/data/veridicality_means.csv",header=T,sep=",")
colnames(contrMeans) <- c("verb","mean_contr","contrMin","contrMax")
contrMeans <- droplevels(subset(contrMeans, contrMeans$verb != "non-contrad. C" & contrMeans$verb != "contradictory C"))
#View(contrMeans)

merged <- means %>%
  left_join(infMeans,by=c("verb")) %>%
  left_join(contrMeans,by="verb")

cols = data.frame(V=levels(t$verb))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be_annoyed"), "F", 
         ifelse(cols$V %in% c("pretend", "think", "suggest", "say"), "NF", 
                ifelse(cols$V %in% c("be_right","demonstrate"),"VNF",
                       ifelse(cols$V %in% c("MC"),"MC","V")))))

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "darkorchid", 
                      ifelse(cols$VeridicalityGroup == "NF", "gray60", 
                             ifelse(cols$VeridicalityGroup == "VNF","dodgerblue",
                                    ifelse(cols$VeridicalityGroup == "MC","black","tomato1"))))
cols

ggplot(merged, aes(x=mean_proj,y=mean_inf)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean inference rating", limits = c(0,1),
                    breaks=c(0,0.25, 0.50, 0.75, 1.00))
  #ylab("Mean inference rating") 
ggsave(file="../graphs/projection-by-inference.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_inf)) +
  # geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean inference rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00))
#ylab("Mean inference rating") 
ggsave(file="../graphs/projection-by-inference-nolabels.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_contr)) +
  # geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=contrMin,ymax=contrMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean contradictoriness rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00))
#ylab("Mean contradictoriness rating") 
ggsave(file="../graphs/projection-by-contradictoriness-nolabels.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=mean_contr)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax),color="gray50",alpha=.5) +
  geom_errorbar(aes(ymin=contrMin,ymax=contrMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  scale_y_continuous(name ="Mean contradictoriness rating", limits = c(0,1),
                   breaks=c(0,0.25, 0.50, 0.75, 1.00))
  #ylab("Mean contradictoriness rating") 
ggsave(file="../graphs/projection-by-contradictoriness.pdf",width=4.2,height=3.5)

# plots with binary entailment 
merged$infEnt <- "no"
merged$infEnt <- ifelse(merged$verb == "be_right" | merged$verb == "see" | merged$verb == "discover" | 
                          merged$verb == "prove" | merged$verb == "confirm","yes","no")
merged$contrEnt <- "no"
merged$contrEnt <- ifelse(merged$verb == "be_right","yes","no")

ggplot(merged, aes(x=mean_proj,y=infEnt)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax,height = .15),color="gray50",alpha=.5) +
  #geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating (higher = more projective)", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  ylab("Entailed") 
ggsave(file="../graphs/projection-by-inferenceEntailment.pdf",width=4.2,height=3.5)

ggplot(merged, aes(x=mean_proj,y=contrEnt)) +
  geom_text_repel(aes(label=verb),alpha=.8,size=4,color=cols$Colors) +
  geom_errorbarh(aes(xmin=projMin,xmax=projMax,height = .15),color="gray50",alpha=.5) +
  #geom_errorbar(aes(ymin=infMin,ymax=infMax),color="gray50",alpha=.5) +
  geom_point(color=cols$Colors) +
  theme(legend.position="none") +
  scale_x_continuous(name ="Mean certainty rating", limits = c(0,1),
                     breaks=c(0,0.25, 0.50, 0.75, 1.00)) +
  ylab("Entailed") 
ggsave(file="../graphs/projection-by-contradictorinessEntailment.pdf",width=4.2,height=3.5)

# plot by-participant variability
cd$PresumedVerbType = as.factor(
  ifelse(cd$verb %in% c("know", "discover", "reveal", "see", "be_annoyed"), "factive", 
         ifelse(cd$verb %in% c("pretend", "think", "suggest", "say"), "plain non-factive", 
                ifelse(cd$verb %in% c("be_right","demonstrate"),"veridical non-factive",
                       ifelse(cd$verb %in% c("MC"),"MC","projective non-factive")))))

means = cd %>%
  group_by(PresumedVerbType,workerid) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) 
nrow(means)
head(means)

factives = means %>%
  filter(PresumedVerbType == "factive") %>%
  mutate(Participant = fct_reorder(as.factor(workerid),Mean))

means$Participant = factor(x=as.character(means$workerid),levels=levels(factives$Participant))
cd$Participant = factor(x=as.character(cd$workerid),levels=levels(factives$Participant))

# get rid of error bars for all but the "factives"
means[means$PresumedVerbType != "factive",]$YMin = means[means$PresumedVerbType != "factive",]$Mean
means[means$PresumedVerbType != "factive",]$YMax = means[means$PresumedVerbType != "factive",]$Mean

ggplot(means, aes(x=Participant, y=Mean, fill=PresumedVerbType)) +
  geom_point(shape=21, data=cd, aes(y=response,fill=PresumedVerbType), alpha=.1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  ylab("Mean certainty rating") +
  xlab("Participant") +
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust=1,vjust=1 )) 
ggsave("../graphs/means-projectivity-by-participant.pdf",height=4,width=25)

## updated code up to here (JT), after updating exclusion criteria

### PAIRWISE DIFFERENCES ###
library(lsmeans)
library(lme4)
library(brms)
str(cd$response)
str(cd$verb)
cd$verb <- as.factor(cd$verb)
str(cd$workerid)
cd$workerid <- as.factor(cd$workerid)
model = lmer(response ~ verb + (1|workerid), data=cd, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# contrast                  estimate     SE   df t.ratio p.value
# MC - pretend              -0.03286 0.0155 7075  -2.113  0.8678
# MC - be_right             -0.06541 0.0155 7075  -4.206  0.0047
# MC - think                -0.07163 0.0155 7075  -4.606  0.0008
# MC - suggest              -0.10445 0.0155 7075  -6.717  <.0001
# MC - say                  -0.12071 0.0155 7075  -7.763  <.0001
# MC - prove                -0.17459 0.0155 7075 -11.228  <.0001
# MC - confirm              -0.21643 0.0155 7075 -13.918  <.0001
# MC - establish            -0.23180 0.0155 7075 -14.907  <.0001
# MC - demonstrate          -0.35141 0.0155 7075 -22.599  <.0001
# MC - announce             -0.44141 0.0155 7075 -28.387  <.0001
# MC - confess              -0.49304 0.0155 7075 -31.707  <.0001
# MC - admit                -0.51406 0.0155 7075 -33.059  <.0001
# MC - reveal               -0.55516 0.0155 7075 -35.702  <.0001
# MC - acknowledge          -0.57887 0.0155 7075 -37.226  <.0001
# MC - hear                 -0.59989 0.0155 7075 -38.579  <.0001
# MC - discover             -0.62989 0.0155 7075 -40.508  <.0001
# MC - inform               -0.65544 0.0155 7075 -42.151  <.0001
# MC - see                  -0.66163 0.0155 7075 -42.548  <.0001
# MC - know                 -0.70746 0.0155 7075 -45.496  <.0001
# MC - be_annoyed           -0.72587 0.0155 7075 -46.680  <.0001
# pretend - be_right        -0.03254 0.0204 7075  -1.598  0.9921
# pretend - think           -0.03876 0.0204 7075  -1.904  0.9469
# pretend - suggest         -0.07159 0.0204 7075  -3.516  0.0596
# pretend - say             -0.08784 0.0204 7075  -4.315  0.0029
# pretend - prove           -0.14173 0.0204 7075  -6.961  <.0001
# pretend - confirm         -0.18357 0.0204 7075  -9.016  <.0001
# pretend - establish       -0.19894 0.0204 7075  -9.771  <.0001
# pretend - demonstrate     -0.31855 0.0204 7075 -15.646  <.0001
# pretend - announce        -0.40855 0.0204 7075 -20.067  <.0001
# pretend - confess         -0.46018 0.0204 7075 -22.602  <.0001
# pretend - admit           -0.48120 0.0204 7075 -23.635  <.0001
# pretend - reveal          -0.52230 0.0204 7075 -25.654  <.0001
# pretend - acknowledge     -0.54601 0.0204 7075 -26.818  <.0001
# pretend - hear            -0.56703 0.0204 7075 -27.851  <.0001
# pretend - discover        -0.59703 0.0204 7075 -29.324  <.0001
# pretend - inform          -0.62258 0.0204 7075 -30.579  <.0001
# pretend - see             -0.62876 0.0204 7075 -30.883  <.0001
# pretend - know            -0.67459 0.0204 7075 -33.134  <.0001
# pretend - be_annoyed      -0.69300 0.0204 7075 -34.038  <.0001
# be_right - think          -0.00622 0.0204 7075  -0.305  1.0000
# be_right - suggest        -0.03905 0.0204 7075  -1.918  0.9431
# be_right - say            -0.05530 0.0204 7075  -2.716  0.4388
# be_right - prove          -0.10919 0.0204 7075  -5.363  <.0001
# be_right - confirm        -0.15102 0.0204 7075  -7.418  <.0001
# be_right - establish      -0.16640 0.0204 7075  -8.173  <.0001
# be_right - demonstrate    -0.28601 0.0204 7075 -14.048  <.0001
# be_right - announce       -0.37601 0.0204 7075 -18.468  <.0001
# be_right - confess        -0.42763 0.0204 7075 -21.004  <.0001
# be_right - admit          -0.44866 0.0204 7075 -22.037  <.0001
# be_right - reveal         -0.48975 0.0204 7075 -24.055  <.0001
# be_right - acknowledge    -0.51346 0.0204 7075 -25.220  <.0001
# be_right - hear           -0.53449 0.0204 7075 -26.252  <.0001
# be_right - discover       -0.56449 0.0204 7075 -27.726  <.0001
# be_right - inform         -0.59004 0.0204 7075 -28.981  <.0001
# be_right - see            -0.59622 0.0204 7075 -29.284  <.0001
# be_right - know           -0.64205 0.0204 7075 -31.535  <.0001
# be_right - be_annoyed     -0.66046 0.0204 7075 -32.440  <.0001
# think - suggest           -0.03283 0.0204 7075  -1.612  0.9912
# think - say               -0.04908 0.0204 7075  -2.411  0.6778
# think - prove             -0.10297 0.0204 7075  -5.057  0.0001
# think - confirm           -0.14481 0.0204 7075  -7.112  <.0001
# think - establish         -0.16018 0.0204 7075  -7.867  <.0001
# think - demonstrate       -0.27979 0.0204 7075 -13.742  <.0001
# think - announce          -0.36979 0.0204 7075 -18.163  <.0001
# think - confess           -0.42141 0.0204 7075 -20.698  <.0001
# think - admit             -0.44244 0.0204 7075 -21.731  <.0001
# think - reveal            -0.48353 0.0204 7075 -23.750  <.0001
# think - acknowledge       -0.50724 0.0204 7075 -24.914  <.0001
# think - hear              -0.52827 0.0204 7075 -25.947  <.0001
# think - discover          -0.55827 0.0204 7075 -27.420  <.0001
# think - inform            -0.58382 0.0204 7075 -28.675  <.0001
# think - see               -0.59000 0.0204 7075 -28.979  <.0001
# think - know              -0.63583 0.0204 7075 -31.230  <.0001
# think - be_annoyed        -0.65424 0.0204 7075 -32.134  <.0001
# suggest - say             -0.01625 0.0204 7075  -0.798  1.0000
# suggest - prove           -0.07014 0.0204 7075  -3.445  0.0745
# suggest - confirm         -0.11198 0.0204 7075  -5.500  <.0001
# suggest - establish       -0.12735 0.0204 7075  -6.255  <.0001
# suggest - demonstrate     -0.24696 0.0204 7075 -12.130  <.0001
# suggest - announce        -0.33696 0.0204 7075 -16.550  <.0001
# suggest - confess         -0.38859 0.0204 7075 -19.086  <.0001
# suggest - admit           -0.40961 0.0204 7075 -20.119  <.0001
# suggest - reveal          -0.45071 0.0204 7075 -22.137  <.0001
# suggest - acknowledge     -0.47442 0.0204 7075 -23.302  <.0001
# suggest - hear            -0.49544 0.0204 7075 -24.335  <.0001
# suggest - discover        -0.52544 0.0204 7075 -25.808  <.0001
# suggest - inform          -0.55099 0.0204 7075 -27.063  <.0001
# suggest - see             -0.55717 0.0204 7075 -27.367  <.0001
# suggest - know            -0.60300 0.0204 7075 -29.618  <.0001
# suggest - be_annoyed      -0.62141 0.0204 7075 -30.522  <.0001
# say - prove               -0.05389 0.0204 7075  -2.647  0.4925
# say - confirm             -0.09572 0.0204 7075  -4.702  0.0005
# say - establish           -0.11110 0.0204 7075  -5.457  <.0001
# say - demonstrate         -0.23071 0.0204 7075 -11.332  <.0001
# say - announce            -0.32071 0.0204 7075 -15.752  <.0001
# say - confess             -0.37233 0.0204 7075 -18.288  <.0001
# say - admit               -0.39336 0.0204 7075 -19.320  <.0001
# say - reveal              -0.43445 0.0204 7075 -21.339  <.0001
# say - acknowledge         -0.45816 0.0204 7075 -22.503  <.0001
# say - hear                -0.47919 0.0204 7075 -23.536  <.0001
# say - discover            -0.50919 0.0204 7075 -25.010  <.0001
# say - inform              -0.53473 0.0204 7075 -26.264  <.0001
# say - see                 -0.54092 0.0204 7075 -26.568  <.0001
# say - know                -0.58675 0.0204 7075 -28.819  <.0001
# say - be_annoyed          -0.60516 0.0204 7075 -29.723  <.0001
# prove - confirm           -0.04184 0.0204 7075  -2.055  0.8947
# prove - establish         -0.05721 0.0204 7075  -2.810  0.3696
# prove - demonstrate       -0.17682 0.0204 7075  -8.685  <.0001
# prove - announce          -0.26682 0.0204 7075 -13.105  <.0001
# prove - confess           -0.31845 0.0204 7075 -15.641  <.0001
# prove - admit             -0.33947 0.0204 7075 -16.674  <.0001
# prove - reveal            -0.38057 0.0204 7075 -18.692  <.0001
# prove - acknowledge       -0.40428 0.0204 7075 -19.857  <.0001
# prove - hear              -0.42530 0.0204 7075 -20.889  <.0001
# prove - discover          -0.45530 0.0204 7075 -22.363  <.0001
# prove - inform            -0.48085 0.0204 7075 -23.618  <.0001
# prove - see               -0.48703 0.0204 7075 -23.921  <.0001
# prove - know              -0.53286 0.0204 7075 -26.172  <.0001
# prove - be_annoyed        -0.55127 0.0204 7075 -27.077  <.0001
# confirm - establish       -0.01537 0.0204 7075  -0.755  1.0000
# confirm - demonstrate     -0.13498 0.0204 7075  -6.630  <.0001
# confirm - announce        -0.22498 0.0204 7075 -11.050  <.0001
# confirm - confess         -0.27661 0.0204 7075 -13.586  <.0001
# confirm - admit           -0.29763 0.0204 7075 -14.619  <.0001
# confirm - reveal          -0.33873 0.0204 7075 -16.637  <.0001
# confirm - acknowledge     -0.36244 0.0204 7075 -17.802  <.0001
# confirm - hear            -0.38346 0.0204 7075 -18.834  <.0001
# confirm - discover        -0.41346 0.0204 7075 -20.308  <.0001
# confirm - inform          -0.43901 0.0204 7075 -21.563  <.0001
# confirm - see             -0.44519 0.0204 7075 -21.867  <.0001
# confirm - know            -0.49102 0.0204 7075 -24.118  <.0001
# confirm - be_annoyed      -0.50943 0.0204 7075 -25.022  <.0001
# establish - demonstrate   -0.11961 0.0204 7075  -5.875  <.0001
# establish - announce      -0.20961 0.0204 7075 -10.295  <.0001
# establish - confess       -0.26124 0.0204 7075 -12.831  <.0001
# establish - admit         -0.28226 0.0204 7075 -13.864  <.0001
# establish - reveal        -0.32336 0.0204 7075 -15.882  <.0001
# establish - acknowledge   -0.34707 0.0204 7075 -17.047  <.0001
# establish - hear          -0.36809 0.0204 7075 -18.079  <.0001
# establish - discover      -0.39809 0.0204 7075 -19.553  <.0001
# establish - inform        -0.42364 0.0204 7075 -20.808  <.0001
# establish - see           -0.42982 0.0204 7075 -21.112  <.0001
# establish - know          -0.47565 0.0204 7075 -23.363  <.0001
# establish - be_annoyed    -0.49406 0.0204 7075 -24.267  <.0001
# demonstrate - announce    -0.09000 0.0204 7075  -4.421  0.0019
# demonstrate - confess     -0.14163 0.0204 7075  -6.956  <.0001
# demonstrate - admit       -0.16265 0.0204 7075  -7.989  <.0001
# demonstrate - reveal      -0.20375 0.0204 7075 -10.007  <.0001
# demonstrate - acknowledge -0.22746 0.0204 7075 -11.172  <.0001
# demonstrate - hear        -0.24848 0.0204 7075 -12.205  <.0001
# demonstrate - discover    -0.27848 0.0204 7075 -13.678  <.0001
# demonstrate - inform      -0.30403 0.0204 7075 -14.933  <.0001
# demonstrate - see         -0.31021 0.0204 7075 -15.237  <.0001
# demonstrate - know        -0.35604 0.0204 7075 -17.488  <.0001
# demonstrate - be_annoyed  -0.37445 0.0204 7075 -18.392  <.0001
# announce - confess        -0.05163 0.0204 7075  -2.536  0.5806
# announce - admit          -0.07265 0.0204 7075  -3.568  0.0503
# announce - reveal         -0.11375 0.0204 7075  -5.587  <.0001
# announce - acknowledge    -0.13746 0.0204 7075  -6.751  <.0001
# announce - hear           -0.15848 0.0204 7075  -7.784  <.0001
# announce - discover       -0.18848 0.0204 7075  -9.258  <.0001
# announce - inform         -0.21403 0.0204 7075 -10.512  <.0001
# announce - see            -0.22021 0.0204 7075 -10.816  <.0001
# announce - know           -0.26604 0.0204 7075 -13.067  <.0001
# announce - be_annoyed     -0.28445 0.0204 7075 -13.971  <.0001
# confess - admit           -0.02102 0.0204 7075  -1.033  1.0000
# confess - reveal          -0.06212 0.0204 7075  -3.051  0.2194
# confess - acknowledge     -0.08583 0.0204 7075  -4.216  0.0045
# confess - hear            -0.10686 0.0204 7075  -5.248  <.0001
# confess - discover        -0.13686 0.0204 7075  -6.722  <.0001
# confess - inform          -0.16240 0.0204 7075  -7.977  <.0001
# confess - see             -0.16859 0.0204 7075  -8.280  <.0001
# confess - know            -0.21442 0.0204 7075 -10.531  <.0001
# confess - be_annoyed      -0.23283 0.0204 7075 -11.436  <.0001
# admit - reveal            -0.04110 0.0204 7075  -2.018  0.9096
# admit - acknowledge       -0.06481 0.0204 7075  -3.183  0.1575
# admit - hear              -0.08583 0.0204 7075  -4.216  0.0045
# admit - discover          -0.11583 0.0204 7075  -5.689  <.0001
# admit - inform            -0.14138 0.0204 7075  -6.944  <.0001
# admit - see               -0.14756 0.0204 7075  -7.248  <.0001
# admit - know              -0.19339 0.0204 7075  -9.499  <.0001
# admit - be_annoyed        -0.21180 0.0204 7075 -10.403  <.0001
# reveal - acknowledge      -0.02371 0.0204 7075  -1.165  0.9999
# reveal - hear             -0.04473 0.0204 7075  -2.197  0.8226
# reveal - discover         -0.07473 0.0204 7075  -3.671  0.0357
# reveal - inform           -0.10028 0.0204 7075  -4.926  0.0002
# reveal - see              -0.10647 0.0204 7075  -5.229  <.0001
# reveal - know             -0.15230 0.0204 7075  -7.480  <.0001
# reveal - be_annoyed       -0.17071 0.0204 7075  -8.385  <.0001
# acknowledge - hear        -0.02102 0.0204 7075  -1.033  1.0000
# acknowledge - discover    -0.05102 0.0204 7075  -2.506  0.6039
# acknowledge - inform      -0.07657 0.0204 7075  -3.761  0.0261
# acknowledge - see         -0.08276 0.0204 7075  -4.065  0.0083
# acknowledge - know        -0.12859 0.0204 7075  -6.316  <.0001
# acknowledge - be_annoyed  -0.14700 0.0204 7075  -7.220  <.0001
# hear - discover           -0.03000 0.0204 7075  -1.474  0.9972
# hear - inform             -0.05555 0.0204 7075  -2.728  0.4296
# hear - see                -0.06173 0.0204 7075  -3.032  0.2296
# hear - know               -0.10756 0.0204 7075  -5.283  <.0001
# hear - be_annoyed         -0.12597 0.0204 7075  -6.187  <.0001
# discover - inform         -0.02555 0.0204 7075  -1.255  0.9997
# discover - see            -0.03173 0.0204 7075  -1.559  0.9942
# discover - know           -0.07756 0.0204 7075  -3.810  0.0219
# discover - be_annoyed     -0.09597 0.0204 7075  -4.714  0.0005
# inform - see              -0.00618 0.0204 7075  -0.304  1.0000
# inform - know             -0.05201 0.0204 7075  -2.555  0.5654
# inform - be_annoyed       -0.07042 0.0204 7075  -3.459  0.0713
# see - know                -0.04583 0.0204 7075  -2.251  0.7898
# see - be_annoyed          -0.06424 0.0204 7075  -3.155  0.1693
# know - be_annoyed         -0.01841 0.0204 7075  -0.904  1.0000
# 
# P value adjustment: tukey method for comparing a family of 21 estimates 

# brms model
cd$verb = relevel(cd$verb,ref="MC")
cd$item = as.factor(paste(cd$verb,cd$content))
model.brms = brm(response ~ verb + (1|workerid) + (1|item), data=cd, family=gaussian())
summary(model.brms)
