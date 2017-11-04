
####################### IGNORE THIS BIT ###############################
setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/4-ProjAI/results')

# read in the data
d_real = read.csv("../run-AMT-experiment/experiment.csv")
nrow(d_real) #7500 (250 Turkers x 30 responses)
names(d_real)
head(d_real)
summary(d_real)

# anonymize the data
d_real$workerid <- match(d_real$workerid, unique(sort(d_real$workerid)))
d <- d_real
table(d$workerid)
saveRDS(d, file="data/d.rds")

####################### START HERE #####################################

# read in the data
d = readRDS(d, file="data/d.rds")
source('helpers.R')

theme_set(theme_bw())

library(plyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(languageR)
library(lme4)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(ucminf)
library(scales)

############## Pre-analysis data clean-up #################

# look at Turkers' comments
unique(d$comments)

# age
table(d$age) #18-74
median(d$age) #32
mean(d$age) #34.3

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 4 to 34 (3 instructions at beginning, 19 missing because another instruction)
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) # trial numbers from 1 to 31 (16 missing because instruction)
d[d$trial > 15,]$trial = d[d$trial > 15,]$trial - 1
unique(d$trial) # trials from 1 to 30

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# american english   American English   Chinese, English             Creole 
# 30                 30                 30                 30 
# Elish            emglish            Eng;osj            Engligh 
# 30                 30                 30                 30 
# english            English            ENGLISH           english  
# 1950               4740                150                 30 
# English           ENGLISH              Englush            Enlgish 
# 60                 30                 30                 30 
# ENLGISH             french German and English            Russian 
# 30                 30                 30                 30 
# Spanish               Thai         vietnamese 
# 60                 30                 30
d <- subset(d, (d$language != "french" & d$language != "Russian" & d$language != "Creole" &
                  d$language != "Spanish" & d$language != "Thai" & d$language != "vietnamese"))
d = droplevels(d)
length(unique(d$workerid)) #243 (data from 7 Turker excluded, 243 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))#243
length(which(is.na(d$ame))) #0 (everybody responded)
table(d$ame) 
# No  Yes 
# 660 6630
d <- subset(d, d$ame == "Yes")
d = droplevels(d)
length(unique(d$workerid)) #221 (data from 22 Turkers excluded, 221 remaining Turkers)

################# Excuding Turkers based on main clause controls #################

### look at how Turkers did on the main clauses 
# main clauses
names(d)
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #2652 (221 Turkers x 6 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- subset(d.MC, d.MC$question_type == "projective")
d.MC.Proj <- droplevels(d.MC.Proj)
nrow(d.MC.Proj) #1326

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2)

# calculate each Turkers mean response to the projection of main clauses
p.means = aggregate(response~workerid, data=d.MC.Proj, FUN="mean")
p.means$YMin = p.means$response - aggregate(response~workerid, data=d.MC.Proj, FUN="ci.low")$response
p.means$YMax = p.means$response + aggregate(response~workerid, data=d.MC.Proj, FUN="ci.high")$response
p.means

ggplot(p.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- subset(d.MC, d.MC$question_type == "ai")
d.MC.AI <- droplevels(d.MC.AI)
nrow(d.MC.AI) #1326

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2)

# calculate each Turkers mean response to the projection of main clauses
ai.means = aggregate(response~workerid, data=d.MC.AI, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC.AI, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC.AI, FUN="ci.high")$response
ai.means

ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 3
# standard deviations away from the overall mean

# get the Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$response > (mean(p.means$response) + 3*sd(p.means$response)),]
p
# high responses: 3, 55, 56, 70, 71, 74, 117, 234, 237 (9 Turkers), all larger than .51 (.51 - .79)

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai
# high responses: 56, 70, 74, 128, 213 (5 Turkers), all larger than .43 (.43-.98)

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% p$workerid | d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #132 (11 unique outlier Turkers x 12 = 6 main clauses x 2 questions)

# check if there's any systematicity to the items that they're outliers on. maybe there's 
# something about some of the items that makes some people interpret things differently? 
# It's quite telling that the 
# error bars on the outliers are huge, which means that they could have well given mostly very 
# low responses and only on a few items gave a higher response, which may indicate either 
# systematicity to some of the items, or just noise.

# for each Turker, plot their response to the 6 main clauses they saw
ggplot(outliers, aes(x=content,y=response,color=question_type)) +
  geom_point() +
  facet_wrap(~workerid)
# 3, 237 gave high responses to 5/6 projection questions, low on one item (hence larger error bar)
# 55 gave high responses to all projection questions (tiny error bar)
# 56, 70, 71, 74 gave high responses to all projection and ai questions
# 117, 234 gave high response to all projection questions
# 128, 213 gave high responses to all ai questions
# from the systematicity of their high ai and projection responses they qualify as outliers

# for each main clause content, plot the responses that the outlier Turkers gave to them
# do this in case the "outlier" Turkers all happened to see the same contents
ggplot(outliers, aes(x=workerid,y=response,color=question_type)) +
  geom_point() +
  facet_wrap(~content)
# 16 of 17 contents represented among outliers (so it's not about a subset of items)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% p$workerid | d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) # 210 remaining Turkers (11 Turkers excluded)

# clean data = cd
cd = d
saveRDS(cd, file="data/cd.rds")
head(cd)

################## Exclusion Turkers based on response times ############
################# no Turkers were excluded #######################

# the goal is to remove Turkers who just clicked through the experiment
# there's two numbers we can look at: the overall time it took them to take the experiment, 
# and how long they took to respond to each item

# How long did the Turkers take overall?
table(d$Answer.time_in_minutes)
mean(d$Answer.time_in_minutes)
sd(d$Answer.time_in_minutes)
min(d$Answer.time_in_minutes)
max(d$Answer.time_in_minutes)

# > mean(d$Answer.time_in_minutes)
# [1] 6.771054
# > sd(d$Answer.time_in_minutes)
# [1] 3.725123
# > min(d$Answer.time_in_minutes)
# [1] 2.098233
# > max(d$Answer.time_in_minutes)
# [1] 28.72848

# Plot the response times below 5 minutes
ggplot(d, aes(x=workerid,y=Answer.time_in_minutes)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(0, 5)

# Identify participants who did the experiment in less than the mean minus 1sd minutes
fast <- d[d$Answer.time_in_minutes < mean(d$Answer.time_in_minutes) - sd(d$Answer.time_in_minutes),]
unique(fast$workerid) #2 5 13 58 80 181 

# these guys did the experiment in 3 minutes or less
table(fast$workerid,fast$Answer.time_in_minutes)

ggplot(fast, aes(x=workerid,y=Answer.time_in_minutes)) +
  geom_point() 

# plot the response times (rt) of the fast Turkers
ggplot(fast, aes(x=workerid,y=rt)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(0, 4000)

min(fast$rt) #1968 (1.9 seconds)

# some Turker completed the experiment fast, but they still took at least 1.9 seconds
# for each response

# now look at how long the Turkers took to respond to individual items

# rt in milliseconds (1000 milliseconds = 1 second)
mean(d$rt)
sd(d$rt)
min(d$rt)
max(d$rt)

# > mean(d$rt)
# [1] 8971.627
# > sd(d$rt)
# [1] 13143.09
# > min(d$rt)
# [1] 1906
# > max(d$rt)
# [1] 625576

# rt in seconds (1000 milliseconds = 1 second)

mean(d$rt/1000)
sd(d$rt/1000)
min(d$rt/1000)
max(d$rt/1000)

# > mean(d$rt/1000)
# [1] 8.971627 
# > sd(d$rt/1000)
# [1] 13.14309
# > min(d$rt/1000)
# [1] 1.906
# > max(d$rt/1000)
# [1] 625.576

# calculate and plot the mean rt for each Turker
mean.rt = aggregate(rt~workerid, data=d, FUN="mean")
mean.rt$YMin = mean.rt$rt - aggregate(rt~workerid, data=d, FUN="ci.low")$rt
mean.rt$YMax = mean.rt$rt + aggregate(rt~workerid, data=d, FUN="ci.high")$rt
mean.rt

min(mean.rt$rt)

ggplot(mean.rt, aes(x=workerid,y=rt)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylab("Mean response time")+
  ylim(1000,10000)

# the fastest response took 1.9 seconds, so there's nothing really to worry about

str(d$rt)
d$rt = as.numeric(d$rt)
# plot the response times below 20 seconds (mean = 9 seconds)
ggplot(d, aes(x=rt))+
  geom_histogram(binwidth = 100) + 
  xlim(0, 20000)+
  xlab("Response times in milliseconds (below 20 seconds")

# Identify participants with response times lower than 4 seconds = 4000 milliseconds
fast_rt <- d[d$rt < 4000,]
unique(fast_rt$workerid) #39 Turkers

# plot the response time for the 39 Turkers with "fast" rts (less than 3 seconds)
ggplot(fast_rt, aes(x=workerid,y=rt)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(1000, 5000)+
  xlab("Response times in milliseconds (below 4 seconds")

# these responses all took two or more seconds

# no Turkers are excluded based on the time it took them to respond!

# exclude the fast Turkers
# d <- subset(d, !(d$workerid %in% fast$workerid))
# d <- droplevels(d)
# length(unique(d$workerid)) #204  remaining Turkers (6 excluded)

length(unique(d$workerid)) #210 remaining Turkers

################## Properties of the data #############
# load clean data (Turkers excluded)
cd = readRDS(cd, file="data/cd.rds")

# age info
names(cd)
table(cd$age) #19-68
median(cd$age) #33
mean(cd$age) #36.1

# What's the distribution of contents across the triggers?
table(cd$content,cd$short_trigger)
# lowest: 24 uses of bmw with know
# highest: 242 uses of olives with MC

#            annoyed discover know  MC NomApp NRRC only possNP stop stupid
# alcatraz        0      100   44 172      0   58    0      0    0      0
# aunt            0        0   48 126      0   76    0    116    0      0
# ballet          0        0    0 144     84    0    0      0  138      0
# bmw            52        0   24 102    120    0    0     78    0      0
# boyfriend       0        0    0 198      0   58    0    116    0      0
# cheat           0        0   64 176      0    0    0      0    0    120
# cupcakes        0        0  108 178      0   80    0      0    0      0
# hat            92        0   50 116      0    0    0    110    0      0
# kids            0        0    0 154      0    0  146      0    0     76
# muffins         0        0    0 188      0   42  122      0    0      0
# nails           0       50    0 114      0    0    0      0  126    102
# olives        122        0    0 242      0    0    0      0    0      0
# pizza          74       78    0  76      0    0  152      0    0      0
# play            0       62   46 114      0    0    0      0  156      0
# soccer         80      130   36  84      0   44    0      0    0      0
# stuntman        0        0    0 172     84    0    0      0    0    122
# veggie          0        0    0 164    132   62    0      0    0      0

################## test if block order mattered ############################
cd = readRDS(cd, file="data/cd.rds")
length(unique(cd$workerid)) #210

# make a data structure tmp that includes only info relevant to the analyses
# use dplyr::select to make sure that select comes from the dplyr package
tmp = cd %>%
  dplyr::select(response, short_trigger, content, question_type, trigger_class, workerid, block)
nrow(tmp) #6300 (6300 / 210 Turkers = 30 items per Turker)

head(tmp)

# "block1" is first, "block2" is second
# the two types of questions were "ai" and "projective"
table(tmp$block,tmp$question_type)
## 
#       ai projective
#block1 1740       1410
#block2 1410       1740
# 1740 / 15 items = 116 Turkers first answered at-issueness questions
# 1410 / 15 items = 94 Turkers first answered projectivity questions
# 116 + 94 = 210 Turkers

str(tmp$response)
library(lmerTest)

# make a subset of the data that only includes the target data with question about at-issueness
tmp_ai = subset(tmp, (trigger_class != "NonProj" & question_type == "ai"))
tmp_ai = droplevels(tmp_ai)
head(tmp_ai)
table(tmp_ai$block,tmp_ai$question_type)

# make a subset of the data that only includes the target data with question about projectivity
tmp_proj = subset(tmp, (trigger_class != "NonProj" & question_type == "projective"))
tmp_proj = droplevels(tmp_proj)
head(tmp_proj)
table(tmp_proj$block,tmp_proj$question_type)

# predict at-issueness response from trigger and block, no interaction
m.ai.2 = lmer(response ~ short_trigger + block + (1|workerid) + (1|content), data=tmp_ai)
summary(m.ai.2)
#responses in block2 significantly higher

# predict at-issueness response from trigger and block, and interaction
m.ai = lmer(response ~ short_trigger * block + (1|workerid) + (1|content), data=tmp_ai)
summary(m.ai)
#responses in block2 not higher than block1
#responses to "only" in block2 higher

# mean at-issueness response to "only"
only <- droplevels(subset(tmp_ai, short_trigger == "only"))
head(only)
agr = aggregate(response ~ block, data=only, FUN="mean")
agr
#block1 .64
#block2 .83
# the prejacent of "only" was judged to be significantly more not-at-issue if judged
# in block2 (after projectivity judgment) than in block1 (before projectivity judgment)

anova(m.ai.2,m.ai)
# model with interaction is better, so effect is carried by "only", not in general by block order

# do the same for projectivity results
m.proj = lmer(response ~ short_trigger * block + (1|workerid) + (1|content), data=tmp_proj)
summary(m.proj)
# responses in block2 not different from block1

m.proj.2 = lmer(response ~ short_trigger + block + (1|workerid) + (1|content), data=tmp_proj)
summary(m.proj.2)
# responses in block2 not different from block1, no interactions significant

m.proj.3 = lmer(response ~ block + (1|workerid) + (1|content), data=tmp_proj)
summary(m.proj.3)

anova(m.proj.2,m.proj)
# model with interaction is not better, so block order did not matter for projectivity

################## make data structure with projectivity and ai wide ############################
################## to be able to predict projectivity from ai #################

length(unique(cd$workerid)) #210

# make a data structure tmp that includes only info relevant to the analyses
# use dplyr::select to make sure that select comes from the dplyr package
tmp = cd %>%
  dplyr::select(response, short_trigger, content, question_type, trigger_class, workerid)
nrow(tmp) #6300 (6300 / 210 Turkers = 30 items per Turker)

# make a new column "variable" that codes a trigger-content-class-workerid combination
tmp$variable = paste(tmp$short_trigger, tmp$content, tmp$trigger_class, tmp$workerid)
tmp$variable
str(tmp$variable)
head(tmp)

# make a new data structure t that has rows with the variable info, the ai and the projective responses 
t = tmp %>%
  dcast(variable ~ question_type , value.var="response")
head(t)

# now add back into t the info in the variable as separate columns
t$short_trigger = sapply(strsplit(as.character(t$variable)," "), "[", 1)
t$content = sapply(strsplit(as.character(t$variable)," "), "[", 2)
t$trigger_class = sapply(strsplit(as.character(t$variable)," "), "[", 3)
t$workerid = sapply(strsplit(as.character(t$variable)," "), "[", 4)
head(t)
nrow(t) #3150 (correct: half of 6300)

# save data structure t 
saveRDS(t, file="data/t.rds")
head(t)

# now we have a semi-wide data structure t that codes for each Turker-item pair 
# the ai and projective response by that Turker to that item
t <- readRDS(t, file="data/t.rds")
nrow(t)

str(t$projective)
str(t$ai)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
str(t$content)
t$content <- as.factor(t$content)
head(t)
table(t$short_trigger)

### are projective contents significantly more projective and NAI than main clauses?
names(t)
library(lmerTest)

# make main clauses the reference level 
t$short_trigger = as.factor(as.character(t$short_trigger))
contrasts(t$short_trigger)
t$short_trigger <- relevel(t$short_trigger, ref = "MC")

# predict projectivity
proj <- lmer(projective ~ short_trigger * ai + (1+short_trigger*ai|workerid) + (1|content), data=t)
# proj does not converge
proj.1 <- lmer(projective ~ short_trigger * ai + (1+ai|workerid) + (1|content), data=t)
summary(proj.1)
proj.2 <- lmer(projective ~ short_trigger + ai + (1+ai|workerid) + (1|content), data=t)
summary(proj.2)
anova(proj.1,proj.2)

# predict not-at-issueness
nai <- lmer(ai ~ short_trigger + (1+short_trigger|workerid) + (1|content), data=t)
# nai does not converge
nai.1 <- lmer(ai ~ short_trigger + (1|workerid) + (1|content), data=t)
summary(nai.1)

# load library for multiple comparisons
library(multcomp)

# multiple comparison wrt projectivity
comp_proj.1 <- glht(proj.1, mcp(short_trigger="Tukey"))
#covariate interactions found -- default contrast might be inappropriate
comp_proj.2 <- glht(proj.2, mcp(short_trigger="Tukey"))
summary(comp_proj.2)

# multiple comparisons wrt not-at-issueness
comp_nai <- glht(nai.1,mcp(short_trigger="Tukey"))
summary(comp_nai)

# make NRRC the reference level because the most projective
proj$short_trigger = as.factor(as.character(proj$short_trigger))
contrasts(proj$short_trigger)
proj$short_trigger <- relevel(proj$short_trigger, ref = "NRRC")

# predict projectivity from trigger and at-issueness
mo.1 = lmer(projective ~ short_trigger * ai + (1+short_trigger|workerid) + (1|content), data=proj)
# failed to converge

mo.2 = lmer(projective ~ ai + short_trigger + (1|workerid) + (1|content), data=proj)
summary(mo.2) 

mo.3 = lmer(projective ~ ai * short_trigger + (1|workerid) + (1|content), data=proj)
summary(mo.3) 

anova(mo.2,mo.3)

# load library for multiple comparisons
library(multcomp)

proj_c$short_trigger = as.factor(proj_c$short_trigger) 
contrasts(proj_c$short_trigger) #annoyed is reference level

# predict not-at-issueness from the trigger
triggers_c = lm(ai ~ short_trigger, data = proj_c)
summary(triggers_c) #model with annoyed as reference level, comparisons to annoyed

# multiple comparisons of trigger by not-at-issueness
comp_nai_c <- glht(triggers_c, mcp(short_trigger="Tukey"))
summary(comp_nai_c)

# predict projectivity from the trigger
triggers2_c = lm(projective ~ short_trigger, data = proj_c)
summary(triggers2_c) #model with annoyed as reference level, comparisons to annoyed

comp_proj_c <- glht(triggers2_c, mcp(short_trigger="Tukey"))
summary(comp_proj_c)

#####################  Plotting ############################ 
names(t)
head(t)
table(t$trigger_class)
theme_set(theme_bw())

### are the 17 contents more projective when realized with trigger than with main clause?
agr = aggregate(projective~short_trigger+content, data=t, FUN="mean")
agr$YMin = agr$projective - aggregate(projective~short_trigger+content, data=t, FUN="ci.low")$projective
agr$YMax = agr$projective + aggregate(projective~short_trigger+content, data=t, FUN="ci.high")$projective
agr

ggplot(agr, aes(x=short_trigger, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  facet_wrap(~content,ncol=1)
ggsave(f="graphs/contents-mean-projective-nonprojective.pdf",height=20,width=6)

### plot the projectivity of the different triggers
str(t$projective)
table(t$short_trigger)
t.proj <- droplevels(subset(t,t$short_trigger != "MC"))
table(t.proj$short_trigger)

mean_proj = aggregate(projective~short_trigger, data=t.proj, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~short_trigger, data=t.proj, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~short_trigger, data=t.proj, FUN="ci.high")$projective
mean_proj

t.proj$trigger_proj <-factor(t.proj$short_trigger, levels=mean_proj[order(mean_proj$projective), "short_trigger"])

ggplot(t.proj, aes(x=trigger_proj, y=projective)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
  #geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-projection.pdf",height=3,width=6)

ggplot(t.proj, aes(x=trigger_proj, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  ylab("Projectivity")+
  xlab("Expression")
ggsave(f="graphs/boxplot-projection.pdf",height=3,width=6)

### plot the not-at-issueness of the different projective content triggers

# calculate mean not-at-issueness for each trigger and relevel by mean
mean_nai = aggregate(ai~short_trigger, data=t.proj, FUN="mean")
mean_nai$YMin = mean_nai$ai - aggregate(ai~short_trigger, data=t.proj, FUN="ci.low")$ai
mean_nai$YMax = mean_nai$ai + aggregate(ai~short_trigger, data=t.proj, FUN="ci.high")$ai
mean_nai

# save mean not-at-issueness for comparison to experiment 5 / experiment 1b
mean_nai_Exp4 <- mean_nai
saveRDS(mean_nai_Exp4, file="data/mean_nai_Exp4.rds")

t.proj$trigger_ai <-factor(t.proj$short_trigger, levels=mean_nai[order(mean_nai$ai), "short_trigger"])

ggplot(t.proj, aes(x=trigger_ai, y=ai)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
#geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-not-at-issueness.pdf",height=3,width=6)

ggplot(t.proj, aes(x=trigger_ai, y=ai)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  ylab("Not-at-issueness ('asking whether')")+
  xlab("Expression")
ggsave(f="graphs/boxplot-not-at-issueness.pdf",height=3,width=6)

##### Correlation between not-at-issueness and projectivity #################

#### plot the correlation between not-at-issueness and projectivity of projective content triggers
#### based on the raw responses by each Turker to each item
AP <- lm(projective ~ ai + short_trigger, data = t.proj)
summary(AP)

ggplot(t.proj, aes(x=ai,y=projective,group=1)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  #geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  #geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  ylab("Projection response mean")+
  xlab("NAI response mean")
ggsave(file="graphs/correlation-raw-responses.pdf",width = 14, height = 6)

#### plot the correlation between mean NAI and mean projectivity for each projective content

# create data structure that codes mean nai response for each trigger (and its class)  
agr = aggregate(ai~short_trigger+trigger_class, data=t, FUN="mean")
agr$YMin = agr$ai - aggregate(ai~short_trigger+trigger_class, data=t, FUN="ci.low")$ai
agr$YMax = agr$ai + aggregate(ai~short_trigger+trigger_class, data=t, FUN="ci.high")$ai
agr

# create data structure that codes mean projection response for each trigger (and its class)
agrr = aggregate(projective~short_trigger+trigger_class, data=t, FUN="mean")
agrr$YMin = agrr$projective - aggregate(projective~short_trigger+trigger_class, data=t, FUN="ci.low")$projective
agrr$YMax = agrr$projective + aggregate(projective~short_trigger+trigger_class, data=t, FUN="ci.high")$projective
agrr

# make the row names of the agrr (projection) to be the trigger names (instead of numbers)
row.names(agrr) = agrr$short_trigger

# put the projective response column from the agrr structure into the agr structure
agr$projective = agrr$projective
agr

# record the YMin and YMax of the agrr structure in the agr structure
agr$pYMin = agrr$YMin
agr$pYMax = agrr$YMax

# now the agr structure (ai response stuff) also has the projective response stuff 
agr

# save data structure for use in comparison with Exp5/1b
mean_proj_NAI_Exp4 <- agr
saveRDS(mean_proj_NAI_Exp4, file="data/mean_proj_NAI_Exp4.rds")

# plot mean ai and projective responses by trigger class, label with trigger name
# includes main clauses
AP <- lm(projective ~ ai, data = mean_proj_NAI_Exp4)
summary(AP)

ggplot(mean_proj_NAI_Exp4, aes(x=ai,y=projective)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                   "Intercept =",signif(AP$coef[[1]],5 ),
                   " Slope =",signif(AP$coef[[2]], 5),
                   " P =",signif(summary(AP)$coef[2,4], 5))) +
  ylab("Mean projectivity")+
  xlab("Mean not-at-issueness ('asking whether')")
ggsave(file="graphs/correlation-by-mean-including-MC.pdf",width = 14, height = 6)

# plot mean ai and projective responses by trigger class, label with trigger name
# excludes main clauses
AP2 <- lm(projective ~ ai, data = mean_proj_NAI_Exp4, trigger_class != "NonProj")
summary(AP2)

ggplot(subset(mean_proj_NAI_Exp4, trigger_class != "NonProj"), aes(x=ai,y=projective)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger, angle=0, hjust=0, vjust=1)) + 
  #geom_text(aes(label=short_trigger), vjust = 1, cex= 5,
            #position=position_jitter(h=.01,w=0.03))+
  labs(title = paste("Adj R2 = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  ylab("Mean projectivity")+
  xlab("Mean not-at-issueness ('asking whether')")
ggsave(file="graphs/correlation-by-mean.pdf",width = 8, height = 4)
