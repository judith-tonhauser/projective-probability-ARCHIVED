setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/2-veridicality/')
source('rscripts/helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
theme_set(theme_bw())

# load raw data
d = read.csv("experiment.csv")
nrow(d) #8400 = 300 participants x 28 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #5.6
median(d$Answer.time_in_minutes) #4.7

d = d %>%
  dplyr::select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# age and gender info
length(which(is.na(d$age))) #56 missing values, i.e., 2 Turkers didn't provide information
table(d$age) #19-73
median(d$age,na.rm=TRUE) #35 (of the 298 Turkers that provide age information)
table(d$gender)
#127 female, 169 male, 2 other, 2 didn't provide info

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "Chinese " & d$language != "Russian" & d$language != "Russian " & d$language != "Telugu"))
d = droplevels(d)
length(unique(d$workerid)) #296 (4 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #286 (10 Turkers excluded, 14 excluded in total for language reasons)

## exclude Turkers based on fillers
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: contradictory controls (YES = 1)
c.bad <- subset(d, d$verb == "control_bad")
c.bad <- droplevels(c.bad)
nrow(c.bad) #1144 / 4 contradictory controls = 286 Turkers

# control_good: noncontradictory controls (NO = 0)
c.good <- subset(d, d$verb == "control_good")
c.good <- droplevels(c.good)
nrow(c.good) #1144

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288

# group mean on contradictory controls
round(mean(c.bad$response),2) #.92

# group mean on noncontradictory controls
round(mean(c.good$response),2) #.15

ggplot(c.bad, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")

ggplot(c.good, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.good$response, n = 10)) +
  ylab("Response") +
  xlab("Participant")

# group means on individual controls
means = aggregate(response ~ contentNr, data=c.bad, FUN="mean")
means
# contentNr  response
# 1 control_bad1 0.9031119
# 2 control_bad2 0.9202448
# 3 control_bad3 0.9134615
# 4 control_bad4 0.9456294

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
# contentNr   response
# 1 control_good1 0.36003497 "Zack believes that I'm married, but I'm actually single"
# 2 control_good2 0.06311189
# 3 control_good3 0.07227273
# 4 control_good4 0.09209790

# as also suggested in the comments, some participants weren't clear on how to interpret "contradictory"
# and may have assumed that the speaker was contradicting Zack in control_good1

#################################################################
##### considering only 3 of the 4 noncontradictory controls #####
##### first analysis, see footnote in paper
c.good2 <- subset(d, d$contentNr == "control_good2" | d$contentNr == "control_good3" | d$contentNr == "control_good4")
c.good2 <- droplevels(c.good2)
nrow(c.good2) #858 / 286 participants = 3 noncontradictory controls
# group mean on 3 noncontradictory controls
round(mean(c.good$response),2) #.08
# if c.good2 is used instead of c.good
# 11 Turkers (instead of 6) gave too high responses
# the 11 Turkers mean response to the noncontradictory controls: .66
# 11 Turkers total excluded, instead of 15
#################################################################

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2288 / 8 items = 286

# Turkers with response means on noncontradictory controls (0) more than 3sd above group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response > (mean(cg.means$response) + 3*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #6 Turkers gave high responses
mean(c.g$response) #.76

# Turkers with response means on contradictory controls (1) more than 3sd below group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response < (mean(cb.means$response) - 3*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #7 Turkers gave low responses
mean(c.b$response) #.35

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #88 / 8 control items = 11 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to noncontradictory items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #24 / 4 control items = 6 Turkers
table(outliers.g$workerid)

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(f1$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be low (noncontradictory) but these 6 Turkers
# gave consistently high responses across the control items

# outliers to contradictory items (1)
outliers.b <- subset(c.bad, c.bad$workerid %in% c.b$workerid)
outliers.b = droplevels(outliers.b)
nrow(outliers.b) #28 / 4 control items = 7 Turkers
table(outliers.b$workerid)

ggplot(outliers.b, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(f1$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be high (contradictory) but these 7 Turkers
# gave consistently low responses across the control items 

# exclude the 11 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #275 Turkers remain (186 - 11)

# clean data = cd
cd = d
write.csv(cd, "data/cd.csv")
nrow(cd) #7700 / 28 items = 275 participants

# age info
table(cd$age) #19-73
length(which(is.na(cd$age))) #28 missing values
median(cd$age,na.rm=TRUE) #35 (of the 270 Turkers that provide age information)
table(cd$gender)
#119 female, 153 male, 2 other, 1 didn't provide info

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control_good" & cd$verb != "control_bad")
t <- droplevels(t)
nrow(t) #5500 / 20 = 275 Turkers
table(t$verb,t$content)

# mean veridicality of the verbs
means = aggregate(response~verb, data=t, FUN="mean")
means$YMin = means$response - aggregate(response~verb, data=t, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~verb, data=t, FUN="ci.high")$response
means

t$verb <-factor(t$verb, levels=means[order(means$response), "verb"])

ggplot(t, aes(x=verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave(f="graphs/boxplot-veridicality.pdf",height=4,width=6.5)

# veridicality rating by participant
means = aggregate(projective~short_trigger, data=t.proj, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~short_trigger, data=t.proj, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~short_trigger, data=t.proj, FUN="ci.high")$projective
mean_proj
t.proj$trigger_proj <-factor(t.proj$short_trigger, levels=mean_proj[order(mean_proj$projective), "short_trigger"])

variances = t %>%
  group_by(workerid) %>%
  summarise(VeriVar = var(response),
            VeriMean=mean(response),
            Veri.ci.low=ci.low(response),
            Veri.ci.high=ci.high(response))
variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,VeriMean),y=VeriMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=VeriMean-Veri.ci.low,ymax=VeriMean+Veri.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean veridicality rating")
ggsave("graphs/veridicality-subjmeans.pdf",height=3,width=6.5)

## pairwise comparison to see which predicates differ from one another
library(lsmeans)
library(lme4)
str(t$response)
str(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeansLT(model, pairwise~verb)
comparison

