# setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/3-projectivity/')
# source('rscripts/helpers.R')
source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(forcats)
theme_set(theme_bw())

## DON'T RUN THIS BIT IF YOU ONLY WANT TO LOAD CLEANED DATA -- SEARCH FOR "END" ##
# load raw data
d = read.csv("../experiment.csv")
nrow(d) #7800 = 300 participants x 26 items
names(d)
length(unique(d$workerid)) #300 participants

mean(d$Answer.time_in_minutes) #7.1
median(d$Answer.time_in_minutes) #6

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,fact,fact_type,contentNr,trigger_class,response,slide_number_in_experiment,age,language,assess,american,gender,comments,Answer.time_in_minutes)
nrow(d) #7800

# look at Turkers' comments
unique(d$comments)

# look at whether Turkers thought they understood the task
table(d$assess)

# age and gender info
length(which(is.na(d$age))) #0 missing values
table(d$age) #21-72
median(d$age,na.rm=TRUE) #36
table(d$gender)
#145 female, 154 male

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "Russian" & d$language != "Ukrainian" & d$language != "Arabic" & d$language != "chinese" & d$language != "hungarian" & d$language != "spanish"))
d = droplevels(d)
length(unique(d$workerid)) #294 (6 Turkers excluded)

table(d$gender)
length(which(is.na(d$american))) #78 (3 people didn't respond)
table(d$american) 
# coding error in HTML file: m=yes, f=no
d <- subset(d, d$american == "m")
d = droplevels(d)
length(unique(d$workerid)) #279 (15 Turkers excluded, 21 excluded in total for language reasons)

## exclude Turkers based on fillers
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1674 / 6 controls = 279 Turkers

# group mean on controls
round(mean(c$response),2) #.21

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")
ggsave(f="graphs/raw-responses-to-controls.pdf",height=4,width=6.5)

# group means on individual controls
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr  response
# 1  control1 0.1883154
# 2  control2 0.1845878
# 3  control3 0.1760932
# 4  control4 0.2450538
# 5  control5 0.1895341
# 6  control6 0.2565950

# Turkers with response means on controls more than 1.5sd above group mean
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 1.5*sd(c.means$response)),]
c.g
unique(length(c.g$workerid)) #26 Turkers gave high responses
mean(c.g$response) #.6

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid)
outliers = droplevels(outliers)
nrow(outliers) #156 / 6 control items = 26 Turkers
table(outliers$response)

# look at the responses to the controls that these "outlier" Turkers did

ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="graphs/raw-responses-to-controls-by-outliers.pdf",height=6,width=10)

# mean response by outliers
o.means = aggregate(response~workerid, data=outliers, FUN="mean")
o.means$YMin = o.means$response - aggregate(response~workerid, data=outliers, FUN="ci.low")$response
o.means$YMax = o.means$response + aggregate(response~workerid, data=outliers, FUN="ci.high")$response
o.means

ggplot(o.means, aes(x=workerid,y=response)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="graphs/mean-responses-to-controls-by-outliers.pdf",height=6,width=10)

# responses here are supposed to be low but these Turkers
# gave consistently high responses across the control items

# exclude the 11 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #253 Turkers remain (279 - 26)

# clean data = cd
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6578 / 20 items = 253 participants

## END -- START RUNNING AFTER THIS TO LOAD ONLY CLEANED DATA ##

cd = read.csv("../data/cd.csv")

# load veridicality means
vmeans = read.csv("../../2-veridicality2/data/veridicality_means.csv")
colnames(vmeans) = c("verb","VeridicalityMean","VeridicalityCILow","VeridicalityCIHigh")

# load prior means
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)

# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# merge veridicality means into cd

cd = left_join(cd,vmeans,by=c("verb"))
cd = left_join(cd,pmeans,by=c("fact"))

# age info
table(cd$age) #21-72
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #37
table(cd$gender)
#124 female, 128 male

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control")
t <- droplevels(t)
nrow(t) #5060 / 20 items = 253 Turkers
table(t$verb,t$content)

names(t)
table(t$trigger_class)

# mean projectivity of the predicates by verb and fact_type
means = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))
# means = as.data.frame(means)

t$Verb <-factor(t$verb, levels=levels(means$Verb))

ggplot(t, aes(x=Verb, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Certainty rating")+
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity.pdf",height=4,width=6.5)

means = t %>%
  group_by(verb, fact_type, VeridicalityMean) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means

ggplot(means, aes(x=Verb, y=Mean, color=fact_type)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity-by-predicate-and-facttype.pdf",height=4,width=6.5)

means = means %>%
  mutate(Verb = fct_reorder(verb,VeridicalityMean))
means

ggplot(means, aes(x=Verb, y=Mean, color=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point(aes(shape = fact_type)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")
ggsave("../graphs/boxplot-projectivity-by-predicate-and-facttype-and veridicality.pdf",height=4,width=6.5)


means = t %>%
  group_by(verb, fact_type, content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
means

ggplot(means, aes(x=Verb, y=Mean, color=content)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=content),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  facet_wrap(~fact_type)
ggsave("../graphs/boxplot-projectivity-by-predicate-and-content.pdf",height=4,width=6.5)

means = t %>%
  group_by(verb, fact_type, content, PriorMean) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(verb,Mean))
means

ggplot(means, aes(x=PriorMean, y=Mean, color=verb)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=verb),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 
ggsave("../graphs/means-projectivity-by-predicate-content-prior.pdf",height=5,width=6.5)

means = t %>%
  mutate(VeridicalityGroup = cut_number(VeridicalityMean,3,labels=c("low","mid","high"))) %>%
  group_by(verb, fact_type, content, PriorMean, VeridicalityGroup) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
means

ggplot(means, aes(x=PriorMean, y=Mean, color=VeridicalityGroup)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  # geom_line(aes(group=content)) +
  geom_smooth(aes(group=VeridicalityGroup),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 

means = t %>%
  group_by(PriorMean,fact_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
means

ggplot(means, aes(x=PriorMean, y=Mean, color=fact_type)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  geom_smooth(method="lm") +
  # geom_line(aes(group=content)) +
  # geom_smooth(aes(group=VeridicalityGroup),method="lm",se=F) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  xlim(0.05,.9) +
  ylim(0.05,.9) +
  ylab("Mean certainty rating") +
  xlab("Prior event probability") 
ggsave("../graphs/means-projectivity-by-prior.pdf",height=4,width=5.5)

### MIXED EFFECTS ANALYSIS ###
library(lme4)
centered = cbind(t, myCenter(t[,c("fact_type","VeridicalityMean","PriorMean")]))
contrasts(t$fact_type)

# analysis with categorical fact type
m = lmer(response ~ cfact_type * cVeridicalityMean + (1+cfact_type|workerid) + (1+cVeridicalityMean|content) + (1+cfact_type|verb),data=centered)
summary(m) # main effects of fact type, but no veridicality effect nor interaction (with this random effects structure -- without random verb effects, there's a veridicality effect)
table(t$fact_type, t$verb)

ranef(m)

# analysis with continuous prior probability of eventuality
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb),data=centered)
summary(m) 


