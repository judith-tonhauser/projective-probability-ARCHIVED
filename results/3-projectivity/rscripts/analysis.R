# JT starts here
setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/3-projectivity/')
source('rscripts/helpers.R')

# JD starts here
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
## JT
cd = read.csv("data/cd.csv")

# load veridicality means
vmeans = read.csv("../2-veridicality2/data/veridicality_means.csv")
colnames(vmeans) = c("verb","VeridicalityMean","VeridicalityCILow","VeridicalityCIHigh")

# load prior means
pmeans = read.csv("../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)

## JD
cd = read.csv("../data/cd.csv")

# load veridicality means
vmeans = read.csv("../../2-veridicality2/data/veridicality_means.csv")
colnames(vmeans) = c("verb","VeridicalityMean","VeridicalityCILow","VeridicalityCIHigh")
vmeans

# load prior means
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)
pmeans

### JD and JT start here

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
ggsave("../graphs/means-projectivity-by-predicate-veridicality-prior.pdf",height=5,width=6.5)

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

### PAIRWISE DIFFERENCES ###
library(lsmeans)
library(lme4)
str(t$response)
str(t$verb)
t$verb <- as.factor(t$verb)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
model = lmer(response ~ verb + (1|workerid), data=t, REML=F)
summary(model)

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                      estimate         SE   df t.ratio p.value
# acknowledge - admit        0.023280632 0.02707261 4807   0.860  1.0000
# acknowledge - announce     0.068260870 0.02707261 4807   2.521  0.5670
# acknowledge - be_annoyed  -0.144664032 0.02707261 4807  -5.344  <.0001
# acknowledge - be_right     0.262015810 0.02707261 4807   9.678  <.0001
# acknowledge - confess      0.055889328 0.02707261 4807   2.064  0.8748
# acknowledge - confirm      0.205335968 0.02707261 4807   7.585  <.0001
# acknowledge - demonstrate  0.117035573 0.02707261 4807   4.323  0.0026
# acknowledge - discover    -0.039525692 0.02707261 4807  -1.460  0.9965
# acknowledge - establish    0.200632411 0.02707261 4807   7.411  <.0001
# acknowledge - hear        -0.043438735 0.02707261 4807  -1.605  0.9891
# acknowledge - inform      -0.099051383 0.02707261 4807  -3.659  0.0343
# acknowledge - know        -0.132094862 0.02707261 4807  -4.879  0.0002
# acknowledge - pretend      0.286126482 0.02707261 4807  10.569  <.0001
# acknowledge - prove        0.219683794 0.02707261 4807   8.115  <.0001
# acknowledge - reveal       0.037944664 0.02707261 4807   1.402  0.9979
# acknowledge - say          0.240079051 0.02707261 4807   8.868  <.0001
# acknowledge - see         -0.079328063 0.02707261 4807  -2.930  0.2716
# acknowledge - suggest      0.241581028 0.02707261 4807   8.923  <.0001
# acknowledge - think        0.210395257 0.02707261 4807   7.772  <.0001
# admit - announce           0.044980237 0.02707261 4807   1.661  0.9839
# admit - be_annoyed        -0.167944664 0.02707261 4807  -6.203  <.0001
# admit - be_right           0.238735178 0.02707261 4807   8.818  <.0001
# admit - confess            0.032608696 0.02707261 4807   1.204  0.9997
# admit - confirm            0.182055336 0.02707261 4807   6.725  <.0001
# admit - demonstrate        0.093754941 0.02707261 4807   3.463  0.0651
# admit - discover          -0.062806324 0.02707261 4807  -2.320  0.7206
# admit - establish          0.177351779 0.02707261 4807   6.551  <.0001
# admit - hear              -0.066719368 0.02707261 4807  -2.464  0.6118
# admit - inform            -0.122332016 0.02707261 4807  -4.519  0.0011
# admit - know              -0.155375494 0.02707261 4807  -5.739  <.0001
# admit - pretend            0.262845850 0.02707261 4807   9.709  <.0001
# admit - prove              0.196403162 0.02707261 4807   7.255  <.0001
# admit - reveal             0.014664032 0.02707261 4807   0.542  1.0000
# admit - say                0.216798419 0.02707261 4807   8.008  <.0001
# admit - see               -0.102608696 0.02707261 4807  -3.790  0.0216
# admit - suggest            0.218300395 0.02707261 4807   8.064  <.0001
# admit - think              0.187114625 0.02707261 4807   6.912  <.0001
# announce - be_annoyed     -0.212924901 0.02707261 4807  -7.865  <.0001
# announce - be_right        0.193754941 0.02707261 4807   7.157  <.0001
# announce - confess        -0.012371542 0.02707261 4807  -0.457  1.0000
# announce - confirm         0.137075099 0.02707261 4807   5.063  0.0001
# announce - demonstrate     0.048774704 0.02707261 4807   1.802  0.9625
# announce - discover       -0.107786561 0.02707261 4807  -3.981  0.0105
# announce - establish       0.132371542 0.02707261 4807   4.889  0.0002
# announce - hear           -0.111699605 0.02707261 4807  -4.126  0.0059
# announce - inform         -0.167312253 0.02707261 4807  -6.180  <.0001
# announce - know           -0.200355731 0.02707261 4807  -7.401  <.0001
# announce - pretend         0.217865613 0.02707261 4807   8.047  <.0001
# announce - prove           0.151422925 0.02707261 4807   5.593  <.0001
# announce - reveal         -0.030316206 0.02707261 4807  -1.120  0.9999
# announce - say             0.171818182 0.02707261 4807   6.347  <.0001
# announce - see            -0.147588933 0.02707261 4807  -5.452  <.0001
# announce - suggest         0.173320158 0.02707261 4807   6.402  <.0001
# announce - think           0.142134387 0.02707261 4807   5.250  <.0001
# be_annoyed - be_right      0.406679842 0.02707261 4807  15.022  <.0001
# be_annoyed - confess       0.200553360 0.02707261 4807   7.408  <.0001
# be_annoyed - confirm       0.350000000 0.02707261 4807  12.928  <.0001
# be_annoyed - demonstrate   0.261699605 0.02707261 4807   9.667  <.0001
# be_annoyed - discover      0.105138340 0.02707261 4807   3.884  0.0153
# be_annoyed - establish     0.345296443 0.02707261 4807  12.754  <.0001
# be_annoyed - hear          0.101225296 0.02707261 4807   3.739  0.0259
# be_annoyed - inform        0.045612648 0.02707261 4807   1.685  0.9812
# be_annoyed - know          0.012569170 0.02707261 4807   0.464  1.0000
# be_annoyed - pretend       0.430790514 0.02707261 4807  15.912  <.0001
# be_annoyed - prove         0.364347826 0.02707261 4807  13.458  <.0001
# be_annoyed - reveal        0.182608696 0.02707261 4807   6.745  <.0001
# be_annoyed - say           0.384743083 0.02707261 4807  14.212  <.0001
# be_annoyed - see           0.065335968 0.02707261 4807   2.413  0.6513
# be_annoyed - suggest       0.386245059 0.02707261 4807  14.267  <.0001
# be_annoyed - think         0.355059289 0.02707261 4807  13.115  <.0001
# be_right - confess        -0.206126482 0.02707261 4807  -7.614  <.0001
# be_right - confirm        -0.056679842 0.02707261 4807  -2.094  0.8605
# be_right - demonstrate    -0.144980237 0.02707261 4807  -5.355  <.0001
# be_right - discover       -0.301541502 0.02707261 4807 -11.138  <.0001
# be_right - establish      -0.061383399 0.02707261 4807  -2.267  0.7572
# be_right - hear           -0.305454545 0.02707261 4807 -11.283  <.0001
# be_right - inform         -0.361067194 0.02707261 4807 -13.337  <.0001
# be_right - know           -0.394110672 0.02707261 4807 -14.558  <.0001
# be_right - pretend         0.024110672 0.02707261 4807   0.891  1.0000
# be_right - prove          -0.042332016 0.02707261 4807  -1.564  0.9919
# be_right - reveal         -0.224071146 0.02707261 4807  -8.277  <.0001
# be_right - say            -0.021936759 0.02707261 4807  -0.810  1.0000
# be_right - see            -0.341343874 0.02707261 4807 -12.608  <.0001
# be_right - suggest        -0.020434783 0.02707261 4807  -0.755  1.0000
# be_right - think          -0.051620553 0.02707261 4807  -1.907  0.9359
# confess - confirm          0.149446640 0.02707261 4807   5.520  <.0001
# confess - demonstrate      0.061146245 0.02707261 4807   2.259  0.7631
# confess - discover        -0.095415020 0.02707261 4807  -3.524  0.0536
# confess - establish        0.144743083 0.02707261 4807   5.346  <.0001
# confess - hear            -0.099328063 0.02707261 4807  -3.669  0.0331
# confess - inform          -0.154940711 0.02707261 4807  -5.723  <.0001
# confess - know            -0.187984190 0.02707261 4807  -6.944  <.0001
# confess - pretend          0.230237154 0.02707261 4807   8.504  <.0001
# confess - prove            0.163794466 0.02707261 4807   6.050  <.0001
# confess - reveal          -0.017944664 0.02707261 4807  -0.663  1.0000
# confess - say              0.184189723 0.02707261 4807   6.804  <.0001
# confess - see             -0.135217391 0.02707261 4807  -4.995  0.0001
# confess - suggest          0.185691700 0.02707261 4807   6.859  <.0001
# confess - think            0.154505929 0.02707261 4807   5.707  <.0001
# confirm - demonstrate     -0.088300395 0.02707261 4807  -3.262  0.1183
# confirm - discover        -0.244861660 0.02707261 4807  -9.045  <.0001
# confirm - establish       -0.004703557 0.02707261 4807  -0.174  1.0000
# confirm - hear            -0.248774704 0.02707261 4807  -9.189  <.0001
# confirm - inform          -0.304387352 0.02707261 4807 -11.243  <.0001
# confirm - know            -0.337430830 0.02707261 4807 -12.464  <.0001
# confirm - pretend          0.080790514 0.02707261 4807   2.984  0.2404
# confirm - prove            0.014347826 0.02707261 4807   0.530  1.0000
# confirm - reveal          -0.167391304 0.02707261 4807  -6.183  <.0001
# confirm - say              0.034743083 0.02707261 4807   1.283  0.9994
# confirm - see             -0.284664032 0.02707261 4807 -10.515  <.0001
# confirm - suggest          0.036245059 0.02707261 4807   1.339  0.9989
# confirm - think            0.005059289 0.02707261 4807   0.187  1.0000
# demonstrate - discover    -0.156561265 0.02707261 4807  -5.783  <.0001
# demonstrate - establish    0.083596838 0.02707261 4807   3.088  0.1875
# demonstrate - hear        -0.160474308 0.02707261 4807  -5.928  <.0001
# demonstrate - inform      -0.216086957 0.02707261 4807  -7.982  <.0001
# demonstrate - know        -0.249130435 0.02707261 4807  -9.202  <.0001
# demonstrate - pretend      0.169090909 0.02707261 4807   6.246  <.0001
# demonstrate - prove        0.102648221 0.02707261 4807   3.792  0.0215
# demonstrate - reveal      -0.079090909 0.02707261 4807  -2.921  0.2768
# demonstrate - say          0.123043478 0.02707261 4807   4.545  0.0010
# demonstrate - see         -0.196363636 0.02707261 4807  -7.253  <.0001
# demonstrate - suggest      0.124545455 0.02707261 4807   4.600  0.0008
# demonstrate - think        0.093359684 0.02707261 4807   3.448  0.0681
# discover - establish       0.240158103 0.02707261 4807   8.871  <.0001
# discover - hear           -0.003913043 0.02707261 4807  -0.145  1.0000
# discover - inform         -0.059525692 0.02707261 4807  -2.199  0.8015
# discover - know           -0.092569170 0.02707261 4807  -3.419  0.0746
# discover - pretend         0.325652174 0.02707261 4807  12.029  <.0001
# discover - prove           0.259209486 0.02707261 4807   9.575  <.0001
# discover - reveal          0.077470356 0.02707261 4807   2.862  0.3144
# discover - say             0.279604743 0.02707261 4807  10.328  <.0001
# discover - see            -0.039802372 0.02707261 4807  -1.470  0.9962
# discover - suggest         0.281106719 0.02707261 4807  10.383  <.0001
# discover - think           0.249920949 0.02707261 4807   9.232  <.0001
# establish - hear          -0.244071146 0.02707261 4807  -9.015  <.0001
# establish - inform        -0.299683794 0.02707261 4807 -11.070  <.0001
# establish - know          -0.332727273 0.02707261 4807 -12.290  <.0001
# establish - pretend        0.085494071 0.02707261 4807   3.158  0.1567
# establish - prove          0.019051383 0.02707261 4807   0.704  1.0000
# establish - reveal        -0.162687747 0.02707261 4807  -6.009  <.0001
# establish - say            0.039446640 0.02707261 4807   1.457  0.9966
# establish - see           -0.279960474 0.02707261 4807 -10.341  <.0001
# establish - suggest        0.040948617 0.02707261 4807   1.513  0.9946
# establish - think          0.009762846 0.02707261 4807   0.361  1.0000
# hear - inform             -0.055612648 0.02707261 4807  -2.054  0.8796
# hear - know               -0.088656126 0.02707261 4807  -3.275  0.1141
# hear - pretend             0.329565217 0.02707261 4807  12.173  <.0001
# hear - prove               0.263122530 0.02707261 4807   9.719  <.0001
# hear - reveal              0.081383399 0.02707261 4807   3.006  0.2285
# hear - say                 0.283517787 0.02707261 4807  10.472  <.0001
# hear - see                -0.035889328 0.02707261 4807  -1.326  0.9990
# hear - suggest             0.285019763 0.02707261 4807  10.528  <.0001
# hear - think               0.253833992 0.02707261 4807   9.376  <.0001
# inform - know             -0.033043478 0.02707261 4807  -1.221  0.9997
# inform - pretend           0.385177866 0.02707261 4807  14.228  <.0001
# inform - prove             0.318735178 0.02707261 4807  11.773  <.0001
# inform - reveal            0.136996047 0.02707261 4807   5.060  0.0001
# inform - say               0.339130435 0.02707261 4807  12.527  <.0001
# inform - see               0.019723320 0.02707261 4807   0.729  1.0000
# inform - suggest           0.340632411 0.02707261 4807  12.582  <.0001
# inform - think             0.309446640 0.02707261 4807  11.430  <.0001
# know - pretend             0.418221344 0.02707261 4807  15.448  <.0001
# know - prove               0.351778656 0.02707261 4807  12.994  <.0001
# know - reveal              0.170039526 0.02707261 4807   6.281  <.0001
# know - say                 0.372173913 0.02707261 4807  13.747  <.0001
# know - see                 0.052766798 0.02707261 4807   1.949  0.9221
# know - suggest             0.373675889 0.02707261 4807  13.803  <.0001
# know - think               0.342490119 0.02707261 4807  12.651  <.0001
# pretend - prove           -0.066442688 0.02707261 4807  -2.454  0.6198
# pretend - reveal          -0.248181818 0.02707261 4807  -9.167  <.0001
# pretend - say             -0.046047431 0.02707261 4807  -1.701  0.9792
# pretend - see             -0.365454545 0.02707261 4807 -13.499  <.0001
# pretend - suggest         -0.044545455 0.02707261 4807  -1.645  0.9855
# pretend - think           -0.075731225 0.02707261 4807  -2.797  0.3577
# prove - reveal            -0.181739130 0.02707261 4807  -6.713  <.0001
# prove - say                0.020395257 0.02707261 4807   0.753  1.0000
# prove - see               -0.299011858 0.02707261 4807 -11.045  <.0001
# prove - suggest            0.021897233 0.02707261 4807   0.809  1.0000
# prove - think             -0.009288538 0.02707261 4807  -0.343  1.0000
# reveal - say               0.202134387 0.02707261 4807   7.466  <.0001
# reveal - see              -0.117272727 0.02707261 4807  -4.332  0.0025
# reveal - suggest           0.203636364 0.02707261 4807   7.522  <.0001
# reveal - think             0.172450593 0.02707261 4807   6.370  <.0001
# say - see                 -0.319407115 0.02707261 4807 -11.798  <.0001
# say - suggest              0.001501976 0.02707261 4807   0.055  1.0000
# say - think               -0.029683794 0.02707261 4807  -1.096  0.9999
# see - suggest              0.320909091 0.02707261 4807  11.854  <.0001
# see - think                0.289723320 0.02707261 4807  10.702  <.0001
# suggest - think           -0.031185771 0.02707261 4807  -1.152  0.9999
# 
# P value adjustment: tukey method for comparing a family of 20 estimates

### MIXED EFFECTS ANALYSIS ###
library(lme4)
library(languageR)
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

### now lets look at projective contents only 
p = droplevels(subset(t,t$verb != "pretend" & t$verb != "be_right" & t$verb != "suggest" & t$verb != "say" & t$verb != "prove" & t$verb != "think" & t$verb != "confirm" & t$verb != "establish" & t$verb != "demonstrate"))
nrow(p) #2783 / 11 projective predicates = 253 Turkers
table(p$verb)
str(p$verb)
str(p$response)
table(p$content)

cp = cbind(p, myCenter(p[,c("fact_type","VeridicalityMean","PriorMean")]))
contrasts(cp$fact_type)
head(cp)

# analysis with categorical fact type
m = lmer(response ~ cfact_type * cVeridicalityMean + (1+cfact_type|workerid) + (1+cVeridicalityMean|content) + (1+cfact_type|verb),data=cp)
summary(m) #main effect of prior, but not of veridicality or interaction

# predict projectivity from veridicality of verb, continuous prior probability of eventuality
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cfact_type|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb),data=cp)
summary(m) #main effect of prior, but not of veridicality or interaction


