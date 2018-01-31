# JT starts here
# setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/3-projectivity/')
#source('rscripts/helpers.R')

# JD starts here
source('helpers.R')

# load required packages
library(tidyverse)
library(dichromat)
library(forcats)
library(RColorBrewer)
theme_set(theme_bw())

## NO NEED TO RUN THIS FIRST BIT IF YOU JUST WANT TO LOAD CLEAN DATA. 
## SEARCH FOR "load clean data for analysis"

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
d <- subset(d, (d$language != "" & d$language != "Russian" & d$language != "Ukrainian" & d$language != "Arabic" & d$language != "chinese" & d$language != "hungarian" & d$language != "spanish"))
d = droplevels(d)
length(unique(d$workerid)) #292 (8 Turkers excluded)

table(d$gender)
length(which(is.na(d$american))) #78 (3 people didn't respond)
table(d$american) 
# coding error in HTML file: m=yes, f=no
d <- subset(d, d$american == "m")
d = droplevels(d)
length(unique(d$workerid)) #277 (15 Turkers excluded, 23 excluded in total for language reasons)

## exclude Turkers based on fillers
names(d)
table(d$contentNr)
table(d$verb)

# make control data subset
c <- subset(d, d$verb == "control")
c <- droplevels(c)
nrow(c) #1662 / 6 controls = 277 Turkers

# group mean on controls
round(mean(c$response),2) #.21

ggplot(c, aes(x=workerid,y=response)) +
  geom_point(aes(colour = content),position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(c.bad$response, n = 10)) +
  ylab("Responses") +
  xlab("Participant")
ggsave(f="../graphs/raw-responses-to-controls.pdf",height=4,width=6.5)

# group means on individual controls
means = aggregate(response ~ contentNr, data=c, FUN="mean")
means
# contentNr  response
# 1  control1 0.1877256
# 2  control2 0.1822744
# 3  control3 0.1749458
# 4  control4 0.2440794
# 5  control5 0.1886282
# 6  control6 0.2560289

# Turkers with response means on controls more than 1.5sd above group mean
c.means = aggregate(response~workerid, data=c, FUN="mean")
c.means$YMin = c.means$response - aggregate(response~workerid, data=c, FUN="ci.low")$response
c.means$YMax = c.means$response + aggregate(response~workerid, data=c, FUN="ci.high")$response
c.means

c.g <- c.means[c.means$response > (mean(c.means$response) + 1.5*sd(c.means$response)),]
c.g
unique(length(c.g$workerid)) #25 Turkers gave high responses
mean(c.g$response) #.6

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid)
outliers = droplevels(outliers)
nrow(outliers) #150 / 6 control items = 25 Turkers
table(outliers$response)

# look at the responses to the controls that these "outlier" Turkers did

ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
ggsave(f="../graphs/raw-responses-to-controls-by-outliers.pdf",height=6,width=10)

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

# exclude the 25 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #252 Turkers remain (277 - 25)

# clean data
cd = d
write.csv(cd, "../data/cd.csv")
nrow(cd) #6552 / 26 items = 252 participants

# load clean clean data for analysis
cd = read.csv("../data/cd.csv")
nrow(cd) #6552

# load contradictoriness means
vmeans = read.csv("../../2-veridicality2/data/veridicality_means.csv")
colnames(vmeans) = c("verb","VeridicalityMean","VeridicalityCILow","VeridicalityCIHigh")
vmeans

# load prior means
pmeans = read.csv("../../1-prior/data/prior_means.csv")
pmeans$fact = gsub(".","",as.character(pmeans$fact),fixed=T)
pmeans

# change cd verb names to match veridicality names
cd = cd %>%
  mutate(verb=recode(verb, annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# merge contradictoriness means into cd
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
nrow(t) #5040 / 20 items = 252 Turkers
table(t$verb,t$content)

names(t)
table(t$trigger_class)

# mean projectivity of the predicates by verb and fact_type
means = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(as.factor(verb),Mean))

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

# SALT paper plot:
means = t %>%
  group_by(verb, fact_type, VeridicalityMean) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))
View(means)

cols = data.frame(V=levels(means$Verb))
cols$VeridicalityGroup = as.factor(ifelse(cols$V %in% c("be_annoyed", "know", "discover", "reveal", "see", "establish", "be_right"), "E", ifelse(cols$V %in% c("pretend", "think", "suggest", "say", "hear"), "NE", "V")))
#cols$Colors =  ifelse(cols$VeridicalityGroup == "E", brewer.pal(3,"Paired")[2], ifelse(cols$VeridicalityGroup == "NE", brewer.pal(3,"Paired")[1],brewer.pal(3,"Paired")[3]))
cols$Colors =  ifelse(cols$VeridicalityGroup == "E", "blue", 
                      ifelse(cols$VeridicalityGroup == "NE", "brown", "black"))


ggplot(means, aes(x=Verb, y=Mean, color=fact_type))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  theme(legend.position="top") +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/means-projectivity-by-predicate-and-facttype.pdf",height=4,width=7)

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
# acknowledge - admit        0.022936508 0.02715554 4788   0.845  1.0000
# acknowledge - announce     0.067857143 0.02715554 4788   2.499  0.5848
# acknowledge - be_annoyed  -0.144642857 0.02715554 4788  -5.326  <.0001
# acknowledge - be_right     0.262500000 0.02715554 4788   9.667  <.0001
# acknowledge - confess      0.054126984 0.02715554 4788   1.993  0.9057
# acknowledge - confirm      0.203888889 0.02715554 4788   7.508  <.0001
# acknowledge - demonstrate  0.115753968 0.02715554 4788   4.263  0.0034
# acknowledge - discover    -0.040158730 0.02715554 4788  -1.479  0.9959
# acknowledge - establish    0.199126984 0.02715554 4788   7.333  <.0001
# acknowledge - hear        -0.044126984 0.02715554 4788  -1.625  0.9874
# acknowledge - inform      -0.099801587 0.02715554 4788  -3.675  0.0324
# acknowledge - know        -0.134841270 0.02715554 4788  -4.966  0.0001
# acknowledge - pretend      0.285158730 0.02715554 4788  10.501  <.0001
# acknowledge - prove        0.218769841 0.02715554 4788   8.056  <.0001
# acknowledge - reveal       0.036031746 0.02715554 4788   1.327  0.9990
# acknowledge - say          0.238849206 0.02715554 4788   8.796  <.0001
# acknowledge - see         -0.078849206 0.02715554 4788  -2.904  0.2877
# acknowledge - suggest      0.240634921 0.02715554 4788   8.861  <.0001
# acknowledge - think        0.208928571 0.02715554 4788   7.694  <.0001
# admit - announce           0.044920635 0.02715554 4788   1.654  0.9846
# admit - be_annoyed        -0.167579365 0.02715554 4788  -6.171  <.0001
# admit - be_right           0.239563492 0.02715554 4788   8.822  <.0001
# admit - confess            0.031190476 0.02715554 4788   1.149  0.9999
# admit - confirm            0.180952381 0.02715554 4788   6.664  <.0001
# admit - demonstrate        0.092817460 0.02715554 4788   3.418  0.0749
# admit - discover          -0.063095238 0.02715554 4788  -2.323  0.7181
# admit - establish          0.176190476 0.02715554 4788   6.488  <.0001
# admit - hear              -0.067063492 0.02715554 4788  -2.470  0.6078
# admit - inform            -0.122738095 0.02715554 4788  -4.520  0.0011
# admit - know              -0.157777778 0.02715554 4788  -5.810  <.0001
# admit - pretend            0.262222222 0.02715554 4788   9.656  <.0001
# admit - prove              0.195833333 0.02715554 4788   7.212  <.0001
# admit - reveal             0.013095238 0.02715554 4788   0.482  1.0000
# admit - say                0.215912698 0.02715554 4788   7.951  <.0001
# admit - see               -0.101785714 0.02715554 4788  -3.748  0.0251
# admit - suggest            0.217698413 0.02715554 4788   8.017  <.0001
# admit - think              0.185992063 0.02715554 4788   6.849  <.0001
# announce - be_annoyed     -0.212500000 0.02715554 4788  -7.825  <.0001
# announce - be_right        0.194642857 0.02715554 4788   7.168  <.0001
# announce - confess        -0.013730159 0.02715554 4788  -0.506  1.0000
# announce - confirm         0.136031746 0.02715554 4788   5.009  0.0001
# announce - demonstrate     0.047896825 0.02715554 4788   1.764  0.9697
# announce - discover       -0.108015873 0.02715554 4788  -3.978  0.0107
# announce - establish       0.131269841 0.02715554 4788   4.834  0.0002
# announce - hear           -0.111984127 0.02715554 4788  -4.124  0.0060
# announce - inform         -0.167658730 0.02715554 4788  -6.174  <.0001
# announce - know           -0.202698413 0.02715554 4788  -7.464  <.0001
# announce - pretend         0.217301587 0.02715554 4788   8.002  <.0001
# announce - prove           0.150912698 0.02715554 4788   5.557  <.0001
# announce - reveal         -0.031825397 0.02715554 4788  -1.172  0.9998
# announce - say             0.170992063 0.02715554 4788   6.297  <.0001
# announce - see            -0.146706349 0.02715554 4788  -5.402  <.0001
# announce - suggest         0.172777778 0.02715554 4788   6.363  <.0001
# announce - think           0.141071429 0.02715554 4788   5.195  <.0001
# be_annoyed - be_right      0.407142857 0.02715554 4788  14.993  <.0001
# be_annoyed - confess       0.198769841 0.02715554 4788   7.320  <.0001
# be_annoyed - confirm       0.348531746 0.02715554 4788  12.835  <.0001
# be_annoyed - demonstrate   0.260396825 0.02715554 4788   9.589  <.0001
# be_annoyed - discover      0.104484127 0.02715554 4788   3.848  0.0175
# be_annoyed - establish     0.343769841 0.02715554 4788  12.659  <.0001
# be_annoyed - hear          0.100515873 0.02715554 4788   3.701  0.0296
# be_annoyed - inform        0.044841270 0.02715554 4788   1.651  0.9849
# be_annoyed - know          0.009801587 0.02715554 4788   0.361  1.0000
# be_annoyed - pretend       0.429801587 0.02715554 4788  15.827  <.0001
# be_annoyed - prove         0.363412698 0.02715554 4788  13.383  <.0001
# be_annoyed - reveal        0.180674603 0.02715554 4788   6.653  <.0001
# be_annoyed - say           0.383492063 0.02715554 4788  14.122  <.0001
# be_annoyed - see           0.065793651 0.02715554 4788   2.423  0.6440
# be_annoyed - suggest       0.385277778 0.02715554 4788  14.188  <.0001
# be_annoyed - think         0.353571429 0.02715554 4788  13.020  <.0001
# be_right - confess        -0.208373016 0.02715554 4788  -7.673  <.0001
# be_right - confirm        -0.058611111 0.02715554 4788  -2.158  0.8255
# be_right - demonstrate    -0.146746032 0.02715554 4788  -5.404  <.0001
# be_right - discover       -0.302658730 0.02715554 4788 -11.145  <.0001
# be_right - establish      -0.063373016 0.02715554 4788  -2.334  0.7107
# be_right - hear           -0.306626984 0.02715554 4788 -11.292  <.0001
# be_right - inform         -0.362301587 0.02715554 4788 -13.342  <.0001
# be_right - know           -0.397341270 0.02715554 4788 -14.632  <.0001
# be_right - pretend         0.022658730 0.02715554 4788   0.834  1.0000
# be_right - prove          -0.043730159 0.02715554 4788  -1.610  0.9886
# be_right - reveal         -0.226468254 0.02715554 4788  -8.340  <.0001
# be_right - say            -0.023650794 0.02715554 4788  -0.871  1.0000
# be_right - see            -0.341349206 0.02715554 4788 -12.570  <.0001
# be_right - suggest        -0.021865079 0.02715554 4788  -0.805  1.0000
# be_right - think          -0.053571429 0.02715554 4788  -1.973  0.9136
# confess - confirm          0.149761905 0.02715554 4788   5.515  <.0001
# confess - demonstrate      0.061626984 0.02715554 4788   2.269  0.7558
# confess - discover        -0.094285714 0.02715554 4788  -3.472  0.0633
# confess - establish        0.145000000 0.02715554 4788   5.340  <.0001
# confess - hear            -0.098253968 0.02715554 4788  -3.618  0.0393
# confess - inform          -0.153928571 0.02715554 4788  -5.668  <.0001
# confess - know            -0.188968254 0.02715554 4788  -6.959  <.0001
# confess - pretend          0.231031746 0.02715554 4788   8.508  <.0001
# confess - prove            0.164642857 0.02715554 4788   6.063  <.0001
# confess - reveal          -0.018095238 0.02715554 4788  -0.666  1.0000
# confess - say              0.184722222 0.02715554 4788   6.802  <.0001
# confess - see             -0.132976190 0.02715554 4788  -4.897  0.0002
# confess - suggest          0.186507937 0.02715554 4788   6.868  <.0001
# confess - think            0.154801587 0.02715554 4788   5.701  <.0001
# confirm - demonstrate     -0.088134921 0.02715554 4788  -3.246  0.1238
# confirm - discover        -0.244047619 0.02715554 4788  -8.987  <.0001
# confirm - establish       -0.004761905 0.02715554 4788  -0.175  1.0000
# confirm - hear            -0.248015873 0.02715554 4788  -9.133  <.0001
# confirm - inform          -0.303690476 0.02715554 4788 -11.183  <.0001
# confirm - know            -0.338730159 0.02715554 4788 -12.474  <.0001
# confirm - pretend          0.081269841 0.02715554 4788   2.993  0.2357
# confirm - prove            0.014880952 0.02715554 4788   0.548  1.0000
# confirm - reveal          -0.167857143 0.02715554 4788  -6.181  <.0001
# confirm - say              0.034960317 0.02715554 4788   1.287  0.9993
# confirm - see             -0.282738095 0.02715554 4788 -10.412  <.0001
# confirm - suggest          0.036746032 0.02715554 4788   1.353  0.9987
# confirm - think            0.005039683 0.02715554 4788   0.186  1.0000
# demonstrate - discover    -0.155912698 0.02715554 4788  -5.741  <.0001
# demonstrate - establish    0.083373016 0.02715554 4788   3.070  0.1959
# demonstrate - hear        -0.159880952 0.02715554 4788  -5.888  <.0001
# demonstrate - inform      -0.215555556 0.02715554 4788  -7.938  <.0001
# demonstrate - know        -0.250595238 0.02715554 4788  -9.228  <.0001
# demonstrate - pretend      0.169404762 0.02715554 4788   6.238  <.0001
# demonstrate - prove        0.103015873 0.02715554 4788   3.794  0.0213
# demonstrate - reveal      -0.079722222 0.02715554 4788  -2.936  0.2682
# demonstrate - say          0.123095238 0.02715554 4788   4.533  0.0010
# demonstrate - see         -0.194603175 0.02715554 4788  -7.166  <.0001
# demonstrate - suggest      0.124880952 0.02715554 4788   4.599  0.0008
# demonstrate - think        0.093174603 0.02715554 4788   3.431  0.0719
# discover - establish       0.239285714 0.02715554 4788   8.812  <.0001
# discover - hear           -0.003968254 0.02715554 4788  -0.146  1.0000
# discover - inform         -0.059642857 0.02715554 4788  -2.196  0.8030
# discover - know           -0.094682540 0.02715554 4788  -3.487  0.0605
# discover - pretend         0.325317460 0.02715554 4788  11.980  <.0001
# discover - prove           0.258928571 0.02715554 4788   9.535  <.0001
# discover - reveal          0.076190476 0.02715554 4788   2.806  0.3519
# discover - say             0.279007937 0.02715554 4788  10.274  <.0001
# discover - see            -0.038690476 0.02715554 4788  -1.425  0.9974
# discover - suggest         0.280793651 0.02715554 4788  10.340  <.0001
# discover - think           0.249087302 0.02715554 4788   9.173  <.0001
# establish - hear          -0.243253968 0.02715554 4788  -8.958  <.0001
# establish - inform        -0.298928571 0.02715554 4788 -11.008  <.0001
# establish - know          -0.333968254 0.02715554 4788 -12.298  <.0001
# establish - pretend        0.086031746 0.02715554 4788   3.168  0.1526
# establish - prove          0.019642857 0.02715554 4788   0.723  1.0000
# establish - reveal        -0.163095238 0.02715554 4788  -6.006  <.0001
# establish - say            0.039722222 0.02715554 4788   1.463  0.9964
# establish - see           -0.277976190 0.02715554 4788 -10.236  <.0001
# establish - suggest        0.041507937 0.02715554 4788   1.529  0.9938
# establish - think          0.009801587 0.02715554 4788   0.361  1.0000
# hear - inform             -0.055674603 0.02715554 4788  -2.050  0.8814
# hear - know               -0.090714286 0.02715554 4788  -3.341  0.0944
# hear - pretend             0.329285714 0.02715554 4788  12.126  <.0001
# hear - prove               0.262896825 0.02715554 4788   9.681  <.0001
# hear - reveal              0.080158730 0.02715554 4788   2.952  0.2588
# hear - say                 0.282976190 0.02715554 4788  10.421  <.0001
# hear - see                -0.034722222 0.02715554 4788  -1.279  0.9994
# hear - suggest             0.284761905 0.02715554 4788  10.486  <.0001
# hear - think               0.253055556 0.02715554 4788   9.319  <.0001
# inform - know             -0.035039683 0.02715554 4788  -1.290  0.9993
# inform - pretend           0.384960317 0.02715554 4788  14.176  <.0001
# inform - prove             0.318571429 0.02715554 4788  11.731  <.0001
# inform - reveal            0.135833333 0.02715554 4788   5.002  0.0001
# inform - say               0.338650794 0.02715554 4788  12.471  <.0001
# inform - see               0.020952381 0.02715554 4788   0.772  1.0000
# inform - suggest           0.340436508 0.02715554 4788  12.537  <.0001
# inform - think             0.308730159 0.02715554 4788  11.369  <.0001
# know - pretend             0.420000000 0.02715554 4788  15.466  <.0001
# know - prove               0.353611111 0.02715554 4788  13.022  <.0001
# know - reveal              0.170873016 0.02715554 4788   6.292  <.0001
# know - say                 0.373690476 0.02715554 4788  13.761  <.0001
# know - see                 0.055992063 0.02715554 4788   2.062  0.8760
# know - suggest             0.375476190 0.02715554 4788  13.827  <.0001
# know - think               0.343769841 0.02715554 4788  12.659  <.0001
# pretend - prove           -0.066388889 0.02715554 4788  -2.445  0.6271
# pretend - reveal          -0.249126984 0.02715554 4788  -9.174  <.0001
# pretend - say             -0.046309524 0.02715554 4788  -1.705  0.9786
# pretend - see             -0.364007937 0.02715554 4788 -13.405  <.0001
# pretend - suggest         -0.044523810 0.02715554 4788  -1.640  0.9861
# pretend - think           -0.076230159 0.02715554 4788  -2.807  0.3509
# prove - reveal            -0.182738095 0.02715554 4788  -6.729  <.0001
# prove - say                0.020079365 0.02715554 4788   0.739  1.0000
# prove - see               -0.297619048 0.02715554 4788 -10.960  <.0001
# prove - suggest            0.021865079 0.02715554 4788   0.805  1.0000
# prove - think             -0.009841270 0.02715554 4788  -0.362  1.0000
# reveal - say               0.202817460 0.02715554 4788   7.469  <.0001
# reveal - see              -0.114880952 0.02715554 4788  -4.230  0.0039
# reveal - suggest           0.204603175 0.02715554 4788   7.534  <.0001
# reveal - think             0.172896825 0.02715554 4788   6.367  <.0001
# say - see                 -0.317698413 0.02715554 4788 -11.699  <.0001
# say - suggest              0.001785714 0.02715554 4788   0.066  1.0000
# say - think               -0.029920635 0.02715554 4788  -1.102  0.9999
# see - suggest              0.319484127 0.02715554 4788  11.765  <.0001
# see - think                0.287777778 0.02715554 4788  10.597  <.0001
# suggest - think           -0.031706349 0.02715554 4788  -1.168  0.9998
# 
# P value adjustment: tukey method for comparing a family of 20 estimates 

## EXPLORATORY SOCIO ANALYSIS (AGE, GENDER)
# age: age of participant
# gender: gender of participant
# speakerGender: gender of speaker of utterance
# subjectGender: gender of subject of attitude predicate

# certainty rating by predicate, fact and speaker gender
means = t %>%
  group_by(verb, fact_type, VeridicalityMean, speakerGender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, Verb = fct_reorder(verb,Mean))

cols = data.frame(V=levels(means$Verb))
cols$VeridicalityGroup = as.factor(ifelse(cols$V %in% c("be_annoyed", "know", "discover", "reveal", "see", "establish", "be_right"), "E", ifelse(cols$V %in% c("pretend", "think", "suggest", "say", "hear"), "NE", "V")))
#cols$Colors =  ifelse(cols$VeridicalityGroup == "E", brewer.pal(3,"Paired")[2], ifelse(cols$VeridicalityGroup == "NE", brewer.pal(3,"Paired")[1],brewer.pal(3,"Paired")[3]))
cols$Colors =  ifelse(cols$VeridicalityGroup == "E", "blue", 
                      ifelse(cols$VeridicalityGroup == "NE", "brown", "green"))

ggplot(means, aes(x=Verb, y=Mean, color=fact_type, shape=speakerGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/speakergender.pdf")

# certainty rating by fact and speaker gender and participant gender
means = t %>%
  group_by(fact_type, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=speakerGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_wrap(~gender) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/speakergender-collapsed.pdf")

# certainty rating by fact and subject gender and participant gender
means = t %>%
  group_by(fact_type, subjectGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=subjectGender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_wrap(~gender) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top")
ggsave("../graphs/subjectgender-collapsed.pdf")

# certainty rating by speaker gender, subject gender and participant gender
means = t %>%
  group_by(fact_type, subjectGender, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=fact_type, fill=gender))+#, alpha=VeridicalityMean)) + 
  #geom_point(color="black", size=4) +
  #geom_point(data=agr_subj, aes(color=content)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Fact type") +
  facet_grid(speakerGender~subjectGender) + 
  ggtitle("Rows: speaker gender -- Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectandspeakergender-collapsed.pdf")

# certainty rating by fact (continuous), speaker gender, subject gender and participant gender
means = t %>%
  group_by(PriorMean, subjectGender, speakerGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=PriorMean, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  geom_smooth(method="lm") +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
                     # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Prior probability of eventuality") +
  facet_grid(speakerGender~subjectGender) + 
  ggtitle("Rows: speaker gender -- Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectandspeakergender-collapsed-continuuous.pdf")

# certainty rating by subject gender and participant gender
means = t %>%
  group_by(PriorMean, subjectGender, gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=PriorMean, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  geom_smooth(method="lm") +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Prior probability of eventuality") +
  facet_wrap(~subjectGender) + 
  ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectgender-collapsed-continuuous.pdf")

# certainty rating by age (3 intervals), subject gender and participant gender
means = t %>%
  mutate(BinnedAge = cut_interval(age, n=3)) %>%
  group_by(fact_type, subjectGender, gender, BinnedAge) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=BinnedAge, color=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant age") +
  facet_grid(fact_type~subjectGender) + 
  ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/subjectgenderage-collapsed-continuuous.pdf")

# certainty rating by age (continous)
means = t %>%
  group_by(age) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) #%>%
  #filter(!is.na(gender)) %>%
  #droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=age))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  geom_smooth(method='lm') +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant age") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/age-continuuous-collapsed.pdf")

# certainty rating by participant gender
means = t %>%
  group_by(gender) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  filter(!is.na(gender)) %>%
  droplevels()
dodge = position_dodge(.9)

ggplot(means, aes(y=Mean, x=gender))+#, alpha=VeridicalityMean)) + 
  geom_point() +
  #geom_point(data=agr_subj, aes(color=content)) +
  # geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_color_manual(name="Prior probability\nof eventuality", breaks=c("factH","factL"),labels=c("high", "low"), 
  # values=brewer.pal(2,"Dark2")) +
  scale_alpha(range = c(.3,1)) +
  ylab("Mean certainty rating") +
  xlab("Participant gender") +
  #facet_grid(fact_type~subjectGender) + 
  #ggtitle("Columns: attitude holder gender") +
  #ggtitle(title="Rows: speaker gender; Columns: participant gender") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color=cols$Colors))#, legend.position = "top") 
ggsave("../graphs/gender-collapsed.pdf")


### MIXED EFFECTS ANALYSIS ###
library(lme4)
library(languageR)
centered = cbind(t, myCenter(t[,c("fact_type","VeridicalityMean","PriorMean")]))
contrasts(t$fact_type)

# analysis with categorical fact type (dispreferred because of information loss in treating prior probability as simply "high" vs "low")
m = lmer(response ~ cfact_type * cVeridicalityMean + (1+cfact_type|workerid) + (1+cVeridicalityMean|content) + (1+cfact_type|verb),data=centered)
summary(m) # main effects of fact type, but no veridicality effect nor interaction (with this random effects structure -- without random verb effects, there's a veridicality effect)
table(t$fact_type, t$verb)

ranef(m)

# analysis with continuous prior probability of eventuality (reported in SALT paper)
library(lmerTest)
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb) + (1|fact),data=centered)
summary(m) 

# analysis that doesn't include prior slope for content (JT, January 2018)
m2 = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1|content) + (1+cPriorMean|verb) + (1|fact),data=centered)
summary(m2)

### now lets look at projective contents only (also reported in SALT paper)
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
m = lmer(response ~ cPriorMean * cVeridicalityMean + (1+cPriorMean|workerid) + (1+cPriorMean|content) + (1+cPriorMean|verb) + (1|fact),data=cp)
summary(m)  #main effect of prior, but not of veridicality or interaction


