setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/2-veridicality2/')
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

mean(d$Answer.time_in_minutes) #6.4
median(d$Answer.time_in_minutes) #5.2

d = d %>%
  select(workerid,rt,subjectGender,speakerGender,content,verb,contentNr,trigger_class,response,slide_number_in_experiment,age,language,asses,american,gender,comments,Answer.time_in_minutes)
nrow(d) #8400

# look at Turkers' comments
unique(d$comments)

# how did Turkers assess their performance?
table(d$asses)
# Did you understand the task?
#Confused   No      Yes 
#700        364     7280

# age and gender info
length(which(is.na(d$age))) #0
table(d$age) #18-72
median(d$age,na.rm=TRUE) #35 
table(d$gender)
#137 female, 162 male, 1 other

### exclude non-American English speakers
length(unique(d$workerid)) #300
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "chinese " & d$language != "Portuguese"))
d = droplevels(d)
length(unique(d$workerid)) #298 (2 Turkers excluded)

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #284 (14 Turkers excluded, 16 excluded in total for language reasons)

## exclude Turkers based on fillers
names(d)
table(d$contentNr)
table(d$verb)

# make relevant data subsets
# control_bad: contradictory controls (YES = 1)
c.bad <- subset(d, d$verb == "control_bad")
c.bad <- droplevels(c.bad)
nrow(c.bad) #1136 / 4 contradictory controls = 284 Turkers

# control_good: noncontradictory controls (NO = 0)
c.good <- subset(d, d$verb == "control_good")
c.good <- droplevels(c.good)
nrow(c.good) #1136

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2272

# group mean on contradictory controls
round(mean(c.bad$response),2) #.94

# group mean on noncontradictory controls
round(mean(c.good$response),2) #.08

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
# 1 control_bad1 0.9382042
# 2 control_bad2 0.9328521
# 3 control_bad3 0.9242958
# 4 control_bad4 0.9567606

means = aggregate(response ~ contentNr, data=c.good, FUN="mean")
means
# contentNr   response
# 1 control_good1 0.17239437 "Zack believes that I'm married, but I'm actually single"
# 2 control_good2 0.05464789
# 3 control_good3 0.05151408
# 4 control_good4 0.06056338

#################################################################
##### considering only 3 of the 4 noncontradictory controls #####
##### first analysis, see footnote in paper
c.good2 <- subset(d, d$contentNr == "control_good2" | d$contentNr == "control_good3" | d$contentNr == "control_good4")
c.good2 <- droplevels(c.good2)
nrow(c.good2) #852
# group mean on 3 noncontradictory controls
round(mean(c.good2$response),2) #.06
# if c.good2 is used instead of c.good
# XX Turkers (instead of XX) gave too high responses
# the XX Turkers mean response to the noncontradictory controls: XX
# XX Turkers total excluded, instead of XX
#################################################################

# all controls
c <- rbind(c.bad,c.good)
nrow(c) #2272 / 8 items = 284 Turkers

# Turkers with response means on noncontradictory controls (0) more than 3sd above group mean
cg.means = aggregate(response~workerid, data=c.good, FUN="mean")
cg.means$YMin = cg.means$response - aggregate(response~workerid, data=c.good, FUN="ci.low")$response
cg.means$YMax = cg.means$response + aggregate(response~workerid, data=c.good, FUN="ci.high")$response
cg.means

c.g <- cg.means[cg.means$response > (mean(cg.means$response) + 3*sd(cg.means$response)),]
c.g
unique(length(c.g$workerid)) #10 Turkers gave high responses
mean(c.g$response) #.62

# Turkers with response means on contradictory controls (1) more than 3sd below group mean
cb.means = aggregate(response~workerid, data=c.bad, FUN="mean")
cb.means$YMin = cb.means$response - aggregate(response~workerid, data=c.bad, FUN="ci.low")$response
cb.means$YMax = cb.means$response + aggregate(response~workerid, data=c.bad, FUN="ci.high")$response
cb.means

c.b <- cb.means[cb.means$response < (mean(cb.means$response) - 3*sd(cb.means$response)),]
c.b
unique(length(c.b$workerid)) #7 Turkers gave low responses
mean(c.b$response) #.47

# how many unique Turkers did badly on the controls?
outliers <- subset(c, c$workerid %in% c.g$workerid | c$workerid %in% c.b$workerid)
outliers = droplevels(outliers)
nrow(outliers) #104 / 8 control items = 13 Turkers
table(outliers$workerid)

# look at the responses to the controls that these "outlier" Turkers did
# outliers to noncontradictory items (0)
outliers.g <- subset(c.good, c.good$workerid %in% c.g$workerid)
outliers.g = droplevels(outliers.g)
nrow(outliers.g) #40 / 4 control items = 10 Turkers
table(outliers.g$workerid)

ggplot(outliers.g, aes(x=workerid,y=response)) +
  geom_point(aes(colour = contentNr)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5,position=position_jitter(h=.01,w=0.02)) +
  #geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(outliers.g$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be low (noncontradictory) but these 10 Turkers
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
  scale_y_continuous(breaks = pretty(outliers.b$response, n = 10)) +
  ylab("Responses") +
  xlab("Participants")
# responses here are supposed to be high (contradictory) but these 7 Turkers
# gave consistently low responses across the control items 

# exclude the 13 Turkers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #271 Turkers remain (184 - 13)

# clean data = cd
cd = d
write.csv(cd, "data/cd.csv")
nrow(cd) #7588 / 28 items = 271 participants

# age info
table(cd$age) #18-72
length(which(is.na(cd$age))) #0 missing values
median(cd$age,na.rm=TRUE) #36
table(cd$gender)
#130 female, 140 male, 1 other

# target data (20 items per Turker)
names(cd)
table(cd$verb)
t <- subset(cd, cd$verb != "control_good" & cd$verb != "control_bad")
t <- droplevels(t)
nrow(t) #5420 / 20 = 271 Turkers
table(t$verb,t$content)

# change the name of the predicates
table(t$verb)
t$verb <- gsub("be_right_that","be_right",t$verb)
t$verb <- gsub("inform_Sam","inform",t$verb)
t$verb <- gsub("annoyed","be_annoyed",t$verb)


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

agr_verb = t %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

agr_subj = t %>%
  group_by(content, verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_verb, aes(x=verb, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_point(data=agr_subj, aes(color=content)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Predicate")
ggsave("graphs/veridicality-means-byitem.pdf",height=4,width=8)

agr_content = t %>%
  group_by(content) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

ggplot(agr_content, aes(x=content, y=Mean)) + 
  geom_point(color="black", size=4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Contradictoriness rating")+
  xlab("Content")
ggsave("graphs/veridicality-means-bycontent.pdf",height=8,width=6)

# veridicality rating by participant
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

comparison = lsmeans(model, pairwise~verb,adjust="tukey")
options(max.print=10000)
comparison

# $contrasts
# contrast                         estimate         SE   df t.ratio p.value
# think - pretend             -0.0489298893 0.02208335 5149  -2.216  0.7910
# think - hear                -0.0671955720 0.02208335 5149  -3.043  0.2094
# think - suggest             -0.0841328413 0.02208335 5149  -3.810  0.0201
# think - say                 -0.1970110701 0.02208335 5149  -8.921  <.0001
# think - announce            -0.2782287823 0.02208335 5149 -12.599  <.0001
# think - inform_Sam          -0.2975276753 0.02208335 5149 -13.473  <.0001
# think - reveal              -0.4556457565 0.02208335 5149 -20.633  <.0001
# think - annoyed             -0.4630627306 0.02208335 5149 -20.969  <.0001
# think - confess             -0.4945756458 0.02208335 5149 -22.396  <.0001
# think - acknowledge         -0.4966789668 0.02208335 5149 -22.491  <.0001
# think - admit               -0.5380073801 0.02208335 5149 -24.363  <.0001
# think - establish           -0.5460885609 0.02208335 5149 -24.729  <.0001
# think - demonstrate         -0.5470110701 0.02208335 5149 -24.770  <.0001
# think - confirm             -0.5907380074 0.02208335 5149 -26.750  <.0001
# think - discover            -0.6084870849 0.02208335 5149 -27.554  <.0001
# think - see                 -0.6429520295 0.02208335 5149 -29.115  <.0001
# think - know                -0.6614022140 0.02208335 5149 -29.950  <.0001
# think - prove               -0.6808856089 0.02208335 5149 -30.833  <.0001
# think - be_right_that       -0.7620295203 0.02208335 5149 -34.507  <.0001
# pretend - hear              -0.0182656827 0.02208335 5149  -0.827  1.0000
# pretend - suggest           -0.0352029520 0.02208335 5149  -1.594  0.9899
# pretend - say               -0.1480811808 0.02208335 5149  -6.706  <.0001
# pretend - announce          -0.2292988930 0.02208335 5149 -10.383  <.0001
# pretend - inform_Sam        -0.2485977860 0.02208335 5149 -11.257  <.0001
# pretend - reveal            -0.4067158672 0.02208335 5149 -18.417  <.0001
# pretend - annoyed           -0.4141328413 0.02208335 5149 -18.753  <.0001
# pretend - confess           -0.4456457565 0.02208335 5149 -20.180  <.0001
# pretend - acknowledge       -0.4477490775 0.02208335 5149 -20.275  <.0001
# pretend - admit             -0.4890774908 0.02208335 5149 -22.147  <.0001
# pretend - establish         -0.4971586716 0.02208335 5149 -22.513  <.0001
# pretend - demonstrate       -0.4980811808 0.02208335 5149 -22.555  <.0001
# pretend - confirm           -0.5418081181 0.02208335 5149 -24.535  <.0001
# pretend - discover          -0.5595571956 0.02208335 5149 -25.338  <.0001
# pretend - see               -0.5940221402 0.02208335 5149 -26.899  <.0001
# pretend - know              -0.6124723247 0.02208335 5149 -27.735  <.0001
# pretend - prove             -0.6319557196 0.02208335 5149 -28.617  <.0001
# pretend - be_right_that     -0.7130996310 0.02208335 5149 -32.291  <.0001
# hear - suggest              -0.0169372694 0.02208335 5149  -0.767  1.0000
# hear - say                  -0.1298154982 0.02208335 5149  -5.878  <.0001
# hear - announce             -0.2110332103 0.02208335 5149  -9.556  <.0001
# hear - inform_Sam           -0.2303321033 0.02208335 5149 -10.430  <.0001
# hear - reveal               -0.3884501845 0.02208335 5149 -17.590  <.0001
# hear - annoyed              -0.3958671587 0.02208335 5149 -17.926  <.0001
# hear - confess              -0.4273800738 0.02208335 5149 -19.353  <.0001
# hear - acknowledge          -0.4294833948 0.02208335 5149 -19.448  <.0001
# hear - admit                -0.4708118081 0.02208335 5149 -21.320  <.0001
# hear - establish            -0.4788929889 0.02208335 5149 -21.686  <.0001
# hear - demonstrate          -0.4798154982 0.02208335 5149 -21.727  <.0001
# hear - confirm              -0.5235424354 0.02208335 5149 -23.708  <.0001
# hear - discover             -0.5412915129 0.02208335 5149 -24.511  <.0001
# hear - see                  -0.5757564576 0.02208335 5149 -26.072  <.0001
# hear - know                 -0.5942066421 0.02208335 5149 -26.907  <.0001
# hear - prove                -0.6136900369 0.02208335 5149 -27.790  <.0001
# hear - be_right_that        -0.6948339483 0.02208335 5149 -31.464  <.0001
# suggest - say               -0.1128782288 0.02208335 5149  -5.111  0.0001
# suggest - announce          -0.1940959410 0.02208335 5149  -8.789  <.0001
# suggest - inform_Sam        -0.2133948339 0.02208335 5149  -9.663  <.0001
# suggest - reveal            -0.3715129151 0.02208335 5149 -16.823  <.0001
# suggest - annoyed           -0.3789298893 0.02208335 5149 -17.159  <.0001
# suggest - confess           -0.4104428044 0.02208335 5149 -18.586  <.0001
# suggest - acknowledge       -0.4125461255 0.02208335 5149 -18.681  <.0001
# suggest - admit             -0.4538745387 0.02208335 5149 -20.553  <.0001
# suggest - establish         -0.4619557196 0.02208335 5149 -20.919  <.0001
# suggest - demonstrate       -0.4628782288 0.02208335 5149 -20.961  <.0001
# suggest - confirm           -0.5066051661 0.02208335 5149 -22.941  <.0001
# suggest - discover          -0.5243542435 0.02208335 5149 -23.744  <.0001
# suggest - see               -0.5588191882 0.02208335 5149 -25.305  <.0001
# suggest - know              -0.5772693727 0.02208335 5149 -26.140  <.0001
# suggest - prove             -0.5967527675 0.02208335 5149 -27.023  <.0001
# suggest - be_right_that     -0.6778966790 0.02208335 5149 -30.697  <.0001
# say - announce              -0.0812177122 0.02208335 5149  -3.678  0.0321
# say - inform_Sam            -0.1005166052 0.02208335 5149  -4.552  0.0009
# say - reveal                -0.2586346863 0.02208335 5149 -11.712  <.0001
# say - annoyed               -0.2660516605 0.02208335 5149 -12.048  <.0001
# say - confess               -0.2975645756 0.02208335 5149 -13.475  <.0001
# say - acknowledge           -0.2996678967 0.02208335 5149 -13.570  <.0001
# say - admit                 -0.3409963100 0.02208335 5149 -15.441  <.0001
# say - establish             -0.3490774908 0.02208335 5149 -15.807  <.0001
# say - demonstrate           -0.3500000000 0.02208335 5149 -15.849  <.0001
# say - confirm               -0.3937269373 0.02208335 5149 -17.829  <.0001
# say - discover              -0.4114760148 0.02208335 5149 -18.633  <.0001
# say - see                   -0.4459409594 0.02208335 5149 -20.194  <.0001
# say - know                  -0.4643911439 0.02208335 5149 -21.029  <.0001
# say - prove                 -0.4838745387 0.02208335 5149 -21.911  <.0001
# say - be_right_that         -0.5650184502 0.02208335 5149 -25.586  <.0001
# announce - inform_Sam       -0.0192988930 0.02208335 5149  -0.874  1.0000
# announce - reveal           -0.1774169742 0.02208335 5149  -8.034  <.0001
# announce - annoyed          -0.1848339483 0.02208335 5149  -8.370  <.0001
# announce - confess          -0.2163468635 0.02208335 5149  -9.797  <.0001
# announce - acknowledge      -0.2184501845 0.02208335 5149  -9.892  <.0001
# announce - admit            -0.2597785978 0.02208335 5149 -11.764  <.0001
# announce - establish        -0.2678597786 0.02208335 5149 -12.129  <.0001
# announce - demonstrate      -0.2687822878 0.02208335 5149 -12.171  <.0001
# announce - confirm          -0.3125092251 0.02208335 5149 -14.151  <.0001
# announce - discover         -0.3302583026 0.02208335 5149 -14.955  <.0001
# announce - see              -0.3647232472 0.02208335 5149 -16.516  <.0001
# announce - know             -0.3831734317 0.02208335 5149 -17.351  <.0001
# announce - prove            -0.4026568266 0.02208335 5149 -18.234  <.0001
# announce - be_right_that    -0.4838007380 0.02208335 5149 -21.908  <.0001
# inform_Sam - reveal         -0.1581180812 0.02208335 5149  -7.160  <.0001
# inform_Sam - annoyed        -0.1655350554 0.02208335 5149  -7.496  <.0001
# inform_Sam - confess        -0.1970479705 0.02208335 5149  -8.923  <.0001
# inform_Sam - acknowledge    -0.1991512915 0.02208335 5149  -9.018  <.0001
# inform_Sam - admit          -0.2404797048 0.02208335 5149 -10.890  <.0001
# inform_Sam - establish      -0.2485608856 0.02208335 5149 -11.256  <.0001
# inform_Sam - demonstrate    -0.2494833948 0.02208335 5149 -11.297  <.0001
# inform_Sam - confirm        -0.2932103321 0.02208335 5149 -13.277  <.0001
# inform_Sam - discover       -0.3109594096 0.02208335 5149 -14.081  <.0001
# inform_Sam - see            -0.3454243542 0.02208335 5149 -15.642  <.0001
# inform_Sam - know           -0.3638745387 0.02208335 5149 -16.477  <.0001
# inform_Sam - prove          -0.3833579336 0.02208335 5149 -17.360  <.0001
# inform_Sam - be_right_that  -0.4645018450 0.02208335 5149 -21.034  <.0001
# reveal - annoyed            -0.0074169742 0.02208335 5149  -0.336  1.0000
# reveal - confess            -0.0389298893 0.02208335 5149  -1.763  0.9698
# reveal - acknowledge        -0.0410332103 0.02208335 5149  -1.858  0.9495
# reveal - admit              -0.0823616236 0.02208335 5149  -3.730  0.0268
# reveal - establish          -0.0904428044 0.02208335 5149  -4.096  0.0067
# reveal - demonstrate        -0.0913653137 0.02208335 5149  -4.137  0.0057
# reveal - confirm            -0.1350922509 0.02208335 5149  -6.117  <.0001
# reveal - discover           -0.1528413284 0.02208335 5149  -6.921  <.0001
# reveal - see                -0.1873062731 0.02208335 5149  -8.482  <.0001
# reveal - know               -0.2057564576 0.02208335 5149  -9.317  <.0001
# reveal - prove              -0.2252398524 0.02208335 5149 -10.200  <.0001
# reveal - be_right_that      -0.3063837638 0.02208335 5149 -13.874  <.0001
# annoyed - confess           -0.0315129151 0.02208335 5149  -1.427  0.9974
# annoyed - acknowledge       -0.0336162362 0.02208335 5149  -1.522  0.9941
# annoyed - admit             -0.0749446494 0.02208335 5149  -3.394  0.0806
# annoyed - establish         -0.0830258303 0.02208335 5149  -3.760  0.0241
# annoyed - demonstrate       -0.0839483395 0.02208335 5149  -3.801  0.0207
# annoyed - confirm           -0.1276752768 0.02208335 5149  -5.782  <.0001
# annoyed - discover          -0.1454243542 0.02208335 5149  -6.585  <.0001
# annoyed - see               -0.1798892989 0.02208335 5149  -8.146  <.0001
# annoyed - know              -0.1983394834 0.02208335 5149  -8.981  <.0001
# annoyed - prove             -0.2178228782 0.02208335 5149  -9.864  <.0001
# annoyed - be_right_that     -0.2989667897 0.02208335 5149 -13.538  <.0001
# confess - acknowledge       -0.0021033210 0.02208335 5149  -0.095  1.0000
# confess - admit             -0.0434317343 0.02208335 5149  -1.967  0.9158
# confess - establish         -0.0515129151 0.02208335 5149  -2.333  0.7115
# confess - demonstrate       -0.0524354244 0.02208335 5149  -2.374  0.6808
# confess - confirm           -0.0961623616 0.02208335 5149  -4.355  0.0023
# confess - discover          -0.1139114391 0.02208335 5149  -5.158  <.0001
# confess - see               -0.1483763838 0.02208335 5149  -6.719  <.0001
# confess - know              -0.1668265683 0.02208335 5149  -7.554  <.0001
# confess - prove             -0.1863099631 0.02208335 5149  -8.437  <.0001
# confess - be_right_that     -0.2674538745 0.02208335 5149 -12.111  <.0001
# acknowledge - admit         -0.0413284133 0.02208335 5149  -1.871  0.9460
# acknowledge - establish     -0.0494095941 0.02208335 5149  -2.237  0.7771
# acknowledge - demonstrate   -0.0503321033 0.02208335 5149  -2.279  0.7492
# acknowledge - confirm       -0.0940590406 0.02208335 5149  -4.259  0.0034
# acknowledge - discover      -0.1118081181 0.02208335 5149  -5.063  0.0001
# acknowledge - see           -0.1462730627 0.02208335 5149  -6.624  <.0001
# acknowledge - know          -0.1647232472 0.02208335 5149  -7.459  <.0001
# acknowledge - prove         -0.1842066421 0.02208335 5149  -8.341  <.0001
# acknowledge - be_right_that -0.2653505535 0.02208335 5149 -12.016  <.0001
# admit - establish           -0.0080811808 0.02208335 5149  -0.366  1.0000
# admit - demonstrate         -0.0090036900 0.02208335 5149  -0.408  1.0000
# admit - confirm             -0.0527306273 0.02208335 5149  -2.388  0.6707
# admit - discover            -0.0704797048 0.02208335 5149  -3.192  0.1434
# admit - see                 -0.1049446494 0.02208335 5149  -4.752  0.0004
# admit - know                -0.1233948339 0.02208335 5149  -5.588  <.0001
# admit - prove               -0.1428782288 0.02208335 5149  -6.470  <.0001
# admit - be_right_that       -0.2240221402 0.02208335 5149 -10.144  <.0001
# establish - demonstrate     -0.0009225092 0.02208335 5149  -0.042  1.0000
# establish - confirm         -0.0446494465 0.02208335 5149  -2.022  0.8940
# establish - discover        -0.0623985240 0.02208335 5149  -2.826  0.3383
# establish - see             -0.0968634686 0.02208335 5149  -4.386  0.0020
# establish - know            -0.1153136531 0.02208335 5149  -5.222  <.0001
# establish - prove           -0.1347970480 0.02208335 5149  -6.104  <.0001
# establish - be_right_that   -0.2159409594 0.02208335 5149  -9.778  <.0001
# demonstrate - confirm       -0.0437269373 0.02208335 5149  -1.980  0.9108
# demonstrate - discover      -0.0614760148 0.02208335 5149  -2.784  0.3672
# demonstrate - see           -0.0959409594 0.02208335 5149  -4.344  0.0024
# demonstrate - know          -0.1143911439 0.02208335 5149  -5.180  <.0001
# demonstrate - prove         -0.1338745387 0.02208335 5149  -6.062  <.0001
# demonstrate - be_right_that -0.2150184502 0.02208335 5149  -9.737  <.0001
# confirm - discover          -0.0177490775 0.02208335 5149  -0.804  1.0000
# confirm - see               -0.0522140221 0.02208335 5149  -2.364  0.6882
# confirm - know              -0.0706642066 0.02208335 5149  -3.200  0.1402
# confirm - prove             -0.0901476015 0.02208335 5149  -4.082  0.0071
# confirm - be_right_that     -0.1712915129 0.02208335 5149  -7.757  <.0001
# discover - see              -0.0344649446 0.02208335 5149  -1.561  0.9921
# discover - know             -0.0529151292 0.02208335 5149  -2.396  0.6644
# discover - prove            -0.0723985240 0.02208335 5149  -3.278  0.1128
# discover - be_right_that    -0.1535424354 0.02208335 5149  -6.953  <.0001
# see - know                  -0.0184501845 0.02208335 5149  -0.835  1.0000
# see - prove                 -0.0379335793 0.02208335 5149  -1.718  0.9769
# see - be_right_that         -0.1190774908 0.02208335 5149  -5.392  <.0001
# know - prove                -0.0194833948 0.02208335 5149  -0.882  1.0000
# know - be_right_that        -0.1006273063 0.02208335 5149  -4.557  0.0009
# prove - be_right_that       -0.0811439114 0.02208335 5149  -3.674  0.0325
# 
# P value adjustment: tukey method for comparing a family of 20 estimates 

