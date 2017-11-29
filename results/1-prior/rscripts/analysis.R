# setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/7-prior-probability/Git-projective-probability/results/1-prior/')
source('helpers.R')

# load required packages
library(tidyverse)
library(forcats)
library(dichromat)
theme_set(theme_bw())

# load raw data
d = read.csv("../experiment.csv")
nrow(d) #2090 (95 participants x 22 items)
names(d)
length(unique(d$workerid)) #95 participants

d = d %>%
  select(workerid,rt,prompt,itemType,itemNr,list,item,response,fact,slide_number_in_experiment,gender,american,age,language,comments,Answer.time_in_minutes)
nrow(d) #2090

# look at Turkers' comments
unique(d$comments)

# age and gender info
table(d$age) #21-75
median(d$age) #33
table(d$gender)
#45 female, 50 male

### exclude non-American English speakers
length(unique(d$workerid))
length(which(is.na(d$language))) #no missing responses
table(d$language) 
d <- subset(d, (d$language != "hindi" & d$language != "female" & d$language != "russian"))
d = droplevels(d)
length(unique(d$workerid)) #92

length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
d <- subset(d, d$american == "0")
d = droplevels(d)
length(unique(d$workerid)) #87 total

## how often was each list completed?
table(d$list)

## exclude Turkers based on fillers
table(d$item)

# make relevant subsets
# filler 1
d.f1 <- subset(d, d$item == "F1")
d.f1 <- droplevels(d.f1)
nrow(d.f1) #87

# filler 2
d.f2 <- subset(d, d$item == "F2")
d.f2 <- droplevels(d.f2)
nrow(d.f2) #87

# both fillers
d.f12 <- rbind(d.f1,d.f2)
nrow(d.f12) #174

# group mean on filler 1
round(mean(d.f1$response),2) #.86

# group mean on filler 2
round(mean(d.f2$response),2) #.03

ggplot(d.f12, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(d.f12$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("Participant")
ggsave(f="graphs/filler-ratings.pdf",height=4,width=20)

# Turkers who gave responses to F1 lower than .8
f1 <- d.f1[d.f1$response < .8,]
f1
nrow(f1) #17

ggplot(f1, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5) +
  geom_text(aes(label=response), vjust = 2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(f1$response, n = 10)) +
  ylab("Responses to filler 1") +
  xlab("Participants who gave bad responses (expected high)")

# Turkers who gave responses to F2 higher than .1
f2 <- d.f2[d.f2$response > .2,]
nrow(f2) #3

ggplot(f2, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5) +
  geom_text(aes(label=response), vjust = -2.5, cex= 5) +
  scale_y_continuous(breaks = pretty(f2$response, n = 10)) +
  ylab("Responses to filler 2") +
  xlab("Participants who gave bad responses (expected low)")

f <- rbind(f1,f2)
nrow(f) #22

ggplot(f, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  #geom_text(aes(label=response), vjust = -1, cex= 5) +
  scale_y_continuous(breaks = pretty(f$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("Participants who gave bad responses (red/F1 expected high, blue/F2 expected low)")
ggsave(f="graphs/bad-filler-ratings.pdf",height=4,width=20)

length(unique(f$workerid)) #19 Turkers

# exclude the 19 Turkers identified above
d <- subset(d, !(d$workerid %in% f1$workerid | d$workerid %in% f2$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #68 Turkers remain

filler <- droplevels(subset(d,d$itemType == "F"))
nrow(filler)

ggplot(filler, aes(x=workerid,y=response)) +
  geom_point(aes(colour = item)) +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_y_continuous(breaks = pretty(filler$response, n = 10)) +
  ylab("Responses to fillers") +
  xlab("'Good' Participant")
ggsave(f="graphs/filler-ratings-good-participants.pdf",height=4,width=20)

# clean data = cd
cd = d
saveRDS(cd, file="../data/cd.rds")
head(cd)
nrow(cd) #1496 / 22 items = 68 participants

# read cleaned data
cd = readRDS("../data/cd.rds")


# age info
table(cd$age) #21-75
median(cd$age) #36
table(cd$gender)
#31 female, 37 male

# target data
target <- subset(d, d$item != "F1" & d$item != "F2" & d$workerid != "1")
target <- droplevels(target)
nrow(target)
table(target$item)

# mean responses and standard deviations of responses to H and L items
mean.HL = aggregate(response~itemType, data=target, FUN="mean")
mean.HL
#H: .7
#L: .16
sd.HL = aggregate(response~itemType, data=target, FUN="sd")
sd.HL
#H: .21
#L: .17

names(target)
table(target$prompt)

target$event <- target$prompt
target$event <- gsub("How likely is it that","",target$event)
target$event <- gsub("\\?","",target$event)
table(target$event)

cols <- c("itemNr","event")
cols
target$eventItemNr <- do.call(paste, c(target[cols], sep=": "))
table(target$eventItemNr)

means = aggregate(response~item+itemType+eventItemNr, data=target, FUN="mean")
means$YMin = means$response - aggregate(response~item+itemType+eventItemNr, data=target, FUN="ci.low")$response
means$YMax = means$response + aggregate(response~item+itemType+eventItemNr, data=target, FUN="ci.high")$response
means

table(target$item) #32 or 35 responses for each item
table(target$event) #67 responses for each event (32 + 35)
print(unique(target$eventItemNr))

target$eventItemNr  = factor(target$eventItemNr, 
                             levels=c("1:  Mary is pregnant",
                                      "2:  Josie went on vacation to France",
                                      "3:  Emma studied on Saturday morning",     
                                      "4:  Olivia sleeps until noon",
                                      "5:  Sophia got a tattoo",
                                      "6:  Mia drank 2 cocktails last night",
                                      "7:  Isabella ate a steak on Sunday",       
                                      "8:  Emily bought a car yesterday",
                                      "9:  Grace visited her sister",
                                      "10:  Zoe calculated the tip",               
                                      "11:  Danny ate the last cupcake",
                                      "12:  Frank got a cat",
                                      "13:  Jackson ran 10 miles",                
                                      "14:  Jayden rented a car",
                                      "15:  Tony had a drink last night",
                                      "16:  Josh learned to ride a bike yesterday",        
                                      "17:  Owen shoveled snow last winter",       
                                      "18:  Julian dances salsa",
                                      "19:  Jon walks to work",                    
                                      "20:  Charley speaks Spanish"))

ggplot(target, aes(x=eventItemNr,y=response)) +
  geom_point(aes(colour = itemType)) +
  geom_point(data = means, size = 3) +
  geom_errorbar(data = means, aes(ymin=YMin, ymax=YMax)) +
  geom_point(alpha = 0.1) +
  #geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  #, "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  theme(axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.position="none") +
  ylab("Likeliness ratings") +
  xlab("Events") 
ggsave(f="graphs/target-ratings.pdf",height=8,width=10)

means = target %>%
  group_by(event,itemType,fact) %>%
  summarize(PriorMean = mean(response)) %>%
  ungroup() %>%
  mutate(itemType = paste("fact",itemType,sep=""))
write.csv(means,file="../data/prior_means.csv",row.names=F,quote=F)
