# created by jdegen, 03/19/2014

setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/2-ProjAI/results')
source('rscripts/helpers.R')
library(grid)
library(ggplot2)
library(plyr)
library(scales)

# load cleaned, melded data file
load("data/cd.RData") 

str(cd$ResponseNew)
cd$ResponseNew <- as.numeric(cd$ResponseNew)

# Number of Projection and NAI responses for each content

table(cd$Trigger,cd$ResponseNew)

means <- ddply(cd, c("Trigger", "QuestionType"), summarise, sum=sum(ResponseNew))
means
# Trigger QuestionType sum
# 1       annoyed           AI  71
# 2       annoyed         Proj  69
# 3      Demonstr           AI  44
# 4      Demonstr         Proj  59
# 5      discover           AI  59
# 6      discover         Proj  52
# 7          know           AI  68
# 8          know         Proj  66
# 9    MainClause           AI   6
# 10   MainClause         Proj   6
# 11       NomApp           AI  70
# 12       NomApp         Proj  71
# 13         NRRC           AI  71
# 14         NRRC         Proj  67
# 15         only           AI  42
# 16         only         Proj  52
# 17       possNP           AI  71
# 18       possNP         Proj  71
# 19 PronounAnaph           AI  54
# 20 PronounAnaph         Proj  42
# 21         stop           AI  56
# 22         stop         Proj  56
# 23       stupid           AI  67
# 24       stupid         Proj  63

## Plot the mean projectivity and the mean NAIness of each content
agr = aggregate(ResponseNew ~ Trigger+QuestionType, data=cd, FUN="mean")
agr$CILow = aggregate(ResponseNew ~ Trigger+QuestionType, data=cd, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ Trigger+QuestionType, data=cd, FUN="ci.high")$Response
agr$YMin = agr$ResponseNew - agr$CILow
agr$YMax = agr$ResponseNew + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$QuestionType,levels=c("NAI","Proj")) 
agr

# relevel Sentence Type by response mean
agr$Trigger2 <-factor(agr$Trigger, levels=agr[order(agr$ResponseNew), "Trigger"])

ggplot(agr, aes(Trigger2, ResponseNew)) +   
  geom_bar(aes(fill = QuestionType), position = "dodge", stat="identity")+
  geom_errorbar(aes(ymin=YMin,ymax=YMax,fill=QuestionType),width=.25,position=dodge) +
  geom_text(aes(x=Trigger2, y=ResponseNew, fill = QuestionType, ymax=ResponseNew, label=round(ResponseNew*100,0), 
                hjust=ifelse(sign(ResponseNew)>0, .5, 0)), vjust = 1,
            position = position_dodge(width=1)) +
  scale_fill_discrete(name="Condition",
                      breaks=c("AI", "Proj"),
                      labels=c("Not-at-issue", "Projection"))+
  scale_x_discrete(name="Content of trigger") +
  scale_y_continuous(name="Response (in percentage, with 95% confidence intervals)")
ggsave(file="graphs/AI-Proj-All-Contents.pdf",width = 14, height = 6)


## Plot means for Proj and NAI with line

ggplot(agr, aes(x=Trigger2, y=ResponseNew, color=QuestionType,group=QuestionType)) +
  #aes(x=date, y=x, color=group, shape=group, group=group)
  geom_point(shape=1) +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, se=TRUE) +   
  xlab("Trigger")+
  ylab("Response")

## Plot mean projection on x-axis and mean NAI on y-axis, and draw regression line

agr
library(reshape2)
agr.wide <- dcast(agr, Trigger ~ QuestionType, value.var="ResponseNew")
agr.wide

lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

str(agr.wide$Proj)
str(agr.wide$AI)

ggplot(agr.wide, aes(x=Proj, y=AI)) +
  geom_point(shape=16) +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, se=TRUE) +  
  geom_text(aes(label=Trigger), vjust = 1, cex= 5, offset = 10) +
  geom_text(aes(x = Proj, y = Ai, label = lm_eqn(df)), parse = TRUE)
  xlab("Projection response mean")+
  ylab("NAI response mean")



# exclude main clause data
names(cd)
table(cd$Trigger)
table(cd$Trigger,cd$ID)
cd.noMC <- subset(cd, cd$Trigger != "MainClause")
cd.noMC = droplevels(cd.noMC)
nrow(cd.noMC) #1628 (1628 / 74 Turkers = 22 items (right because even though there are
# 6 main clauses, there are only two main clause types, with 3 tokens each)
nrow(cd) #1924 (1924 / 74 = 26)

agr.NoMC = aggregate(ResponseNew ~ Trigger+QuestionType, data=cd.noMC, FUN="mean")
agr.NoMC$CILow = aggregate(ResponseNew ~ Trigger+QuestionType, data=cd.noMC, FUN="ci.low")$Response
agr.NoMC$CIHigh = aggregate(ResponseNew ~ Trigger+QuestionType, data=cd.noMC, FUN="ci.high")$Response
agr.NoMC$YMin = agr$ResponseNew - agr$CILow
agr.NoMC$YMax = agr$ResponseNew + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$QuestionType,levels=c("AI","Proj")) 
agr.NoMC

# relevel Sentence Type by response mean
agr.NoMC$Trigger2 <-factor(agr.NoMC$Trigger, levels=agr.NoMC[order(agr.NoMC$ResponseNew), "Trigger"])

ggplot(agr.NoMC, aes(x=Trigger2, y=ResponseNew, color=QuestionType,group=QuestionType)) +
  #aes(x=date, y=x, color=group, shape=group, group=group)
  geom_point(shape=1) +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, se = FALSE) +   
  xlab("Trigger")+
  ylab("Response")

###############
              
ggplot(agr, aes(Trigger2, Response))+
  geom_point(aes(fill = QuestionType,color = QuestionType), position = "dodge", stat="identity")+
  geom_smooth(Response=lm)+
  #geom_point(position = position_jitter(w = 0.3, h = 0.3))+
  xlab("Required Sampling Frequency for Valid Monthly Values")+
  ylab("Actual Sampling Frequency")

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=Trigger,y=Response,fill=QuestionType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="white",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_text(aes(x=Trigger, y=Response, ymax=Response, label=round(Response*100,0), 
                hjust=ifelse(sign(Response)>0, 1, 0)), vjust = 1,
            position = position_dodge(width=1)) +
  scale_x_discrete(name="Types of content") +
  scale_y_continuous(name="Proportion of NAI responses")
ggsave(f="graphs/proportion-NAI-responses-by-content-type.pdf",width=10,height=6)


ggplot(cd, aes(Trigger, Response,fill=QuestionType))+
  geom_point(position = position_jitter(w = 0.3, h = 0.3))+
  xlab("Required Sampling Frequency for Valid Monthly Values")+
  ylab("Actual Sampling Frequency")

# make scatter plot with line through it
plot(jitter(agr$Trigger, factor=1),
     jitter(agr$Response, factor=0.4),
     axes=F,
     xlab="Entropy of response distribution",
     ylab="Probability of correct inference with RNN model", cex.axis = 1.5, cex.lab = 1.5, lwd = 2)
axis(1, cex.axis = 1.5)
axis(2, at=seq(0,1, by=0.1), label=seq(0,1, by=0.1), cex.axis = 1.5)
fit = glm(formula = data$jooCorrect ~ data$TriResponseEntropy, family = "binomial")
curve(plogis(fit$coef[1] + fit$coef[2]*x), add=TRUE, lwd = 4)




## Plot by AIness with the two EAS not distinguished
agr = aggregate(Response ~ SentenceTypeN, data=cd, FUN="mean")
agr$CILow = aggregate(Response ~ SentenceTypeN, data=cd, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ SentenceTypeN, data=cd, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr
# I just did this to make things look nicer, serves only an aesthetic purpose. 
#And in your case, you probably won't have a Choice variable anyway

# relevel Sentence Type by response mean
agr$SentenceType2 <-factor(agr$SentenceTypeN, levels=agr[order(agr$Response), "SentenceTypeN"])

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=SentenceType2,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="white",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_text(aes(x=SentenceType2, y=Response, ymax=Response, label=round(Response*100,0), 
                hjust=ifelse(sign(Response)>0, 1, 0)), vjust = 1,
            position = position_dodge(width=1)) +
  scale_x_discrete(name="Types of content") +
  scale_y_continuous(name="Proportion of NAI responses")
ggsave(f="graphs/proportion-NAI-responses-by-content-type2.pdf",width=10,height=6)

# Get means (i.e., proportions) and confidence intervals. In your case, value needs to be the 1/0 variable. 
# redresponse and redimptype will be whatever variables you want to split by 
# (eg if you have just one variable that codes all the conditions, you'll just have value ~ conditionvariable)
agr = aggregate(Response ~ Adj + AIness, data=cd, FUN="mean")
agr$CILow = aggregate(Response ~ Adj + AIness, data=cd, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ Adj + AIness, data=cd, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr
# I just did this to make things look nicer, serves only an aesthetic purpose. 
#And in your case, you probably won't have a Choice variable anyway

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=Adj,y=Response,fill=Choice)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Adjective") +
  scale_y_continuous(name="Proportion of NAI responses")


# Plot only the two types of evaluative adjective sentences 
# with annoyed and discover for comparison

cd.target <- subset(cd, cd$SentenceType == "Comp-of-Annoyed" 
                    | cd$SentenceType == "Comp-of-Discover"
                    | cd$SentenceType == "VP-of-EAS1" 
                    | cd$SentenceType == "VP-of-EAS2")
cd.target = droplevels(cd.target)
nrow(cd.target) 

table(cd.target$AIness)

agr = aggregate(Response ~ SentenceType, data=cd.target, FUN="mean")
agr$CILow = aggregate(Response ~ SentenceType, data=cd.target, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ SentenceType, data=cd.target, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr

# relevel Sentence Type by response mean
agr$SentenceType2 <-factor(agr$SentenceType, levels=agr[order(agr$Response), "SentenceType"])

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=SentenceType2,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Sharpening implication") +
  scale_y_continuous(name="Proportion of NAI responses")
ggsave(f="graphs/proportion-NAI-responses-by-EAS.pdf",width=8,height=4)

# To see if there is variation among speakers

cd.target <- subset(cd, cd$SentenceType == "Comp-of-Annoyed" 
                    | cd$SentenceType == "Comp-of-Discover"
                    | cd$SentenceType == "VP-of-EAS1" 
                    | cd$SentenceType == "VP-of-EAS2")
cd.target = droplevels(cd.target)
nrow(cd.target) 


agr = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="mean")
agr$CILow = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=SentenceType,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Sharpening implication") +
  scale_y_continuous(name="Proportion of NAI responses") +
  facet_wrap(~workerid)
ggsave(f="graphs/subject-variation-with-AIness-of-EAS-and-complements.pdf",width=14,height=10)


# To see if there is variation among adjectives

cd.target <- subset(cd, cd$SentenceType == "VP-of-EAS1" 
                    | cd$SentenceType == "VP-of-EAS2")
cd.target = droplevels(cd.target)
nrow(cd.target) 

agr = aggregate(Response ~ Adj + SentenceType, data=cd.target, FUN="mean")
agr$CILow = aggregate(Response ~ Adj + SentenceType, data=cd.target, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ Adj + SentenceType, data=cd.target, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=SentenceType,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Adjective") +
  scale_y_continuous(name="Proportion of NAI responses") +
  facet_wrap(~Adj)
ggsave(f="graphs/adjective-variation.pdf",width=14,height=10)



# To see if there is variation among speakers wrt annoyed and discover

cd.target <- subset(cd, cd$SentenceType == "Comp-of-Annoyed" 
                    | cd$SentenceType == "Comp-of-Discover")
cd.target = droplevels(cd.target)
nrow(cd.target) 


agr = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="mean")
agr$CILow = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ SentenceType + workerid, data=cd.target, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
#agr$Choice = factor(x=agr$AIness,levels=c("SAI","VPAI")) 
agr

# Plot! (You probably won't have a fill variable)
ggplot(agr, aes(x=SentenceType,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",show_guide=F,position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Sharpening implication") +
  scale_y_continuous(name="Proportion of NAI responses") +
  facet_wrap(~workerid)
ggsave(f="graphs/subject-variation-with-complements.pdf",width=14,height=10)





