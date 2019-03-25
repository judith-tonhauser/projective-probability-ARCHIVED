# Prior probability work
# compare binary and gradient

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
library(brms)
theme_set(theme_bw())

# load clean data for analysis ----

# load clean binary projectivity data for analysis
d_proj_b = read.csv("../../8-projectivity-no-fact-binary/data/cd.csv")
d_proj_nb = read.csv("../../5-projectivity-no-fact/data/cd.csv") %>%
  mutate(verb=recode(verb, control = "MC", annoyed = "be_annoyed", be_right_that = "be_right", inform_Sam = "inform"))

# load clean binary inference entailment data for analysis
d_inf_b = read.csv("../../7-veridicality3-binary/data/cd.csv")
d_inf_nb = read.csv("../../4-veridicality3/data/cd.csv")

# load clean binary contradictoriness entailment data for analysis
d_contr_b = read.csv("../../6-veridicality2-binary/data/cd.csv")
d_contr_nb = read.csv("../../2-veridicality2/data/cd.csv")


# for projectivity data, plot proportions against mean slider ratings
p_prop = d_proj_b %>%
  group_by(verb) %>%
  summarize(Prop = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMinP = Prop - CILow, YMaxP = Prop + CIHigh, Verb = fct_reorder(as.factor(verb),Prop))

p_means = d_proj_nb %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinM = Mean - CILow, YMaxM = Mean + CIHigh) %>%
  select(-CILow,-CIHigh)

pd = p_prop %>%
  left_join(p_means) %>%
  mutate(VeridicalityGroup = factor(case_when(
    verb %in% c("know", "discover", "reveal", "see", "be_annoyed") ~ "F", 
    verb %in% c("pretend", "think", "suggest", "say") ~ "NF", 
    verb %in% c("be_right","demonstrate") ~ "VNF",
    verb %in% c("MC") ~ "MC",
    TRUE ~ "V")))

ggplot(pd, aes(x=Mean, y=Prop, fill=VeridicalityGroup)) +
  geom_errorbar(aes(ymin=YMinP,ymax=YMaxP),width=0) +
  geom_errorbarh(aes(xmin=YMinM,xmax=YMaxM),width=0) +
  geom_point(shape=21,stroke=.5,size=2.5,color="black") +
  # scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  # scale_alpha(range = c(.3,1)) +
  scale_fill_manual(values=c("darkorchid","black","gray60","tomato1","dodgerblue")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  # guides(fill=FALSE) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 # color=cols$Colors)) +
  # theme(legend.position="top") +
  ylab("Proportion of 'yes' answers") +
  xlab("Mean certainty rating") +
  xlim(c(0,1)) +
  ylim(c(0,1))
ggsave("../graphs/projectivity.pdf",height=4,width=6)

cor(pd$Prop,pd$Mean,method="spearman")
