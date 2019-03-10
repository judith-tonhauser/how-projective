# set working directory to point to 'rscripts' (default when opening file in RStudio)

# load required packages
require(tidyverse)
library(ggrepel)

# load helper functions
source('../../helpers.R')

# set black and white plot background
theme_set(theme_bw())

d = read.csv("../data/data_preprocessed.csv")

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1"))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)

t$Trigger = factor(x=ifelse(t$short_trigger == "established","establish",ifelse(t$short_trigger == "confessed","confess",ifelse(t$short_trigger == "revealed","reveal",ifelse(t$short_trigger == "discovered","discover",ifelse(t$short_trigger == "learned","learn",ifelse(t$short_trigger == "found_out","find_out",ifelse(t$short_trigger == "saw","see",ifelse(t$short_trigger == "is_amused","amused",ifelse(t$short_trigger == "realize","realize",ifelse(t$short_trigger == "is_aware","aware",ifelse(t$short_trigger == "noticed","notice",ifelse(t$short_trigger == "is_annoyed","annoyed",ifelse(t$short_trigger == "MC","MC","NA"))))))))))))),levels=c("MC","establish","confess","reveal","discover","learn","find_out","see","amused","realize","aware","notice","annoyed"))

# paper figure 5a (projectivity means by target expression)
mean_proj = aggregate(projective~Trigger, data=t, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~Trigger, data=t, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~Trigger, data=t, FUN="ci.high")$projective
mean_proj
t$trigger_proj <- factor(t$Trigger, levels=mean_proj[order(mean_proj$projective), "Trigger"])

ggplot(t, aes(x=trigger_proj, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Projectivity rating")+
  xlab("Expression")
ggsave(f="graphs/boxplot-projection-with-MCs.pdf",height=3,width=10)

# paper figure 5b (by-subject projectivity means)
t.proj <- droplevels(subset(t,t$Trigger != "MC"))
mean_proj = aggregate(projective~Trigger, data=t.proj, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~Trigger, data=t.proj, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~Trigger, data=t.proj, FUN="ci.high")$projective
mean_proj
t.proj$trigger_proj <-factor(t.proj$Trigger, levels=mean_proj[order(mean_proj$projective), "Trigger"])
variances = t.proj %>%
  group_by(workerid) %>%
  summarise(ProjVariance = var(projective),ProjMean=mean(projective),Proj.ci.low=ci.low(projective),Proj.ci.high=ci.high(projective),AIVariance = var(ai),AIMean=mean(ai),AI.ci.low=ci.low(ai),AI.ci.high=ci.high(ai))
variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,ProjMean),y=ProjMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=ProjMean-Proj.ci.low,ymax=ProjMean+Proj.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Mean projectivity rating")
ggsave("graphs/projection-subjectmeans.pdf",height=3,width=10)
ggsave("graphs/projection-subjectmeans.png",height=3,width=10)


# exclude main clauses (fillers)
t_nomc = droplevels(subset(t, short_trigger != "MC"))

# overall correlation coefficient reported in the paper
cor(t_nomc$projective,t_nomc$ai)

# paper figure 6
agr = t_nomc %>%
  group_by(Trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai

# collapsed correlation coefficient reported in paper
cor(agr$mean_ai,agr$mean_proj)

ggplot(agr, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating ('asking whether')") +
  ylab("Mean projectivity ratinsg") +
  xlim(0.35,1) +
  ylim(0.35,1) 
ggsave(file="graphs/ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)

# paper figure 13a (mean at-issueness ratings by target expression)
mean_nai = aggregate(ai~Trigger, data=t, FUN="mean")
mean_nai$YMin = mean_nai$ai - aggregate(ai~Trigger, data=t, FUN="ci.low")$ai
mean_nai$YMax = mean_nai$ai + aggregate(ai~Trigger, data=t, FUN="ci.high")$ai
mean_nai
t$trigger_ai <-factor(t$Trigger, levels=mean_nai[order(mean_nai$ai), "Trigger"])

ggplot(t, aes(x=trigger_ai, y=ai)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness rating \n ('asking whether')")+
  xlab("Expression")
ggsave(f="graphs/boxplot-not-at-issueness-with-MCs.pdf",height=3,width=10)

# paper figure B (by-item projectivity against at-issueness ratings)
head(t_nomc)
t_nomc$Item = as.factor(paste(t_nomc$short_trigger, t_nomc$content))
ggplot(t_nomc, aes(x=ai,y=projective,color=Trigger)) +
  geom_smooth(method="lm") +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Not-at-issueness rating") +
  ylab("Projectivity rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~Item) +
  theme(legend.position="top") +
  guides(colour = guide_legend(nrow = 1))
ggsave("graphs/subject-variability-aiproj-exp1b.pdf",width=20,height=25)

