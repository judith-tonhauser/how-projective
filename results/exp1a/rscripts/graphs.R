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


# paper figure 2a (projectivity means by target expression)
mean_proj = aggregate(projective~short_trigger, data=t, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~short_trigger, data=t, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~short_trigger, data=t, FUN="ci.high")$projective
mean_proj
t$trigger_proj <- factor(t$short_trigger, levels=mean_proj[order(mean_proj$projective), "short_trigger"])

ggplot(t, aes(x=trigger_proj, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Projectivity rating")+
  xlab("Expression")
ggsave(f="graphs/boxplot-projection-with-MCs.pdf",height=3,width=6.5)
ggsave(f="graphs/boxplot-projection-with-MCs.png",height=3,width=6.5)

# paper figure 2b (by-subject projectivity means)
t.proj <- droplevels(subset(t,t$short_trigger != "MC"))
mean_proj = aggregate(projective~short_trigger, data=t.proj, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~short_trigger, data=t.proj, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~short_trigger, data=t.proj, FUN="ci.high")$projective
mean_proj
t.proj$trigger_proj <-factor(t.proj$short_trigger, levels=mean_proj[order(mean_proj$projective), "short_trigger"])
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
ggsave("graphs/projection-subjectmeans.pdf",height=3,width=6.5)
ggsave("graphs/projection-subjectmeans.png",height=3,width=6.5)


# exclude main clauses (fillers)
t_nomc = droplevels(subset(t, short_trigger != "MC"))

# reorder trigger levels
t_nomc$Trigger = factor(x=as.character(t_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# overall correlation coefficient reported in the paper
cor(t_nomc$projective,t_nomc$ai)

# paper figure 3
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

# projection means of annoyed (.96) and discover (.86)/ ai means of annoyed (.97) and discover (.87)
round(agr$mean_proj,2)
#only   discover  know  stop  stupid  NRRC  annoyed NomApp  possNP
#0.76   0.86      0.92  0.87  0.85    0.96  0.96    0.95    0.94

ggplot(agr, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating ('asking whether')") +
  ylab("Mean projectivity rating") +
  xlim(0.65,1) +
  ylim(0.65,1)
ggsave(file="graphs/ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)

# paper figure 4
t_nomc$Item = as.factor(paste(t_nomc$short_trigger, t_nomc$content))
examples = t_nomc %>%
  filter(Item %in% c("stupid cheat", "NRRC aunt"))
ggplot(examples, aes(x=ai,y=projective)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  xlab("Not-at-issueness rating") +
  ylab("Projectivity rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~Item)
ggsave("graphs/subject-projai-examples.pdf",width=5,height=2.5)

# paper figure 10a (mean at-issueness ratings by target expression)
mean_nai = aggregate(ai~short_trigger, data=t, FUN="mean")
mean_nai$YMin = mean_nai$ai - aggregate(ai~short_trigger, data=t, FUN="ci.low")$ai
mean_nai$YMax = mean_nai$ai + aggregate(ai~short_trigger, data=t, FUN="ci.high")$ai
mean_nai
t$trigger_ai <-factor(t$short_trigger, levels=mean_nai[order(mean_nai$ai), "short_trigger"])

ggplot(t, aes(x=trigger_ai, y=ai)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness rating \n ('asking whether')")+
  xlab("Expression")
ggsave(f="graphs/boxplot-not-at-issueness-with-MCs.pdf",height=3,width=6.5)

# paper figure A1 (by-item projectivity against at-issueness ratings)
ggplot(t_nomc, aes(x=ai,y=projective,color=Trigger)) +
  geom_smooth(method="lm") +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Not-at-issueness rating") +
  ylab("Projectivity rating") +
  xlim(0,1) +
  ylim(0,1) +
  facet_wrap(~Item,nrow=8,ncol=6) +
  theme(legend.position="top") +
  guides(colour = guide_legend(nrow = 1))
ggsave("graphs/subject-variability-aiproj-exp1a.pdf",width=9,height=12.3)

# additional plot: by-item means
agr = t_nomc %>%
  group_by(content,Trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai

ggplot(agr, aes(x=mean_ai,y=mean_proj,color=Trigger)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.4,1) +
  ylim(0.4,1)


# additional plots: block effect
agr = t_nomc %>%
  group_by(Trigger,block_ai) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai
agr$Order = ifelse(agr$block_ai=="block1","ai-proj","proj-ai")

ggplot(agr, aes(x=mean_ai,y=mean_proj,color=Trigger,group=Order)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  geom_smooth(method="lm") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.55,1) +
  ylim(0.55,1) +
  facet_wrap(~Order)


