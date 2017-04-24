
## JD working directory
setwd('/Users/titlis/cogsci/projects/stanford/projects/projection-NAI-variability/results/ai-meta-analysis/')
## JT working directory
setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/Git-variability/results/ai-meta-analysis/')

library(ggrepel)

## code for both starts here
source('rscripts/helpers.R')

# read in the data
exp1a = readRDS(file="../exp1a/data/t.rds") %>%
  select(ai,short_trigger,content) %>%
  mutate(SubExperiment="a")
exp1b = readRDS(file="../exp1b/data/t.rds") %>%
  select(ai,short_trigger,content) %>%
  mutate(SubExperiment="b")
exp2a = readRDS(file="../exp2a/data/t.rds") %>%
  select(response,short_trigger,content) %>%
  mutate(SubExperiment="a")
exp2b = readRDS(file="../exp2b/data/t.rds") %>%
  select(response,short_trigger,content) %>%
  mutate(SubExperiment="b")
nrow(exp1a)
nrow(exp1b)
nrow(exp2a)
nrow(exp2b)

# exp1a$Experiment = "1"
# exp1b$Experiment = "1"
# exp2a$Experiment = "2"
# exp2b$Experiment = "2"

exp1 = rbind(exp1a,exp1b)
exp2 = rbind(exp2a,exp2b)

agr1 = exp1 %>%
  group_by(short_trigger,SubExperiment) %>%
  summarise(mean_exp1 = mean(ai), ci.low_exp1=ci.low(ai), ci.high_exp1=ci.high(ai))
agr2 = exp2 %>%
  group_by(short_trigger,SubExperiment) %>%
  summarise(mean_exp2 = mean(response), ci.low_exp2=ci.low(response), ci.high_exp2=ci.high(response))

d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .62
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

ggplot(d, aes(x=mean_exp1,y=mean_exp2,color=SubExperiment)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.5,color="gray") +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.5,color="gray") +
  geom_point() +
  xlab("Not-at-issueness ('asking whether', Exp. 1)") +
  ylab("Not-at-issueness ('are you sure', Exp. 2)") +
  xlim(.3,1) +
  ylim(.3,1)
ggsave("graphs/correlation-bytrigger.pdf",width=6,height=4)

# how much does exp 1 measure explain exp 2 measure?
m = lm(mean_exp2 ~ 1+mean_exp1, data = d)
summary(m)

# aggregate by both trigger and content
agr1 = exp1 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp1 = mean(ai), ci.low_exp1=ci.low(ai), ci.high_exp1=ci.high(ai))
agr2 = exp2 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp2 = mean(response), ci.low_exp2=ci.low(response), ci.high_exp2=ci.high(response))

d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","content","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .31
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

ggplot(d, aes(x=mean_exp1,y=mean_exp2,color=short_trigger)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  # geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.5,color="gray") +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.5,color="gray") +
  geom_point() +
  xlab("Not-at-issueness ('asking whether', Exp. 1)") +
  ylab("Not-at-issueness ('are you sure', Exp. 2)") +
  xlim(0,1) +
  ylim(0,1)
ggsave("graphs/correlation-bytrigger-bycontent.pdf",width=7,height=4)