# set working directory, e.g.
# setwd('/Users/judith/how-projective/results/exp2a/')
setwd("")

# load required packages
require(tidyverse)

# load helper functions
source('../helpers.R')

# set black and white plot background
theme_set(theme_bw())

# load data
t = read.csv("data/data_preprocessed.csv")

# aggregate responses by trigger for merging in means from exp 1a to plot
tagr = t %>%
  group_by(short_trigger) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# load and aggregate data from exp 1a to merge in projectivity means
t.proj <- read.csv(file="../exp1a/data/data_preprocessed.csv")
tagr.proj = t.proj %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response) %>%
  group_by(short_trigger) %>%
  summarise(mean_proj = mean(projective), ci_low_proj = ci.low(projective), ci_high_proj = ci.high(projective))
tagr.proj = as.data.frame(tagr.proj)
tagr.proj$ci_min_proj = tagr.proj$mean_proj - tagr.proj$ci_low_proj
tagr.proj$ci_max_proj = tagr.proj$mean_proj + tagr.proj$ci_high_proj
head(tagr.proj)

means = tagr %>%
  select(short_trigger,mean_ai,ci_min_ai,ci_max_ai) %>%
  inner_join(tagr.proj[,c("short_trigger","mean_proj","ci_min_proj","ci_max_proj")],by=c("short_trigger"))

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc) 

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# uncollapsed correlation coefficient reported in paper
cor(means_nomc$mean_ai,means_nomc$mean_proj)

# figure 8
ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  geom_point() +
  # geom_smooth(method="lm") +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating ('are you sure')") +
  ylab("Mean projectivity rating") +
  xlim(0.3,1) +
  ylim(0.3,1)
ggsave(file="graphs/ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)

# aggregate responses by trigger and content for merging in means from exp 1a to get uncollapsed correlation coefficient
tagr = t %>%
  group_by(short_trigger, content) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# aggregate data from exp 1a to merge in projectivity means 
tagr.proj = t.proj %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response) %>%
  group_by(short_trigger, content) %>%
  summarise(mean_proj = mean(projective), ci_low_proj = ci.low(projective), ci_high_proj = ci.high(projective))
tagr.proj = as.data.frame(tagr.proj)
tagr.proj$ci_min_proj = tagr.proj$mean_proj - tagr.proj$ci_low_proj
tagr.proj$ci_max_proj = tagr.proj$mean_proj + tagr.proj$ci_high_proj

means = tagr %>%
  select(short_trigger,content,mean_ai,ci_min_ai,ci_max_ai) %>%
  inner_join(tagr.proj[,c("short_trigger","content","mean_proj","ci_min_proj","ci_max_proj")],by=c("short_trigger","content"))

nrow(means) # this only leaves 60 data points for analysis (of which 17 are MCs, i.e., only 43)

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc)
means_nomc$cmean_ai = myCenter(means_nomc$mean_ai)

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# correlation coefficient reported in paper
cor(means_nomc$mean_ai,means_nomc$mean_proj)

# figure 10b
mean_nai = aggregate(response~short_trigger, data=t, FUN="mean")
mean_nai$YMin = mean_nai$response - aggregate(response~short_trigger, data=t, FUN="ci.low")$response
mean_nai$YMax = mean_nai$response + aggregate(response~short_trigger, data=t, FUN="ci.high")$response
mean_nai

t$trigger_ai <-factor(t$short_trigger, levels=mean_nai[order(mean_nai$response), "short_trigger"])

ggplot(t, aes(x=trigger_ai, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness rating \n ('are you sure')")+
  xlab("Expression")
ggsave(f="graphs/boxplot-not-at-issueness-with-MCs.pdf",height=3,width=6.5)
