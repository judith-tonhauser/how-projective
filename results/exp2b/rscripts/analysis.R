# set working directory, e.g.
# setwd('/Users/judith/how-projective/results/exp2b/')
setwd("")

# load required packages
require(tidyverse)
library(simr)
library(rms)
library(lmerTest)

# load helper functions
source('../helpers.R')

# load data
t = read.csv("data/data_preprocessed.csv")

# aggregate responses by trigger and content for merging in means from exp 1b to run regression analysis
tagr = t %>%
  group_by(short_trigger, content) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# load and aggregate data from exp 1b to merge in projectivity means
t.proj <- read.csv(file="../exp1b/data/data_preprocessed.csv")

tagr.proj = t.proj %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response) %>%
  group_by(short_trigger, content) %>%
  summarise(mean_proj = mean(projective), ci_low_proj = ci.low(projective), ci_high_proj = ci.high(projective))
tagr.proj = as.data.frame(tagr.proj)
tagr.proj$ci_min_proj = tagr.proj$mean_proj - tagr.proj$ci_low_proj
tagr.proj$ci_max_proj = tagr.proj$mean_proj + tagr.proj$ci_high_proj
head(tagr.proj)

means = tagr %>%
  select(short_trigger,content,mean_ai,ci_min_ai,ci_max_ai) %>%
  inner_join(tagr.proj[,c("short_trigger","content","mean_proj","ci_min_proj","ci_max_proj")],by=c("short_trigger","content"))

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc)
means_nomc$cmean_ai = myCenter(means_nomc$mean_ai)

means_nomc$Trigger = factor(x=ifelse(means_nomc$short_trigger == "established","establish",ifelse(means_nomc$short_trigger == "confessed","confess",ifelse(means_nomc$short_trigger == "revealed","reveal",ifelse(means_nomc$short_trigger == "discovered","discover",ifelse(means_nomc$short_trigger == "learned","learn",ifelse(means_nomc$short_trigger == "found_out","find_out",ifelse(means_nomc$short_trigger == "saw","see",ifelse(means_nomc$short_trigger == "is_amused","amused",ifelse(means_nomc$short_trigger == "realize","realize",ifelse(means_nomc$short_trigger == "is_aware","aware",ifelse(means_nomc$short_trigger == "noticed","notice",ifelse(means_nomc$short_trigger == "is_annoyed","annoyed","NA")))))))))))),levels=c("establish","confess","reveal","discover","learn","find_out","see","amused","realize","aware","notice","annoyed"))

# run models and get p-values via likelihood ratio tests
m = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m)

m.0 = lmer(mean_proj ~  (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.0)

m.1 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1|content), data = means_nomc, REML=F)
summary(m.1)

m.2 = lmer(mean_proj ~ cmean_ai + (1|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML = F)
summary(m.2)

m.3 = lmer(mean_proj ~ cmean_ai + (0+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.3)

m.4 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (0+cmean_ai|content), data = means_nomc, REML=F)
summary(m.4)

anova(m.1,m) # p-value for by-content slope -- ns
anova(m.2,m) # p-value for by-trigger slope -- ns
anova(m.3,m) # p-value for by-trigger intercept -- .0001
anova(m.4,m) # p-value for by-content intercept -- .ns
anova(m.0,m) # p-value for by-content intercept -- .ns

# so, only include by-trigger intercepts. this is the model reported in the paper:
m.report = lmer(mean_proj ~ cmean_ai + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report)

# test restricted cubic splines for at-issueness -- ns
m = lmer(mean_proj ~ rcs(cmean_ai,3) + (1|short_trigger), data = means_nomc, REML=F)
summary(m)

m.report = lmer(mean_proj ~ cmean_ai + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report)

m.report.0 = lmer(mean_proj ~ 1 + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report.0)

anova(m.report.0,m.report) # p-value for at-issueness -- ns

m.reportcontent = lmer(mean_proj ~ cmean_ai + (1|short_trigger) + (1|content), data = means_nomc, REML=F)
summary(m.reportcontent)

m.norandomtrigger = lmer(mean_proj~cmean_ai + (1|content),data=means_nomc)
summary(m.norandomtrigger)

anova(m.norandomtrigger,m.reportcontent) # p-value for random effect (a little dodgy because by-content intercept included in comparison, but because there's no variation anyway it doesn't matter)

# at-issueness significant in model with no random effects
m.norandom = lm(mean_proj~cmean_ai,data=means_nomc)
summary(m.norandom)

# power analysis -- definitely enough power
# effect sizes of at-issueness in exp. 1a and 1b, for comparison: .37, .34. in exp. 2a: .29. let's test for power of m.report to detect effect size of .29.
fixef(m.report)["cmean_ai"] = .29
fixef(m.report)["cmean_ai"]
powerSim(m.report) # power:  100% (99.63, 100)

fixef(m)["cmean_ai"] = .29
fixef(m)["cmean_ai"]
powerSim(m) # power:  100% (99.63, 100)
