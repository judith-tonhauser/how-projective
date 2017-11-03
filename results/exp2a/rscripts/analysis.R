# set working directory, e.g.
# setwd('/Users/judith/how-projective/results/exp2a/')
setwd("")

# load required packages
require(tidyverse)
library(lsmeans)
library(simr)
library(lmerTest)

# load helper functions
source('../helpers.R')

# load data
t = read.csv("data/data_preprocessed.csv")

# aggregate responses by trigger and content for merging in means from exp 1a to run regression analysis
tagr = t %>%
  group_by(short_trigger, content) %>%
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

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# run models and get p-values via likelihood ratio tests
m = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m)

m.0 = lmer(mean_proj ~ (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.0)

m.1 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1|content), data = means_nomc, REML=F)
summary(m.1)

m.2 = lmer(mean_proj ~ cmean_ai + (1|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML = F)
summary(m.2)

m.3 = lmer(mean_proj ~ cmean_ai + (0+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.3)

m.4 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (0+cmean_ai|content), data = means_nomc, REML=F)
summary(m.4)

m.report.0 = lmer(mean_proj ~ 1 + (1|content), data = means_nomc, REML=F)
summary(m.report.0)

anova(m.1,m) # p-value for by-content slope -- ns
anova(m.2,m) # p-value for by-trigger slope -- ns
anova(m.3,m) # p-value for by-trigger intercept -- ns
anova(m.4,m) # p-value for by-content intercept -- .02
anova(m.0,m) # p-value for at-issueness -- .06

# so, only include by-content intercepts. this is the model reported in the paper:
m.report = lmer(mean_proj ~ cmean_ai + (1|content), data = means_nomc, REML=F)
summary(m.report)

anova(m.report.0,m.report) # p-value for at-issueness -- .0001  

# power analysis

# effect sizes of at-issueness in exp. 1a and 1b, for comparison: .37, .34
# effect sizes in exp. 2a and 2b: .25/.29 (depending on random effects structure)

# we analyze the power of two different models: 
# m -- has full maximal random effects structure and detected only marginally significant effect of at-issueness (beta = .25)
fixef(m)["cmean_ai"]
powerSim(m) # power: 60.40% (57.29, 63.45)

# m.report -- has only by-content random intercepts after model comparison (beta = .29)
fixef(m.report)["cmean_ai"]
powerSim(m.report) # power: 99.80% (99.28, 99.98)
