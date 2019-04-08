# set working directory to point to 'rscripts' (default when opening file in RStudio)

# load required packages
require(tidyverse)
library(ggrepel)
library(lsmeans)
library(lme4)
library(lmerTest)

# load helper functions
source('../../helpers.R')

d = read.csv("../data/data_preprocessed.csv")

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = as.factor(ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1")))) %>%
  select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)

# exclude main clauses (fillers)
t_nomc = droplevels(subset(t, short_trigger != "MC"))

# center the block and at-issueness variables
t_nomc = cbind(t_nomc,myCenter(t_nomc[,c("block_ai","ai")]))

# reorder trigger levels
t_nomc$Trigger = factor(x=as.character(t_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# main analysis of interest: predict projectivity from at-issueness, while controlling for block (random effects by subject, lexical content, and target expression)

# the model reported in the paper:
m.mr.1 = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.1)

# get p-values via likelihood ratio tests
m.mr.0a = lmer(projective ~ cai + cblock_ai + (1+cai|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0a)

m.mr.0b = lmer(projective ~ cai + cai : cblock_ai + (1+cai|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0b)

m.mr.0c = lmer(projective ~ cblock_ai + cai : cblock_ai + (1+cai|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0c)

anova(m.mr.0a,m.mr.1) # p-value for interaction
anova(m.mr.0b,m.mr.1) # p-value for block
anova(m.mr.0c,m.mr.1) # p-value for at-issueness

# get p-values for random effects
m.0a = lmer(projective ~ cai * cblock_ai + (0+cai|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0b = lmer(projective ~ cai * cblock_ai + (1|workerid) + (1+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0c = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0d = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (1|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0e = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (1+cai|content) + (0+cai|short_trigger), data=t_nomc, REML=F)
m.0f = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (1+cai|content) + (1|short_trigger), data=t_nomc, REML=F)

anova(m.0a,m.mr.1) # p-value for by-participant intercepts
anova(m.0b,m.mr.1) # p-value for by-participant slopes for at-issueness
anova(m.0c,m.mr.1) # p-value for by-content intercepts
anova(m.0d,m.mr.1) # p-value for by-content slopes for at-issueness
anova(m.0e,m.mr.1) # p-value for by-trigger intercepts
anova(m.0f,m.mr.1) # p-value for by-trigger slopes for at-issueness

# pairwise comparisons on short_trigger using tukey (reported in table 3)
# run the model again with trigger as fixed effect so you can do multiple comparisons (with no at-issueness or block effects)
m.mr.fixedtrigger = lmer(projective ~ short_trigger + (1|workerid), data=t_nomc, REML=F)

pc = lsmeans(m.mr.fixedtrigger, revpairwise~short_trigger, adjust="tukey")
pc
