# set working directory to point to 'rscripts' (default when opening file in RStudio)

## code for both starts here
source('../../helpers.R')

# load required packages for pre-processing data
require(tidyverse)

# read in the raw data
d = readRDS(file="../data/d.rds")

# look at Turkers' comments
unique(d$comments)

# age info
table(d$age) #18-74
median(d$age) #32

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 4 to 44 
#(3 instructions at beginning, 24 missing because another instruction)
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) # trial numbers from 1 to 41 (21 missing because instruction)
d[d$trial > 20,]$trial = d[d$trial > 20,]$trial - 1
unique(d$trial) # trials from 1 to 40

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# anglish         Chinese         ebglish         english 
# 40              40              40            2360 
# English         ENGLISH        english         English  
# 6960             160              40             120 
# englush         Englush         Enlgish French, English 
# 40              40              40              40 
# Russian          telugu 
# 40              40 
d <- subset(d, (d$language != "Chinese" & d$language != "Russian" 
                & d$language != "telugu"))                  
d = droplevels(d)
length(unique(d$workerid)) #247 (data from 3 Turker excluded, 247 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))#247
length(which(is.na(d$ame))) #0 (everybody responded)
table(d$ame) 
# Yes 
# 9880
# d <- subset(d, d$ame == "Yes")
# d = droplevels(d)
# length(unique(d$workerid)) #no Turkers excluded

#247 remaining Turkers

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #3952 (247 Turkers x 8 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- subset(d.MC, d.MC$question_type == "projective")
d.MC.Proj <- droplevels(d.MC.Proj)
nrow(d.MC.Proj) #1976

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2)

# calculate each Turkers mean response to the projection of main clauses
p.means = aggregate(response~workerid, data=d.MC.Proj, FUN="mean")
p.means$YMin = p.means$response - aggregate(response~workerid, data=d.MC.Proj, FUN="ci.low")$response
p.means$YMax = p.means$response + aggregate(response~workerid, data=d.MC.Proj, FUN="ci.high")$response
p.means

ggplot(p.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- subset(d.MC, d.MC$question_type == "ai")
d.MC.AI <- droplevels(d.MC.AI)
nrow(d.MC.AI) #1976

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2)

# calculate each Turkers mean response to the projection of main clauses
ai.means = aggregate(response~workerid, data=d.MC.AI, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC.AI, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC.AI, FUN="ci.high")$response
ai.means

ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 3
# standard deviations away from the overall mean

# get the Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$response > (mean(p.means$response) + 3*sd(p.means$response)),]
p

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% p$workerid | d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #192 (12 unique outlier Turkers x 16 = 8 main clauses x 2 questions)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% p$workerid | d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) # 235 remaining Turkers (12 Turkers excluded)

# write cleaned dataset to file
write.csv(d[,c(1,2,3,4,5,6,7,8,9,12,51)], file="data/data_preprocessed.csv",row.names=F,quote=F)
