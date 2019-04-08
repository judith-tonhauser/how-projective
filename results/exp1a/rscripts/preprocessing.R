# set working directory to point to 'rscripts' (default when opening file in RStudio)

# load helper functions
source('../../helpers.R')

# load required packages for pre-processing data
require(tidyverse)

# read in the raw data
d = read_csv(file="../data/experiment.csv")

# look at Turkers' comments
unique(d$comments)

# ages
table(d$age) #19-71
median(d$age) #33

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue, by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 4 to 34 (3 instructions at beginning, 19 missing because another instruction)
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) # trial numbers from 1 to 31 (16 missing because instruction)
d[d$trial > 15,]$trial = d[d$trial > 15,]$trial - 1
unique(d$trial) # trials from 1 to 30

# exclude non-English speakers and non-American English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# american english   American English   Chinese, English             Creole 
# 30                 30                 30                 30 
# Elish            emglish            Eng;osj            Engligh 
# 30                 30                 30                 30 
# english            English            ENGLISH           english  
# 1950               4740                150                 30 
# English           ENGLISH              Englush            Enlgish 
# 60                 30                 30                 30 
# ENLGISH             french German and English            Russian 
# 30                 30                 30                 30 
# Spanish               Thai         vietnamese 
# 60                 30                 30
d <- subset(d, (d$language != "french" & d$language != "Russian" & d$language != "Creole" &
                  d$language != "Spanish" & d$language != "Thai" & d$language != "vietnamese"))
d = droplevels(d)
length(unique(d$workerid)) #243 (data from 7 Turker excluded, 243 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))#243
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 
# No  Yes 
# 660 6630
d <- subset(d, d$american == "Yes")
d = droplevels(d)
length(unique(d$workerid)) #221 (data from 22 Turkers excluded, 221 remaining Turkers)

# exclude Turkers based on main clause controls --- XXX criterion

# get main clause trials
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #2652 (221 Turkers x 6 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- subset(d.MC, d.MC$question_type == "projective")
d.MC.Proj <- droplevels(d.MC.Proj)
nrow(d.MC.Proj) #1326

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2)

# calculate each Turker's mean response to the projection of main clauses
p.means = aggregate(response~workerid, data=d.MC.Proj, FUN="mean")
p.means$YMin = p.means$response - aggregate(response~workerid, data=d.MC.Proj, FUN="ci.low")$response
p.means$YMax = p.means$response + aggregate(response~workerid, data=d.MC.Proj, FUN="ci.high")$response

# plot means
ggplot(p.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# at-issueness of main clause data
d.MC.AI <- subset(d.MC, d.MC$question_type == "ai")
d.MC.AI <- droplevels(d.MC.AI)
nrow(d.MC.AI) #1326

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2)

# calculate each Turker's mean response to the projection of main clauses
ai.means = aggregate(response~workerid, data=d.MC.AI, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC.AI, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC.AI, FUN="ci.high")$response

# plot means
ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# get the outlier Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$response > (mean(p.means$response) + 3*sd(p.means$response)),]
p

# get the outlier Turkers who are more than 3 standard deviations above the mean on at-issueness 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% p$workerid | d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) # 210 remaining Turkers (11 Turkers excluded)

# write cleaned dataset to file
write.csv(d[,c(1,2,3,4,5,6,7,8,9,12,51)], file="../data/data_preprocessed.csv",row.names=F,quote=F)
