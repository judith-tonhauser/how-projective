# set working directory to point to 'rscripts' (default when opening file in RStudio)

# load required packages
require(tidyverse)

# load helper functions
source('../../helpers.R')

# read in the rawdata
d = readRDS(file="../data/d.rds")

# look at Turkers' comments
unique(d$comments)

# age info
table(d$age) #20-77
# convert 3' into 30
d$age <- gsub("\`","0",d$age)
# change into numeric
d$age <- as.numeric(d$age)
median(d$age) #30
mean(d$age) #33.1

# change the response so that what was 1/at-issue is now 0/at-issue
# by subtracting the responses from 1
table(d$short_trigger,d$response)
d$response = 1 - d$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 3 to 17 (2 instruction slides at beginning)
d$trial = d$slide_number_in_experiment - 2
unique(d$trial) # trial numbers from 1 to 15

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# englih    english    eNGLISH    English    ENGLISH 
# 15       1050         15       2490         60 
# english    English     englsih     Enlish   mandarin 
# 15         30         15         15         15 
# Tamil Vietnamese 
# 15         15
d <- subset(d, (d$language != "Tamil" & d$language != "Vietnamese" 
                & d$language != "mandarin"))
d = droplevels(d)
length(unique(d$workerid)) #247 (data from 3 Turkers excluded, 247 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid)) #247
length(which(is.na(d$ame))) #15 (one Turker did not respond)
table(d$ame) 
#No  Yes 
#30 3660
d <- subset(d, d$ame == "Yes") #(also exclude the Turker who did not respond)
d = droplevels(d)
length(unique(d$workerid)) #244 (data from 3 Turkers excluded, 244 remaining Turkers)

# exclude Turkers based on main clause controls

# get main clause trials
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #1464 (244 Turkers x 6 MCs)

# group projection mean (all Turkers, all clauses)
round(mean(d.MC$response),2)

# calculate each Turker's mean response to the not-at-issueness of main clauses
ai.means = aggregate(response~workerid, data=d.MC, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC, FUN="ci.high")$response
ai.means

ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# identify Turkers whose response mean on NAI of main clauses is more than 3
# standard deviations higher than the mean 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #36 (6 unique outlier Turkers x 6 main clauses)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #238 remaining Turkers, 6 excluded

# write cleaned dataset to file
write.csv(d[,c(1,2,3,4,5,6,7,8,9,11,50)], file="data/data_preprocessed.csv",row.names=F,quote=F)
