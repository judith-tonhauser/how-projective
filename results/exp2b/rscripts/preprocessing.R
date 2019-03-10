# set working directory to point to 'rscripts' (default when opening file in RStudio)

# load required packages
require(tidyverse)

# load helper functions
source('../../helpers.R')

# read in the rawdata
d = readRDS(file="../data/d.rds")

# age info
table(d$age) #18-77
median(d$age) #29
mean(d$age) #31.4

# look at Turkers' comments
unique(d$comments)

# change the response so that what was 1/at-issue is now 0/at-issue
# by subtracting the responses from 1
table(d$short_trigger,d$response)
d$response = 1 - d$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 3 to 22 
# (2 instruction slides at beginning)
d$trial = d$slide_number_in_experiment - 2
unique(d$trial) # trial numbers from 1 to 20

# exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# Cantonese             egnlish              Engish 
# 20                  20                  20 
# english             English            English  
# 1320                3500                  20 
# English and Spanish              Korean             Russian 
# 20                  20                  20 
# Spanish          Vietnamese 
# 20                  20 
d <- subset(d, (d$language != "Cantonese" & d$language != "Vietnamese" 
                & d$language != "Korean" & d$language != "Russian"
                & d$language != "Spanish"))
d = droplevels(d)
length(unique(d$workerid)) #245 (data from 5 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #245
length(which(is.na(d$ame))) #20 (one Turker did not respond)
table(d$ame) 
# Yes 
# 4880
d <- subset(d, d$ame == "Yes") #(exclude the Turker who did not respond)
d = droplevels(d)
length(unique(d$workerid)) #244 (data from 1 Turkers excluded)

# exclude Turkers based on main clause controls

# get main clause trials
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #1952 (244 Turkers x 8 MCs)

# group projection mean (all Turkers, all clauses)
round(mean(d.MC$response),2)

# calculate each Turkers mean response to the not-at-issueness of main clauses
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
nrow(outliers) #48 (6 outlier Turkers x 8 main clauses)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #238 remaining Turkers 

# write cleaned dataset to file
write.csv(d[,c(1,2,3,4,5,6,7,8,9,11,50)], file="data/data_preprocessed.csv",row.names=F,quote=F)
