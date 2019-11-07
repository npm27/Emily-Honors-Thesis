##set up
dat = read.csv("auto score 11_4.csv")

#get n
length(unique(dat$Subject))

library(reshape)

##quick data screening
summary(dat)
table(dat$Subject)

##fix out of range JOLs
dat$JOL[ dat$JOL > 100] = NA
summary(dat$JOL)

##put recall on same scale as JOLs
dat$scored = dat$scored * 100
summary(dat$scored)

##remove NAs
dat = na.omit(dat)

##check descriptives
#JOLs
tapply(dat$JOL,
       list(dat$ExperimentName, dat$Direction), mean)

#Recall
tapply(dat$scored,
        list(dat$ExperimentName, dat$Direction), mean)

#split by condition and add column for condition type
read = subset(dat,
              dat$ExperimentName == "READ_JOL A" | dat$ExperimentName == "READ_JOL B" | dat$ExperimentName == "READ_JOL C" | dat$ExperimentName == "READ_JOL D")
read$type = rep("READ")

RL = subset(dat,
            dat$ExperimentName == "RL_JOL A" | dat$ExperimentName == "RL_JOL B" | dat$ExperimentName == "RL_JOL C" | dat$ExperimentName == "RL_JOL D")
RL$type = rep("RL")

IS = subset(dat,
            dat$ExperimentName == "IS_JOL A" | dat$ExperimentName == "IS_JOL B" | dat$ExperimentName == "IS_JOL C" | dat$ExperimentName == "IS_JOL D")
IS$type = rep("IS")

#put the things back together
combined = rbind(read, RL, IS)

##lets try descriptives again!
#JOLs
tapply(combined$JOL,
       list(combined$type, combined$Direction), mean)

#Recall
tapply(combined$scored,
       list(combined$type, combined$Direction), mean)
