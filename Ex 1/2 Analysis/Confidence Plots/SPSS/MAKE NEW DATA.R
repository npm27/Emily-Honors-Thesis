library(readxl)

dat = read_xlsx("Ex1 Raw.xlsx", sheet = "Master Data Sheet")

table(dat$JOL)

dat$JOL = as.numeric(dat$JOL)

table(dat$JOL)

dat$JOL[dat$JOL > 100] = NA

round(dat$JOL, -1)

dat$JOL_rounded = round(dat$JOL, -1)

##Okay, add in encoding condition##
dat$condition = substr(dat$ExperimentName, start = 1, stop = 2)

dat$condition[dat$condition == "RE"] = "READ"

table(dat$condition)

#install.packages("pivottabler")

#Make a pivot table
library(pivottabler)

RL = subset(dat,
            dat$condition == "RL")
READ = subset(dat,
              dat$condition == "READ")
IS = subset(dat,
             dat$condition == "IS")

#Write subsets to .csv
write.csv(RL, file = "RL_11_02.csv")
write.csv(IS, file = "IS_11_02.csv")
write.csv(READ, file = "READ_11_02.csv")
