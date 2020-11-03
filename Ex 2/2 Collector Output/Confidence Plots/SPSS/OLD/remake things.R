library(readxl)

no_warning = read_xlsx("No Warning Conf Plots.xlsx", sheet = "Master")
warning = read_xlsx("Warning Conf Plots.xlsx", sheet = "Master")

##Start w/ the no warning data
table(no_warning$JOL)
summary(no_warning$JOL)

no_warning$JOL = as.numeric(no_warning$JOL)

table(no_warning$JOL)

no_warning$JOL[no_warning$JOL > 100] = NA

no_warning$JOL_rounded = round(no_warning$JOL, -1)

##now do the warning data
table(warning$JOL)
summary(warning$JOL)

warning$JOL = as.numeric(warning$JOL)

table(warning$JOL)

warning$JOL[warning$JOL > 100] = NA

warning$JOL_rounded = round(warning$JOL, -1)

##Add in labels
no_warning$warning = rep("no")
warning$warning = rep("yes")

combined = rbind(no_warning, warning)

##Cut out the bad participants
combined = subset(combined,
                  combined$Username != "5b5b9fdfe")

combined = subset(combined,
                  combined$Username != "5e17594")

combined = subset(combined,
                  combined$Username != "5e605068")

combined = subset(combined,
                  combined$Username != "w10015978cjb")

combined = subset(combined,
                  combined$Username != "w843943JC")

combined = subset(combined,
                  combined$Username != "w993002_MR")

combined = subset(combined,
                  combined$Username != "w10084909JR")

combined = subset(combined,
                  combined$Username != "w917542_rm")

combined = subset(combined,
                  combined$Username != "w979990amf")

combined = subset(combined,
                  combined$Username != "w10016099CJ")

combined = subset(combined,
                  combined$Username != "w10021803mm")

combined = subset(combined,
                  combined$Username != "w982395_JEM")

####Now make subsets####
unique(combined$Condition.Description)

IS = subset(combined,
            combined$Condition.Description == "ITEM SPECIFIC")

RL = subset(combined,
            combined$Condition.Description == "RELATIONAL")

READ = subset(combined,
              combined$Condition.Description == "READ")

##Write subsets to .csv
#write.csv(IS, file = "is_11_3.csv", row.names = F)
#write.csv(RL, file = "rl_11_3.csv", row.names = F)
#write.csv(READ, file = "read_11_3.csv", row.names = F)
