####set things up####
#load libraries
library(reshape)
library(dplyr)

dat = read.csv("raw 2_11.csv")

summary(dat)
table(dat$ExperimentName)
table(dat$Procedure.Trial.)

#remove practice
dat = subset(dat,
             dat$Procedure.Trial. != "Practice1")
D = dat

D$Direction = as.numeric(D$Direction)

D.JOL3 = subset(D,
                D$Direction != "1")

D.JOL3$Direction = factor(D.JOL3$Direction,
                          levels = c(2,3,4,5),
                          labels = c("B", "F", "S", "U"))
summary(D)

D.recall = subset(D,
                  D$Procedure.Trial. == "recallproc1" | D$Procedure.Trial. == "recallproc2")
D.JOL1 = subset(D.JOL3,
                D.JOL3$Procedure.Trial. == "StudyProc3" | D.JOL3$Procedure.Trial. == "StudyProc5")
D.JOL2 = subset(D.JOL3,
                D.JOL3$Procedure.Trial. == "StudyProc7" | D.JOL3$Procedure.Trial. == "StudyProc8")

##Next, get things sorted
#jols
#block1
D.JOL1 = D.JOL1[order(D.JOL1$Subject, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$ListNum, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$Subject, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$Block, decreasing = FALSE), ]

#block 2
D.JOL2 = D.JOL2[order(D.JOL2$Subject, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$ListNum, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$Subject, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$Block, decreasing = FALSE), ]

#recall
D.recall = D.recall[order(D.recall$Subject, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$ListNum, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$Subject, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$Block, decreasing = FALSE), ]

#put back together
D.JOL = rbind(D.JOL2, D.JOL1)
D.JOL$Type = rep("JOL")

#sort the combined JOL
D.JOL = D.JOL[order(D.JOL$ListNum, decreasing = FALSE), ]
D.JOL = D.JOL[order(D.JOL$Subject, decreasing = FALSE), ]
D.JOL = D.JOL[order(D.JOL$Block, decreasing = FALSE), ]

#combined jols and recall
D.recall$Type = rep("recall")

D = rbind(D.recall, D.JOL)

#combined jols and recall
D.recall$Type = rep("recall")

D = rbind(D.recall, D.JOL)

##Now separate jol and recall trials
D.recall = subset(D,
                  D$Type == "recall")
D.JOL = subset(D,
               D$Type == "JOL")

##split JOL and recall by block
#JOLs

#drop unused columns (recall)
D.JOL = D.JOL[ , -c(13:15)]

#now subset
D1 = subset(D.JOL,
            D.JOL$Block == 1)
D2 = subset(D.JOL,
            D.JOL$Block == 2)

#Recall
#drop jol columns
D.recall = D.recall[ , -c(9:12, 16:19)]

#subset
D3 = subset(D.recall,
            D.recall$Block == 1)
D4 = subset(D.recall,
            D.recall$Block == 2)

#now get all jols and RTs/ recall responses into one column
#time for more subsetting!

##block 1
#study proc 3
proc3 = subset(D1,
               D1$Procedure.Trial. == "StudyProc3")
#study proc 7
proc7 = subset(D1,
               D1$Procedure.Trial. == "StudyProc7")

##block 2
#study proc 5
proc5 = subset(D2,
               D2$Procedure.Trial. == "StudyProc5")

#study proc 8
proc8 = subset(D2,
               D2$Procedure.Trial. == "StudyProc8")

##now start removing unused columns
#proc 3
proc3 = proc3[ , -c(11:16)]
colnames(proc3)[9:10] = c("JOL", "RT")

#proc 5
proc5 = proc5[ , -c(9:10, 13:16)]
colnames(proc5)[9:10] = c("JOL", "RT")

#proc 7
proc7 = proc7[ , -c(9:12, 15:16)]
colnames(proc7)[9:10] = c("JOL", "RT")

#proc 8
proc8 = proc8[ , -c(9:14)]
colnames(proc8)[9:10] = c("JOL", "RT")

##PUT THEM BACK TOGETHER!
JOLs = rbind(proc3, proc7, proc5, proc8)

#sort
JOLs = JOLs[order(JOLs$ListNum, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Subject, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Block, decreasing = FALSE), ]

#recall
recall2 = D4[ - c(7, 9, 10)]
colnames(recall2)[8] = "Response"

recall1 = D3[ - c(7, 10, 11)]
colnames(recall1)[8] = "Response"

##put back together
recall = rbind(recall1, recall2)

#now combine JOLs and recall
combinedD = cbind(JOLs, recall)
combinedD = combinedD[ , -c(13:16)]

write.csv(combinedD, file = "Relational_D.csv", row.names = F)
