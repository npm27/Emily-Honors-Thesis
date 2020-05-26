##Set up
dat = read.csv("Ex 2 Part 1 Processed.csv")

library(lrd)

length(unique(dat$Subject)) #17 total subjects

summary(dat)

#Fix out of range JOL scores
dat$JOL[dat$JOL > 100] = NA

summary(dat)

##Let's score some data!
#note that the two scoring functions are coming from QuickScore
#I haven't hosted this thing online yet and the package isn't finished
#So these are just two functions stored in a random place on my laptop

#First, subset the data for what is needed
ID = dat$Subject
answer = dat$Response

#Now i need to get the key
#can do this by taking the cue-target pairs and cutting out everything but the target
dat$cue_target =  sub("^[^-]*", "", dat$cue_target)
dat$cue_target = substring(dat$cue_target, 3)

key = dat$cue_target

#Now make sure everything is lowercase
key = tolower(key)
answer = tolower(as.character(answer))

#Okay! Now run the scoring functions
output = percent_match(answer, key = key, id = ID)

#now score! Lets set .75 as our match criteria
score_recall(output, set.cutoff = .75) #This one writes the output to a .csv file

scored = read.csv("output.csv")

dat$Recall = scored$scored

#write.csv(dat, file = "Ex 2 part 1 Scored.csv", row.names = F)
