# ANA 505 Week 3 Activity
# Michelle Tan - 07/18/2024

install.packages('ISLR')
library(ISLR)

install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

data(Hitters)

Hitters <- na.omit(Hitters)

#build the initial tree
tree <- rpart(Salary ~ Years + HmRun, data=Hitters, control=rpart.control(cp=.0001))

#view results
printcp(tree)


#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

print(best)
#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

print(pruned_tree)

prp(pruned_tree)