# Code for forming association rules and testing them
# Author: TEAM

#install.packages('gdata')

library("arules")
library("gdata")
library("arulesViz")

# Read the required file
trainegs = read.transactions(file="Case_VRoots.csv", quote = "", rm.duplicates=TRUE, format="single", sep=",", cols=c("CaseID","title"));

# Association rules
rules <- apriori(trainegs, parameter = list(supp=0.025, conf=0.25, minlen=1))

# find redundant rules
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# Remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

rules <- rules.pruned

# Sort by lift, conf and support
rules_conf <- sort(rules, by="confidence", decreasing=TRUE)
rules_sup <- sort(rules, by="support", decreasing=TRUE)
rules_lift <- sort(rules, by="lift", decreasing=TRUE) 

# Summary of rules
summary(rules)
inspect(rules)

# Plot top 10 rules
plot(rules_conf[1:10], method="graph", control=list(type="items"))
plot(rules_sup[1:10], method="graph", control=list(type="items"))
plot(rules_lift[1:10],method="graph")

# Item Frequency Plot
itemFrequencyPlot(trainegs,topN=20,type="absolute")

#######################################################################
# The Supporting Functions
#######################################################################

# Remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" "," ",itemstrg),","))[[1]])
}

# Execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  antecedent = paste("{",item,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" "," ",toString(sub("\\}"," ",sub(".*=> \\{","",firingrules))))
}

# Count how many predictions are in the basket of items already seen by that user 
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$caseID == baskID,"title"][[1]]
  cnt = 0 
  for (p in plist) {
    p = trim(p)
    if (grepl(p, blist)) cnt = cnt+1
  }
  cnt
}

# Count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len == 0) 0 # avoid counting an empty list
  else len
}

#########################################################################################
# Testing stage
#########################################################################################

# Read the test data
testegs = read.csv(file="Case_VRoots_Test.csv", header = TRUE, quote = "");
colnames(testegs) <- c("caseID","title")

# Execute rules against test data
rulesDF = as(rules,"data.frame")
testegs$preds = apply(testegs,1,function(X) makepreds(X["title"], rulesDF))

# Extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ caseID, data = testegs, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# Extract unique items for each test user
baskets = as.data.frame(aggregate(title ~ caseID, data = testegs, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["title"]))

# Count how many unique predictions made are correct
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["caseID"])))
print(correctpreds)

# Count total number of unique predictions made
totalpreds = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 
print(totalpreds)

precision = correctpreds*100/totalpreds

# Printing the results
cat("Precision = ",precision)
cat("Correctly Predicted = ",correctpreds)
cat("Total Predicted = ",totalpreds)