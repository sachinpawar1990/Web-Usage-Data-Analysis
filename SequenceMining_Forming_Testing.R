# Code to perform sequence Mining
# @author: TEAM

library(arulesSequences)
library(arulesViz)

# Read the data obtained from Python conversion into basket format
# as required for CSPADE algorithm
data <- read_baskets(con = "F:/Dropbox backup/WebAnalytics/Web Analytics/Barry/Solutions/Sequence Rules/cSpade_Items_Title.txt",info = c("sequenceID","eventID","SIZE"))

# Check if the basket is in the desired format
as(head(data), "data.frame") # view first few rows of the data

# Obtain the sequences using Cspade
seqs <- cspade(data, parameter = list(support = 0.025),
               control = list(verbose = TRUE))

# Display the Sequences
as(seqs,"data.frame")  # view the sequences

# Convert the Sequences obtained into rules using RuleInduction
rules <- ruleInduction(seqs, confidence = 0.25,control = list(verbose = TRUE))

# Sort Sequence Mining Rules by Lift
rules.sorted <- sort(rules, by="lift")

# Display the rules sorted w.r.t lift
as(rules.sorted,"data.frame")

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

# Count how many predictions are correct
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
testegs = read.csv(file="F:/Dropbox backup/WebAnalytics/Web Analytics/Barry/Solutions/Sequence Rules/Case_VRoots_Test.csv", header = TRUE, quote = "");
colnames(testegs) <- c("caseID","title")

# Execute rules against test data
rulesDF = as(rules,"data.frame")

testegs$preds = apply(testegs,1,function(X) makepreds(X["title"], rulesDF))

# Extract unique predictions for each user
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

precision = correctpreds * 100/ totalpreds

# Printing the results
cat("Precision = ",precision)
cat("Correctly Predicted = ",correctpreds)
cat("Total Predicted = ",totalpreds)