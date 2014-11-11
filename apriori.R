# Simple correlation analysis for a dataset.
# Codes modified from various sources.
# Needs packages "arules" and "arulesViz"
# install.packages("arules")
# install.packages("arulesViz")

# Obtaining the rules, suppose the data are in "dataset".
library(arules)
rules <- apriori(dataset)
inspect(rules)

# Draw graphs from the rules.
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))