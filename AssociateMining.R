#install.packages("arules")
#install.packages("arulesViz")
library(tidyverse)
library(arules) 
library(arulesViz)
groceries_read = scan("R files/groceries.txt", what = "", sep = "\n")
head(groceries_read)
str(groceries_read)
#summary(groceries_read)
groceries = strsplit(groceries_read, ",")
gtransact = as(groceries, "transactions")
summary(gtransact)
itemFrequencyPlot(gtransact, topN = 20)
inspect(gtransact[1:20,])

cat("Considering multiple combinations of support and confidence")

grules_1 = apriori(gtransact, 
                     parameter=list(support=.005, confidence=.1, minlen=2))
arules::inspect(grules_1)
plot(head(grules_1, 10, by='lift'), method='graph')

cat("we see that people who buy ham are likely to buy white bread")
cat("people who buy berries are likely to buy whipper cream")
cat("people who buy herbs are likely to buy root vegetables")

cat("Tuning the parameter to get a good combinations of products with higher confidence")
grules_2 = apriori(gtransact, 
                      parameter=list(support=0.0015, confidence=0.8, minlen=2))
arules::inspect(grules_2)


plot(head(grules_2, 10, by='lift'), method='graph')

cat("we see that people who buy liquor are more likely to buy wine and beer")
cat("people who buy whole milk are more likely to buy yogurt and whipped cream")
cat("people who buy tropical fruits are more likely to buy citrus fruit and root vegetables")


cat(" Choose a subset for better analysis")

inspect(subset(grules_2, subset=lift > 4 & confidence > 0.7))

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(grules_2)

# can swap the axes and color scales
plot(grules_2, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(grules_2, method='two-key plot')

cat("from the above plots we can see that high confidence has less support and lift")

############################################################

# can now look at subsets driven by the plot
#inspect(subset(grules_2, support > 0.035))
#inspect(subset(grules_2, confidence > 0.6))
#inspect(subset(grules_2, lift > 20))


# graph-based visualization
# export
# associations are represented as edges
# For rules, each item in the LHS is connected
# with a directed edge to the item in the RHS. 
#grocery_graph = associations2igraph(subset(grules_2, lift>4), associationsAsNodes = FALSE)
#igraph::write_graph(grocery_graph, file='grocery.graphml', format = "graphml")
