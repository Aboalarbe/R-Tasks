library(readr)
library(arules)
library(arulesViz)
library(RColorBrewer)
df <- read.transactions("transactions.csv", format="basket", sep=",", skip=0)
summary(df)

# plot the frequency of items
itemFrequencyPlot(df,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

# Min Support as 0.002, confidence as 0.20, maximum length as 3
association_rules <- apriori(df, parameter = list(support=0.002, conf=0.20,maxlen=3))
summary(association_rules)
# sorting grocery rules by lift to determine actionable rules
inspect(sort(association_rules, by = "lift"))

# Min Support as 0.002, confidence as 0.20, maximum length as 2
association_rules2 <- apriori(df, parameter = list(supp=0.002, conf=0.20,maxlen=2))

#higher lift with maxmium length 2
inspect(sort(association_rules2, by = "lift")[1])
#higher lift with maxmium length 3
inspect(sort(association_rules, by = "lift")[1])




