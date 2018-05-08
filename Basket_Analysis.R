# source : https://www.analyticsvidhya.com/blog/2017/08/mining-frequent-items-using-apriori-algorithm/
#ref : http://r-statistics.co/Association-Mining-With-R.html

library(arules)
library(arulesViz)
library(RColorBrewer)
data("Epub")

data("Groceries")
inspect(head(Groceries, 3))
inspect(head(Epub, 3))

a_df3 <- data.frame(
  TID = c(1,1,2,2,2,3), 
  item=c("a","b","a","b","c", "b")
)
a_df3
trans4 <- as(split(a_df3[,"item"], a_df3[,"TID"]), "transactions")
trans4
inspect(trans4)
data(iris)

irisDisc <- discretizeDF(iris)
head(irisDisc)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.80))

arules::itemFrequencyPlot(Groceries,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",ylab="Item Frequency (Relative)")

plot(rules[1:20],
     method = "graph",
     control = list(type = "items"))


plot(rules[1:20],
     method = "scatterplot",
     engine = "htmlwidget")
arulesViz::plotly_arules(rules)
