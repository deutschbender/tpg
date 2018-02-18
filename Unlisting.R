##-------------
#### How to unlist lists with ability to choose elements of the data structure
##-------------
setwd()
# Library and list creation
library(data.table)

mylist <- list(list=list("kpi1"=2,"name"="trigger1","kpi2"=6.3, "send_date" ="2017-01-18 12:23:45"),
               list=list("kpi1"=7,"name"="trigger3","kpi2"=5.1, "send_date" ="2017-01-18 12:23:45"), 
               list=list("kpi1"=6,"name"="trigger3","kpi2"=5.8, "send_date" ="2017-01-19 12:23:45"),
               list=list("kpi1"=134,"name"="trigger4","kpi2"=12.789, "send_date" ="2017-01-18 12:23:45"))

# Extraction of lists call "list"
myTrigger <- mylist[grepl("list", names(mylist))]


# creation of unique list name necessary to unlist with 1, 2 or n elements of the list

triggerNames <- c()
for (i in 1:length(myTrigger)){
  triggerNames <- append(triggerNames, 
                         paste(myTrigger[[i]]$name, myTrigger[[i]]$send_date, sep="")
                         , i)
}


# creation of columns usefull with dcast from the unlist created rownames
names(myTrigger) <- triggerNames
test <- data.frame(unlist(myTrigger))
colnames(test) <- c("unlisted")
test$name <- row.names(test)
test$nameVariable <- substring(test$name, regexpr("\\.",test$name)+1, last = 1000000L)
test$nameValue <- substring(test$name, 1, regexpr("\\.",test$name)-1)

# cast the df to data.table and application dcast with all kpi
test <- data.table(test)
tes.w <- dcast(test, nameValue~nameVariable, value.var = "unlisted")

# Cleansing of the dt
tes.w <- tes.w[,name:= NULL]

Sys.getenv("VAR1")
file.edit("~/.Renviron")
Sys.getenv("id")
