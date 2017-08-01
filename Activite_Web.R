
library(readxl)
library(readr)
library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(lubridate)
library(plotly)

library(readxl)
Activite_Web_Quotidien <- read_excel("C:/Users/Julien/Desktop/TPG/Activité Web Quotidien (1).xlsx", skip = 3)

Activite_Web_Quotidien$DATECOMMANDE <- 
  as.POSIXct(Activite_Web_Quotidien$DATECOMMANDE * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")

Activite_Web_Quotidien$DATECOMMANDE <- as.Date(Activite_Web_Quotidien$DATECOMMANDE)
data <- mpg
i <- ggplot(economics, aes(date, unemploy))
i <- i + geom_line(color="skyblue")
i

economics <- economics 
f <- ggplot(data = mpg, aes(x = class, y = hwy))
f <- f + geom_col()
f

b <- ggplot(Activite_Web_Quotidien, aes(x = DATECOMMANDE, y = CAHTWEB))
b <- b + geom_line(color="skyblue")
b <- b + ylab("CA HT") + xlab("DATE")
b <- b + theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1,vjust = 1))
b

 b_ly <- ggplotly(b)
 b_ly
## Ca Web Ht
plot_ly(Activite_Web_Quotidien, x=~DATECOMMANDE, y=~CAHTWEB, type="scatter", mode="lines", fill="tozeroy")
 
## NB d e1ere Commande
plot_ly(Activite_Web_Quotidien, x=~DATECOMMANDE, y=~NB1ERECMD, type="scatter", mode="lines", fill="tozeroy")

graphNbCmd <- plot_ly(Activite_Web_Quotidien, x = ~DATECOMMANDE, y = ~NBCMD, name = 'Nb Cmd Total', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~NB3EANECMD, name = 'Nb 3e et + cmd', mode = 'lines') %>% 
  add_trace(y = ~NB2ECMD, name = 'Nb 2e cmd', mode = 'lines') %>%
   add_trace(y = ~NB1ERECMD, name = 'Nb 1ere cmd', mode = 'lines')
graphNbCmd

graphCa <- plot_ly(Activite_Web_Quotidien, x = ~DATECOMMANDE, y = ~CAHTWEB, name = 'CA HT Total', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~CAHT3EANECMD, name = 'CA HT 3e et + cmd', mode = 'lines') %>% 
  add_trace(y = ~CAHT2ECMD, name = 'CA HT 2e cmd', mode = 'lines') %>%
  add_trace(y = ~CAHT1ERECMD, name = 'CA HT 1ere cmd', mode = 'lines')
graphCa

