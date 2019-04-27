##### ----Chargement des librairies ----- ##### 

install.packages("googledrive")


## data manipulation
library(data.table)
library(dtplyr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(stringi)
library(digest)
library(caTools)
library(jsonlite)
library(httr)
library(googledrive)

# # CREDENTIAL FOR TS API
keyname <- ""        
keysecret <- ""
endpoint_base_url <- "https://api.trustedshops.com/rest/restricted/v2/shops/"

##LOAD AVIS ALLREADY DOWNLOADED
avis_downloaded <- setDT(read_delim("avis_commande.csv", ";", escape_double = FALSE, col_types = cols(orderreference = col_character()),  trim_ws = TRUE))


##  CTRL
str(avis_downloaded)
min(avis_downloaded$creationday)
table(avis_downloaded$typecmd)
n_distinct(avis_downloaded$orderreference)

## CREATE SEQ OF API CALL
date_deb <- max(as.Date(avis_downloaded$creationday) -3)
date_seq <- seq(date_deb, Sys.Date(), 1)

endpoint_list <- c()
for (i in 1:(length(date_seq)-1)) { 
  endpoint_list[i] <- paste(endpoint_base_url, 
                          "reviews?startDate=", 
                          as.character(date_seq[i]), 
                          "&endDate=", 
                          as.character(date_seq[i+1]), sep= "")
  }


## API CALL FOR DATA DOWLOAD
reply <- list() 
for (j in 1:length(endpoint_list)) { 
  reply_temp <- GET(endpoint_list[j],
                  authenticate(keyname, keysecret, type = "basic"))
  reply_temp <- fromJSON(rawToChar(reply_temp$content))
  reply <- append(reply, reply_temp) 
}

##CONVERT JSON INTO RECTANGULAR DATA
df_reply <- data.frame() 
df_crit <- data.frame()
for (k in 1:length(reply)) { 
  df_reply_temp <- reply[k]$response$data$shop$reviews
    if (length(df_reply_temp) > 0) { 
      if (!"netRecommendationScore" %in% colnames(df_reply_temp)) {
        df_reply_temp$netRecommendationScore <- NA
        }
      df_criteria <- data.frame()
        for(l in 1:nrow(df_reply_temp)) {
          df_criteria_temp <- df_reply_temp$criteria[[l]]
          df_criteria_temp$UID <- df_reply_temp$UID[[l]]
          df_criteria <- rbind(df_criteria, df_criteria_temp)
        }
      df_reply_temp <- df_reply_temp[,c("changeDate","comment","confirmationDate", "consumerEmail",
                                    "creationDate","mark","orderReference",
                                    "UID","netRecommendationScore")]
      df_crit <- rbind(df_crit, df_criteria)
      df_reply <- rbind(df_reply, df_reply_temp) 
    }
}

## CLEANSING RECTANGULAR DATA 
df_reply <- unique(df_reply)
df_crit <- unique(df_crit)
df_crit_wide <- dcast(df_crit, UID~type, value.var = "mark")
df_reply_final <- merge(df_reply, df_crit_wide, by = "UID")

df_reply_final <- df_reply_final %>%
  mutate(changeDate = ymd_hms(changeDate),
         confirmationDate = ymd_hms(confirmationDate), 
         creationDate = ymd_hms(creationDate),
         typeCmd = ifelse(nchar(orderReference) >= 12, "web", "tlv"),
         netRecommendationScore = as.numeric(netRecommendationScore),
         mark = as.numeric(mark),
         DELIVERY = as.numeric(DELIVERY),
         GOODS = as.numeric(GOODS),
         SERVICE = as.numeric(SERVICE)
         )


df_reply_final <- df_reply_final %>%
  mutate(creationDay = as.Date(creationDate, "day"),
         consumerEmail = sapply(consumerEmail, digest, algo="md5", serialize=F),
         nps_type = case_when(
           netRecommendationScore > 8 ~ "promoteur",
           netRecommendationScore < 7 ~ "detracteur",
           is.na(netRecommendationScore) ~ "NA",
           TRUE ~ "neutre")
         )


## CTRL
summary(df_reply_final)

##SELECT USEFULL COLUMNS
dt_reply_final <- setDT(df_reply_final)
setnames(dt_reply_final, names(dt_reply_final), str_to_lower(names(dt_reply_final)))
dt_reply_final <- dt_reply_final[,.(uid, orderreference, creationdate, creationday, mark, comment, delivery, goods, service, netrecommendationscore, typecmd, nps_type)]



avis_commande <- rbind(avis_downloaded, dt_reply_final)


setorder(avis_commande, uid, -creationday)
setkey(avis_commande, uid)
avis_commande <- avis_commande[J(unique(uid)), mult = "first"]
avis_commande[,comment:= str_replace_all(comment, "\r\n", " ")] 
avis_commande[,comment:= str_replace_all(comment, ";", "-")] 

write.table(avis_commande,
            "avis_commande.csv", 
            quote= T,
            dec=".",
            row.names=F,
            na = "",
            sep=";",
            qmethod = "escape",
            fileEncoding = "UTF-8")
drive_update("tpg_avis_commande.csv", media = "avis_commande.csv" , verbose = TRUE)


## CLEAN TEMP DATA FROM WORK SPACE
rm(list = ls(all = T))

## Product reviews
replyProd <- GET("https://cdn1.api.trustedshops.com/shops/X0E8B09A41653E9D2746E27B625C4E068/products/public/v1/feed.json")
replyProdContent <- fromJSON(rawToChar(replyProd$content))
products <- replyProdContent$response$data$shop$products
prod_reviews <- products$productReviews
names(prod_reviews) <- products$sku

prod_reviews_df <- data.frame() 
for (a in 1: length(prod_reviews)) {
  dft <- prod_reviews[[a]]
  dft$code_produit <- names(prod_reviews)[a]
  dft$orderReference <- c("")
  dft$mark <- c("")
  
  for (k in 1: nrow(dft)) {
    dft$orderReference[k] <- dft$order[["orderReference"]][[k]]
    dft$mark[k] <- dft$criteria[[k]][["mark"]][[1]]
  }
  dft <- dft[, c("creationDate","comment","mark","uuid","code_produit","orderReference")]
  prod_reviews_df <- rbind(prod_reviews_df, dft)
} 

avis_produits <- setDT(prod_reviews_df)
avis_produits[,comment:= str_replace_all(comment, "\r\n", " ")] 
avis_produits[,comment:= str_replace_all(comment, ";", "-")] 


write.table(avis_produits,
            "avis_produit.csv", 
            quote= T,
            qmethod = "escape",
            dec=",",
            row.names=F,
            na = "",
            sep=";",
            fileEncoding = "UTF-8")
drive_update("tpg_avis_produit.csv", media =  "avis_produit.csv", verbose = TRUE)

## CLEAN TEMP DATA FROM WORK SPACE
rm(list = ls(all = T))
