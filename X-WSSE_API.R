library(stringi)
library(digest)
library(caTools)
library(jsonlite)
library(httr)

setwd("C:/Users/Julien/Desktop/TPG")

emarsys_api <- fromJSON("emarsys_sandbox.json")
# emarsys_api <- fromJSON("emarsys_api.json")

keyname <- emarsys_api$keyname          
keysecret <- emarsys_api$keysecret 



# Checking contact id endpoint
endpoint <- paste0(emarsys_api$version,"contact/checkids")


   
#Create nonce
nonce <- stri_rand_strings(1, 24, pattern = "[A-Za-z0-9]")
#Create timestamp
timestamp <- format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%dT%H:%M:%SZ")
#Concatentate nonce, timestamp, shared secret, then sha1 then base64
pwDigest <- paste(nonce, timestamp, keysecret, sep="")
pwDigest.sha1 <- digest(pwDigest, algo="sha1", serialize=FALSE)
passwordDigest <- base64encode(charToRaw(pwDigest.sha1))
#Build & Return X-WSSE Header 
headers <- c('X-WSSE'=paste0('UsernameToken Username="',keyname,'", ',
                               'PasswordDigest="',passwordDigest,'", ',
                               'Nonce="',nonce,'", ',
                               'Created="',timestamp,'"'))

r <- GET(paste0('https://suite17.emarsys.net/api/v2/contact/?3=',doubon[1,2]) 
         add_headers('',.headers = headers))
reply <- fromJSON(rawToChar(r$content))
contactId <- replay$data$id

response <- POST(url, config=add_headers('',.headers=BuildHeader()), body=body)


doublon <- data.frame(email = c("", ""), stringsAsFactors = F)
doublon$emailencoded <- URLencode(doublon$email, reserved = TRUE) 

doublon.json <- toJSON(doublon, pretty=TRUE)


body <- paste0('{
  "key_id": "3",
  "external_ids": [
    "',doublon[1,1],'"
    ],
  "get_multiple_ids": 1
}')




