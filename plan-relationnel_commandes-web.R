
## Traitement des fichiers des commandes pour l'analyse des cohortes

## EXECUTER PREALABLEMENT LE SCRIPT GA - Commandes par campagne.R

setwd("C:/Users/jvanne01/Work Folders/Desktop/Stat/R")
## Chargement des packages
library(data.table)
library(bit64)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(googleAnalyticsR)
library(RODBC)
library(readxl)
library(stringr)
library(dtplyr)
library(fst)


## Lecture des fichiers de commandes
cmd <- list.files(path = "C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/commandes_web", full.names = TRUE, recursive = TRUE)
cmd <- data.frame(cmd)
cmd$istxt <- ifelse(grepl(".txt", cmd$cmd),"bon","mauvais")
cmd <- data.frame(cmd[cmd$istxt == "bon",])
cmd$cmd <- factor(cmd$cmd)

## Boucle de chargement desfichiers de commandes
commandes <- data.table()
for (i in 1:nrow(cmd)) {
  commandesTemp <- read_delim(as.character(cmd[i,1]), 
                                    "\t", escape_double = FALSE, col_types = cols(`Téléphone` = col_character(),
                                                                                   `Date commande` = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                                                                  `Date livraison souhaitée` = col_date(format = "%d/%m/%Y")), 
                                    locale = locale(decimal_mark = ","), 
                                    trim_ws = TRUE)
  commandesTemp <- setDT(commandesTemp[,1:31])
  commandes <- rbind(commandes, commandesTemp)
}
rm(commandesTemp)
setnames(commandes, names(commandes),
        str_replace_all(str_replace_all(str_replace_all(tolower(names(commandes)), "[éèê]", "e"), "[[:blank:]]|-|/|'", "_"),"\\)|\\(", ""))

max(commandes$date_commande)

eolas_con <- odbcConnect("Eolas")
query_eolas_cmd <- paste("SELECT 
    cmd.CMD_NUMERO as numero_de_commande,
    clt.CLI_NUMCLIENT as code_client,
    clt.CLI_FACEMAIL as email,
    FROM_UNIXTIME(cmd.cmd_datecommande) AS date_commande,
    FROM_UNIXTIME(cmd.cmd_datelivraisonsouhaitee) AS date_livraison,
    cmd.CMD_CODEPROMO as code_promo,
    cmd.CMD_SOURCE as commande_api ,
    cmd.CMD_PRIXFINALTTC as montant_total_ttc,
    cmd.CMD_FRAISLIVRAISON as frais_livraison,
    cmd.CMD_CODEPROMOREMISE as remise_code_promo,
    (cmd.CMD_PRIXFINALTTC + cmd.CMD_FRAISLIVRAISON + cmd.CMD_CODEPROMOREMISE) as montant_total_ttc,
    cmd.cme_code as etat_commande,
    cms.CMS_NUMERO as commande_rang
  FROM
    toupargelfo.COMMANDE AS cmd
    left join toupargelfo.COMMANDESTATISTIQUE as cms on cmd.ID_COMMANDE = cms.id_commande
    left join toupargelfo.CLIENT as clt on clt.ID_CLIENT = cmd.ID_CLIENT
  WHERE
    cmd.cme_code in ('CME_ATTENTE','CME_PREPARATION','CME_LIVRAISON','CME_LIVREE','CME_TERMINEE')
    and cmd.CMD_TELEVENTE = 0;")
commandes <- sqlQuery(eolas_con, query_eolas_cmd, believeNRows=FALSE, stringsAsFactors = F)
odbcClose(eolas_con)
commandes <- setDT(commandes)
setnames(commandes, names(commandes), tolower(names(commandes)))

write.fst(commandes, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/commandes_eolas.fst")




## Nettoyage et nouvelles variables
# setnames(commandes,
#          c("Numéro de Client","Numéro de commande","Email","Date commande","Commande internet","Commande API","Code promo", "Etat de commande", "Date livraison souhaitée", "Salarié", "Montant total","Remise code promo","Remise 1ère commande","Téléphone","Heure livraison souhaitée"),
#          c("idClient", "idCommande","email","dateCommande","cmdInternet","cmdApi","codePromo", "etatCommande","dateLivSouhaitee","salarie","montantTotal","remiseCodePromo","remise1ereCmd","telephone","heureLivSouhaitee"))

commandes[,numero_de_commande:=as.character(numero_de_commande)]
commandes[,date_commande_b:=as.Date(floor_date(date_commande, "day"))]
commandes[,day_commande:=day(date_commande)]
commandes[,month_commande:=month(date_commande)]
commandes[,year_commande:=year(date_commande)]
commandes[,jour_commande:=wday(date_commande_b, label = TRUE,  abbr = FALSE)]
levels(commandes$jour_commande) <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi","vendredi","samedi")
commandes[,jour_commande:=as.character(jour_commande)]
commandes[,date_livraison_souhaitee_b:=floor_date(date_livraison_souhaitee, "day")]
commandes[,delai:=as.numeric(difftime(date_livraison_souhaitee_b, date_commande_b, units = "days"))]
commandes[,date_livraison_souhaitee_b:=wday(date_livraison_souhaitee_b, label = TRUE,  abbr = FALSE)]
levels(commandes$date_livraison_souhaitee_b) <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi","vendredi","samedi")
commandes[,date_livraison_souhaitee_b:=as.character(date_livraison_souhaitee_b)]

# deduplication des commandes modifies
setorder(commandes, date_commande)
setkey(commandes, numero_de_commande)
commandes <- commandes[J(unique(numero_de_commande)), mult = "last"]

# CTRL DEDUPLICATION
commandes[numero_de_commande == "1712294481092"]
nrow(commandes[,.(nb_lignes = length(numero_de_client)), by = .(numero_de_commande)][nb_lignes > 1])


## Ranking des commandes
setorder(commandes, numero_de_client, date_commande)
setkey(commandes, numero_de_client)
commandes[,rang_cmd := frank(date_commande),by = list(numero_de_client)]

n_distinct(commandes$numero_de_commande) == nrow(commandes)


## Contrôles divers sur la table commandes
table(commandes$commande_internet) 
table(commandes$salarie) 
table(commandes$etat_de_commande) 
table(commandes$commande_api, useNA = "ifany") 
str(commandes)

## Cleansing et qualification code promo
commandes[, code_promo := str_trim(toupper(iconv(code_promo, to='ASCII//TRANSLIT')))]
commandes[, email := str_trim(tolower(iconv(email, to='ASCII//TRANSLIT')))]
commandes[, etat_de_commande:= str_replace_all(str_replace_all(str_replace_all(tolower(etat_de_commande), "[éèê]", "e"), "[[:blank:]]|-|/|'", "_"),"\\)|\\(", "")]
commandes <- commandes[!(etat_de_commande == "annulee")]
commandes[, code_promo_ko:= case_when(
  !is.na(code_promo) & remise_code_promo == 0 ~ "oui", # code promo non applique
  !is.na(code_promo) & remise_code_promo > 0 ~ "non",# code promo applique une reduction
  TRUE ~ "NC")]



commandes[, code_promo_clean := case_when(
  grepl("^(B|G|S|V)P[0-9]{1}",code_promo) ~ substr(code_promo,1,3), # code groupon Showroon
  grepl("^(B|G|S|V)P[0-9]{2}",code_promo) ~ substr(code_promo,1,4), # code groupon Showroon bis
  grepl("^TPG[0-9]{7}",code_promo) ~ substr(code_promo,1,4), # code groupon Showroon ter
  grepl("^(PAR|PPAR)",code_promo) ~ substr(code_promo,1,3), # code parrainage
  grepl("^CP",code_promo) ~ substr(code_promo,1,3), # code carte postale
  grepl("CD1A",code_promo) ~ substr(code_promo,1,4), # code CDiscount
  grepl("RDN",code_promo) ~ substr(code_promo,1,3), # code Radin
  grepl("^AMB",code_promo) ~ substr(code_promo,1,3), # code Ambassadeur
  grepl("^SHOP1",code_promo) ~ substr(code_promo,1,5), # code Shopmium
  TRUE ~ code_promo)]

commandes[, type_offre := case_when(
  grepl("^(G|S|V)P[0-9]{1,2}", code_promo_clean) & code_promo_ko == "non" ~ "groupon_showroom", # code groupon Showroon
  grepl("^TPG[0-9]{7}", code_promo_clean) & code_promo_ko == "non" ~ "groupon_showroom", # code groupon Showroon
  grepl("^BP[0-9]{1,2}",code_promo_clean)  & code_promo_ko == "non" ~ "banques", # code groupon Showroon bis
  grepl("^(PAR|PPAR)",code_promo_clean)& code_promo_ko == "non" ~ "parrainage", # code parrainage
  grepl("^CP",code_promo_clean) & code_promo_ko == "non" ~ "carte_postale", # code carte postale
  grepl("CD1A",code_promo_clean) & code_promo_ko == "non" ~ "cdiscount", # code CDiscount
  grepl("RDN",code_promo_clean) & code_promo_ko == "non" ~ "radin", # code Radin
  grepl("^AMB",code_promo_clean) & code_promo_ko == "non" ~ "ambassadeur", # Code ambassadeur du goût 
  grepl("^SHOP",code_promo_clean) & code_promo_ko == "non" ~ "shopmium", # Code shopmium
  grepl("^WEB[0-9]{1}",code_promo_clean) & code_promo_ko == "non" ~ "cata_web", # Code shopmium
  grepl("^DECOUVERTE",code_promo_clean) & code_promo_ko == "non" ~ "1ere_commande", # Code ambassadeur du goût 
  grepl("BIENVENUE",code_promo_clean) & code_promo_ko == "non" ~ "1ere_commande", # Code 1ere commande
  grepl("BIENVENUE(10|20)",code_promo_clean) & code_promo_ko == "non" ~ "carnet_bienvenue", # Code 1ere commande
  grepl("^WELCOME",code_promo_clean) & code_promo_ko == "non" ~ "1ere_commande",
  grepl("^GIVEME5",code_promo_clean) & code_promo_ko == "non" ~ "abonnement_news", # Code abonnement_news
  grepl("^PREM",code_promo_clean) & code_promo_ko == "non" ~ "PRM_ADL", 
  grepl("^CB(FRAIS205|MER10|FRAIS05|BIENVENUE06|FRAIS10|BOULPAT6|LEGUMES6|ENTREEPC8|BIO10|VIANVOL10|BIENVENUE07)",code_promo_clean) & code_promo_ko == "non" ~ "carnet_bienvenue", # Code Carnet de bienvenue
  code_promo_clean %in% c("VIP2017","PERE17","PENTE15","8MAI","PAQ10","VAL14","ANNIV","KDO|PRIV20", "PAPA25") & code_promo_ko == "non" ~ "code_sms", # Code SMS
  !is.na(code_promo_clean) & code_promo_ko == "non" ~ "autre_offre",
  !is.na(code_promo_clean) & code_promo_ko == "oui" ~ "erreur_code",
  is.na(code_promo_clean) ~ "sans offre")]

# Redressement code_promo non normalisé
code_promo <- setDT(read_delim("C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/code_promo/code_promo_2015.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE))
code_promo <- code_promo[,.(Code, `Libellé du code`)]
setnames(code_promo, names(code_promo), c("code_promo", "libelle_code"))
commandes <- merge(commandes, code_promo, by = "code_promo", all.x = T)
commandes[,type_offre:= case_when(
  grepl("rosedeal|Groupon", libelle_code) & type_offre == "autre_offre" ~ "groupon_showroom",
  TRUE ~ type_offre)]
commandes[grepl("rosedeal|Groupon", libelle_code) & type_offre == "autre_offre"]

table(commandes$type_offre, useNA = "ifany")
ctrl <- commandes[is.na(type_offre)]
commandes <- commandes[,-43]


## RECUPERATION DES DONNEES GA
dtGa <- read.fst("C:/Users/jvanne01/Work Folders/Desktop/Stat/data/ga_cmd_with_campaign.fst", as.data.table = TRUE)
dtGa <- dtGa[,.(channelGrouping,medium,campaign,num_cmd_6char)]

## CTRL DATA GOOGLE ANALYTICS
dtGa[,.(n_distinct(num_cmd_6char)), by = .(medium)]
dtGa[is.na(medium)]


## MERGE Données GA et Commandes et choix de source
commandes[,num_cmd_6char:=ifelse(nchar(numero_de_commande)>7,substring(numero_de_commande,7,15),numero_de_commande)]
setkey(dtGa, num_cmd_6char)
setkey(commandes, num_cmd_6char)
commandesWSource <- merge(commandes,dtGa, by ="num_cmd_6char", all.x = T)

## CTRL OF MERGE COMMANDEAND DATA GA
table(commandesWSource$medium, useNA = "ifany")
## Sauvegarde des commandes qualifiées
commandesWSource[,montant_total_reajuste:=case_when(
    type_offre == "groupon_showroom" & remise_code_promo == 70 ~ montant_total+35,
    type_offre == "groupon_showroom" & remise_code_promo == 50 ~ montant_total+25,
    TRUE ~ montant_total)]
commandesWSource[,remise_reajuste:=case_when(
    type_offre == "groupon_showroom" & remise_code_promo == 70 ~ 35,
    type_offre == "groupon_showroom" & remise_code_promo == 50 ~ 25,
    TRUE ~ remise_code_promo)]
commandesWSource[,commande_deal:=case_when(
  type_offre == "groupon_showroom" ~ "oui",
  TRUE ~ "non")]
commandesWSource[,code_promo_saisi:=ifelse(is.na(code_promo), 0, 1)]

setnames(commandesWSource, "numero_de_client", "code_client")
commandesWSource[,type_client:=case_when(grepl("^I", code_client) ~ "web", TRUE ~ "tlv")]
write.fst(commandesWSource, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/commandesWSource.fst")


## Analyse PRM ## Analyse PRM ## Analyse PRM ## Analyse PRM ###############
###########################################################################
commandesWSource <- read.fst("C:/Users/jvanne01/Work Folders/Desktop/Stat/data/commandesWSource.fst", as.data.table = TRUE)


my.data <- data.frame(letters[1:10])
code_promo <- commandesWSource[!is.na(code_promo),.(code_promo)]
df_code_promo <- data.frame(code_promo)
df_code_promo$code_promo <- as.factor(df_code_promo$code_promo)
code_promo_v <- paste(df_code_promo[,1], collapse = "")

unique_char <- strsplit(code_promo_v, "") 
df_uniq_char <- data.frame(letter = unique_char[[1]])
df_uniq_char_B <- data.frame(letter = unique(as.character(df_uniq_char$letter)), stringsAsFactors = F)



length(code_promo)
max(commandesWSource$date_commande)

commandes_2017_2018 <- commandesWSource[dateLivSouhaiteeb >= ymd('2017-01-01')]
commandes_2017_2018 <- commandes_2017_2018[,.(idCommande, idClient, Agence, dateLivSouhaiteeb, dateCommandeb, montantTotal, Achats, Commune)]


write.table(commandes_2017_2018, file = "//civ-nas01/civ-data/Echanges/Sites Internet/Toupargel e-commerce/_Reporting/commandes_2017_2018.txt",
            sep = "\t",
            dec = ",",
            row.names = F)

commandes_2017_2018


eolas_abonnes <- data.table(read_delim("C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/PRM/eolas_contacts/eolas_abonnes.txt", 
                            "\t",
                            escape_double = FALSE,
                            trim_ws = TRUE))

eolas_clients <- data.table(read_delim("C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/PRM/eolas_contacts/eolas_clients.txt", 
                            "\t",
                            escape_double = FALSE,
                            trim_ws = TRUE))

eolas_prospects <- data.table(read_delim("C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/PRM/eolas_contacts/eolas_prospects.txt", 
                            "\t",
                            escape_double = FALSE,
                            trim_ws = TRUE))

setnames(eolas_abonnes, names(eolas_abonnes), c("email","dateInscription","typeInscrition"))
setnames(eolas_prospects, names(eolas_prospects), c("typeProspection","prospect","email","indiceProspect","parrain","codeParrain","codeProspecteur","origineClient","typeCommande","emailEnvoye","dateValidite"))
setnames(eolas_clients, names(eolas_clients), c("eolasClt_civilite","eolasClt_nom","eolasClt_prenom","eolasClt_codeClient","eolasclt_adresse","eolasClt_codePostal","eolasClt_ville","eolasClt_telephone","eolasClt_portable","email","eolasClt_salarie","eolasClt_gold","eolasClt_abonneNewsletter","eolasClt_dateDerniereConnexion","eolasClt_dateCreation","eolasClt_dateModification","eolasClt_dernierRedacteur","eolasClt_nbCommandes","eolasClt_pointsFidelite","eolasClt_dateMajPoints","eolasClt_compteBloque","eolasClt_cibleSparkow","eolasClt_noteFm","eolasClt_recence"))

eolas_clients[,email:= str_trim(tolower(iconv(email, to='ASCII//TRANSLIT')))]
eolas_abonnes[,email:= str_trim(tolower(iconv(email, to='ASCII//TRANSLIT')))]
eolas_prospects[,email:= str_trim(tolower(iconv(email, to='ASCII//TRANSLIT')))]
eolas_clients[,':='(eolasClt_dateCreation=dmy(eolasClt_dateCreation),
                    eolasClt_dateDerniereConnexion=dmy(eolasClt_dateDerniereConnexion))]

eolas_abonnes[,':='(dateInscription=dmy(dateInscription))]
setkey(eolas_abonnes, email)
setorder(eolas_abonnes, email, dateInscription)
eolas_abonnes <- eolas_abonnes[J(unique(email)), mult = "first"]

eolas_prospects[,':='(dateValidite=dmy(dateValidite),
                      dateCollecte=dmy(dateValidite) %m+% months(-6))]

as.Date("2014-12-22") 

write.fst(eolas_abonnes, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/eolas_abonnes.fst")
write.fst(eolas_clients, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/eolas_clients.fst")
write.fst(eolas_prospects, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/eolas_prospects.fst")



## TRaitemetn fichier email prospect CRM




emailProspectCRM <- data.table(read_delim("C:/Users/jvanne01/Work Folders/Desktop/Stat/exports/PRM/20180108 - Verif redressement email.csv", 
              ";", escape_double = FALSE, col_names = FALSE, 
              trim_ws = TRUE))

setnames(emailProspectCRM, names(emailProspectCRM),
         c("INDICE_PROSPECT","REDRESSEMENT","DATE_MODIF_EMAIL_REDRESSEE","SOURCE_REDRESSEE","email",
           "DATE_CREATION_PROSPECT","DATE_CREATION_EMAIL1","DATE_MODIF_EMAIL1","SOURCE_EMAIL1",
           "UTIL_CREA_EMAIL1","PREMIERE_ORIGINE_PROSPECT","CODE_CLIENT","CLI_DATE_CRE",
           "CLI_EMAIL1","CLI_DATE_CREATION_EMAIL1","CLI_DATE_DESINSCRIPTION_NL_EMAIL1",
           "CLI_CODE_ACTEUR_EMAIL1","CLI_CODE_PROSPECTEUR"))


emailProspectCRM[,email:=tolower(iconv(email, to='ASCII//TRANSLIT'))]
setorder(emailProspectCRM, email, DATE_CREATION_PROSPECT)
setkey(emailProspectCRM, email)
emailProspectCRM <- emailProspectCRM[J(unique(email)), mult = "first"]
write.fst(emailProspectCRM, "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/crm_email_prospect.fst")

## ---- 
#### TRAITEMENT DES PREMIERES COMMANDES
## ---- 

countFirstCmd <- commandesWSource[rankAllCmd == 1]
countFirstCmd[,codePromoclean:=ifelse(isGpSP=="oui", substr(codePromo,1,3), codePromo)]
countFirstCmd[,montantHt:=as.numeric(`Montant HT`)]
as.double(countFirstCmd$`Montant HT`)
#### PremiÃ¨res commande par canal


commandesWSourceInt <- commandesWSource[cmdInternet == "Oui"]
countFirstCmdCanal <- commandesWSourceInt[,.(nbCmd=n_distinct(idCmd6Char.x)), by = list(yearCommande,channelGrouping)]
sum(countFirstCmdCanal$nbCmd)
table(commandesWSourceInt$Salarié)

setkey(countFirstCmdCanal,year)
countFirstCmdyear <- countFirstCmd[,.(nbCmd=n_distinct(idCmdChar)), by = list(year(dateCommande))]
setkey(countFirstCmdyear,year)
countFirstCmdCanal <- left_join(countFirstCmdCanal, countFirstCmdyear, by="year", all.x = T)
countFirstCmdCanal[,ratio:=nbCmd.x/nbCmd.y]
countFirstCmdCanal[,year.f:=as.factor(year)]
countFirstCmdCanal[,ratio.c:=round(ratio*100, digits = 1)]

## Grpahe des premiÃ¨re commande par canal et par annÃ©e
gg0 <- ggplot(countFirstCmdCanal[year %in% c(2017,2016)], aes(x = year.f, y = ratio))
gg0 <- gg0 + geom_bar(aes(fill = channelGrouping), stat="identity", position = "dodge")
gg0 <- gg0 + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=7))
pp0 <- ggplotly(gg0) 
pp0

## Grpahe des premiÃ¨re commande par annÃ©e et par canal
gg1 <- ggplot(countFirstCmdCanal[year %in% c(2017,2016)], aes(x = channelGrouping, y = ratio.c))
gg1 <- gg1 + geom_bar(aes(fill = year.f), stat="identity", position = "dodge")
gg1 <- gg1 + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=7))
pp1 <- ggplotly(gg1) 
pp1


#### PremiÃ¨res commande par code promo

countFirstCmdCP <- countFirstCmd[,.(nbCmd=n_distinct(idCmdChar), caHt=sum(montantHt)), by = list(year(dateCommande),isGpSP,codePromoclean)]
setkey(countFirstCmdCP,year)
countFirstCmdCP <- left_join(countFirstCmdCP, countFirstCmdyear, by="year", all.x = T)
countFirstCmdCP[,ratio:=nbCmd.x/nbCmd.y]
countFirstCmdCP[,year.f:=as.factor(year)]
countFirstCmdCP[,ratio.c:=round(ratio*100, digits = 1)]


table(countFirstCmdCP$codePromoclean)



# ## Graphe de la base client
# #
# DT.REDUCE[,INDMJOIN:=floor_date(DATEJOIN, "month")]
# countJoin <-  DT.REDUCE[,.(nbEmail=n_distinct(EMAIL)),by=.(DATEJOIN)]
# countJoin <- countJoin[order(countJoin$DATEJOIN)]
# p <- plot_ly(countJoin, x = ~DATEJOIN, y = ~nbEmail, type = 'scatter', mode = 'lines')
# p
# 
# countMonthJoin <-  DT.REDUCE[,.(nbEmail=n_distinct(EMAIL)),by=.(INDMJOIN)]
# countMonthJoin <- countMonthJoin[order(countMonthJoin$INDMJOIN)]
# p01 <- plot_ly(countMonthJoin, x = ~INDMJOIN, y = ~nbEmail, type = 'scatter', mode = 'lines')
# 
# p01
# 
# 
# ## Budget 2018
# 
# commandes2017 <- commandes[yearCommande == "2017" & cmdInternet == "Oui"]
# countByNbCmd2017 <- commandes2017[,.(nbCommandes = n_distinct(idCmdChar)), by=.(rankAllCmdB)]  
# sum(countByNbCmd2017$nbCommandes)
# 
# write.table(countByNbCmd2017, file = "C:/Users/jvanne01/Work Folders/Desktop/Stat/data/countByNbCmd2017.txt",
#             sep = "\t",
#             dec = ",",
#             row.names = F)
# 
# write.table(statCmdFirst, file = "//civ-nas01/civ-data/Echanges/Sites Internet/Toupargel e-commerce/_ECRM.txt",
#             sep = "\t",
#             dec = ",",
#             row.names = F)
## Classe par nombre de commandes
# statsCmdBF2016[,classNbCommande:=cut(rankAllCmd
#                                      , breaks = c(-Inf, 0, 1 , 2 ,5 ,10,20,50, +Inf)
#                                      ,labels = c("A - 0 cmd", "B - 1 cmd", "C - 2 cmd", "D - 3 a 5 cmd","E - 6 a 10 cmd", "F - 11 a 20 cmd","G - 21 a 50 cmd", "H - 51 cmd et plus"))]


