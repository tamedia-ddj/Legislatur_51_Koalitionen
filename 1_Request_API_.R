library(httr)
library(tidyverse)
library(tidyjson)

## Set working directory ####
setwd("WORKINGDIRECTORY")
getwd()


## Alle Geschäfte der Legislaturperiode 49/50/51 ####

get_votes <- function(legislative_period, welche){
  
  url_vote <- str_c("https://ws.parlament.ch/odata.svc/Vote?$top=10000&$filter=Language%20eq%20'DE'%20and%20IdLegislativePeriod%20eq%20", legislative_period, "&$format=json")
  r <- GET(url_vote)
  r$status_code
  temp_a <- jsonlite::fromJSON(content(r, "text"))
  
  df_Ids <- as_tibble(temp_a$d) %>%
    select(c(ID, RegistrationNumber, BusinessNumber,
             BusinessShortNumber, BusinessTitle, BusinessAuthor,
             BillNumber, BillTitle, 
             IdLegislativePeriod, IdSession, SessionName, 
             Subject))
  
  
  ## Nur Schlussabstimmungen oder alle?
  print(welche)
  if (welche == "gesamt"){
    df_Ids <- df_Ids %>% filter(grepl("gesamtabstimmung|Vote sur l'ensemble|Votazione sul complesso|schlussabstimmung|Vote final|Votazione finale", Subject, ignore.case = TRUE))
  }

  
  ## get all votes
  df_votes <- tibble()
  
  for (i in seq_along(row.names(df_Ids))){
    IdVote <- df_Ids$ID[i] 
    print(i)
    print(IdVote)
    url <- str_c("https://ws.parlament.ch/odata.svc/Voting?$top=10000&$filter=Language%20eq%20'DE'%20and%20IdVote%20eq%20", IdVote, "&$format=json")
    
    r <- GET(url)
    r$status_code
    
    df_temp <- jsonlite::fromJSON(content(r, "text"))$d %>% tibble() %>%
      select(c("IdVote", "ID", "RegistrationNumber",
               "PersonNumber", "FirstName", "LastName", "ParlGroupName", 
               "DecisionText", "BusinessTitle", 
               "IdLegislativePeriod", "IdSession", "Subject"))
    
    df_votes <- rbind(df_votes, df_temp)
  }
  return(list(df_Ids, df_votes))
}


## Alle Schluss- und Gesamtabstimmungen holen
obj_49 <- get_votes(49, "gesamt")
obj_50 <- get_votes(50, "gesamt")
obj_51 <- get_votes(51, "gesamt")

df_Ids_49 <- obj_49[[1]]
df_votes_49 <- obj_49[[2]]

df_Ids_50 <- obj_50[[1]]
df_votes_50 <- obj_50[[2]]

df_Ids_51 <- obj_51[[1]]
df_votes_51 <- obj_51[[2]]


## ALLE Abstimmungen holen (langsam!)
obj_49_alle <- get_votes(49, "alle")
obj_50_alle <- get_votes(50, "alle")
obj_51_alle <- get_votes(51, "alle")

df_Ids_49_alle <- obj_49_alle[[1]]
df_votes_49_alle <- obj_49_alle[[2]]

df_Ids_50_alle <- obj_50_alle[[1]]
df_votes_50_alle <- obj_50_alle[[2]]

df_Ids_51_alle <- obj_51_alle[[1]]
df_votes_51_alle <- obj_51_alle[[2]]


## Daten speichern

write_csv(df_Ids_49, "Data_output/Ids_49.csv")
write_csv(df_Ids_50, "Data_output/Ids_50.csv")
write_csv(df_Ids_51, "Data_output/Ids_51.csv")

write_csv(df_votes_49, "Data_output/Votes_49.csv")
write_csv(df_votes_50, "Data_output/Votes_50.csv")
write_csv(df_votes_51, "Data_output/Votes_51.csv")

write_csv(df_Ids_49_alle, "Data_output/Ids_49_alle.csv")
write_csv(df_Ids_50_alle, "Data_output/Ids_50_alle.csv")
write_csv(df_Ids_51_alle, "Data_output/Ids_51_alle.csv")

write_csv(df_votes_49_alle, "Data_output/Votes_49_alle.csv")
write_csv(df_votes_50_alle, "Data_output/Votes_50_alle.csv")
write_csv(df_votes_51_alle, "Data_output/Votes_51_alle.csv")


## Infos zu allen Parlamentariern in diesen Legislaturperioden holen ####

PersonNumbers <- unique(rbind(df_votes_49, df_votes_50, df_votes_51)[, "PersonNumber"])$PersonNumber
df_parls <- tibble()
for (PersonNumber in PersonNumbers){
  print(PersonNumber)
  url_parl <- str_c("https://ws.parlament.ch/odata.svc/Person?$top=20&$filter=Language%20eq%20'DE'%20and%20PersonNumber%20eq%20", PersonNumber, "&$format=json")
  r <- GET(url_parl)
  r$status_code
  temp_a <- jsonlite::fromJSON(content(r, "text"))
  
  df_parls_temp <- as_tibble(temp_a$d) %>% select(c(ID, Language,
                                                    PersonNumber, PersonIdCode, LastName, 
                                                    FirstName, GenderAsString, DateOfBirth, MaritalStatusText,
                                                    NumberOfChildren))
  df_parls <- rbind(df_parls, df_parls_temp)
  
}

## Parlamentarier Daten speichern
df_parls %>% write_csv("Data_output/alle_parlamentarier.csv")
