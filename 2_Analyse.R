library(tidyverse)

## Set working directory ####
setwd("D:/Tamedia/GSync/Datenteam/Projekte/202012_Koalitionsanalyse_2020/")
getwd()


## Daten einlesen ####

## Nur Schlussabstimmungen
df_Ids_50 <- read_csv("Data_output/Ids_50.csv")
df_Ids_51 <- read_csv("Data_output/Ids_51.csv")

df_votes_50 <- read_csv("Data_output/Votes_50.csv")
df_votes_51 <- read_csv("Data_output/Votes_51.csv")

## Alle Abstimmungen
df_Ids_50_alle <- read_csv("Data_output/Ids_50_alle.csv")
df_Ids_51_alle <- read_csv("Data_output/Ids_51_alle.csv")

df_votes_50_alle <- read_csv("Data_output/Votes_50_alle.csv")
df_votes_51_alle <- read_csv("Data_output/Votes_51_alle.csv")

## Daten Aufbereiten ####

## Eindeutiger Name
# Carefull, Daniel Frei changed party --> Nicht relevant in 51
df_votes_50$FullName <- str_c(df_votes_50$FirstName, " ", df_votes_50$LastName)
df_votes_51$FullName <- str_c(df_votes_51$FirstName, " ", df_votes_51$LastName)
df_votes_50_alle$FullName <- str_c(df_votes_50_alle$FirstName, " ", df_votes_50_alle$LastName)
df_votes_51_alle$FullName <- str_c(df_votes_51_alle$FirstName, " ", df_votes_51_alle$LastName)


## Ordnungsanträge entfernen
df_Ids_51 <- df_Ids_51 %>%     filter(Subject != "Motion d'ordre Dettling: répéter le vote final sur 16.452") %>% 
                               filter(Subject != "Motion d'ordre Matter Thomas: 18.061 répéter le vote sur l'ensemble")
df_votes_51 <- df_votes_51 %>% filter(Subject != "Motion d'ordre Dettling: répéter le vote final sur 16.452") %>% 
                               filter(Subject != "Motion d'ordre Matter Thomas: 18.061 répéter le vote sur l'ensemble")


## Einheitliche Fraktionsnamen festlegen

table(df_votes_50$ParlGroupName)
df_votes_50$ParlGroupName <- df_votes_50$ParlGroupName %>%
                                          str_replace("CVP-Fraktion", "Mitte") %>% 
                                          str_replace("Fraktion der Schweizerischen Volkspartei", "SVP") %>% 
                                          str_replace("Sozialdemokratische Fraktion", "SP") %>% 
                                          str_replace("FDP-Liberale Fraktion", "FDP") %>% 
                                          str_replace("Grüne Fraktion", "Gruene") %>% 
                                          str_replace("Fraktion BD", "Mitte") %>% 
                                          str_replace("Grünliberale Fraktion", "GLP")

table(df_votes_51$ParlGroupName)
df_votes_51$ParlGroupName <- df_votes_51$ParlGroupName %>%
                                          str_replace("Die Mitte Fraktion. CVP.EVP.BDP.", "Mitte") %>% 
                                          str_replace("Fraktion der Schweizerischen Volkspartei", "SVP") %>% 
                                          str_replace("Sozialdemokratische Fraktion", "SP") %>% 
                                          str_replace("FDP-Liberale Fraktion", "FDP") %>% 
                                          str_replace("Grüne Fraktion", "Gruene") %>% 
                                          str_replace("Grünliberale Fraktion", "GLP")

table(df_votes_50_alle$ParlGroupName)
df_votes_50_alle$ParlGroupName <- df_votes_50_alle$ParlGroupName %>%
                            str_replace("Die Mitte Fraktion. CVP.EVP.BDP.", "Mitte") %>% 
                            str_replace("Fraktion der Schweizerischen Volkspartei", "SVP") %>% 
                            str_replace("Sozialdemokratische Fraktion", "SP") %>% 
                            str_replace("FDP-Liberale Fraktion", "FDP") %>% 
                            str_replace("Grüne Fraktion", "Gruene") %>% 
                            str_replace("Fraktion BD", "Mitte") %>% 
                            str_replace("CVP-Fraktion", "Mitte") %>% 
                            str_replace("Grünliberale Fraktion", "GLP")

table(df_votes_51_alle$ParlGroupName)
df_votes_51_alle$ParlGroupName <- df_votes_51_alle$ParlGroupName %>%
                                          str_replace("Die Mitte Fraktion. CVP.EVP.BDP.", "Mitte") %>% 
                                          str_replace("Fraktion der Schweizerischen Volkspartei", "SVP") %>% 
                                          str_replace("Sozialdemokratische Fraktion", "SP") %>% 
                                          str_replace("FDP-Liberale Fraktion", "FDP") %>% 
                                          str_replace("Grüne Fraktion", "Gruene") %>% 
                                          str_replace("Grünliberale Fraktion", "GLP")


## Funktion für Geschlossenheit der Fraktionen ####
get_fraktionen <- function(df_votes){
  
  ## Welche Abstimmungne wurden angenommen?
  res_votes <- df_votes %>% group_by(IdVote) %>% 
                            summarise(ja = sum(DecisionText == "Ja"),
                                      nein = sum(DecisionText == "Nein"),
                                      valid = sum((DecisionText == "Ja") | (DecisionText == "Nein")),
                                      enthaltung = sum(DecisionText == "Enthaltung"),
                                      entschuldigt = sum(DecisionText == "Entschuldigt gemäss Art. 57 Abs. 4"),
                                      unentschuldigt = sum(DecisionText == "Hat nicht teilgenommen"),
                                      praesident = sum(DecisionText == "Die Präsidentin/der Präsident stimmt nicht")) %>% 
                            mutate(ja_anteil = ifelse(ja == 0, 0, ja/valid)) %>% # Weil sonst potentiell div/0
                            mutate(angenommen = ja_anteil > 0.5)
  
  
  ## Welche Fraktionen haben wie gestimmt?
  res_frakt_long <- df_votes %>% group_by(IdVote, ParlGroupName, DecisionText) %>% 
                                 summarise(stimmen = n())
  
  
  res_frakt <- res_frakt_long %>% pivot_wider(id_cols = c(IdVote, ParlGroupName),
                                              names_from = DecisionText,
                                              values_from = stimmen) %>% 
    mutate_at(3:8, ~replace(., is.na(.), 0)) %>% # NAs sind effektiv 0 Stimmen
    mutate(valid = Ja + Nein) %>% 
    # mutate(anwesend = Ja + Nein + Enthaltung) %>%
    mutate(anwesend = Ja + Nein + Enthaltung + `Hat nicht teilgenommen`) %>% # Achtung, andere Definition als bei der Analyse letztes Jahr!
    left_join(res_votes[, c("IdVote", "angenommen")], by = "IdVote") %>%  # Wurde die Abstimmunge angenommen?
    mutate(anteil_ja = ifelse(Ja == 0, 0, Ja / anwesend)) %>% 
    mutate(anteil_mehrheit = ifelse(angenommen, anteil_ja, 1 - anteil_ja))
  
  
  ## Welche Fraktion hat mit der Mehrheit gestimmt?
  res_frakt_wide <- res_frakt %>% pivot_wider(id_cols = IdVote, 
                                              names_from = ParlGroupName,
                                              values_from = anteil_mehrheit) %>% 
                                  left_join(res_votes[, c("IdVote", "ja_anteil", "angenommen")],
                                            by = "IdVote") 
  
  if(!("andere" %in% colnames(res_frakt_wide))){
    res_frakt_wide <- res_frakt_wide %>% mutate(andere = 0) %>% 
                                         select(c(IdVote, andere, FDP, GLP, Gruene, Mitte, SP, SVP, ja_anteil, angenommen))
    print("ok")
  }
  
  return(res_frakt_wide)
}
  


## Funktion für Detailierte Koalitionszuweisung
get_koalitionen_det <- function(res_frakt_wide, lim_oben, lim_unten){
  
  res_koalitionen_matrix <- res_frakt_wide %>% 
    
    ## Pur Links
    mutate(k_SP = ifelse(              (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    < lim_oben) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
      
    mutate(k_SP_Gruene = ifelse(       (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
    
  
    ## Mitte Links
    mutate(k_SP_Gruene_Mitte = ifelse( (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%


    mutate(k_SP_Gruene_GLP = ifelse(   (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_SP_Gruene_GLP_Mitte = ifelse((SP  > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_SP_Mitte_GLP = ifelse(    (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%

    mutate(k_SP_GLP = ifelse(          (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
    
    mutate(k_SP_Mitte = ifelse(        (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
    
    mutate(k_Gruene_Mitte_GLP = ifelse((SP     < lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_Gruene_Mitte = ifelse(    (SP     < lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_Gruene_GLP = ifelse(      (SP     < lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    < lim_unten),1, 0)) %>%
    
    
    ## Mitte
    mutate(k_Mitte_FDP_GLP = ifelse(   (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_Mitte_FDP = ifelse(       (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_Mitte_GLP = ifelse(       (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    mutate(k_FDP_GLP = ifelse(         (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>%
  
    
    ## Mitte Rechts
    mutate(k_SVP_FDP_Mitte = ifelse(   (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
  
    mutate(k_SVP_FDP_GLP = ifelse(     (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
  
    mutate(k_SVP_FDP_Mitte_GLP = ifelse((SP    < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
  
    
    mutate(k_SVP_Mitte_GLP = ifelse(   (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
  
    mutate(k_SVP_Mitte = ifelse(       (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_oben  ) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
    
    mutate(k_SVP_GLP = ifelse(         (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
    

    ## Pur Rechts
    mutate(k_SVP = ifelse(             (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_oben) &
                                       (Mitte  < lim_oben) &
                                       (FDP    < lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%

    mutate(k_SVP_FDP = ifelse(         (SP     < lim_unten) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_oben) &
                                       (Mitte  < lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
  
    
    ## Grosse Koalition
    mutate(k_Gross = ifelse(           (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>% 
  
    
    ## Bundesratskoalitionen
    mutate(k_Bundesrat_alle = ifelse(  (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    < lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%     
    
    mutate(k_Bundesrat_GLP = ifelse(   (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    > lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%    
    
    mutate(k_Bundesrat_Gruene = ifelse((SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    > lim_oben),1, 0)) %>%
 
  
    ## Alle gegen SVP
    mutate(k_gegen_SVP_gross = ifelse( (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>% 
  
    mutate(k_SP_Mitte_FDP = ifelse(    (SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    < lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>% 
    
    mutate(k_SP_Mitte_GLP_FDP = ifelse((SP     > lim_oben) &
                                       (Gruene < lim_oben) &
                                       (GLP    > lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0)) %>%     
    
    
    ## Alle gegen FDP
    mutate(k_gegen_FDP = ifelse(       (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben) &
                                       (Mitte  > lim_oben) &
                                       (FDP    < lim_unten) &
                                       (SVP    > lim_oben),1, 0)) %>%  

  
    ## Unheilige
    mutate(k_SVP_SP = ifelse(          (SP     > lim_oben) &
                                       (Gruene < lim_unten) &
                                       (GLP    < lim_unten  ) &
                                       (Mitte  < lim_unten) &
                                       (FDP    < lim_unten) &
                                       (SVP    > lim_oben),1, 0)) %>% 
    
    mutate(k_SVP_SP_Gruene = ifelse(   (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_unten  ) &
                                       (Mitte  < lim_unten) &
                                       (FDP    < lim_unten) &
                                       (SVP    > lim_oben),1, 0)) %>% 
    
    mutate(k_SVP_Gruene = ifelse(      (SP     < lim_unten) &
                                       (Gruene > lim_oben) &
                                       (GLP    < lim_unten  ) &
                                       (Mitte  < lim_unten) &
                                       (FDP    < lim_unten) &
                                       (SVP    > lim_oben),1, 0)) %>% 


  ## Progressive Allianz
  mutate(k_prog_Allianz = ifelse(      (SP     > lim_oben) &
                                       (Gruene > lim_oben) &
                                       (GLP    > lim_oben  ) &
                                       (Mitte  < lim_unten) &
                                       (FDP    > lim_oben) &
                                       (SVP    < lim_unten),1, 0))
  
  
  ## Häufigkeit der Koalitionen  
  Nr_koalitionen <- dim(res_koalitionen_matrix)[2]-10
  
  # Kontrolle, ob es Überschneidungen der Koalitionen gibt
  res_koalitionen_matrix$kontrolle <- rowSums(res_koalitionen_matrix[11:(11+Nr_koalitionen-1)], na.rm = TRUE)
  if(max(res_koalitionen_matrix$kontrolle) > 1){
    print("Achtung, überlappene Zuweisungen!")
  } else{print("Keine Überlappungen")}
  
  res_koalitionen <-  data.frame(Anzahl = apply(res_koalitionen_matrix[, 11:(11+Nr_koalitionen-1)], 
                                                MARGIN = 2, 
                                                FUN = sum, na.rm = TRUE)) %>% 
                                          rownames_to_column(var = "Koalition") %>% tibble() %>% 
                                          mutate(Anteil = Anzahl / dim(res_koalitionen_matrix)[1])
  
  res_koalitionen[Nr_koalitionen+1, "Koalition"] <- "Total Zugewiesen"
  res_koalitionen[Nr_koalitionen+1, "Anzahl"] <- sum(res_koalitionen[1:Nr_koalitionen, "Anzahl"])
  res_koalitionen[Nr_koalitionen+1, "Anteil"] <-  sum(res_koalitionen[1:Nr_koalitionen, "Anteil"])
  
  res_koalitionen[Nr_koalitionen+2, "Koalition"] <- "ALLE"
  res_koalitionen[Nr_koalitionen+2, "Anzahl"] <- dim(res_koalitionen_matrix)[1]
  res_koalitionen[Nr_koalitionen+2, "Anteil"] <-  1
  
  ## Resultate zurückgeben
  return(list(res_koalitionen_matrix, res_koalitionen))
}


## Resultate Koalitionen ####
# Welche Grenzen werden für die Abgrenzung der Koalitionen vwerndet?
lim_oben <- 0.66 # mindestens lim_oben um zu einer Koalition dazuzugehören
lim_unten <- 0.4 # höchstens lim_unten um als Gegener der Koalition zu gelten


#### Schlussabstimmungen

## 49
res_frakt_wide_49 <- get_fraktionen(df_votes_49)
obj_49_det <- get_koalitionen_det(res_frakt_wide_49, lim_oben, lim_unten)

res_koalitionen_matrix_49_det <- obj_49_det[[1]]
res_koalitionen_49_det <- obj_49_det[[2]]

res_koalitionen_matrix_49_det %>% left_join(df_Ids_49, by = c("IdVote" = "ID")) %>% 
                                  write_delim("Data_output/res_frakt_49_det.csv", delim = ",")


## 50
res_frakt_wide_50 <- get_fraktionen(df_votes_50)
obj_50_det <- get_koalitionen_det(res_frakt_wide_50, lim_oben, lim_unten)

res_koalitionen_matrix_50_det <- obj_50_det[[1]]
res_koalitionen_50_det <- obj_50_det[[2]]

res_koalitionen_matrix_50_det %>% left_join(df_Ids_50, by = c("IdVote" = "ID")) %>% 
                                  write_delim("Data_output/res_frakt_50_det.csv", delim = ",")

## 51
res_frakt_wide_51 <- get_fraktionen(df_votes_51)
obj_51_det <- get_koalitionen_det(res_frakt_wide_51, lim_oben, lim_unten)

res_koalitionen_matrix_51_det <- obj_51_det[[1]]
res_koalitionen_51_det <- obj_51_det[[2]]

res_koalitionen_matrix_51_det %>% left_join(df_Ids_51, by = c("IdVote" = "ID")) %>% 
                                  write_delim("Data_output/res_frakt_51_det.csv", delim = ",")


## Schlussabstimmugen speichern

res_koalitionen_495010_det <- res_koalitionen_49_det %>% left_join(res_koalitionen_50_det, by = "Koalition", suffix = c("_49", "_50")) %>%
                                                         left_join(res_koalitionen_51_det, by = "Koalition") %>% 
                                                         rename(Anteil_51 = Anteil) %>% rename(Anzahl_51 = Anzahl) %>% 
                                                         select(c(Koalition, Anzahl_49, Anzahl_50, Anzahl_51,
                                                                  Anteil_49, Anteil_50, Anteil_51))
res_koalitionen_495010_det %>% write.table("clipboard", sep="\t", row.names=FALSE)



#### ALLE Abstimmungen

## 50 ALLE
res_frakt_wide_50_alle <- get_fraktionen(df_votes_50_alle)
obj_50_alle_det <- get_koalitionen_det(res_frakt_wide_50_alle, lim_oben, lim_unten)
res_koalitionen_matrix_50_alle_det <- obj_50_alle_det[[1]]
res_koalitionen_50_alle_det <- obj_50_alle_det[[2]]


res_koalitionen_matrix_50_alle_det %>% left_join(df_Ids_50_alle, by = c("IdVote" = "ID")) %>% 
                                       write_delim("Data_output/res_frakt_50_alle_det.csv", delim = ",")

## 51 ALLE
res_frakt_wide_51_alle <- get_fraktionen(df_votes_51_alle)
obj_51_alle_det <- get_koalitionen_det(res_frakt_wide_51_alle, lim_oben, lim_unten)
res_koalitionen_matrix_51_alle_det <- obj_51_alle_det[[1]]
res_koalitionen_51_alle_det <- obj_51_alle_det[[2]]

res_koalitionen_matrix_51_alle_det %>% left_join(df_Ids_51_alle, by = c("IdVote" = "ID")) %>% 
                                       write_delim("Data_output/res_frakt_51_alle_det.csv", delim = ",")



## ALLE speichern
res_koalitionen_495010_alle_det <- res_koalitionen_50_alle_det %>% left_join(res_koalitionen_51_alle_det, by = "Koalition", suffix = c("_50", "_51")) %>% 
                                                                   select(c(Koalition, Anzahl_50, Anzahl_51, Anteil_50, Anteil_51))
                                                                   
res_koalitionen_495010_alle_det %>% write.table("clipboard", sep="\t", row.names=FALSE)



## Erfolg der Fraktionen ####
# Achtung, hier wird "anteil_ja" anders definiert als oben, und zwar Gleich wie 
# um den Erfolg einer Abstimmung zu beurteilen: anteil_ja = ja/valid
# (und nich anteil_ja = ja/anwesend)

erfolg_fraktionen <- function(df_votes){
  
  ## Welche Abstimmungne wurden angenommen?
  res_votes <- df_votes %>% group_by(IdVote) %>% 
                            summarise(ja = sum(DecisionText == "Ja"),
                                      nein = sum(DecisionText == "Nein"),
                                      valid = sum((DecisionText == "Ja") | (DecisionText == "Nein")),
                                      enthaltung = sum(DecisionText == "Enthaltung"),
                                      entschuldigt = sum(DecisionText == "Entschuldigt gemäss Art. 57 Abs. 4"),
                                      unentschuldigt = sum(DecisionText == "Hat nicht teilgenommen"),
                                      praesident = sum(DecisionText == "Die Präsidentin/der Präsident stimmt nicht")) %>% 
                            mutate(ja_anteil = ifelse(ja == 0, 0, ja/valid)) %>% # Weil sonst potentiell div/0
                            mutate(angenommen_gesamt = ja_anteil > 0.5)
  
  
  ## Welche Fraktionen waren wie erfolgreich?
  res_frakt_long <- df_votes %>% group_by(IdVote, ParlGroupName, DecisionText) %>% 
                                 summarise(stimmen = n())
  
  
  res_frakt <- res_frakt_long %>% pivot_wider(id_cols = c(IdVote, ParlGroupName),
                                              names_from = DecisionText,
                                              values_from = stimmen) %>% 
                                  mutate_at(3:8, ~replace(., is.na(.), 0)) %>% # NAs sind effektiv 0 Stimmen
                                  mutate(valid = Ja + Nein) %>% 
                                  # mutate(anwesend = Ja + Nein + Enthaltung) %>%
                                  # mutate(anwesend = Ja + Nein + Enthaltung + `Hat nicht teilgenommen`) %>% # Achtung, andere Definition als bei der Analyse letztes Jahr!
                                  left_join(res_votes[, c("IdVote", "angenommen_gesamt", "ja_anteil")], by = "IdVote") %>%  # Wurde die Abstimmunge angenommen?
                                  rename(c("anteil_ja_gesamt" = "ja_anteil")) %>% 
                                  mutate(anteil_ja_partei = ifelse(Ja == 0, 0, Ja / valid)) %>% 
                                  mutate(angenommen_partei = anteil_ja_partei > 0.5) %>% 
                                  mutate(erfolg_partei = angenommen_partei == angenommen_gesamt)
  
  erfolg_fraktionen <- res_frakt %>% group_by(ParlGroupName) %>% 
                                     summarise(Anzahl_erfolge = sum(erfolg_partei),
                                               Anzahl_abstimmungen = n()) %>%  
                                     mutate(erfolg_proc = Anzahl_erfolge/Anzahl_abstimmungen) 
  return(erfolg_fraktionen)
}


## Schluss und Gesamtabstimmungen
erfolg_fraktion_50 <- erfolg_fraktionen(df_votes_50)
erfolg_fraktion_51 <- erfolg_fraktionen(df_votes_51)

erfolg_fraktionen_comb <- erfolg_fraktion_50 %>% left_join(erfolg_fraktion_51,
                                                           by = "ParlGroupName", suffix = c("_50", "_51"))
erfolg_fraktionen_comb %>% write.table("Data_output/erfolg_fraktionen.csv", sep="\t", row.names=FALSE)

## Alle Abstimmungen
erfolg_fraktion_50_alle <- erfolg_fraktionen(df_votes_50_alle)
erfolg_fraktion_51_alle <- erfolg_fraktionen(df_votes_51_alle)

erfolg_fraktionen_alle_comb <- erfolg_fraktion_50_alle %>% left_join(erfolg_fraktion_51_alle,
                                                           by = "ParlGroupName", suffix = c("_50", "_51"))
erfolg_fraktionen_alle_comb %>% write.table("Data_output/erfolg_fraktionen_alle.csv", sep="\t", row.names=FALSE)



## Erfolg der Parlamentarier ####


## Get Info von Politiker
# Achtung, Daniel Frei hat die Partei gewechselt, bei 51 ist das aber kein Problem, weil er nicht mehr dabei ist.

df_parls <- unique(rbind(df_votes_51)[, c("PersonNumber", "ParlGroupName")]) %>% 
  left_join(read_csv("Data_output/alle_parlamentarier.csv"), by = "PersonNumber")

erfolg_parl <- function(df_votes){
    
  ## Welche Abstimmungne wurden angenommen?
  res_votes <- df_votes %>% group_by(IdVote) %>% 
                            summarise(ja = sum(DecisionText == "Ja"),
                                      nein = sum(DecisionText == "Nein"),
                                      valid = sum((DecisionText == "Ja") | (DecisionText == "Nein")),
                                      enthaltung = sum(DecisionText == "Enthaltung"),
                                      entschuldigt = sum(DecisionText == "Entschuldigt gemäss Art. 57 Abs. 4"),
                                      unentschuldigt = sum(DecisionText == "Hat nicht teilgenommen"),
                                      praesident = sum(DecisionText == "Die Präsidentin/der Präsident stimmt nicht")) %>% 
                            mutate(ja_anteil = ifelse(ja == 0, 0, ja/valid)) %>% # Weil sonst potentiell div/0
                            mutate(angenommen_gesamt = ja_anteil > 0.5)
                          
  res_parl_long <- df_votes %>% group_by(IdVote, PersonNumber, DecisionText, ParlGroupName) %>% 
                                summarise(stimmen = n())
  
  
  res_parl <- res_parl_long %>% pivot_wider(id_cols = c(IdVote, PersonNumber, ParlGroupName),
                                                        names_from = DecisionText,
                                                        values_from = stimmen) %>% 
                                mutate_at(4:9, ~replace(., is.na(.), 0)) %>% # NAs sind effektiv 0 Stimmen
                                mutate(anwesend = Ja + Nein + Enthaltung + `Hat nicht teilgenommen`) %>% # Achtung, andere Definition als bei der Analyse letztes Jahr!
                                mutate(valid = ifelse((Ja + Nein) == 1, TRUE, FALSE)) %>%
                                left_join(res_votes[, c("IdVote", "angenommen_gesamt", "ja_anteil")], by = "IdVote") %>%  # Wurde die Abstimmunge angenommen?
                                rename(c("anteil_ja_gesamt" = "ja_anteil")) %>% 
                                mutate(erfolg_parl = (valid & (Ja == angenommen_gesamt)))
  
  erfolg_parl <- res_parl %>% group_by(PersonNumber, ParlGroupName) %>% 
                              summarise(Anzahl_erfolge = sum(erfolg_parl),
                                        Anzahl_anwesend = sum(anwesend)) %>% 
                              mutate(erfolg_proc = Anzahl_erfolge/Anzahl_anwesend) %>% 
                              arrange(-erfolg_proc) %>% 
                              left_join(df_parls[, c("PersonNumber", "PersonIdCode", "LastName", "FirstName", "GenderAsString")],
                                        by = "PersonNumber") %>% 
                              mutate(FullName = str_c(FirstName, " ", LastName))
  return(erfolg_parl)
}

erfolg_parl_51 <- erfolg_parl(df_votes_51)
erfolg_parl_51 %>% write_delim("Data_output/erfolg_parlamentarier_51.csv", delim = ",")
