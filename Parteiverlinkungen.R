library(data.table)
library(tosca)

prep = readRDS(file.path("Objekte", "docsred.rds"))
# clean = readRDS(file.path("Objekte", "cleanred.rds"))
# clean duerfen wir leider nicht veroeffentlichen!

# all = read.csv("all.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
# all duerfen wir leider nicht veroeffentlichen (enthaelt alle Tweets mit meta-Informationen)!
politiker = read.csv("UserID_Partei.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
all$partei = politiker$Partei[match(all$user_id, politiker$user_id)]

# finde die URL cores
urltypen = sapply(strsplit(clean$meta$url_new_expanded, "\\."), function(x) x[2])
taz = sapply(strsplit(gsub("http(s)*://", "", x = clean$meta$url_new_expanded), "\\."), function(x) x[1])
urltypen[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")] =
  taz[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")]
tab = table(urltypen)
# filtere alle Medien, die mindestens 11 mal verlinkt wurden
pot = names(tab)[tab > 10]
# spitte Tabelle fuer die verschiedenen Parteien
tab = sapply(split(urltypen, clean$meta$partei), function(x) table(factor(x[x %in% pot], levels = pot)))

tab2 = as.data.table(tab)
# zusammenfassen von CDU/CSU
tab2[,"CDU/CSU" := CDU + CSU]
# auswaehlen der interessierenden Parteien
tab2 = tab2[, c("AfD", "DIE GRUENEN", "DIE LINKE", "FDP", "SPD", "CDU/CSU")]
tab2 = as.data.frame(tab2)
rownames(tab2) = rownames(tab)

setwd("Parteiverlinkungen")

# CSVs fuer einzelne Parteien schreiben
for(partei in c("AfD", "DIE GRUENEN", "DIE LINKE", "FDP", "SPD", "CDU/CSU")){
  write.csv(tab2[order(tab2[,partei], decreasing = TRUE),][1:15,], paste0(gsub("/", " ", partei), ".csv"))
}

# CSVs schreiben fuer Sortierung nach Medienname
write.csv(tab, "cut10.csv")
write.csv(tab2, "cut10processed.csv")
