library(data.table)
library(tosca)

prep = readRDS(file.path("Modellieren", "Objekte", "docsred.rds"))
clean = readRDS(file.path("Modellieren", "Objekte", "cleanred.rds"))
# clean duerfen wir leider nicht veroeffentlichen!

all = read.csv("all.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
# all duerfen wir leider nicht veroeffentlichen (enthaelt alle Tweets mit meta-Informationen)!
politiker = read.csv("UserID_Partei.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
all$partei = politiker$Partei[match(all$user_id, politiker$user_id)]

# finde die URL cores
urltypen = sapply(strsplit(clean$meta$url_new_expanded, "\\."), function(x) x[2])
taz = sapply(strsplit(gsub("http(s)*://", "", x = clean$meta$url_new_expanded), "\\."), function(x) x[1])
urltypen[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")] =
  taz[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")]
tab = table(urltypen)

# zunaechst kein Filter
pot = names(tab)
tmp = sapply(split(urltypen, clean$meta$partei), function(x) table(factor(x[x %in% pot], levels = pot)))
# filtere alle Medien, die mindestens 11 mal verlinkt wurden
pot = names(tab)[tab > 10]
# spitte Tabelle fuer die verschiedenen Parteien
sonstige = colSums(tmp[!(rownames(tab) %in% pot),])
tab = sapply(split(urltypen, clean$meta$partei), function(x) table(factor(x[x %in% pot], levels = pot)))

tab2 = as.data.table(tab)
tab2 = rbind(tab2, as.data.table(t(sonstige)))
# zusammenfassen von CDU/CSU
tab2[,"CDU/CSU" := CDU + CSU]
# auswaehlen der interessierenden Parteien
tab2 = tab2[, c("AfD", "DIE GRUENEN", "DIE LINKE", "FDP", "SPD", "CDU/CSU")]
tab2 = as.data.frame(tab2)
rownames(tab2) = c(rownames(tab), "Sonstige")

setwd("Parteiverlinkungen")

# CSVs fuer einzelne Parteien schreiben
for(partei in c("AfD", "DIE GRUENEN", "DIE LINKE", "FDP", "SPD", "CDU/CSU")){
  tmp = tab2[order(tab2[,partei], decreasing = TRUE),]
  write.csv(tmp, paste0(gsub("/", " ", partei), ".csv"))
  tmp = t(t(tmp)/colSums(tmp))
  write.csv(tmp, paste0(gsub("/", " ", partei), "rel.csv"))
}

# CSVs schreiben fuer Sortierung nach Medienname
write.csv(tab, "cut10.csv")
write.csv(t(t(tab)/colSums(tab)), "cut10rel.csv")
write.csv(tab2, "cut10processed.csv")
write.csv(t(t(tab2)/colSums(tab2)), "cut10processedrel.csv")
