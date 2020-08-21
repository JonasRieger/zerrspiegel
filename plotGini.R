library(ldaPrototype)
library(ineq)
library(tosca)

if(FALSE){
  prep = readRDS(file.path("Modellieren", "Objekte", "docsred.rds"))
  clean = readRDS(file.path("Modellieren", "Objekte", "cleanred.rds"))
  # clean duerfen wir leider nicht veroeffentlichen!
  parteien = clean$meta$partei[match(names(prep), clean$meta$id)]
  saveRDS(parteien, file.path("Modellieren", "Helper", "parteiProText.rds"))
}
parteivec = readRDS(file.path("Modellieren", "Helper", "parteiProText.rds"))

batch = readRDS(file.path("Modellieren", "BatchK5bis75.rds"))
setFileDir(batch, file.path("Modellieren", "BatchK5bis75"))

lda = getLDA(batch)

topicperpartei = lapply(lda, function(x)
  do.call(rbind, lapply(split(getAssignments(x), parteivec), function(y) tabulate(unlist(y)+1, nbins = getK(x)))))

if(FALSE){
  parteinamen = rownames(topicperpartei[[1]])
  parteishort = parteinamen
  parteishort[parteishort == "DIE GRUENEN"] = "Gruene"
  parteishort[parteishort == "DIE LINKE"] = "Linke"
  parteishort[parteishort == "Die blaue Partei"] = "Blaue"
  saveRDS(parteishort, file.path("Modellieren", "Helper", "parteinamenShort.rds"))
  saveRDS(parteinamen, file.path("Modellieren", "Helper", "parteinamen.rds"))

  fdp = rgb(red = 255/255, green = 237/255, blue = 0)
  cdublack = rgb(red = 0/255, green = 0/255, blue = 0/255)
  cdurot = rgb(red = 235/255, green = 39/255, blue = 41/255)
  gruene = rgb(red = 100/255, green = 161/255, blue = 45/255)
  spd = rgb(red = 227/255, green = 0/255, blue = 15/255)
  linkepurpur = rgb(red = 152/255, green = 2/255, blue = 103/255)
  linkeggplot = "#BE3075"
  afd = rgb(red = 0/255, green = 158/255, blue = 224/255)
  blaue = rgb(red = 0/255, green = 102/255, blue = 153/255)
  csublau = rgb(red = 0, green = 137/255, blue = 207/255)
  csublack = rgb(red = 0, green = 0, blue = 0)
  csugrey = "#4c514a"
  parteicol = c(afd, cdublack, csugrey, blaue, gruene, linkeggplot, fdp, "white", spd)
  names(parteicol) = c("AfD", "CDU", "CSU", "Die blaue Partei", "DIE GRUENEN",
    "DIE LINKE", "FDP", "Parteilos", "SPD")
  saveRDS(parteicol, file.path("Modellieren", "Helper", "parteifarben.rds"))
}
partei = readRDS(file.path("Modellieren", "Helper", "parteinamenShort.rds"))
col = readRDS(file.path("Modellieren", "Helper", "parteifarben.rds"))

colbox = col
colbox[partei == "CDU"] = "#4c514a" # sonst Median nicht erkennbar

colp = col
colp[col == "white"] = "grey" # sonst Punkte nicht erkennbar

ohneBlauundParteilos = !partei %in% c("Blaue", "Parteilos")

# Parteien, absolut
pdf(file.path("Plots", "GiniParteienAbs.pdf"), width = 12, height = 7)
giniparteienabs = do.call(cbind, lapply(topicperpartei, function(x) apply(x, 1, Gini)))

colnames(giniparteienabs) = 5:75
boxplot(t(giniparteienabs), col = colbox, ylim = c(0,0.6),
  names = partei, ylab = "Gini (K = 5, ..., 75)", xlab = "Partei")
boxplot(t(giniparteienabs[, as.integer(colnames(giniparteienabs)) > 19]), col = colbox, ylim = c(0,0.6),
  names = partei, ylab = "Gini (K = 20, ..., 75)", xlab = "Partei")

plot(5:75, giniparteienabs[1,], col = col[1], pch = 20, xlab = "K", ylab = "Gini",
  type = "n", ylim = c(0,0.6), xlim = c(5, 80))
for(i in seq_len(nrow(giniparteienabs))){
  points(5:75, giniparteienabs[i,], col = colp[i], pch = 20, type = "b")
}
hoehe = giniparteienabs[,71]
hoehe[partei == "CSU"] = hoehe[partei == "Parteilos"] +0.01
hoehe[partei == "FDP"] = hoehe[partei == "AfD"] -0.01
hoehe[partei == "CDU"] = hoehe[partei == "FDP"] -0.01
text(76, hoehe, partei, col = colp, adj = 0)
dev.off()

# Parteien, relativ
pdf(file.path("Plots", "GiniParteienRel.pdf"), width = 12, height = 7)
giniparteienrel = do.call(cbind, lapply(topicperpartei, function(x) apply(t(t(x)/colSums(x)), 1, Gini)))
colnames(giniparteienrel) = 5:75
boxplot(t(giniparteienrel), col = colbox, ylim = c(0,0.6), names = partei, ylab = "Gini (K = 5, ..., 75)", xlab = "Partei")
boxplot(t(giniparteienrel[, as.integer(colnames(giniparteienrel)) > 19]), col = colbox, ylim = c(0,0.6), names = partei, ylab = "Gini (K = 20, ..., 75)", xlab = "Partei")

plot(5:75, giniparteienrel[1,], col = col[1], pch = 20, xlab = "K", ylab = "Gini",
  type = "n", ylim = c(0,0.6), xlim = c(5, 80))
for(i in seq_len(nrow(giniparteienrel))){
  points(5:75, giniparteienrel[i,], col = colp[i], pch = 20, type = "b")
}
#sorted = sort(giniparteienrel[,71])
#ind = abs(sorted - median(sorted)) - c(seq(4,1,-1), seq(0,4,1))*0.02
#sorted[ind<0] = (median(sorted) + seq(-4,4,1)*0.02)[ind<0]
#text(76, sorted, partei[order(giniparteienrel[,71])], col = colp[order(giniparteienrel[,71])], adj = 0)
hoehe = giniparteienrel[,71]
hoehe[partei == "FDP"] = hoehe[partei == "Parteilos"] -0.01
hoehe[partei == "Linke"] = hoehe[partei == "CDU"] +0.01
text(76, hoehe, partei, col = colp, adj = 0)
dev.off()

# Parteien, absolut ohne Blaue und Parteilos und Union statt CDU und CSU
pdf(file.path("Plots", "GiniParteienAbs2.pdf"), width = 12, height = 7)
giniparteienabs2 = rbind(giniparteienabs[ohneBlauundParteilos,], sapply(topicperpartei, function(x) Gini(colSums(x[2:3,]))))
rownames(giniparteienabs2)[8] = "CDU/CSU"
giniparteienabs2 = giniparteienabs2[c(1,4:8),]
ordering = order(apply(giniparteienabs2, 1, median), decreasing = TRUE)
giniparteienabs2 = giniparteienabs2[ordering,]
rownames(giniparteienabs2)[6:5] = c("Gruene", "Linke")
colboxred = colbox[match(rownames(giniparteienabs2), partei)]
rownames(giniparteienabs2)[6:5] = c("DIE GRUENEN", "DIE LINKE")
colboxred[is.na(colboxred)] = "#4c514a"

boxplot(t(giniparteienabs2), col = colboxred, ylim = c(0,0.4), ylab = "Gini (K = 5, ..., 75)", xlab = "Fraktion")
boxplot(t(giniparteienabs2[, as.integer(colnames(giniparteienabs2)) > 19]), col = colboxred, ylim = c(0,0.4), ylab = "Gini (K = 20, ..., 75)", xlab = "Fraktion")

colboxred[colboxred == "#4c514a"] = "black"
plot(5:75, giniparteienabs2[1,], xlab = "K", ylab = "Gini", type = "n", ylim = c(0,0.4), xlim = c(5, 83))
for(i in seq_len(nrow(giniparteienabs2))){
  points(5:75, giniparteienabs2[i,], col = colboxred[i], pch = 20, type = "b")
}
text(76, giniparteienabs2[,71], rownames(giniparteienabs2), col = colboxred, adj = 0)
dev.off()

# Parteien, relativ ohne Blaue und Parteilos und zusaetzlich Union
pdf(file.path("Plots", "GiniParteienRel2.pdf"), width = 12, height = 7)
giniparteienrel2 = do.call(cbind, lapply(
  lapply(topicperpartei, function(x) rbind(x[c(1,4:9),], colSums(x[2:3,]))),
  function(x) apply(t(t(x)/colSums(x)), 1, Gini)))
colnames(giniparteienrel2) = 5:75
giniparteienrel2 = giniparteienrel2[c(1,3:5, 7:8),]
rownames(giniparteienrel2)[6] = "CDU/CSU"
ordering = order(apply(giniparteienrel2, 1, median), decreasing = TRUE)
giniparteienrel2 = giniparteienrel2[ordering,]
rownames(giniparteienrel2)[c(6,3)] = c("Gruene", "Linke")
colboxred = colbox[match(rownames(giniparteienrel2), partei)]
rownames(giniparteienrel2)[c(6,3)] = c("DIE GRUENEN", "DIE LINKE")
colboxred[is.na(colboxred)] = "#4c514a"

boxplot(t(giniparteienrel2), col = colboxred, ylim = c(0,0.3), ylab = "Gini (K = 5, ..., 75)", xlab = "Fraktion")
boxplot(t(giniparteienrel2[, as.integer(colnames(giniparteienrel2)) > 19]), col = colboxred, ylim = c(0,0.3), ylab = "Gini (K = 20, ..., 75)", xlab = "Fraktion")

saveRDS(t(giniparteienrel2[, as.integer(colnames(giniparteienrel2)) > 19]), file = file.path("Modellieren", "Helper", "ginipp.rds"))

colboxred[colboxred == "#4c514a"] = "black"
plot(5:75, giniparteienrel2[1,], xlab = "K", ylab = "Gini", type = "n", ylim = c(0,0.3), xlim = c(5, 83))
for(i in seq_len(nrow(giniparteienrel2))){
  points(5:75, giniparteienrel2[i,], col = colboxred[i], pch = 20, type = "b")
}
text(76, giniparteienrel2[,71], rownames(giniparteienrel2), col = colboxred, adj = 0)
dev.off()
