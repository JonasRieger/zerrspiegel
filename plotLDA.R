library(ldaPrototype)
library(tosca)
library(ineq)
library(beanplot)

vocab = readRDS(file.path("Modellieren", "Objekte", "vocab.rds"))
parteivec = readRDS(file.path("Modellieren", "Helper", "parteiProText.rds"))
parteivec2 = parteivec
parteivec2[parteivec2 %in% c("CDU", "CSU")] = "CDU/CSU"
col = readRDS(file.path("Modellieren", "Helper", "parteifarben.rds"))
prep = readRDS(file.path("Modellieren", "Objekte", "docsred.rds"))
clean = readRDS(file.path("Modellieren", "Objekte", "cleanred.rds"))
# clean duerfen wir leider nicht veroeffentlichen!
politiker = read.csv("UserID_Partei.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
politiker$fraktion = politiker$Partei
politiker$fraktion[politiker$fraktion %in% c("CDU", "CSU")] = "CDU/CSU"

for(K in c(20,40,50,60)){
  
  res = readRDS(file.path("Modellieren", paste0("proto", K, ".rds")))
  proto = getLDA(res)
  assignments = getAssignments(proto)
  
  #cSVs schreiben
  topwords = topWords(getTopics(proto), numWords = 20)
  Anteil = round(rowSums(getTopics(proto)) / sum(getTopics(proto)) * 100, 4)
  out = rbind(Anteil, topwords)
  colnames(out) = paste("Topic", seq_len(K))
  write.csv(out, file = file.path("Plots", paste0("LDA", K, ".csv")), fileEncoding = "UTF-8")
  
  # PDF schreiben
  pdf(file.path("Plots", paste0("LDA", K, ".pdf")), width = 12, height = 7)
  
  topicperpartei = do.call(rbind,
    lapply(split(assignments, parteivec),
      function(x) tabulate(unlist(x)+1, nbins = getK(proto))))
  topicperpartei = rbind(topicperpartei,
    colSums(topicperpartei[rownames(topicperpartei) %in% c("CDU", "CSU"),]))
  rownames(topicperpartei)[nrow(topicperpartei)] = "CDU/CSU"
  topicperpartei = topicperpartei[!rownames(topicperpartei) %in% c("CDU", "CSU"), ]
  # topWords(getTopics(proto), 5)
  
  # Reduzierung auf die entscheidenen Fraktionen
  topicperpartei2 = topicperpartei[!rownames(topicperpartei) %in%
      c("Die blaue Partei", "Parteilos"),]
  topicperpartei2 = topicperpartei2[c(1,4,6,2,5,3),]
  names(col)[names(col) == "CDU"] = "CDU/CSU"
  
  # Anteil der Parteien an den totalen Topiczuordnungen
  bp = barplot(t(t(topicperpartei2)/colSums(topicperpartei2)),
    col = col[match(rownames(topicperpartei2), names(col))],
    xlab = "Topic", ylab = "Anteil der totalen Topiczuordnungen pro Fraktion",
    width = colSums(topicperpartei2))
  text(bp, -0.01, labels = topWords(getTopics(proto)),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  
  # Anteil der Parteien an den relativen Topiczuordnungen
  # dh: erste Normierung der Parteien (in Summe jede Fraktion 1)
  # dann noch: Normierung der Topics
  topicperparteirel = t(t(topicperpartei2)/colSums(topicperpartei2))
  topicperparteirel = topicperparteirel /rowSums(topicperpartei2)
  
  bp = barplot(t(t(topicperparteirel)/colSums(topicperparteirel)),
    col = col[match(rownames(topicperpartei2), names(col))],
    xlab = "Topic", ylab = "Anteil der normierten Topiczuordnungen pro Fraktion",
    width = colSums(topicperpartei2))
  text(bp, -0.01, labels = topWords(getTopics(proto)),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  text(bp, 1.01, labels = round(apply(topicperparteirel, 2, Gini), 2),
    srt = 45, adj = 0, xpd = TRUE, cex = 0.8)
  text(-min(colSums(topicperpartei2)), 1.01, "Gini",
    srt = 45, adj = 0, xpd = TRUE, cex = 0.8)
  
  # als Heatmap
  colnames(topicperparteirel) = topWords(getTopics(proto))
  heatmap(topicperparteirel, scale = "none",
    col = colorspace::diverge_hsv(1000, power = 0.7), main = "Topiczuordnungen")
  
  # Gini pro Twitterer
  ginitwitterer = list()
  for(i in setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos"))){
    tmp = politiker$user_id[politiker$fraktion == i]
    tmp = tmp[tmp %in% clean$meta$user_id]
    val = numeric(length(tmp))
    j = 1
    for(uid in tmp){
      tab = tabulate(
        unlist(assignments[names(prep) %in% clean$meta$id[clean$meta$user_id == uid]])+1,
        getK(proto)) / colSums(topicperpartei2)
      val[j] = Gini(tab)
      j = j+1
    }
    ginitwitterer[[i]] = val
  }
  ginipartei = apply(topicperparteirel, 1, Gini)
  ordering1 = order(sapply(ginitwitterer, mean), decreasing = TRUE)
  ordering2 = order(ginipartei[match(names(ginitwitterer), names(ginipartei))], decreasing = TRUE)
  ginitwitterer = ginitwitterer[ordering2]
  beanplot(ginitwitterer, col = as.list(col[match(names(ginitwitterer), names(col))]),
    xlab = "Fraktion", ylab = "Gini pro Twitterer", cutmin = 0, cutmax = 1, log = "",
    ylim = c(0,1))
  points(1:6 + 0.2, ginipartei[match(names(ginitwitterer), names(ginipartei))], pch = 8)
  
  ginipp = readRDS(file.path("Modellieren", "Helper", "ginipp.rds"))
  ordering3.1 = match(colnames(ginipp), names(ginitwitterer))
  ordering3.2 = match(colnames(ginipp), names(ginipartei))
  
  plot(0, 0, type = "n", xlim = c(0.4,6.2), axes = FALSE, ylim = c(0,1), xlab = "Fraktion", ylab = "Gini")
  beanplot(ginitwitterer[ordering3.1], at = 1:6 - 0.2,
    col = as.list(col[match(colnames(ginipp), names(col))]), add = TRUE, names = rep("", 6), yaxis = FALSE)
  boxplot(ginipp, boxwex = 0.2, at = 1:6 + 0.1,
    col = gsub("#000000", "grey", col[match(colnames(ginipp), names(col))]), names = rep("", 6), yaxis = FALSE, add = TRUE)
  points(1:6 - 0.05, ginipartei[ordering3.2], pch = 21, bg = col[match(colnames(ginipp), names(col))])
  mtext(colnames(ginipp), side = 1, line = 1, at = 1:6-0.05)
  
  ## neue Plots (einzeln..)
  plot(0, 0, type = "n", xlim = c(0.4,6.2), ylim = c(0,1), xlab = "Fraktion", ylab = "Gini", axes = FALSE)
  axis(2)
  beanplot(ginitwitterer[ordering3.1], at = 1:6,
    col = as.list(col[match(colnames(ginipp), names(col))]), names = colnames(ginipp), add = TRUE)
  
  boxplot(ginipp, boxwex = 0.2, at = 1:6, xlab = "Fraktion", ylab = "Gini",
    col = gsub("#000000", "grey", col[match(colnames(ginipp), names(col))]), names = colnames(ginipp), yaxis = FALSE)
  points(1:6 + 0.2, ginipartei[ordering3.2], pch = 21, bg = col[match(colnames(ginipp), names(col))])
  
  # Engagement
  engage = (clean$meta$favorite_count + clean$meta$retweet_count)[match(names(prep), clean$meta$id)]
  pot = setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos"))
  topicengagement = do.call(rbind, lapply(pot, function(x){
    u = t(sapply(assignments[parteivec2 == x], function(y) tabulate(y+1, nbins = K)))
    u = u/rowSums(u)
    u[is.na(u)] = 0
    colSums(u * engage[parteivec2 == x])
  }))
  rownames(topicengagement) = pot
  colnames(topicengagement) = topWords(getTopics(proto))
  topicengagement = topicengagement[match(rownames(topicperparteirel), pot),]
  topicengagementrel = topicengagement/rowSums(topicengagement)
  
  bp = barplot(t(t(topicengagement)/colSums(topicengagement)),
    col = col[match(rownames(topicengagementrel), names(col))],
    xlab = "Topic", ylab = "Anteil des Engagements pro Fraktion",
    width = colSums(topicengagement), names.arg = rep("", K))
  text(bp, -0.01, labels = topWords(getTopics(proto)),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  
  bp = barplot(t(t(topicengagementrel)/colSums(topicengagementrel)),
    col = col[match(rownames(topicengagementrel), names(col))],
    xlab = "Topic", ylab = "Anteil des normierten Engagements pro Fraktion",
    width = colSums(topicengagement), names.arg = rep("", K))
  text(bp, -0.01, labels = topWords(getTopics(proto)),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  cor(apply(topicperpartei2, 2, Gini), colSums(topicengagement), method = "spearman")
  # [1] 0.07368421
  
  # als Heatmap
  heatmap(topicengagementrel, scale = "none",
    col = colorspace::diverge_hsv(1000, power = 0.7), main = "Engagement")
  
  # tsne
  dev = colSums(topicengagement) - mean(colSums(topicengagement))
  pal = colorspace::diverge_hsv(1000, power = 0.3)
  m = max(abs(dev))
  pal = data.frame(col = pal, val = seq(from = -m, to = m, length.out = 1000))
  colText1 = as.character(sapply(dev, function(x) pal$col[which.min(abs(pal$val - x))]))
  
  dev = apply(topicperparteirel, 2, Gini) - mean(apply(topicperparteirel, 2, Gini))
  pal = colorspace::diverge_hsv(1000, power = 0.3)
  m = max(abs(dev))
  pal = data.frame(col = pal, val = seq(from = -m, to = m, length.out = 1000))
  colText2 = as.character(sapply(dev, function(x) pal$col[which.min(abs(pal$val - x))]))
  
  #parteiwoerter = do.call(rbind, lapply(setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos")),
  #  function(x) tabulate(unlist(lapply(prep[parteivec2 == x], function(y) y[1,])), length(vocab))))
  #top = rbind(getTopics(proto), parteiwoerter)
  top = getTopics(proto)
  jacc = as.dist(1- getSimilarity(jaccardTopics(t(top))))
  set.seed(1895)
  dims = Rtsne::Rtsne(jacc, perplexity = round(sqrt(K/2)), max_iter = 6000, theta = 0)
  plot(dims$Y, type = "n", xlab = "Erste Dimension", ylab = "Zweite Dimension", asp = 1,
    cex.axis = 0.8, cex.lab = 0.8, main = "Farbe per Engagement")
  pointsize = rowSums(top)
  points(dims$Y, bg = colText1, pch = 21, cex = 5.3*pointsize/max(pointsize))
  textdim = dims$Y
  textdim[,1] = textdim[,1]+5
  text(textdim, adj = 0, labels = topWords(getTopics(proto)))
  
  plot(dims$Y, type = "n", xlab = "Erste Dimension", ylab = "Zweite Dimension", asp = 1,
    cex.axis = 0.8, cex.lab = 0.8, main = "Farbe per Gini")
  points(dims$Y, bg = colText2, pch = 21, cex = 5.3*pointsize/max(pointsize))
  text(textdim, adj = 0, labels = topWords(getTopics(proto)))
  
  dev.off()
}

#################################################
nam30 = c("Grenzwerte/Fahrverbote", "Politik allgemein", "Sonstiges", "Ungleichheit/Umverteilung",
  "Wahlen", "Datenschutz", "Wirtschaft", "Brexit", "Klimaschutz", "Migration", "Linke Programmatik", "Autoindustrie",
  "Kriminalität", "AfD-Spendenaffäre", "Sozialstaat", "Veranstaltungen", "Rechtsextremismus",
  "Medien", "Sonstiges", "Innere Sicherheit", "Parlament", "Verkehr", "Naturschutz", "EU", "Außenpolitik",
  "Mietpreise", "Haushalt", "Türkei/Syrien", "Minderheiten", "Konflikte")

K = 30

res = readRDS(file.path("Modellieren", paste0("proto", K, ".rds")))
proto = getLDA(res)
assignments = getAssignments(proto)

#cSVs schreiben
topwords = topWords(getTopics(proto), numWords = 20)
Anteil = round(rowSums(getTopics(proto)) / sum(getTopics(proto)) * 100, 4)
out = rbind(Anteil, topwords)
colnames(out) = paste("Topic", seq_len(K))
write.csv(out, file = file.path("Plots", paste0("LDA", K, ".csv")), fileEncoding = "UTF-8")

# PDF schreiben
pdf(file.path("Plots", paste0("LDA", K, ".pdf")), width = 12, height = 7)

topicperpartei = do.call(rbind,
  lapply(split(assignments, parteivec),
    function(x) tabulate(unlist(x)+1, nbins = getK(proto))))
topicperpartei = rbind(topicperpartei,
  colSums(topicperpartei[rownames(topicperpartei) %in% c("CDU", "CSU"),]))
rownames(topicperpartei)[nrow(topicperpartei)] = "CDU/CSU"
topicperpartei = topicperpartei[!rownames(topicperpartei) %in% c("CDU", "CSU"), ]
# topWords(getTopics(proto), 5)

# Reduzierung auf die entscheidenen Fraktionen
topicperpartei2 = topicperpartei[!rownames(topicperpartei) %in%
    c("Die blaue Partei", "Parteilos"),]
topicperpartei2 = topicperpartei2[c(1,4,6,2,5,3),]
names(col)[names(col) == "CDU"] = "CDU/CSU"

# Anteil der Parteien an den totalen Topiczuordnungen
par(mar = c(6,4,1,0))
bp = barplot(t(t(topicperpartei2)/colSums(topicperpartei2)),
  col = col[match(rownames(topicperpartei2), names(col))],
  xlab = "", ylab = "Anteil der totalen Topiczuordnungen pro Fraktion",
  width = colSums(topicperpartei2))
text(bp, -0.01, labels = nam30,
  srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

# Anteil der Parteien an den relativen Topiczuordnungen
# dh: erste Normierung der Parteien (in Summe jede Fraktion 1)
# dann noch: Normierung der Topics
par(mar = c(6,4,2,0))
topicperparteirel = t(t(topicperpartei2)/colSums(topicperpartei2))
topicperparteirel = topicperparteirel /rowSums(topicperpartei2)

bp = barplot(t(t(topicperparteirel)/colSums(topicperparteirel)),
  col = col[match(rownames(topicperpartei2), names(col))],
  xlab = "", ylab = "Anteil der normierten Topiczuordnungen pro Fraktion",
  width = colSums(topicperpartei2))
text(bp, -0.01, labels = nam30,
  srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
text(bp, 1.01, labels = round(apply(topicperparteirel, 2, Gini), 2),
  srt = 45, adj = 0, xpd = TRUE, cex = 0.8)
text(-min(colSums(topicperpartei2)), 1.01, "Gini",
  srt = 45, adj = 0, xpd = TRUE, cex = 0.8)

# als Heatmap
colnames(topicperparteirel) = topWords(getTopics(proto))
heatmap(topicperparteirel, scale = "none",
  col = colorspace::diverge_hsv(1000, power = 0.7), main = "",
  labCol = nam30, margins = c(10,2))

# Gini pro Twitterer
par(mar = c(4,4,1,1))
ginitwitterer = list()
for(i in setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos"))){
  tmp = politiker$user_id[politiker$fraktion == i]
  tmp = tmp[tmp %in% clean$meta$user_id]
  val = numeric(length(tmp))
  j = 1
  for(uid in tmp){
    tab = tabulate(
      unlist(assignments[names(prep) %in% clean$meta$id[clean$meta$user_id == uid]])+1,
      getK(proto)) / colSums(topicperpartei2)
    val[j] = Gini(tab)
    j = j+1
  }
  ginitwitterer[[i]] = val
}
ginipartei = apply(topicperparteirel, 1, Gini)
ordering1 = order(sapply(ginitwitterer, mean), decreasing = TRUE)
ordering2 = order(ginipartei[match(names(ginitwitterer), names(ginipartei))], decreasing = TRUE)
ginitwitterer = ginitwitterer[ordering2]
beanplot(ginitwitterer, col = as.list(col[match(names(ginitwitterer), names(col))]),
  xlab = "Fraktion", ylab = "Gini pro Twitterer", cutmin = 0, cutmax = 1, log = "",
  ylim = c(0,1))
points(1:6 + 0.2, ginipartei[match(names(ginitwitterer), names(ginipartei))], pch = 8)

ginipp = readRDS(file.path("Modellieren", "Helper", "ginipp.rds"))
ordering3.1 = match(colnames(ginipp), names(ginitwitterer))
ordering3.2 = match(colnames(ginipp), names(ginipartei))

plot(0, 0, type = "n", xlim = c(0.4,6.2), axes = FALSE, ylim = c(0,1), xlab = "Fraktion", ylab = "Gini")
beanplot(ginitwitterer[ordering3.1], at = 1:6 - 0.2,
  col = as.list(col[match(colnames(ginipp), names(col))]), add = TRUE, names = rep("", 6), yaxis = FALSE)
boxplot(ginipp, boxwex = 0.2, at = 1:6 + 0.1,
  col = gsub("#000000", "grey", col[match(colnames(ginipp), names(col))]), names = rep("", 6), yaxis = FALSE, add = TRUE)
points(1:6 - 0.05, ginipartei[ordering3.2], pch = 21, bg = col[match(colnames(ginipp), names(col))])
mtext(colnames(ginipp), side = 1, line = 1, at = 1:6-0.05)

## neue Plots (einzeln..)
plot(0, 0, type = "n", xlim = c(0.4,6.2), ylim = c(0,1), xlab = "Fraktion", ylab = "Gini", axes = FALSE)
axis(2)
beanplot(ginitwitterer[ordering3.1], at = 1:6,
  col = as.list(col[match(colnames(ginipp), names(col))]), names = colnames(ginipp), add = TRUE)

boxplot(ginipp, boxwex = 0.2, at = 1:6, xlab = "Fraktion", ylab = "Gini",
  col = gsub("#000000", "grey", col[match(colnames(ginipp), names(col))]), names = colnames(ginipp), yaxis = FALSE)
points(1:6 + 0.2, ginipartei[ordering3.2], pch = 21, bg = col[match(colnames(ginipp), names(col))])

# Engagement
par(mar = c(6,4,1,0))
engage = (clean$meta$favorite_count + clean$meta$retweet_count)[match(names(prep), clean$meta$id)]
pot = setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos"))
topicengagement = do.call(rbind, lapply(pot, function(x){
  u = t(sapply(assignments[parteivec2 == x], function(y) tabulate(y+1, nbins = K)))
  u = u/rowSums(u)
  u[is.na(u)] = 0
  colSums(u * engage[parteivec2 == x])
}))
rownames(topicengagement) = pot
colnames(topicengagement) = topWords(getTopics(proto))
topicengagement = topicengagement[match(rownames(topicperparteirel), pot),]
topicengagementrel = topicengagement/rowSums(topicengagement)

bp = barplot(t(t(topicengagement)/colSums(topicengagement)),
  col = col[match(rownames(topicengagementrel), names(col))],
  xlab = "", ylab = "Anteil des Engagements pro Fraktion",
  width = colSums(topicengagement), names.arg = rep("", K))
text(bp, -0.01, labels = nam30,
  srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

bp = barplot(t(t(topicengagementrel)/colSums(topicengagementrel)),
  col = col[match(rownames(topicengagementrel), names(col))],
  xlab = "", ylab = "Anteil des normierten Engagements pro Fraktion",
  width = colSums(topicengagement), names.arg = rep("", K))
text(bp, -0.01, labels = nam30,
  srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

apply(topicengagementrel, 1, function(x) cor(apply(topicperparteirel, 2, Gini), x, method = "spearman"))
#AfD         FDP     CDU/CSU DIE GRUENEN         SPD   DIE LINKE 
#0.1154616  -0.4082314  -0.4416018   0.3704116  -0.2609566  -0.1844271 


# als Heatmap
heatmap(topicengagementrel, scale = "none",
  col = colorspace::diverge_hsv(1000, power = 0.7), main = "",
  margins = c(10,2), labCol = nam30)


### EINSCHUB
###########
z = t(rbind(topicengagement, colSums(topicengagement)))
colnames(z)[7] = "gesamt"
rownames(z) = nam30

write.csv(z, file = file.path("Plots", "engagement.csv"), fileEncoding = "UTF-8")

##############
tabu = table(clean$meta$partei)
tabu["CDU/CSU"] = sum(tabu["CDU"] + tabu["CSU"])
z = topicengagement / as.numeric(tabu[match(rownames(topicengagement), names(tabu))])

heatmap(z/rowSums(z), scale = "none",
  col = colorspace::diverge_hsv(1000, power = 0.7), main = "",
  margins = c(10,2), labCol = nam30)

z2 = rbind(z, colMeans(z))
z2 = t(z2)
colnames(z2)[7] = "im Mittel"
z2 = rbind(z2, colMeans(z2))
rownames(z2) = c(nam30, "im Mittel")
write.csv(z2, file = file.path("Plots", "engagement.csv"), fileEncoding = "UTF-8")

#####################

# tsne
par(mar = c(4.2,4,1,1))
dev = colSums(topicengagement) - mean(colSums(topicengagement))
pal = colorspace::diverge_hsv(1000, power = 0.3)
m = max(abs(dev))
pal = data.frame(col = pal, val = seq(from = -m, to = m, length.out = 1000))
colText1 = as.character(sapply(dev, function(x) pal$col[which.min(abs(pal$val - x))]))

dev = apply(topicperparteirel, 2, Gini) - mean(apply(topicperparteirel, 2, Gini))
pal = colorspace::diverge_hsv(1000, power = 0.3)
m = max(abs(dev))
pal = data.frame(col = pal, val = seq(from = -m, to = m, length.out = 1000))
colText2 = as.character(sapply(dev, function(x) pal$col[which.min(abs(pal$val - x))]))

#parteiwoerter = do.call(rbind, lapply(setdiff(unique(politiker$fraktion), c("Die blaue Partei", "Parteilos")),
#  function(x) tabulate(unlist(lapply(prep[parteivec2 == x], function(y) y[1,])), length(vocab))))
#top = rbind(getTopics(proto), parteiwoerter)
top = getTopics(proto)
jacc = as.dist(1- getSimilarity(jaccardTopics(t(top))))
set.seed(1895)
dims = Rtsne::Rtsne(jacc, perplexity = round(sqrt(K/2)), max_iter = 6000, theta = 0)
plot(dims$Y, type = "n", xlab = "Erste Dimension", ylab = "Zweite Dimension", asp = 1,
  cex.axis = 0.8, cex.lab = 0.8, main = "Farbe per Engagement")
pointsize = rowSums(top)
points(dims$Y, bg = colText1, pch = 21, cex = 5.3*pointsize/max(pointsize))
textdim = dims$Y
textdim[,1] = textdim[,1]+5
text(textdim, adj = 0, labels = topWords(getTopics(proto)))

plot(dims$Y, type = "n", xlab = "Erste Dimension", ylab = "Zweite Dimension", asp = 1,
  cex.axis = 0.8, cex.lab = 0.8, main = "Farbe per Gini")
points(dims$Y, bg = colText2, pch = 21, cex = 5.3*pointsize/max(pointsize))
text(textdim, adj = 0, labels = topWords(getTopics(proto)))

plot(c(0, max(z)), c(0, max(topicperpartei2)), type = "n", xlab = "Engagament-Score pro Tweet",
  ylab = "Anzahl Topiczuordnungen")
corel = numeric(0)
for(a in rownames(z)){
  points(z[a,], topicperpartei2[a,], bg = col[names(col) == a], pch = 21)
  corel[a] = cor(z[a,], topicperpartei2[a,], method = "spearman")
}

plot(c(0, max(z)), c(0, 1), type = "n", xlab = "Engagament-Score pro Tweet",
  ylab = "Anzahl Topiczuordnungen (mit dem Maximum normiert)")
for(a in rownames(z)){
  points(z[a,], topicperpartei2[a,]/max(topicperpartei2[a,]), bg = col[names(col) == a], pch = 21)
}

plot(c(0, 1), c(0, 1), type = "n", xlab = "Engagament-Score pro Tweet (mit dem Maximum normiert)",
  ylab = "Anzahl Topiczuordnungen (mit dem Maximum normiert)")
for(a in rownames(z)){
  points(z[a,]/max(z[a,]), topicperpartei2[a,]/max(topicperpartei2[a,]), bg = col[names(col) == a], pch = 21)
}

dev.off()
