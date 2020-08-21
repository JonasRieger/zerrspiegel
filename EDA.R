if(FALSE){
  mdb = c(200, 152, 67, 91, 69, 80, 46, 2)
  mdb[9] = 709-sum(mdb)
  names(mdb) = c("CDU", "SPD", "DIE GRUENEN", "AfD", "DIE LINKE", "FDP", "CSU",
    "Die blaue Partei", "Parteilos")
  saveRDS(mdb, file.path("Modellieren", "Helper", "mdb.rds"))
}

library(ggplot2)
library(ggparliament)

prep = readRDS(file.path("Modellieren", "Objekte", "docsred.rds"))
clean = readRDS(file.path("Modellieren", "Objekte", "cleanred.rds"))
# clean duerfen wir leider nicht veroeffentlichen!
parteivec = readRDS(file.path("Modellieren", "Helper", "parteiProText.rds"))
col = readRDS(file.path("Modellieren", "Helper", "parteifarben.rds"))
mdb = readRDS(file.path("Modellieren", "Helper", "mdb.rds"))

all = read.csv("all.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
politiker = read.csv("UserID_Partei.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
all$partei = politiker$Partei[match(all$user_id, politiker$user_id)]

btag = data.frame(
  year = 2017,
  country = "Germany",
  house = "Bundestag",
  party_long = names(mdb),
  party_short = names(mdb),
  seats = as.integer(mdb),
  colour = col[match(names(mdb), names(col))], stringsAsFactors = FALSE)
btag = btag[c(5,2,3,1,7,6,4,8,9),]
btag$colour[btag$party_short == "Parteilos"] = "grey"
btaggg = parliament_data(election_data = btag, parl_rows = 10, type = 'semicircle',
  party_seats = btag$seats)
btaggg$twitter = c(
  rep(1, 60), rep(0, btag$seats[btag$party_short == "DIE LINKE"] - 60),
  rep(1, 118), rep(0, btag$seats[btag$party_short == "SPD"] - 118),
  rep(1, 64), rep(0, btag$seats[btag$party_short == "DIE GRUENEN"] - 64),
  rep(1, 108), rep(0, btag$seats[btag$party_short == "CDU"] - 108),
  rep(1, 26), rep(0, btag$seats[btag$party_short == "CSU"] - 26),
  rep(1, 73), rep(0, btag$seats[btag$party_short == "FDP"] - 73),
  rep(1, 84), rep(0, btag$seats[btag$party_short == "AfD"] - 84),
  rep(1, 1), rep(0, btag$seats[btag$party_short == "Die blaue Partei"] - 1),
  rep(1, 2), rep(0, btag$seats[btag$party_short == "Parteilos"] - 2))
btagplot = ggplot(btaggg, aes(x, y, colour = party_short)) +
  geom_parliament_seats(size = 3) +
  labs(colour="Partei") +
  theme_ggparliament(legend = TRUE) +
  scale_colour_manual(values = btaggg$colour, limits = btaggg$party_short) +
  geom_highlight_government(twitter == 1, colour = "blue", size = 2.7) +
  theme(
    plot.margin=unit(c(-0.5, 1, 0.5, 0.5), units="line"))

# Maßzahlen
sum(btaggg$twitter)
mean(btaggg$twitter)

account_created = merge(unique(data.frame(user_id = all$user_id,
  date = as.Date(as.POSIXct(all$account_created_at)), stringsAsFactors = FALSE)),
  politiker)
pdf(file.path("Plots", "AccountsSmaller.pdf"), width = 12, height = 4)
plot(account_created$date, jitter(rep(0, 522)), axes = FALSE, xlab = "Datum",
  ylab = "", col = col[match(account_created$Partei, names(col))], pch = 20)
axis(1, at = seq.Date(as.Date("2009-01-01"), as.Date("2019-01-01"), "years")[seq(1, 11, 2)], labels = seq(2009, 2019, 2))
dev.off()

## neuer Plot: ecdf pro Partei
pdf(file.path("Plots", "AccountsNew.pdf"), width = 12, height = 5)
u = lapply(lapply(split(account_created$date, account_created$Partei), sort),
  function(x) stepfun(unique(x), c(0, cumsum(table(x)))))
plot(account_created$date, rep(40, 522), ylim = c(0, 110), type = "n", xlab = "Datum",
  ylab = "Anzahl Twitterer")
abline(v = as.Date(c("2009-09-27", "2013-09-22", "2017-09-24")), lty = 3)
#points(account_created$date, jitter(rep(55, 522), 45),
#  col = ggplot2::alpha(col[match(account_created$Partei, names(col))], 0.15), pch = 20)
for(i in names(u)){
  plot(u[[i]], add = TRUE, col = col[match(i, names(col))], do.points = F)
}
dev.off()

# Links pro Partei
urltypen = sapply(strsplit(clean$meta$url_new_expanded, "\\."), function(x) x[2])
taz = sapply(strsplit(gsub("http(s)*://", "", x = clean$meta$url_new_expanded), "\\."), function(x) x[1])
urltypen[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")] =
  taz[taz %in% c("rp-online", "taz", "jungefreiheit", "netzpolitik", "cducsu", "afdkompakt")]
tab = table(urltypen)
pot = names(tab)[tab > 100]
tab = sapply(split(urltypen, clean$meta$partei), function(x) table(factor(x[x %in% pot], levels = pot)))
write.csv(tab, file = file.path("Plots", "HPproPartei.csv"), fileEncoding = "UTF-8")
tab = cbind(tab, tab[, "CDU"] + tab[, "CSU"])
tab = tab[, !colnames(tab) %in% c("CDU", "CSU", "Parteilos", "Die blaue Partei")]
colnames(tab)[ncol(tab)] = "CDU/CSU"




pdf(file.path("Plots", "EDA.pdf"), width = 12, height = 7)

btagplot

# Anzahl Twitterer pro Partei
tab = table(politiker$Partei)
ordering = order(tab, decreasing = TRUE)
barplot(sort(tab, decreasing = TRUE), col = col[ordering], xlab = "Partei",
  ylab = "Anzahl MdB-Twitterer")

# Anzahl Tweets pro Partei
tab = table(all$partei)
ordering = order(tab, decreasing = TRUE)
barplot(sort(tab, decreasing = TRUE), col = col[ordering], xlab = "Partei",
  ylab = "Anzahl MdB-Tweets")

ntweetspolitiker = lapply(split(all$user_id, all$partei), table)
ordering = order(sapply(ntweetspolitiker, mean), decreasing = TRUE)
library(beanplot)
beanplot(ntweetspolitiker[ordering], col = as.list(col[ordering]), xlab = "Partei",
  ylab = "Anzahl Tweets pro MdB-Twitterer", cutmin = 0, cutmax = 3200)

# Anzahl Artikel pro Partei in unserer Analyse
# + relative Anzahl als Text (Sortierung variabel)
tab = table(parteivec)
rel = tab/table(all$partei)
ordering1 = order(tab, decreasing = TRUE)
ordering2 = order(rel, decreasing = TRUE)
ordering = ordering2
bp = barplot(tab[ordering], col = col[ordering], xlab = "Partei",
  ylab = "Anzahl verlinkter Inhalte in der Analyse", ylim = c(0, max(tab)+1000))
text(bp, tab[ordering] +500, paste0(round(rel[ordering], 2)*100, "%"))
ordering = ordering1
bp = barplot(tab[ordering], col = col[ordering], xlab = "Partei",
  ylab = "Anzahl verlinkter Inhalte in der Analyse", ylim = c(0, max(tab)+1000))
text(bp, tab[ordering] +500, paste0(round(rel[ordering], 2)*100, "%"))

# Anzahl Artikel pro Partei und Twitterer in unserer Analyse
linkperparteitweeterrel = linkperparteitweeter = list()
for(i in names(tab)){
  tmp = politiker$user_id[politiker$Partei == i]
  tmp = tmp[tmp %in% all$user_id]
  rels = abs = numeric(length(tmp))
  j = 1
  for(uid in tmp){
    n = sum(all$user_id == uid)
    nlink = sum(clean$meta$user_id == uid)
    rels[j] = nlink/n
    abs[j] = nlink
    j = j+1
  }
  linkperparteitweeter[[i]] = abs
  linkperparteitweeterrel[[i]] = rels
}
ordering = order(sapply(linkperparteitweeter, mean), decreasing = TRUE)
beanplot(linkperparteitweeter[ordering], col = as.list(col[ordering]), xlab = "Partei",
  ylab = "Anzahl verlinkter Inhalte in der Analyse pro MdB-Twitterer",
  cutmin = 0, cutmax = 3200)
ordering = order(sapply(linkperparteitweeterrel, mean), decreasing = TRUE)
beanplot(linkperparteitweeterrel[ordering], col = as.list(col[ordering]), xlab = "Partei",
  ylab = "relative Anzahl verlinkter Inhalte in der Analyse pro MdB-Twitterer",
  cutmin = 0, cutmax = 1)


# Anzahl Woerter pro Partei in unserer Analyse
tab = sapply(split(prep, parteivec), function(x) sum(sapply(x, ncol)))
ordering = order(tab, decreasing = TRUE)
barplot(sort(tab, decreasing = TRUE), col = col[ordering], xlab = "Partei",
  ylab = "Anzahl Woerter von verlinkten Inhalten in der Analyse")

tab = lapply(split(prep, parteivec), function(x) sapply(x, ncol))
ordering = order(sapply(tab, mean), decreasing = TRUE)
beanplot(tab[ordering], col = as.list(col[ordering]), xlab = "Partei",
  ylab = "Anzahl Woerter von verlinkten Inhalten in der Analyse pro Artikel",
  cutmin = 0, cutmax = max(unlist(tab)), maxstripline = 0)

dev.off()
