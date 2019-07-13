#### Talia Kart ####
wektor.kart <- c("2P", "2S","2T","2K",
             "3P", "3S","3T","3K",
             "4P", "4S","4T","4K",
             "5P", "5S","5T","5K",
             "6P", "6S","6T","6K",
             "7P", "7S","7T","7K",
             "8P", "8S","8T","8K",
             "9P", "9S","9T","9K",
             "10P", "10S","10T","10K",
             "JP", "JS","JT","JK",
             "DP", "DS","DT","DK",
             "KP", "KS","KT","KK",
             "AP", "AS","AT","AK")
#####
for(d in 2:5)
{
l.graczy = d
karty.d = l.graczy*10
lista.graczy = c()
for(i in 1:l.graczy)
{
  lista.graczy[i] = (paste("Gracz ",i))
}
ost.m = matrix(ncol = l.graczy, nrow = 5000)
for(k in 1:5000)
{
losowanie = sample(1:52, size = karty.d, replace = F)
macierz.graczy = macierz.graczy.pozycja = matrix(ncol = l.graczy, nrow = 10)
poz.los = c()
karty = c()
for(i in 1:l.graczy)
{
  for(j in 1:10)
  {
    los = losowanie[i*j]
    poz.los = c(poz.los, los)
    karta = wektor.kart[los]
    karty = c(karty, karta)
  }
  macierz.graczy[,i] = karty
  macierz.graczy.pozycja[,i] = poz.los
  karty = c()
  poz.los = c()
}

colnames(macierz.graczy) = lista.graczy
colnames(macierz.graczy.pozycja) = lista.graczy
winner = c()
for(i in 1:10)
{
  winner.x = which(macierz.graczy.pozycja[i,] == max(macierz.graczy.pozycja[i,]))
  winner = c(winner, winner.x)
}

war = max(table(winner))
numer.gracza.w = as.numeric(which(table(winner) == war))

ost.m[k,numer.gracza.w] = 1
ost.m[k,-numer.gracza.w] = 0
}

colnames(ost.m) <- colnames(macierz.graczy)
ost.m.p = ost.m
ost.m.p = colSums(ost.m.p)
print(ost.m.p/nrow(ost.m))
}