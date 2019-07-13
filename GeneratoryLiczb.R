#### Wczytanie bazy ####
library(readxl)
Symulacja <- read_excel("C:/Users/Yogi/Desktop/Symulacja/Symulacja.xlsx")
#### Czêœæ 1 - Metody Reprezentacyjne ####
#### Treœæ zadania 
#### 1. Na podstawie powy¿szego zbioru nale¿y oszacowaæ minimaln¹ liczebnoœæ próby wymagan¹ w badaniu.
#### Przedsiêbiorstwo zlecaj¹ce badanie nie pozwala na zastosowanie próby wstêpnej, dlatego nale¿y odrazu
#### wylosowaæ próbê zasadnicz¹. W badaniu nale¿y oszacowaæ frakcjê osób z nadwag¹ oraz œrednie wydatki na leczenie.
#### Maksymalny b³¹d który mo¿na przyj¹æ w badaniu frakcji wynosi 3%, a b³¹d w badaniu wydatków nie wiêkszy
#### ni¿ 1 z³. Wiadomo ¿e w poprzednich badaniach wariancja wydatków na leczenie wynios³a 20.

Dane <- Symulacja
N <- length(Dane$nr.osoby)
p <- 0.5
q <- 1-p
S <- 20
d.frakcja <- 0.03; d.wydatki = 1
nmin.frakcja <- round((N/(1+(N*d.frakcja^2)/(4*p*q))),0)
nmin.wydatki <- round((N/(1+(N*d.wydatki^2)/(4*S^2))),0)              
nmin <- max(nmin.frakcja, nmin.wydatki)

estymatory <- function(proba)
{
  n = length(proba$nr.osoby)
  p = sum(proba$Nadwaga0N1T)/n
  q = 1 - p
  D2p.frakcja = ((N-n)/(N-1))*((p*q)/n)
  Dp.frakcja = D2p.frakcja^(0.5)
  D2p.wydatki = (1-(n/N))*((var(proba$wydatki.na.leczenie.z³))/n)
  Dp.wydatki = D2p.wydatki^(0.5)
  srednie.wydatki <- mean(proba$wydatki.na.leczenie.z³)
  
  wnk <- list(n = n, p = p, D2p.frakcja = D2p.frakcja,
              Dp.frakcja = Dp.frakcja, D2p.wydatki = D2p.wydatki
              , Dp.wydatki = Dp.wydatki, srednie.wydatki = srednie.wydatki)
  return(wnk)
}

blad.wydatki <- function(proba)
{
  n = length(proba$nr.osoby)
  D2p.wydatki = (1-(n/N))*((var(proba$wydatki.na.leczenie.z³))/n)
  Dp.wydatki = D2p.wydatki^(0.5)
  wnk <- Dp.wydatki
  return(wnk)
}
blad.frakcja <- function(proba)
{
  n = length(proba$nr.osoby)
  p = sum(proba$Nadwaga0N1T)/n
  q = 1 - p
  D2p.frakcja = ((N-n)/(N-1))*((p*q)/n)
  Dp.frakcja = D2p.frakcja^(0.5)
  wnk <- Dp.frakcja
  return(wnk)
}
srednia.wydatki <- function(proba)
{
  n = length(proba$nr.osoby)
  srednie.wydatki = sum(proba$wydatki.na.leczenie.z³)/n
  wnk <- srednie.wydatki
  return(srednie.wydatki)
}
srednia.waga <- function(proba)
{
  n = length(proba$nr.osoby)
  srednie.wagi = sum(proba$Nadwaga0N1T)/n
  wnk <- srednie.wagi
  return(srednie.wagi)
}
#### Czêœæ 2 - Generatory Losowe ####
#### Na podstawie powy¿szego zadania nale¿y sprawdziæ jak zachowuj¹ siê b³êdy pomiaru w przypadku ró¿nych generatorów liczb losowych, które losuj¹
#### probê w zadaniu
LCG <- 1
proba.LCG <- 1
a <- 1
wnk.wydatki.LCG.blad <- NA
wnk.srednia.wydatki.LCG <- NA
wnk.waga.LCG.blad <- NA
wnk.srednia.waga.LCG <- NA
for(j in 1:500)
{
  LCG <- 1
  for(i in 2:(nmin+1500))
  {
    a = sample(x=nmin, size = 1)
    LCG[1] <- sample(x = N,size = 1)
    LCG[i] <- (a* LCG[i-1])%%(N+1)
  }
  if(length(unique(LCG)) < nmin)
  {
    for(i in 2:(nmin+1500))
    {
      a = sample(x=nmin, size = 1)
      LCG[1] <- sample(x = N,size = 1)
      LCG[i] <- (a* LCG[i-1])%%(N+1)
    }
  }
  proba.LCG <- Dane[unique(LCG),]
  proba.LCG <- proba.LCG[1:nmin,]
  wnk.wydatki.LCG.blad <- c(wnk.wydatki.LCG.blad, blad.wydatki(proba.LCG))
  wnk.srednia.wydatki.LCG <- c(wnk.srednia.wydatki.LCG, srednia.wydatki(proba.LCG))
  wnk.waga.LCG.blad <- c(wnk.waga.LCG.blad, blad.frakcja(proba.LCG))
  wnk.srednia.waga.LCG <- c(wnk.srednia.waga.LCG, srednia.waga(proba.LCG))
}
wnk.wydatki.LCG.blad = wnk.wydatki.LCG.blad[2:length(wnk.wydatki.LCG.blad)]
wnk.srednia.wydatki.LCG = wnk.srednia.wydatki.LCG[2:length(wnk.srednia.wydatki.LCG)]
wnk.waga.LCG.blad = wnk.waga.LCG.blad[2:length(wnk.waga.LCG.blad)]
wnk.srednia.waga.LCG <- wnk.srednia.waga.LCG[2:length(wnk.srednia.waga.LCG)]
LCG.add <- 1
proba.LCG.add <- 1
a <- 1
wnk.wydatki.LCG.add.blad <- NA
wnk.srednia.wydatki.LCG.add <- NA
wnk.waga.LCG.add.blad <- NA
wnk.srednia.waga.LCG.add <- NA
for(j in 1:500)
{
  LCG.add <- 1
  for(i in 2:(nmin+1500))
  {
    a = sample(x=nmin, size = 1)
    c = 17
    LCG.add[1] <- sample(x = N,size = 1)
    LCG.add[i] <- (a* LCG.add[i-1] + c)%%(N+1)
  }
  if(length(unique(LCG.add)) < nmin)
  {
    for(i in 2:(nmin+1500))
    {
      a = sample(x=nmin, size = 1)
      c = 17
      LCG.add[1] <- sample(x = N,size = 1)
      LCG.add[i] <- (a* LCG.add[i-1] + c)%%(N+1)
    }
  }
  proba.LCG.add <- Dane[unique(LCG.add),]
  proba.LCG.add <- proba.LCG.add[1:nmin,]
  wnk.wydatki.LCG.add.blad <- c(wnk.wydatki.LCG.add.blad, blad.wydatki(proba.LCG.add))
  wnk.srednia.wydatki.LCG.add <- c(wnk.srednia.wydatki.LCG.add, srednia.wydatki(proba.LCG.add))
  wnk.waga.LCG.add.blad <- c(wnk.waga.LCG.add.blad, blad.frakcja(proba.LCG.add))
  wnk.srednia.waga.LCG.add <- c(wnk.srednia.waga.LCG.add, srednia.waga(proba.LCG.add))
}
wnk.srednia.waga.LCG.add <- na.omit(wnk.srednia.waga.LCG.add)
wnk.srednia.wydatki.LCG.add <- na.omit(wnk.srednia.wydatki.LCG.add)
wnk.waga.LCG.add.blad <- na.omit(wnk.waga.LCG.add.blad)
wnk.wydatki.LCG.add.blad <- na.omit(wnk.wydatki.LCG.add.blad)
kwadrat.generator <- 1
proba.kwadrat <- 1
a <- 1
wnk.wydatki.blad.kwadrat <- NA
wnk.srednia.wydatki.kwadrat <- NA
wnk.waga.blad.kwadrat <- NA
wnk.srednia.waga.kwadrat <- NA
for(j in 1:500)
{
  kwadrat.generator <- 1
  for(i in 2:(nmin+1500))
  {
    a = sample(x=nmin, size = 1)
    c = 17
    b = 5
    kwadrat.generator[1] <- sample(x = N,size = 1)
    kwadrat.generator[i] <- (a* (kwadrat.generator[i-1])^2 + b*kwadrat.generator[i-1] + c)%%(N+1)
  }
  if(length(unique(kwadrat.generator)) < nmin)
  {
    for(i in 2:(nmin+1500))
    {
      a = sample(x=nmin, size = 1)
      c = 17
      b = 5
      kwadrat.generator[1] <- sample(x = N,size = 1)
      kwadrat.generator[i] <- (a* (kwadrat.generator[i-1])^2 + b*kwadrat.generator[i-1] + c)%%(N+1)
    }
  }
  proba.kwadrat <- Dane[unique(kwadrat.generator),]
  proba.kwadrat <- proba.kwadrat[1:nmin,]
  wnk.wydatki.blad.kwadrat <- c(wnk.wydatki.blad.kwadrat, blad.wydatki(proba.kwadrat))
  wnk.srednia.wydatki.kwadrat <- c(wnk.srednia.wydatki.kwadrat, srednia.wydatki(proba.kwadrat))
  wnk.waga.blad.kwadrat <- c(wnk.waga.blad.kwadrat, blad.frakcja(proba.kwadrat))
  wnk.srednia.waga.kwadrat <- c(wnk.srednia.waga.kwadrat, srednia.waga(proba.kwadrat))
}
wnk.wydatki.blad.kwadrat <- na.omit(wnk.wydatki.blad.kwadrat)
wnk.srednia.wydatki.kwadrat <- na.omit(wnk.srednia.wydatki.kwadrat)
wnk.waga.blad.kwadrat <- na.omit(wnk.waga.blad.kwadrat)
wnk.srednia.waga.kwadrat <- na.omit(wnk.srednia.waga.kwadrat)
szescian.generator <- 1
proba.szescian <- 1
a <- 1
wnk.wydatki.blad.szescian <- NA
wnk.srednia.wydatki.szescian <- NA
wnk.waga.blad.szescian <- NA
wnk.srednia.waga.szescian <- NA
for(j in 1:500)
{
  szescian.generator <- 1
  for(i in 2:(nmin+1500))
  {
    a = sample(x=nmin, size = 1)
    c = 17
    b = 5
    szescian.generator[1] <- sample(x = N,size = 1)
    szescian.generator[i] <- (a* (szescian.generator[i-1])^3 + b*szescian.generator[i-1]^2 + c*szescian.generator[i-1] + c)%%(N+1)
  }
  if(length(unique(szescian.generator)) < nmin)
  {
    for(i in 2:(nmin+1500))
    {
      a = sample(x=nmin, size = 1)
      c = 17
      b = 5
      szescian.generator[1] <- sample(x = N,size = 1)
      szescian.generator[i] <- (a* (szescian.generator[i-1])^3 + b*szescian.generator[i-1]^2 + c*szescian.generator[i-1] + c)%%(N+1)
    }
  }
  proba.szescian <- Dane[unique(szescian.generator),]
  proba.szescian <- proba.szescian[1:nmin,]
  wnk.wydatki.blad.szescian <- c(wnk.wydatki.blad.szescian, blad.wydatki(proba.szescian))
  wnk.srednia.wydatki.szescian <- c(wnk.srednia.wydatki.szescian, srednia.wydatki(proba.szescian))
  wnk.waga.blad.szescian <- c(wnk.waga.blad.szescian, blad.frakcja(proba.szescian))
  wnk.srednia.waga.szescian <- c(wnk.srednia.waga.szescian, srednia.waga(proba.szescian))
}
wnk.wydatki.blad.szescian <- na.omit(wnk.wydatki.blad.szescian)
wnk.srednia.wydatki.szescian <- na.omit(wnk.srednia.wydatki.szescian)
wnk.waga.blad.szescian <- na.omit(wnk.waga.blad.szescian)
wnk.srednia.waga.szescian <- na.omit(wnk.srednia.waga.szescian)
wnk.wydatki.blad.sample <- NA
wnk.srednia.wydatki.sample <- NA
wnk.waga.blad.sample <- NA
wnk.srednia.waga.sample <- NA
proba.sample <- NA
for(i in 1:500)
{
  proba.sample <- sample(x = N, size = nmin)
  proba.sample <- Dane[proba.sample,]
  wnk.wydatki.blad.sample <- c(wnk.wydatki.blad.sample, blad.wydatki(proba.sample))
  wnk.srednia.wydatki.sample <- c(wnk.srednia.wydatki.sample, srednia.wydatki(proba.sample))
  wnk.waga.blad.sample <- c(wnk.waga.blad.sample, blad.frakcja(proba.sample))
  wnk.srednia.waga.sample <- c(wnk.srednia.waga.sample, srednia.waga(proba.sample))
}
wnk.wydatki.blad.sample <- na.omit(wnk.wydatki.blad.sample)
wnk.srednia.wydatki.sample <- na.omit(wnk.srednia.wydatki.sample)
wnk.waga.blad.sample <- na.omit(wnk.waga.blad.sample)
wnk.srednia.waga.sample <- na.omit(wnk.srednia.waga.sample)
gen.norm.wykl <- 1
proba.norm.wykl <- 1
wnk.wydatki.blad.norm.wykl <- NA
wnk.srednia.wydatki.norm.wykl <- NA
wnk.waga.blad.norm.wykl <- NA
wnk.srednia.waga.norm.wykl <- NA
### Wiem ¿e poni¿szy geneartor nie jest skonstruowany poprawnie, jednak¿e daje on lepsze wyniki w wydatkach dlatego go zostawi³em
### no i widaæ ¿e siê stara³em :) 
### Generator mia³ byæ jedn¹ z metod eliminacji zaproponowan¹ u Wieczorkowskiego i Zieliñskiego
for(i in 1:500)
{
    gen.norm.wykl <- 1
    X = rexp(nmin*2,1)
    c <- which(X < 1)
    X = X[c]
    if(length(X) < length(nmin))
    {
      X = rexp(nmin*3,1)
      c <- which(X < 1)
      X = X[c]
    }
    U = runif(nmin*3)
    for(j in 2:(nmin+3000))
    {
      gen.norm.wykl[1] <- runif(1)
      gen.norm.wykl[j] <- (2*exp(1)/(pi))*U[j-1]*exp(1)^(-X[j-1])
    }
    
    gen.norm.wykl <- na.omit(abs(gen.norm.wykl-1))
    gen.norm.wykl <- as.integer(gen.norm.wykl * N)
    gen.norm.wykl <- unique(gen.norm.wykl)
  gen.norm.wykl <- Dane[gen.norm.wykl,]
  proba.norm.wykl <- gen.norm.wykl[1:nmin,]
  wnk.wydatki.blad.norm.wykl <- c(wnk.wydatki.blad.norm.wykl, blad.wydatki(proba.norm.wykl))
  wnk.srednia.wydatki.norm.wykl <- c(wnk.srednia.wydatki.norm.wykl, srednia.wydatki(proba.norm.wykl))
  wnk.waga.blad.norm.wykl <- c(wnk.waga.blad.norm.wykl, blad.frakcja(proba.norm.wykl))
  wnk.srednia.waga.norm.wykl <- c(wnk.srednia.waga.norm.wykl, srednia.waga(proba.norm.wykl))
  rm(gen.norm.wykl)
}
wnk.wydatki.blad.norm.wykl <- na.omit(wnk.wydatki.blad.norm.wykl)
wnk.srednia.wydatki.norm.wykl <- na.omit(wnk.srednia.wydatki.norm.wykl)
wnk.waga.blad.norm.wykl <- na.omit(wnk.waga.blad.norm.wykl)
wnk.srednia.waga.norm.wykl <- na.omit(wnk.srednia.waga.norm.wykl)
#### Czêœæ 3 Wyniki badania ####
srednie.LCG.add <- round(c(mean(wnk.wydatki.LCG.add.blad),mean(wnk.srednia.wydatki.LCG.add),mean(wnk.waga.LCG.add.blad),mean(wnk.srednia.waga.LCG.add)),6)
srednie.LCG <- round(c(mean(wnk.wydatki.LCG.blad),mean(wnk.srednia.wydatki.LCG),mean(wnk.waga.LCG.blad),mean(wnk.srednia.waga.LCG)),6)
srednie.kwadrat <- round(c(mean(wnk.wydatki.blad.kwadrat),mean(wnk.srednia.wydatki.kwadrat),mean(wnk.waga.blad.kwadrat),mean(wnk.srednia.waga.kwadrat)),6)
srednie.szescian <- round(c(mean(wnk.wydatki.blad.szescian),mean(wnk.srednia.wydatki.szescian),mean(wnk.waga.blad.szescian),mean(wnk.srednia.waga.szescian)),6)
srednie.sample <- round(c(mean(wnk.wydatki.blad.sample),mean(wnk.srednia.wydatki.sample),mean(wnk.waga.blad.sample),mean(wnk.srednia.waga.sample)),6)
srednia.norm.wykl <- round(c(mean(wnk.wydatki.blad.norm.wykl),mean(wnk.srednia.wydatki.norm.wykl),mean(wnk.waga.blad.norm.wykl),mean(wnk.srednia.waga.norm.wykl)),6)
macierz <- matrix(data = c(srednie.LCG,srednie.LCG.add,srednie.kwadrat,srednie.szescian,srednie.sample,srednia.norm.wykl),ncol = 6, nrow = 4)
rownames(macierz) <- c("D(p) wydatki","Y wydatki", "D(p) Waga", "Y waga");colnames(macierz) <- c("LCG Multiplikatywne","LCG Addytywne", "Kwadratowy generator kongurencyjny","Szeœcienny generator kongurencyjny","Wbudowany sample","Norm Wyk³ - Wieczorkowski")
macierz
