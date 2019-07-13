##### Lekarz przyjmuje pacjentów zgodnie z rozk³adem Poisson'a ze œredni¹ 
##### równ¹ 10 minut i pracuje przez 8h. Zapisanych jest 48 pacjentów, gdzie
#### za³o¿eniem jest ¿e ka¿dy pacjent w gabinecie bêdzie 10 minut. 
#### Jaki jest procent, ¿e doktor bêdzie musia³ zostaæ po godzinach
#### pracy aby przyj¹æ wszystkich pacjentów, jaki ¿e wyjdzie równo o swojej godzinie,
#### jaki ¿e bêdzie móg³ wyjœæ szybciej z pracy?
#### Jaki procent pacjentów bêdzie musia³o wejœæ spóŸnionych do gabinetu?
Doktor.Dom = c()
liczba.spoznien = c()
czas.ca³kowity = 60*8
czas = seq(0, czas.ca³kowity, 10)
histogram = c()
N = 1000
for(i in 1:N)
{
  losp = rpois(48, 10)
  los = c(0, cumsum(losp))
  df = data.frame(czas, los)
  df$roznica = df$czas - df$los
  spoznienie.pacjentow = length(which(df$roznica>0))
  x = ifelse(df$roznica[length(df$roznica)]>0, "Zostaje po pracy",
             ifelse(df$roznica[length(df$roznica)]==0, "Wychodzi równo",
                    ifelse(df$roznica[length(df$roznica)]<0, "Wychodzi wczeœniej")))
  Doktor.Dom = c(Doktor.Dom, x)
  liczba.spoznien = c(liczba.spoznien, spoznienie.pacjentow)
  histogram = c(histogram, losp)
}

histogram = data.frame(histogram)
library(ggplot2)
ggplot(data = histogram, aes(x = histogram))+
  geom_histogram(binwidth = 1, col = "red", fill = "green", alpha = 0.2) + 
  labs(title = "Rozk³ad spêdzania czasu u doktora") + 
  xlab("Rozk³ad czasu") + 
  ylab("Liczebnoœæ") + 
  theme(axis.text.x = element_text(size =15),
        axis.text.y = element_text(size =15),
        title = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18))
table(Doktor.Dom)/N
print(paste("Udzia³ osób które wejd¹ spóŸnione do gabinetu wynosi =", sum(liczba.spoznien)/N, "%"))
