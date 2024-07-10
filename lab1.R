#Zadanie 1 
wektor <- c(5,10,15,20,25)
wektor[wektor>15]
#help(mean)
srednia <- mean(wektor)
suma1 <- sum(wektor[1],wektor[2],wektor[3])
suma <- sum(wektor[1:3])

#Zadanie 2
wyniki <- c(75, 48, 90, 60, 30)
for (i in wyniki){
  if (i >= 60){
    cat("Zaliczony\n")
  } else {
    cat("Niezaliczony\n")
  }
}

#Zadanie 3
df <- data.frame(
  Imię = c("Jan", "Ola", "Ela"),
  Wiek = c(25,30,28),
  Punkty = c(85,92,78)
)
ocena <- c()
for (punkt in df$Punkty){
  if (punkt>= 91){ocena <- c(ocena,5)}
  else if(punkt >= 81){ocena <- c(ocena,4.5 )}
  else if(punkt >= 71){ocena <- c(ocena,4 )}
  else if(punkt >= 61){ocena <- c(ocena,3.5 )}
  else if(punkt >= 50){ocena <- c(ocena,3 )}
  else{ocena <- c(ocena,2)}
}
new_df <- data.frame(df, ocena )
print(new_df)

new_df[new_df$Wiek < 30,]

#install.packages("ggplot2")
#install.packages("devtools")


#Zadanie 4


dane <- data.frame(
  Przedmiot = c("Analiza i Bazy Danych", "Metody numeryczne", "Eksploracja danych"),
  Średnia = c(71, 67, 89),
  Rocznik = c(2022,2022,2022)
)
library(ggplot2)

# Tworzenie wykresu słupkowego
wykres_slupkowy <- ggplot(dane, aes(x = Przedmiot, y = Średnia)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Średnia wyników rocznika 2022",
       x = "Przedmiot",
       y = "Średnia")

# Wyświetlenie wykresu
print(wykres_slupkowy)

#Zadanie domowe 

# Test 1 Wyniki testów Matematyki dla Grupy A i Grupy B
wyniki_grupa_A_test1  <- c(60, 65, 75, 68, 62)
wyniki_grupa_B_test1 <- c(78, 80, 85, 92, 88)

sredniaA <- mean(wyniki_grupa_A_test1)
sredniaB <- mean(wyniki_grupa_B_test1)

if ( sredniaA > 70 & sredniaB > 70 ){
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 1", "Grupa 2"),
    Średnia = c(sredniaA,sredniaB)
  )
  
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Średnia wyników z Matematyki",
         x = "Przedmiot",
         y = "Średnia")
  
  print(wykres_slupkowy_test1)
} else if (sredniaA <= 70 & sredniaB > 70){ 
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 2"),
    Średnia = c(sredniaB)
  )
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Średnia wyników z Matematyki bez grupy 1, która nie osiągnęła 70",
       x = "Przedmiot",
       y = "Średnia")
  print(wykres_slupkowy_test1)
}else if (sredniaB <= 70 & sredniaA > 70){ 
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 1"),
    Średnia = c(sredniaA)
  )
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Średnia wyników z Matematyki bez grupy 2, która nie osiągnęła 70",
         x = "Przedmiot",
         y = "Średnia")
    print(wykres_slupkowy_test1)
}else{cat("Żadna z grup nie osiagnęła wyniku 70")}

# Test 2 Wyniki testów Matematyki dla Grupy A i Grupy B
wyniki_grupa_A_test2 <- c(80, 65, 75, 68, 72)
wyniki_grupa_B_test2 <- c(78, 80, 85, 92, 88)


sredniaA <- mean(wyniki_grupa_A_test2)
sredniaB <- mean(wyniki_grupa_B_test2)

if ( sredniaA > 70 & sredniaB > 70 ){
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 1", "Grupa 2"),
    Średnia = c(sredniaA,sredniaB)
  )
  
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Średnia wyników z Matematyki",
         x = "Przedmiot",
         y = "Średnia")
  
  print(wykres_slupkowy_test1)
} else if (sredniaA <= 70 & sredniaB > 70){ 
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 2"),
    Średnia = c(sredniaB)
  )
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Średnia wyników z Matematyki bez grupy 1, która nie osiągnęła 70",
         x = "Przedmiot",
         y = "Średnia")
  print(wykres_slupkowy_test1)
}else if (sredniaB <= 70 & sredniaA > 70){ 
  dane_test1 <- data.frame(
    Przedmiot = c("Grupa 1"),
    Średnia = c(sredniaA)
  )
  wykres_slupkowy_test1 <- ggplot(dane_test1, aes(x = Przedmiot, y = Średnia)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Średnia wyników z Matematyki bez grupy 2, która nie osiągnęła 70",
         x = "Przedmiot",
         y = "Średnia")
  print(wykres_slupkowy_test1)
}else{cat("Żadna z grup nie osiagnęła wyniku 70")}
