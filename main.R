# NIE EDYTOWAĆ *****************************************************************
dsn_database = "wbauer_adb_2023"   # Specify the name of  Database
dsn_hostname = "pgsql-196447.vipserv.org"  # Specify host name 
dsn_port = "5432"                # Specify your port number. 
dsn_uid = "wbauer_adb"         # Specify your username. 
dsn_pwd = "adb2020"        # Specify your password.

library(DBI)
library(RPostgres)
library(testthat)

con <- dbConnect(Postgres(), dbname = dsn_database, host=dsn_hostname, port=dsn_port, user=dsn_uid, password=dsn_pwd) 
# ******************************************************************************

#install.packages(c("devtools", "testthat", "knitr")) 

film_in_category <- function(category_id){
  # Funkcja zwracająca wynik zapytania do bazy o tytuł filmu, język, oraz kategorię dla zadanego id kategorii.
  # Przykład wynikowej tabeli:
  # |   |title          |language    |category|
  # |0	|Amadeus Holy	|English	|Action|
  # 
  # Tabela wynikowa ma być posortowana po tylule filmu i języku.
  # 
  # Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL
  # 
  # Parameters:
  # category_id (integer): wartość id kategorii dla którego wykonujemy zapytanie
  # 
  # Returns:
  # DataFrame: DataFrame zawierający wyniki zapytania
  # 
if (is.numeric(category_id) && round(category_id) == category_id) { 
  df <- dbGetQuery(con, paste("SELECT f.title AS title, l.name AS language, c.name AS category 
    FROM category c
    INNER JOIN film_category fc ON c.category_id = fc.category_id
    INNER JOIN film f ON fc.film_id = f.film_id 
    INNER JOIN language l ON f.language_id = l.language_id 
    WHERE c.category_id =", category_id,
    "ORDER BY f.title ASC, l.name ASC"))
  return(df)
}
  return(NULL)
}


number_films_in_category <- function(category_id){
  #   Funkcja zwracająca wynik zapytania do bazy o ilość filmów w zadanej kategori przez id kategorii.
  #     Przykład wynikowej tabeli:
  #     |   |category   |count|
  #     |0	|Action 	|64	  | 
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     category_id (integer): wartość id kategorii dla którego wykonujemy zapytanie
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
if (is.numeric(category_id) && round(category_id) == category_id){
  df <- dbGetQuery(con, paste("SELECT c.name AS category, COUNT(c.name) AS count
    FROM category c
    INNER JOIN film_category fc ON c.category_id = fc.category_id
    WHERE c.category_id =", category_id,
    "GROUP BY c.name ORDER BY category"))
  return(df) 
}
  return(NULL)
}


number_film_by_length <- function(min_length, max_length){
  #   Funkcja zwracająca wynik zapytania do bazy o ilość filmów dla poszczegulnych długości pomiędzy wartościami min_length a max_length.
  #     Przykład wynikowej tabeli:
  #     |   |length     |count|
  #     |0	|46 	    |64	  | 
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     min_length (int,double): wartość minimalnej długości filmu
  #     max_length (int,double): wartość maksymalnej długości filmu
  #     
  #     Returns:
  #     pd.DataFrame: DataFrame zawierający wyniki zapytania
if (is.numeric(min_length) && is.numeric(max_length) && min_length <= max_length){
  df <- dbGetQuery(con, paste("SELECT film.length AS length, COUNT(film.length) AS count
  FROM film
  WHERE film.length >= ", min_length, " AND film.length <= ", max_length, "
  GROUP BY film.length"))
  return(df) 
}
  return(NULL)
}



client_from_city<- function(city){
  #   Funkcja zwracająca wynik zapytania do bazy o listę klientów z zadanego miasta przez wartość city.
  #     Przykład wynikowej tabeli:
  #     |   |city	    |first_name	|last_name
  #     |0	|Athenai	|Linda	    |Williams
  #     
  #     Tabela wynikowa ma być posortowana po nazwisku i imieniu klienta.
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     city (character): nazwa miaste dla którego mamy sporządzić listę klientów
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania

if (is.character(city)) {
  df <- dbGetQuery(con, sprintf("SELECT city.city AS city, customer.first_name, customer.last_name
  FROM customer
  INNER JOIN address ON customer.address_id = address.address_id
  INNER JOIN city ON address.city_id = city.city_id
  WHERE city.city = '%s'
  ORDER BY customer.last_name ASC, customer.first_name ASC", city))
  return(df)
}
  return(NULL)
}

avg_amount_by_length<-function(length){
  #   Funkcja zwracająca wynik zapytania do bazy o średnią wartość wypożyczenia filmów dla zadanej długości length.
  #     Przykład wynikowej tabeli:
  #     |   |length |avg
  #     |0	|48	    |4.295389
  #     
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     length (integer,double): długość filmu dla którego mamy pożyczyć średnią wartość wypożyczonych filmów
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania

if (is.numeric(length)) {
  df <- dbGetQuery(con, sprintf("SELECT film.length AS length, AVG(payment.amount) AS avg
  FROM film
  INNER JOIN inventory ON film.film_id = inventory.film_id
  INNER JOIN rental ON inventory.inventory_id = rental.inventory_id
  INNER JOIN payment ON rental.rental_id = payment.rental_id
  WHERE film.length = %s
  GROUP BY film.length", length))
  return(df)
}
  return(NULL)
}


client_by_sum_length<-function(sum_min){
  #   Funkcja zwracająca wynik zapytania do bazy o sumaryczny czas wypożyczonych filmów przez klientów powyżej zadanej wartości .
  #     Przykład wynikowej tabeli:
  #     |   |first_name |last_name  |sum
  #     |0  |Brian	    |Wyman  	|1265
  #     
  #     Tabela wynikowa powinna być posortowane według sumy, imienia i nazwiska klienta.
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     sum_min (integer,double): minimalna wartość sumy długości wypożyczonych filmów którą musi spełniać klient
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
if (is.numeric(sum_min)) {
  df <- dbGetQuery(con, sprintf("SELECT customer.first_name AS first_name, customer.last_name AS last_name, SUM(film.length) AS sum
  FROM customer
  INNER JOIN rental ON rental.customer_id = customer.customer_id
  INNER JOIN inventory ON inventory.inventory_id = rental.inventory_id
  INNER JOIN film ON film.film_id = inventory.film_id
  GROUP BY customer.first_name, customer.last_name
  HAVING SUM(film.length) >= %s
  ORDER BY SUM(film.length) ASC, customer.last_name ASC, customer.first_name ASC", sum_min))
  return(df)
}
  return(NULL)
}


category_statistic_length<-function(name){
  #   Funkcja zwracająca wynik zapytania do bazy o statystykę długości filmów w kategorii o zadanej nazwie.
  #     Przykład wynikowej tabeli:
  #     |   |category   |avg    |sum    |min    |max
  #     |0	|Action 	|111.60 |7143   |47 	|185
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     name (character): Nazwa kategorii dla której ma zostać wypisana statystyka
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania

if (is.character(name)){
  df <- dbGetQuery(con, sprintf("SELECT category.name AS category, AVG(film.length) AS avg, SUM(film.length) AS sum, MIN(film.length) AS min, MAX(film.length) AS max
    FROM category
    INNER JOIN film_category ON film_category.category_id = category.category_id
    INNER JOIN film ON film.film_id = film_category.film_id
    WHERE category.name = '%s'
    GROUP BY category.name", name))
  return(df)
}
  return(NULL)
}



# NIE EDYTOWAĆ *****************************************************************
test_dir("tests/testthat")
# ******************************************************************************

#ODPOWIEDZI NA PYTANIA 

#1 Znajdź listę wszystkich filmów o tej samej długości.
ad1 <- number_film_by_length(120,120)
print('Pytanie nr 1 :')
print(ad1)
df1 <- dbGetQuery(con, "SELECT film.title AS title,
                       film.length AS length
                       FROM film
                       WHERE film.length = 120
                       ORDER BY film.title ASC")
print(df1)

#2 Znajdź wszystkich klientów mieszkających w tym samym mieście.
print('Pytanie nr 2 :')
ad2 <- client_from_city("Abu Dhabi")
print(ad2)

#3 Oblicz średni koszt wypożyczenia wszystkich filmów.
print('Pytanie nr 3 :')

df3 <- dbGetQuery(con, "SELECT AVG(film.rental_rate) AS average FROM film")
print(df3)

#4 Oblicz i wyświetl liczbę filmów we wszystkich kategoriach.
print('Pytanie nr 4 :')

df4 <- dbGetQuery(con, "SELECT category.name AS category, 
                       COUNT(film.title) AS number 
               FROM category
               INNER JOIN film_category ON category.category_id = film_category.category_id
               INNER JOIN film ON film_category.film_id = film.film_id
               GROUP BY category ORDER BY category ")
print(df4)
#5 Wyświetl liczbę wszystkich klientów pogrupowanych według kraju.

print('Pytanie nr 5 :')

df5 <- dbGetQuery(con,"SELECT country.country AS country,
                       COUNT(customer.first_name) AS number_of_customers 
               FROM country
               INNER JOIN city ON country.country_id = city.country_id
               INNER JOIN address ON city.city_id = address.city_id
               INNER JOIN customer ON address.address_id = customer.address_id
               GROUP BY country ORDER BY country ")

print(df5)

#6 Wyświetl informacje o sklepie, który ma więcej niż 100 klientów i mniej niż 300 klientów.

print('Pytanie nr 6 :')

df6 <- dbGetQuery(con, "SELECT address.address AS address, 
                       address.address2 AS address2,
                       address.district AS district,
                       address.postal_code AS postal_code,
                       city.city AS city,
                       country.country AS country
                FROM address
                INNER JOIN store ON address.address_id = store.address_id
                INNER JOIN city ON address.city_id = city.city_id
                INNER JOIN country ON city.country_id = country.country_id
                WHERE store.store_id = 
                (SELECT customer.store_id AS store_id
                FROM customer
                INNER JOIN address ON customer.address_id = address.address_id
                GROUP BY store_id
                HAVING COUNT(customer.customer_id) BETWEEN 100 AND 300)")

print(df6)

#7 Wybierz wszystkich klientów, którzy oglądali filmy ponad 200 godzin
print('Pytanie nr 7 :')

df7 <- dbGetQuery(con, "SELECT customer.last_name AS last_name,
                       SUM(film.length) AS length 
               FROM customer
               INNER JOIN rental ON customer.customer_id = rental.customer_id 
               INNER JOIN inventory ON rental.inventory_id = inventory.inventory_id
               INNER JOIN film ON inventory.film_id = film.film_id
               GROUP BY last_name
               HAVING SUM(film.length) > 1200 ORDER BY length")
print(df7)
#8 Oblicz średnią wartość wypożyczenia filmu.

print('Pytanie nr 8 :')

df8 <- dbGetQuery(con, "SELECT film.title AS title, 
                       AVG(payment.amount) AS average
               FROM film
               INNER JOIN inventory ON film.film_id = inventory.inventory_id
               INNER JOIN rental ON inventory.inventory_id = rental.inventory_id
               INNER JOIN payment ON rental.rental_id = payment.rental_id
               WHERE title = 'Blade Polish' 
               GROUP BY film.title") 

print(df8)

#9 Oblicz średnią wartość długości filmu we wszystkich kategoriach.
print('Pytanie nr 9 :')

df9 <- dbGetQuery(con, "SELECT category.name AS category,
                       AVG(film.length) AS average
               FROM category
               INNER JOIN film_category ON category.category_id = film_category.category_id
               INNER JOIN film ON film_category.film_id = film.film_id 
               GROUP BY category ORDER BY category") 

print(df9)
#10 Znajdź najdłuższe tytuły filmowe we wszystkich kategoriach.
print('Pytanie nr 10 :')

df10 <- dbGetQuery(con, "SELECT category.name AS category, film.title AS longest_film_title,
        film.length AS max_length
    FROM category
    INNER JOIN film_category ON category.category_id = film_category.category_id
    INNER JOIN film ON film_category.film_id = film.film_id
    WHERE (category.name, film.length) IN (
        SELECT category.name AS category, MAX(film.length) AS max_length
        FROM category
        INNER JOIN film_category ON category.category_id = film_category.category_id
        INNER JOIN film ON film_category.film_id = film.film_id
        GROUP BY category)")


print(df10)
#11 Znajdź najdłuższy film we wszystkich kategoriach. Porównaj wynik z pkt 10.

print('Pytanie nr 11 :')

df11 <- dbGetQuery(con, "SELECT DISTINCT ON (category.name)
                      category.name AS category,
                      film.title AS title,
                      MAX(film.length) AS max_length
               FROM category
               JOIN film_category ON category.category_id = film_category.category_id
               JOIN film ON film_category.film_id = film.film_id
               GROUP BY category.name, film.title
               ORDER BY category.name, max_length DESC")
print(df11)
