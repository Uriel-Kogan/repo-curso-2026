library(nycflights13)
install.packages("tidyverse")
library(tidyverse)
#> ── Attaching core tidyverse packages ───────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
#> ── Conflicts ─────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


# 1.1
flights |> 
  filter(arr_delay >= 120) |> 
  print()

# 1.2
flights |> 
  filter(dest %in% c("IAH", "HOU")) |> 
  print()

# 1.3
flights |> 
  filter(carrier %in% c("UA", "AA", "DL")) |> 
  print()

# 1.4
flights |> 
  filter(month %in% c(7, 8, 9)) |> 
  print()

# 1.5
flights |> 
  filter(arr_delay > 120 & dep_delay <= 0) |> 
  print()

# 1.6
flights |> 
  filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30) |> 
  print()

# 2.1
flights |> 
  arrange(desc(dep_delay)) |> 
  print()

# 2.2
flights |> 
  arrange(dep_time) |> 
  print()

# 3
flights |> 
  mutate(speed_mph = distance / air_time * 60) |> 
  arrange(desc(speed_mph)) |> 
  print()

# 4
flights |> 
  distinct(year, month, day) |> 
  count() |> 
  print()
# Resultado del count es 365, por lo tanto hubo vuelos en cada día del año.

# 5.1
flights |> 
  arrange(desc(distance)) |> 
  print()

# 5.2
flights |> 
  arrange(distance) |> 
  print()

# 6
cat("El resultado final de los datos es el mismo. Sin embargo, usar filter() antes de arrange() es mucho mas eficiente computacionalmente porque reduce el numero de filas a ordenar.")

# EJERCICIOS DE COLUMNS 

# 1
# Relacion: dep_time - sched_dep_time = dep_delay
# (Teniendo en cuenta que dep_time y sched_dep_time estan en formato HHMM, 
# por lo que hay que convertirlos a minutos desde la medianoche para operar de forma exacta)

# 2
flights |> select(dep_time, dep_delay, arr_time, arr_delay) |> print()
flights |> select(starts_with("dep_"), starts_with("arr_")) |> print()
flights |> select(4, 6, 7, 9) |> print()
flights |> select(c(dep_time, dep_delay, arr_time, arr_delay)) |> print()
flights |> select(dep_time:arr_delay, -sched_dep_time, -sched_arr_time) |> print()

# 3
# No genera un error, dplyr ignora los duplicados y muestra la columna una sola vez.
flights |> select(dep_time, dep_time, arr_time) |> print()

# 4
# any_of() selecciona las columnas nombradas en el vector que existan en el dataframe sin lanzar error si alguna falta.
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> select(any_of(variables)) |> print()

# 5
# Por defecto, los auxiliares de select() ignoran mayusculas y minusculas.
# Para cambiar ese comportamiento y hacer que distinga mayusculas, se usa ignore.case = FALSE.
flights |> select(contains("TIME")) |> print()
flights |> select(contains("TIME", ignore.case = FALSE)) |> print()

# 6
flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min) |> 
  print()

# 7
# No funciona porque select(tailnum) elimina todas las demas columnas del dataframe (dejando solo tailnum).
# Al llegar a arrange(arr_delay), la columna arr_delay ya no existe en el flujo de datos.

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed)) |> 
  print()

# EJERCICIOS DE GROUPS (3.5.5 Exercises)

# 1
# Carrier con peores retrasos promedio de llegada:
flights |> 
  group_by(carrier) |> 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  arrange(desc(avg_arr_delay)) |> 
  print()

# Desenredar efectos de aeropuertos vs aerolineas:
# Es dificil separarlos por completo porque muchas aerolineas vuelan a destinos especificos (o tienen hubs). 
# Si comparamos dentro de cada combinacion carrier-destino, muchas celdas tienen pocos o ningun vuelo de la competencia:
flights |> 
  group_by(carrier, dest) |> 
  summarize(n = n(), .groups = "drop") |> 
  print()

# 2
flights |> 
  group_by(dest) |> 
  slice_max(dep_delay, n = 1, na_rm = TRUE) |> 
  relocate(dest, dep_delay) |> 
  print()

# 3
# Variacion de retrasos a lo largo del dia:
flights |> 
  group_by(hour) |> 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = hour, y = avg_dep_delay)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Retrasos promedio de salida a lo largo del dia",
    x = "Hora del dia",
    y = "Retraso promedio (minutos)"
  )

# 4
# Si n es negativo en slice_min() / slice_max(), selecciona todas las filas EXCEPTO las n mas pequeñas / grandes.
# Por ejemplo, slice_min(df, x, n = -1) selecciona todas las filas menos la que tiene el valor minimo.

# 5
# count() es un atajo de group_by() seguido de summarize(n = n()).
# El argumento sort = TRUE ordena los resultados en orden descendente segun el conteo.

# 6
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

# 6.a
# group_by() no cambia el orden ni el contenido visible.
df |> 
  group_by(y) |> 
  print()

# 6.b
#  A diferencia de group_by(), arrange() cambia el orden fisico de las filas.
df |> 
  arrange(y) |> 
  print()

# 6.c
# Agrupa por y y calcula el promedio de x para cada grupo 
df |> 
  group_by(y) |> 
  summarize(mean_x = mean(x)) |> 
  print()

# 6.d
# Agrupa por y y z, luego calcula el promedio de x para cada combinacion.

df |> 
  group_by(y, z) |> 
  summarize(mean_x = mean(x)) |> 
  print()

# 6.e
# Es igual al anterior, pero .groups = "drop" elimina toda la estructura de agrupacion final, evitando el mensaje informativo.
df |> 
  group_by(y, z) |> 
  summarize(mean_x = mean(x), .groups = "drop") |> 
  print()

# 6.f
# Diferencia entre summarize() y mutate():
# summarize() reduce el dataset devolviendo 1 fila por grupo (4 filas en total).
df |> 
  group_by(y, z) |> 
  summarize(mean_x = mean(x)) |> 
  print()

# mutate() mantiene todas las filas originales (5 filas) y añade la media calculada para el grupo de cada fila.
df |> 
  group_by(y, z) |> 
  mutate(mean_x = mean(x)) |> 
  print()

# EJERCICIOS DE KEYS / JOINS (19.2.4 Exercises)

# 1
# La relacion entre weather y airports es que weather$origin es una clave foranea
# que apunta a airports$faa (clave primaria).
# En el diagrama deberia verse una flecha desde weather$origin hacia airports$faa.


# 2
# Si weather tuviera datos de todos los aeropuertos de EE. UU., se podria conectar
# la combinacion (dest, time_hour) de flights con (origin, time_hour) de weather
# para conocer el clima en el aeropuerto de destino al aterrizar.


# 3
# Registro duplicado por el cambio de horario de verano:
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1) |> 
  print()

# Ocurrio el 3 de noviembre de 2013 a la 1:00 AM, momento en que los relojes 
# se retrasaron una hora, generando dos registros para la misma hora.


# 4
# Para dias especiales se puede crear una tabla 'dias_especiales':
# - Columnas: fecha (o year, month, day) y festivo.
# - Clave primaria: fecha (o la clave compuesta year, month, day).
# - Se uniria a flights mediante la clave foranea (year, month, day).


# 5
# Relaciones en Lahman:
# 
# Batting, People, Salaries:
# - People: PK = playerID
# - Batting: Compound PK = (playerID, yearID, stint) | FK playerID -> People$playerID
# - Salaries: Compound PK = (playerID, yearID, teamID) | FK playerID -> People$playerID
#
# People, Managers, AwardsManagers:
# - People: PK = playerID
# - Managers: Compound PK = (playerID, yearID, inseason) | FK playerID -> People$playerID
# - AwardsManagers: Compound PK = (playerID, awardID, yearID) | FK playerID -> People$playerID
#
# Batting, Pitching, Fielding:
# - Tienen una relacion 1:1 o 1:N compartiendo la clave compuesta (playerID, yearID, stint).
# - Representan distintas facetas del rendimiento del mismo jugador en un mismo año y etapa.

# EJERCICIOS DE JOINS (19.3.4 Exercises)

# 1

flights_48h <- flights |> 
  group_by(time_hour) |> 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  mutate(delay_48h = slider::slide_dbl(avg_dep_delay, mean, na.rm = TRUE, .before = 47)) |> 
  arrange(desc(delay_48h)) |> 
  slice_head(n = 1) |> 
  inner_join(weather, by = "time_hour")

# Patrón: Las mayores concentraciones de retrasos suelen coincidir con tormentas, 
# precipitaciones altas, mala visibilidad o vientos fuertes/ráfagas en los aeropuertos de NYC.


# 2

top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  head(10)

# Opción A: Usando semi_join()
flights |> 
  semi_join(top_dest, by = "dest")

# Opción B: Usando %in%
flights |> 
  filter(dest %in% top_dest$dest)


# 3
# ¿Todos los vuelos tienen datos de clima correspondientes a su hora?
# No, hay horas que carecen de registros meteorológicos.
flights |> 
  anti_join(weather, by = c("origin", "time_hour")) |> 
  count()


# 4
# Patrón en los tailnum que no están en la tabla planes:
flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(carrier, sort = TRUE)

# Alrededor del 90% de los vuelos sin datos de avión corresponden a las aerolíneas 
# American Airlines (AA) y Envoy Air / American Eagle (MQ), ya que no reportaron datos a esa base.


# 5
# Agregar una columna a planes con todas las aerolíneas que han volado cada avión:
planes_carriers <- flights |> 
  filter(!is.na(tailnum)) |> 
  distinct(tailnum, carrier) |> 
  group_by(tailnum) |> 
  summarize(
    n_carriers = n(),
    carriers = paste(carrier, collapse = ", ")
  )

planes |> 
  left_join(planes_carriers, by = "tailnum")

# Se rechaza la hipótesis: Aunque la mayoría de aviones pertenecen a una sola aerolínea (n_carriers == 1), 
# algunos han sido operados por más de una aerolínea durante el año.


# 6
# Añadir latitud y longitud de origen y destino:
# Es más fácil renombrar las columnas ANTES del join para evitar nombres duplicados (como lat.x, lat.y).
location_coords <- airports |> 
  select(faa, lat, lon)

flights |> 
  left_join(location_coords |> rename(origin_lat = lat, origin_lon = lon), by = c("origin" = "faa")) |> 
  left_join(location_coords |> rename(dest_lat = lat, dest_lon = lon), by = c("dest" = "faa"))


# 7
# Distribución espacial de los retrasos por destino:
delay_by_dest <- flights |> 
  group_by(dest) |> 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE))

airports |> 
  inner_join(delay_by_dest, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat, color = avg_arr_delay, size = avg_arr_delay)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  coord_quickmap() +
  labs(
    title = "Distribucion espacial de retrasos promedio por aeropuerto de destino",
    x = "Longitud", y = "Latitud",
    color = "Retraso prom. (min)", size = "Retraso prom. (min)"
  )


# 8

june13_delays <- flights |> 
  filter(year == 2013, month == 6, day == 13) |> 
  group_by(dest) |> 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE))

airports |> 
  inner_join(june13_delays, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat, color = avg_arr_delay, size = avg_arr_delay)) +
  borders("state") +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c() +
  coord_quickmap() +
  labs(
    title = "Retrasos masivos de llegada el 13 de junio de 2013",
    x = "Longitud", y = "Latitud"
  )

# Explicación buscada en google: Ese día hubo una serie de tormentas severas y un complejo de severas
# turbulencias/derechos en el sureste y costa este de EE. UU., provocando retrasos masivos.