# Tarea 03: Penguin Plots

library(tidyverse)
library(palmerpenguins)

# Ejercicio 1
nrow(penguins)
ncol(penguins)

# Ejercicio 2
?penguins

# Ejercicio 3
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

# Ejercicio 4
ggplot(penguins, aes(x = species, y = bill_depth_mm)) +
  geom_boxplot()

# Ejercicio 5
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# Ejercicio 6
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE)

# Ejercicio 7
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(na.rm = TRUE) +
  labs(caption = "Data come from the palmerpenguins package.")

# Ejercicio 8
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

# Ejercicio 9
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = island)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Ejercicio 10
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_smooth(data = penguins, aes(x = flipper_length_mm, y = body_mass_g))


ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# EJERCICIOS DE VISUALIZING DISTRIBUTIONS 

# 1
# Al asignar species a la estetica 'y', el grafico de barras se orienta horizontalmente 
# en lugar de verticalmente.
ggplot(penguins, aes(y = species)) +
  geom_bar()

# 2
# 'color' modifica únicamente el borde de las barras, mientras que 'fill' rellena el interior de las barras.
# Para cambiar el color de las barras, la estética más útil es 'fill'.
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "green")

# 3
# El argumento 'bins' en geom_histogram() especifica el numero total de intervalos/barras  
# en los que se divide el rango de la variable continua.

# 4
# Histogramas probando distintos anchos de intervalo (binwidth):
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.47)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.02)

# Con un binwidth chico (como 0.01), se ven picos en valores redondos o fraccionarios 
# comunes , lo que refleja cómo los diamantes se tallan para alcanzar pesos comerciales clave.

# EJERCICIOS DE VISUALIZING RELATIONSHIPS

# 1
# Categoricas: manufacturer, model, trans, drv, fl, class
# Numericas: displ, year, cyl, cty, hwy
# Se puede ver el tipo de variable en la primera fila debajo de los nombres de columna 
# al ejecutar 'mpg' en la consola o usando glimpse(mpg).
glimpse(mpg)

# 2
# Grafico base:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# Mapeando variable numerica (cty) a color, size, color+size, shape:
ggplot(mpg, aes(x = displ, y = hwy, color = cty)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = cty)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = cty, size = cty)) +
  geom_point()

# Esto lanza una advertencia/error porque 'shape' no acepta variables continuas/numericas:
# ggplot(mpg, aes(x = displ, y = hwy, shape = cty)) + geom_point()

# Comportamiento:
# - Numericas: color genera una escala continua/degradada; size varia en una escala continua.
# - Categoricas: color genera colores discretos bien diferenciados; size no es recomendado (asigna tamaños discretos);
#   shape solo funciona con variables categoricas (hasta 6 niveles por defecto).

# 3
# La estetica 'linewidth' esta diseñada para geometrias de linea (como geom_line o geom_path).
# En geom_point() no afecta el tamaño ni la apariencia de los puntos.
ggplot(mpg, aes(x = displ, y = hwy, linewidth = cty)) +
  geom_point()

# 4
# Si se mapea la misma variable a multiples esteticas (ej. color y shape), ambas esteticas 
# cambian simultaneamente representando la misma informacion de manera redundante.
ggplot(mpg, aes(x = displ, y = hwy, color = class, shape = class)) +
  geom_point()

# 5
# Scatterplot con color por especie:
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

# Faceteado por especie:
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  facet_wrap(~species)

# Revelacion: En el grafico global parece haber una relacion negativa entre largo y profundidad del pico.
# Pero al colorear por especie, vemos que dentro de cada especie la relacion es positiva.

# 6
# Genera dos leyendas porque las esteticas color y shape tienen etiquetas/titulos distintos en labs() 
# (color tiene 'Species' con mayuscula, mientras que shape mantiene el nombre por defecto 'species').
# Para combinarlas en una sola leyenda, ambas esteticas deben compartir exactamente el mismo titulo:
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

# 7
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# Respuestas:
# ¿Cual es la distribucion de especies presentes en cada isla?
# ¿Cual es la distribucion de islas en las que habita cada especie?

# EJERCICIOS DE SAVING YOUR PLOTS

# 1
# El grafico que se guarda en "mpg-plot.png" es el SEGUNDO (el grafico de dispersion de cty vs hwy).
# Esto ocurre porque ggsave() guarda la ultima figura ("last plot") que fue generada o mostrada.

# 2
# Para guardar el grafico como PDF en lugar de PNG, simplemente tenés que cambiar la extension del archivo a .pdf:
ggsave("mpg-plot.pdf")

# Para conocer todos los formatos soportados por ggsave(), se consulta la documentacion oficial ejecutando:
?ggsave