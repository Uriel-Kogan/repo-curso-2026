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