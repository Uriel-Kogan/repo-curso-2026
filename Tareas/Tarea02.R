# Tarea 02: R Base

# Numeros & Strings
x <- 10.5
y <- 4
texto <- "HELLO WORLD"

# Booleanos
es_verdadero <- TRUE
es_falso <- FALSE

# Operadores
suma <- x + y
comparacion <- x > y
resultado_logico <- es_verdadero & es_falso

print(paste("Suma:", suma))
print(paste("¿x es mayor que y?:", comparacion))

# Parte 2: Estructuras de control de flujo


# Conditionals: if else
a <- 22
b <- 67

if (a > b) {
  print("a es mayor que b")
} else if (a == b) {
  print("a y b son iguales")
} else {
  print("a es menor que b")
}

# While Loop
i <- 1
while (i <= 3) {
  print(paste("contador while:", i))
  i <- i + 1
}

# For Loop
for (x in 1:4) {
  print(paste("elemento for:", x))
}

# Parte 3: Estructuras de datos


# Vectors
mi_vector <- c(1, 2, 3, 4, 5)

# Lists
mi_lista <- list(nombre = "gustavo", valores = mi_vector, activo = TRUE)

# Matrices
mi_matriz <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

# Arrays
mi_array <- array(1:8, dim = c(2, 2, 2))

# Data Frames
mi_dataframe <- data.frame(
  ID = c(101, 102, 103),
  Categoria = c("A", "B", "A"),
  Monto = c(150.5, 230.0, 89.9)
)

# Factors
mi_factor <- factor(c("bajo", "medio", "alto", "medio", "bajo"))

# Acá muestro los resultados 
print("--- Vector ---")
print(mi_vector)

print("--- Matriz ---")
print(mi_matriz)

print("--- Data Frame ---")
print(mi_dataframe)

print("--- Factor ---")
print(mi_factor)