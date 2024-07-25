# Función recursiva para calcular y^z
exponentiation <- function(y, z) {
  if (z == 0) {
    return(1)
  } else if (z == 1) {
    return(y)
  } else {
    half <- exponentiation(y, floor(z / 2))
    if (z %% 2 == 0) {
      return(half * half)
    } else {
      return(half * half * y)
    }
  }
}

# Función principal para calcular x^(y^z)
power <- function(x, y, z) {
  # Calcula y^z usando la función recursiva
  power_y_z <- exponentiation(y, z)

  # Calcula x^(y^z)
  return(x^power_y_z)
}

# Solicitar al usuario que ingrese los valores
x <- as.numeric(readline(prompt = "Ingrese el valor de x: "))
y <- as.numeric(readline(prompt = "Ingrese el valor de y: "))
z <- as.numeric(readline(prompt = "Ingrese el valor de z: "))

# Verificar si los valores ingresados son válidos
if (is.na(x) || is.na(y) || is.na(z) || y < 0 || z < 0) {
  cat("Por favor, ingrese valores válidos para x, y, y z (deben ser números no negativos).\n")
} else {
  # Calcular el resultado
  result <- power(x, y, z)

  # Mostrar el resultado en el formato deseado
  cat(sprintf("%d ^ (%d ^ %d) = %d\n", x, y, z, result))
}
