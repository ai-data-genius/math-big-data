# Algorithme FFT (Transformée de Fourier Rapide)
fft_recursive <- function(x) {
  N <- length(x)
  if (N <= 1) {
    return(x)
  }
  even <- fft_recursive(x[seq(2, N, by = 2)])
  odd <- fft_recursive(x[seq(1, N, by = 2)])
  T <- exp(-2i * pi * (0:(N/2-1)) / N)
  return(c(even + T * odd, even - T * odd))
}

# Exemple de calcul FFT avec une sinusoïde plus distincte
N <- 32  # Taille de la séquence
f <- 4  # Fréquence de la sinusoïde
x <- sin(2 * pi * f * (1:N) / N)  # Séquence de données contenant une sinusoïde


# Calculer la FFT
fft_result <- fft_recursive(round(x, digits = 10))

print(fft_result)


# Calculer la magnitude de la séquence FFT
magnitude <- Mod(fft_result)

# Afficher la magnitude
print(magnitude)

# Graphique pour la partie réelle et imaginaire des composantes
par(mfrow=c(2, 1))  # Divise la zone de tracé en deux parties

# Partie réelle (sans arrondi)
real_part <- Re(fft_result)
barplot(real_part, col = "skyblue", main = "Partie Réelle des Composantes", xlab = "Fréquence", ylab = "Amplitude")
xlabel <- seq(0, length(fft_result) - 1, by = 1)
axis(1, at = xlabel, labels = xlabel)

# Partie imaginaire
imag_part <- Im(fft_result)
barplot(imag_part, col = "lightcoral", main = "Partie Imaginaire des Composantes", xlab = "Fréquence", ylab = "Amplitude")
xlabel <- seq(0, length(fft_result) - 1, by = 1)
axis(1, at = xlabel, labels = xlabel)

# Rétablir le paramètre de disposition par défaut
par(mfrow=c(1, 1))

# Mesurer le temps d'exécution de l'algorithme FFT
x <- rnorm(2^16)  # Créer une séquence de données aléatoires de taille 2^16
system.time(fft_result <- fft_recursive(x))