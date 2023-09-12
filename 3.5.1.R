### Question 1 ###

# Définir les paramètres
N <- 8
Te <- 1/16  # Période d'échantillonnage (fe = 16 Hz)
t <- seq(0, (N-1)*Te, by=Te)  # Créer un vecteur de temps

print(t)

# Définir la fonction x(t)
x <- function(t) {
  return(2 * sin(8 * pi * t) + 8 * cos(4 * pi * t))
}

# Échantillonner le signal x(t) pour obtenir xn
xn <- x(t)

# Calculer la TFD en utilisant la fonction fft()
Xk <- fft(xn)

print(Xk)

### Question 2 ###

library(ggplot2)

# Créer un data frame pour le signal xn
df_signal <- data.frame(t, xn)

# Tracer le signal dans le domaine temporel
(
  ggplot(df_signal, aes(x=t, y=xn))
  + geom_line()
  + labs(x="Temps", y="Amplitude")
  + ggtitle("Signal x(t)")
)

# Calculer le spectre d'amplitude
amplitude_spectrum <- abs(Xk)

# Créer un data frame pour le spectre d'amplitude
df_spectrum <- data.frame(
  frequency = seq(0, (N-1)*(1/Te), by=1/Te),
  amplitude_spectrum
)

# Tracer le spectre d'amplitude dans le domaine fréquentiel
(
  ggplot(df_spectrum, aes(x=frequency, y=amplitude_spectrum))
  + geom_bar(stat="identity")
  + labs(x="Fréquence (Hz)", y="Amplitude")
  + ggtitle("Spectre d'Amplitude")
)
