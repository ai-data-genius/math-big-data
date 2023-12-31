### Question 1 ###

# Définir les paramètres
N1 <- 24
Te <- 1/16  # Période d'échantillonnage (fe = 16 Hz)
t1 <- seq(0, (N1-1)*Te, by=Te)  # Créer un vecteur de temps
n1 <- 0:(N1-1)
xt1 <- 3 * sin(8 * pi * t1) + 4 * cos(6 * pi * t1)

# Définir la fonction x(t)
x <- function(t) {
  return(3 * sin(8 * pi * t1) + 4 * cos(6 * pi * t1))
}

# Échantillonner le signal x(t) pour obtenir xn
xn <- x(t1)

# Calculer la TFD
Xk1 <- complex(real=rep(0,N1), imaginary=rep(0,N1)) # Initialiser un vecteur complexe

for (k in 0:(N1-1)) {
  for (nn in n1) {
    Xk1[k+1] = Xk1[k+1] + xt1[nn+1] * exp(-1i * 2 * pi * k * nn / N1)
  }
}

Xk1 <- round(Xk1, digits = 8)

print(Xk1)

### Question 2 ###

library(ggplot2)

# Créer un data frame pour le signal xn
df_signal <- data.frame(t1, xn)

# Créer un vecteur de temps pour une interpolation plus fine
t_interpolation <- seq(0, (N1-1)*Te, length.out = 1000)

# Calculer la sinusoïde correspondante à votre signal avec interpolation
sinusoide_interpol <- 3 * sin(8 * pi * t_interpolation) + 4 * cos(6 * pi * t_interpolation)

# Créer un data frame pour la sinusoïde interpolée
df_sinusoide_interpol <- data.frame(t1 = t_interpolation, sinusoide = sinusoide_interpol)

# Tracer le signal et la sinusoïde interpolée sur le même graphique
(
  ggplot() +
    geom_line(data = df_signal, aes(x = t1, y = xn), color = "blue", linetype = "dashed") +
    geom_path(data = df_sinusoide_interpol, aes(x = t1, y = sinusoide), color = "red") +
    geom_point(data = df_signal, aes(x = t1, y = xn), color = "blue") +  # Ajouter des points bleus pour le signal
    labs(x = "Temps", y = "Amplitude") +
    ggtitle("Signal x(t) et Sinusoïde correspondante (interpolée)")
)

# Calculer le spectre d'amplitude
amplitude_spectrum <- Mod(Xk1)

# Créer un data frame pour le spectre d'amplitude
df_spectrum <- data.frame(
  frequency = seq(0, (N1-1)*(1/Te), by=1/Te),
  amplitude_spectrum
)

# Effectuer une interpolation pour obtenir une courbe lisse
smoothed_spectrum <- spline(df_spectrum$frequency, df_spectrum$amplitude_spectrum, n=1000)

# Créer un nouveau data frame pour les données lissées
df_smoothed_spectrum <- data.frame(
  frequency = smoothed_spectrum$x,
  amplitude_spectrum = smoothed_spectrum$y
)

# Tracer le spectre d'amplitude lissé avec les points originaux
(
  ggplot(df_spectrum, aes(x=frequency))
  + geom_line(aes(y=amplitude_spectrum), color="blue", linetype = "dashed")  # Tracer le spectre d'amplitude original sous forme de courbe bleue
  + geom_line(data=df_smoothed_spectrum, aes(x=frequency, y=amplitude_spectrum), color="red")  # Tracer le spectre d'amplitude lissé sous forme de courbe rouge
  + geom_point(data=df_spectrum, aes(x=frequency, y=amplitude_spectrum), color="blue")  # Ajouter des points bleus aux sommets de la courbe originale
  + labs(x="Fréquence (Hz)", y="Amplitude")
  + ggtitle("Spectre d'Amplitude Lissé avec Sinusoïde et Points")
)
