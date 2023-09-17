
library(crayon)

# Implémenter la TFD inverse
ifft <- function(Xk) {
  N <- length(Xk)
  n <- 0:(N-1)
  xn <- complex(real=rep(0,N), imaginary=rep(0,N))
  
  for (nn in n) {
    for (k in 0:(N-1)) {
      xn[nn+1] <- xn[nn+1] + Xk[k+1] * exp(2i * pi * k * nn / N)
    }
    xn[nn+1] <- xn[nn+1] / N
  }
  
  return(xn)
}

### Exercice 1 ###
# Appliquer la TFD inverse aux résultats de l'exercice 1 pour retrouver le signal initial
signal_retrouve_exo_1 <- Re(ifft(Xk))  # Utilisez la partie réelle de la TFDI

# Créer un data frame pour le signal retrouvé
df_signal_retrouve_exo_1 <- data.frame(t, signal_retrouve_exo_1)

# Créer un vecteur de temps pour une interpolation plus fine
t_interpolation_retrouve <- seq(0, (N-1)*Te, length.out = 1000)

# Calculer la sinusoïde correspondante à votre signal avec interpolation
sinusoide_interpol_retrouve <- 2 * sin(8 * pi * t_interpolation_retrouve) + 8 * cos(4 * pi * t_interpolation_retrouve)

# Créer un data frame pour la sinusoïde interpolée
df_sinusoide_interpol <- data.frame(t = t_interpolation_retrouve, sinusoide = sinusoide_interpol_retrouve)

# Tracer le signal et la sinusoïde interpolée sur le même graphique
(
  ggplot() +
    geom_line(data = df_signal_retrouve_exo_1, aes(x = t, y = Re(signal_retrouve_exo_1)), color = "blue", linetype = "dashed") +
    geom_path(data = df_sinusoide_interpol, aes(x = t, y = sinusoide), color = "red") +
    geom_point(data = df_signal_retrouve_exo_1, aes(x = t, y = Re(signal_retrouve_exo_1)), color = "blue") +  # Ajouter des points bleus pour le signal
    labs(x = "Temps", y = "Amplitude") +
    ggtitle("Signal Retrouvé - Exercice 1")
)

# # Calculer le spectre d'amplitude
# amplitude_spectrum_exo_1 <- Mod(Xk)
# 
# # Créer un data frame pour le spectre d'amplitude
# df_spectrum_exo_1 <- data.frame(
#   frequency = seq(0, (N-1)*(1/Te), by=1/Te),
#   amplitude_spectrum = amplitude_spectrum_exo_1
# )
# 
# # Effectuer une interpolation pour obtenir une courbe lisse
# smoothed_spectrum_exo_1 <- spline(df_spectrum_exo_1$frequency, df_spectrum_exo_1$amplitude_spectrum, n=1000)
# 
# # Créer un nouveau data frame pour les données lissées
# df_smoothed_spectrum_exo_1 <- data.frame(
#   frequency = smoothed_spectrum_exo_1$x,
#   amplitude_spectrum = smoothed_spectrum_exo_1$y
# )
# 
# # Tracer le spectre d'amplitude lissé avec les points originaux
# (
#   ggplot(df_spectrum_exo_1, aes(x=frequency))
#   + geom_line(aes(y=amplitude_spectrum), color="blue", linetype = "dashed")  # Tracer le spectre d'amplitude original sous forme de courbe bleue
#   + geom_line(data=df_smoothed_spectrum_exo_1, aes(x=frequency, y=amplitude_spectrum), color="red")  # Tracer le spectre d'amplitude lissé sous forme de courbe rouge
#   + geom_point(data=df_spectrum_exo_1, aes(x=frequency, y=amplitude_spectrum), color="blue")  # Ajouter des points bleus aux sommets de la courbe originale
#   + labs(x="Fréquence (Hz)", y="Amplitude")
#   + ggtitle("Spectre d'Amplitude Lissé - Exercice 1")
# )

### Exercice 2 ###
# Appliquer la TFD inverse aux résultats de l'exercice 2 pour retrouver le signal initial
signal_retrouve_exo_2 <- Re(ifft(Xk1))  # Utilisez la partie réelle de la TFDI

# Créer un data frame pour le signal retrouvé
df_signal_retrouve_exo_2 <- data.frame(t1, signal_retrouve_exo_2)

# Créer un vecteur de temps pour une interpolation plus fine
t_interpolation_retrouve <- seq(0, (N1-1)*Te, length.out = 1000)

# Calculer la sinusoïde correspondante au signal retrouvé avec interpolation
sinusoide_interpol_retrouve <- 3 * sin(8 * pi * t_interpolation_retrouve) + 4 * cos(6 * pi * t_interpolation_retrouve)

# Créer un data frame pour la sinusoïde interpolée du signal retrouvé
df_sinusoide_interpol_retrouve <- data.frame(t1 = t_interpolation_retrouve, sinusoide = sinusoide_interpol_retrouve)

# Tracer le signal retrouvé et la sinusoïde interpolée sur le même graphique
(
  ggplot() +
    geom_line(data = df_signal_retrouve_exo_2, aes(x = t1, y = signal_retrouve_exo_2), color = "blue", linetype = "dashed") +
    geom_path(data = df_sinusoide_interpol_retrouve, aes(x = t1, y = sinusoide), color = "red") +
    geom_point(data = df_signal_retrouve_exo_2, aes(x = t1, y = signal_retrouve_exo_2), color = "blue") +
    labs(x = "Temps", y = "Amplitude") +
    ggtitle("Signal Retrouvé - Exercice 2")
)

# # Calculer le spectre d'amplitude
# amplitude_spectrum_exo_2 <- Mod(Xk1)
# 
# # Créer un data frame pour le spectre d'amplitude
# df_spectrum_exo_2 <- data.frame(
#   frequency = seq(0, (N1-1)*(1/Te), by=1/Te),
#   amplitude_spectrum = amplitude_spectrum_exo_2
# )
# 
# # Effectuer une interpolation pour obtenir une courbe lisse
# smoothed_spectrum_exo_2 <- spline(df_spectrum_exo_2$frequency, df_spectrum_exo_2$amplitude_spectrum, n=1000)
# 
# # Créer un nouveau data frame pour les données lissées
# df_smoothed_spectrum_exo_2 <- data.frame(
#   frequency = smoothed_spectrum_exo_2$x,
#   amplitude_spectrum = smoothed_spectrum_exo_2$y
# )
# 
# # Tracer le spectre d'amplitude lissé avec les points originaux
# (
#   ggplot(df_spectrum_exo_2, aes(x=frequency))
#   + geom_line(aes(y=amplitude_spectrum), color="blue", linetype = "dashed")  # Tracer le spectre d'amplitude original sous forme de courbe bleue
#   + geom_line(data=df_smoothed_spectrum_exo_2, aes(x=frequency, y=amplitude_spectrum), color="red")  # Tracer le spectre d'amplitude lissé sous forme de courbe rouge
#   + geom_point(data=df_spectrum_exo_2, aes(x=frequency, y=amplitude_spectrum), color="blue")  # Ajouter des points bleus aux sommets de la courbe originale
#   + labs(x="Fréquence (Hz)", y="Amplitude")
#   + ggtitle("Spectre d'Amplitude Lissé - Exercice 2")
# )