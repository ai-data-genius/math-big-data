# Charger le package "crayon"
library(crayon)

### Exercice 1 ###
# Appliquer la TFDI aux résultats de l'exercice 1
ifft_result_exo_1 <- fft(Xk, inverse = TRUE) / N  # Inversez la TFD

# Créer un data frame pour le signal retrouvé
df_signal_retrouve_exo_1 <- data.frame(t, Re(ifft_result_exo_1))  # Utilisez la partie réelle de la TFDI

# Tracer le signal retrouvé dans le domaine temporel
(
  ggplot(df_signal_retrouve_exo_1, aes(x=t, y=Re(ifft_result_exo_1)))
  + geom_line()
  + labs(x="Temps", y="Amplitude")
  + ggtitle("Signal Retrouvé - Exercice 1")
)

### Exercice 2 ###
# Appliquer la TFDI aux résultats de l'exercice 2 (utiliser une nouvelle variable)
ifft_result_exo_2 <- fft(Xk1, inverse = TRUE) / N2  # Inversez la TFD

# Créer un data frame pour le signal retrouvé
df_signal_retrouve_exo_2 <- data.frame(t, Re(ifft_result_exo_2))  # Utilisez la partie réelle de la TFDI

# Tracer le signal retrouvé dans le domaine temporel
(
  ggplot(df_signal_retrouve_exo_2, aes(x=t, y=Re(ifft_result_exo_2)))
  + geom_line()
  + labs(x="Temps", y="Amplitude")
  + ggtitle("Signal Retrouvé - Exercice 2")
)