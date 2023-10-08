chi_squared_test <- function(theoretical, observed) {
  # Calcul de la somme des carrés des écarts entre les valeurs théoriques et observées
  chi_squared <- sum((observed - theoretical)^2 / theoretical)
  
  # Degrés de liberté
  df <- length(theoretical) - 1
  
  # Calcul de la valeur de p
  p_value <- 1 - pchisq(chi_squared, df)
  
  # Création d'un vecteur contenant les résultats
  results <- c(chi_squared, df, p_value)
  
  return(results)
}

# Données
theoretical <- c(10, 10, 15, 20, 30, 15)
observed <- c(30, 14, 34, 45, 57, 20)

# Appliquez le test du chi-squared
test_results <- chi_squared_test(theoretical, observed)

# Affichez les résultats
cat("Statistique de test chi-squared:", test_results[1], "\n")
cat("Degrés de liberté:", test_results[2], "\n")
cat("Valeur de p:", test_results[3], "\n")

# Seuil de signification
alpha <- 0.05

# Interprétation
if (test_results[3] < alpha) {
  cat("La distribution de l'étude de marché diffère significativement des observations. Il pourrait y avoir un problème.\n")
} else {
  cat("La distribution de l'étude de marché semble être en accord avec les observations.\n")
}