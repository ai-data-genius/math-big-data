library(FactoMineR)
library(factoextra)
library(dplyr)
library(plotly)

# Lecture des données
decathlon_data <- read.table("decathlon.dat", header=TRUE, sep=" ")

# Suppression de la colonne 'COMPET'
decathlon_data <- decathlon_data[, !(names(decathlon_data) %in% c("COMPET"))]

# Calcul de la matrice de corrélation
correlation_matrix <- cor(decathlon_data)
print(correlation_matrix)

# 2. Identifier les paires de variables les plus corrélées, les moins corrélées et les plus opposées

# Extraction des corrélations sans la diagonale et les valeurs en double
correlations <- correlation_matrix[upper.tri(correlation_matrix)]
  
# La paire la plus corrélée
most_correlated_idx <- which.max(correlations)
most_correlated_names <- rownames(correlations)[most_correlated_idx]
cat("Most Correlated:", most_correlated_names, "with value", correlations[most_correlated_idx], "\n")

# La paire la plus opposée
most_opposed_idx <- which.min(correlations)
most_opposed_names <- rownames(correlations)[most_opposed_idx]
cat("Most Opposed:", most_opposed_names, "with value", correlations[most_opposed_idx], "\n")

# La paire la moins corrélée
least_correlated_idx <- which.min(abs(correlations))
least_correlated_names <- rownames(correlations)[least_correlated_idx]
cat("Least Correlated:", least_correlated_names, "with value", correlations[least_correlated_idx], "\n")


# 3. Visualiser la matrice de corrélation pour observer les regroupements
corrplot::corrplot(correlation_matrix, method = "shade", order = "hclust", tl.col = "black", tl.srt = 45)

# B. Analyse en Composantes Principales (ACP)

# 4. Réalisation de l'ACP
decathlon_pca <- PCA(decathlon_data %>% select(-c(RANG, POINTS)), scale.unit = TRUE)
# Tracé du scree plot
fviz_eig(decathlon_pca, addlabels = TRUE, ylim = c(0, 40))

# Affichage des valeurs propres
eigenvalues <- decathlon_pca$eig
print(eigenvalues)

# Pourcentage d'inertie total retenu par les trois premières composantes principales
cum_inertia <- sum(eigenvalues[1:3, 2])
cat("Pourcentage d'inertie des trois premières composantes:", cum_inertia, "%\n")

# 5. Détermination des trois principales composantes
ind_coord <- decathlon_pca$ind$coord[, 1:3]
print(ind_coord)
# Plot the PCA object
plot(decathlon_pca, axes = c(1, 2))
plot(decathlon_pca, axes = c(2, 3))


# 6. Corrélations par rapport à C1, C2, et C3
variables_correlation <- decathlon_pca$var$cor
print(variables_correlation)

# Visualisation des cercles de corrélation
factoextra::fviz_pca_var(decathlon_pca, axes = c(1, 2))  # Cercle de corrélation pour (C1, C2)
factoextra::fviz_pca_var(decathlon_pca, axes = c(2, 3))  # Cercle de corrélation pour (C2, C3)

# 7. Variables déterminant les principales composantes (utilisation d'un seuil, par exemple 0.7 pour une forte corrélation)
highly_correlated_vars <- which(abs(variables_correlation[, 1:3]) > 0.6, arr.ind = TRUE)
print(highly_correlated_vars)

# 8. Effet de taille (exemple hypothétique : multiplication de toutes les performances par une variable de taille)
# Création d'une nouvelle variable représentant l'effet de taille
decathlon_data$effet_taille <- decathlon_data$haut * decathlon_data$long

# Réaliser à nouveau l'ACP en incluant l'effet de taille
decathlon_pca_ajusté <- PCA(decathlon_data %>% select(-c(RANG, POINTS)), scale.unit = TRUE)

# Visualisation des résultats de l'ACP ajustée
factoextra::fviz_pca_var(decathlon_pca_ajusté, axes = c(1, 2))  # Cercle de corrélation pour (C1, C2 ajusté)
factoextra::fviz_pca_var(decathlon_pca_ajusté, axes = c(2, 3))  # Cercle de corrélation pour (C2, C3 ajusté)

