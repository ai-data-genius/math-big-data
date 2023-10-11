path <- "C:\\divino\\cours\\master\\m2\\s1\\maths\\sympathique.txt"
data <- read.table(path, header=TRUE, sep=" ")

####################################
# Affichage du tableau sympathique #
####################################

print(data)

##################################################################
# Exercice 1 : Calculer le pourcentage de chaque caractéristique #
# d'une personne sympathique et le pourcentage des catégories    #
# professionnelles sur l'ensemble des votes.                     #
##################################################################

characteristics <- data[-nrow(data), -c(1, ncol(data))]
sum_characteristics <- colSums(characteristics)

print("Pourcentage de chacune des caractéristiques :")
print((sum_characteristics / sum(sum_characteristics)) * 100)

sum_categories <- rowSums(characteristics)
percentage_categories <- (sum_categories / sum(sum_characteristics)) * 100
print("Pourcentage de chacune des catégories professionnelles :")
names(percentage_categories) <- data$CAT[-nrow(data)] 
print(percentage_categories)


###################################################################
# Exercice 2 : Combien de personnes ont été sondées ? Donner la   #
# proportion des employés pour qui être honnête rend sympathique. #
# Quelle est la proportion d'employés parmi les gens qui pensent  #
# qu'être honnête rend sympathique ?                              #
###################################################################

print(paste("Il y a", data[nrow(data),]["TOTAL"] / 3, "personnes qui ont été sondées car il y a 3 votes par personne."))
print(paste("La proportion des employés pour qui être honnête rend sympathique est de", (data[data$CAT == "EMPL", "HONN"] / data[data$CAT == "EMPL", "TOTAL"]) * 100, "%"))
print(paste("La proportion des employés parmi les gens qui pensent qu'être honnête rend sympathique est de", (data[data$CAT == "EMPL", "HONN"] / data[nrow(data), "HONN"]) * 100, "%"))


################################################################################
# Exercice 3 : Pourquoi 7 valeurs propres ?                                    #
# Réponse : Les valeurs propres représentent la variance expliquée par chaque  #
# axe factoriel. En général, on retient les axes dont les valeurs propres sont #
# significativement plus grandes que les autres. Dans ce cas, il semble que 7  #
# axes ont des valeurs propres significatives, ce qui signifie que ces 7 axes  #
# capturent l'essentiel de la variance dans les données. Les autres axes ont   #
# des valeurs propres proches de zéro, ce qui signifie qu'ils n'expliquent     #
# pas beaucoup de variance.                                                    #
################################################################################

library(FactoMineR)

df <- subset(data, select = -TOTAL)
df <- subset(df, select = -CAT)
df <- df[-9,]
res_ac <- FactoMineR::CA(df, graph = TRUE)

print(res_ac$eig)


################################################################################
# Exercice 4 : Quelles sont les modalités qui définissent le premier axe       #
# factoriel ? Et le second ? Préciser sur  quel(s) critère(s)                  #
# on se peut se fonder                                                         #
################################################################################

# Utiliser la fonction dimdesc pour obtenir les modalités qui définissent
# le premier et le second axe
resultats_dimdesc <- dimdesc(res_ac, axes = c(1, 2), proba = 0.05)

# Afficher les modalités qui définissent le premier axe
print(resultats_dimdesc["Dim 1"])

# Afficher les modalités qui définissent le second axe
print(resultats_dimdesc["Dim 2"])


#########################################################################
# Exercice 5 : Quelles sont les modalités (lignes et colonnes) qui sont #
# particulièrement mal repr±entés par le premier plan factoriel ?       #
#########################################################################

# Utiliser la fonction dimdesc pour obtenir les modalités qui définissent
# le premier axe
resultats_dimdesc <- dimdesc(res_ac, axes = c(1, 2), proba = 0.05)

# Extraire les coordonnées des modalités pour le premier axe (row)
resultats_dimdesc_axe1 <- resultats_dimdesc["Dim 1"]$`Dim 1`$row

# Créer un dataframe avec les modalités et leurs coordonnées sur le premier axe
print(data.frame(Modalite = rownames(resultats_dimdesc_axe1), Axe1 = resultats_dimdesc_axe1))

# Extraire les coordonnées des modalités pour le deuxième axe (row)
resultats_dimdesc_axe2 <- resultats_dimdesc["Dim 2"]$`Dim 2`$row

# Créer un dataframe avec les modalités et leurs coordonnées sur le deuxième axe
print(data.frame(Modalite = rownames(resultats_dimdesc_axe2), Axe2 = resultats_dimdesc_axe2))


#############################################################################
# Exercice 6 : Quelle déduction peut-on faire du fait que OUVR et PAYS sont #
# proches sur le graphique ? Même question pour VEND et honn                #
#############################################################################

