#Nettoyage de l’espace de travail
rm(list=ls())

# initialisation
p = 49 # Nombre de variables explicatives
moyx = 0
varx =0
n = 0
# Connexion au fichier puis boucle de lecture s´equentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  n=n+1
  zz <- scan(text=oneLine, quiet=T, sep=";")
  x=zz[1:p]
  
  # Calcul récursif de la moyenne et de la variance pour les variables explicatives
  
  varx=varx +(((n-1)*((x - moyx)^2)/n) - varx)/n
  moyx=moyx+(1/n)*(x-moyx)
 
}
close(nfile)
# Calcul du vecteur des écart-types
sigmax = sqrt(varx)
 
round(sqrt(sum(sigmax^2)), 3)

# Nous trouvons : 2095.148

##################################### Partie 2 ##########################################

# Definition de la fonction pi(z)
pif <- function(x){
  return(1/(1+exp(-x)))
}


# Initialisation
lambda <- 1
p <- 49
Q <- lambda * diag(p+1)
theta <- rep(0, p+1)

# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  
  # Centrage-réduction des variables explicatives
  x <- zz[1:p]
  y <- zz[p+1]
  
  # Mise à jour récursive de theta
  phi <- c(1, (x - moyx) / sqrt(varx))
  
  r <- sum(theta * phi) # Utilisation de sum pour le produit sclaire
                        # Cela évite les problème de produits
  vec <- pif(r)
  
  an <- vec * (1 - vec)
  
  # Mise à jour de Q
  scalar <- 1 + an * sum(Q %*% phi * phi)
  Q <- Q - (an * (Q %*% phi %*% t(phi) %*% Q)) / scalar
  
  # Mise à jour de theta
  theta <- theta + (Q %*% phi) * (y - vec)
}

# Fermeture du fichier.
close(nfile)


# Norme du vecteur theta
round(sqrt(sum(theta^2)),4)
# Si on met ( round, 3) on trouve : 1.955 , 
# Mais si on fait (round,4) on trouve : 1.9547

round(sum(sqrt(diag(Q))), 3)

# Cette valeur vaut : 6.605


##################### Interval de confiances ##########################################


# Calcul de l'erreur standard des estimateurs
se <- sqrt(diag(Q))

# Quantile de la distribution normale pour un intervalle de confiance de 95%
z <- qnorm(0.975)

# Calcul des intervalles de confiance à 95%
IC.binf <- theta - z * se # Bornes inférieures
IC.bsup <- theta + z * se # Bornes supérieures

# On retire la constante
IC.binf <- IC.binf[-1]
IC.bsup <- IC.bsup[-1]

# Identification des variables significatives
# Une variable est considérée comme non significative si 0 est inclus dans son intervalle de confiance
varsignif <- (1:p)[!(IC.binf <= 0 & IC.bsup >= 0)]

# Affichage des indices des variables significatives
varsignif

#Nombe de variables significatives
length(varsignif)
# Nous obtenons 18 variables significatives.

#somme de varsignif
sum(varsignif)
# Cette somme vaut : 259

################################# Commentaires #############################################

# L'analyse de notre modèle de régression logistique récursive a permis d'identifier 
# 18 variables significatives sur un total de 49, en se basant sur les intervalles 
# de confiance à 95%. Cette sélection souligne l'efficacité de notre approche récursive 
# dans la distinction des variables ayant une influence réelle sur notre variable cible, 
# parmi un ensemble large de prédicteurs.

############################### Partie 3 ########################################################



# Initialisation
seuil <- 0.5
N00 <- 0 # Vrais Négatifs (VN)
N01 <- 0 # Faux Positifs (FP)
N10 <- 0 # Faux Négatifs (FN)
N11 <- 0 # Vrais Positifs (VP)

# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  
  # Centrage-réduction des variables explicatives
  x <- zz[1:p]
  phi <- c(1, (x - moyx) / sqrt(varx))
  
  # Calcul de la prédiction
  proba <- pif(sum(theta * phi))
  ychap <- ifelse(proba > seuil, 1, 0) # Prédiction binaire
  
  # Vérité terrain
  y <- zz[p + 1]
  
  # Mise à jour de la table de confusion
  if (y == 0 && ychap == 0) N00 <- N00 + 1
  if (y == 0 && ychap == 1) N01 <- N01 + 1
  if (y == 1 && ychap == 0) N10 <- N10 + 1
  if (y == 1 && ychap == 1) N11 <- N11 + 1
}
close(nfile)

# Calcul des indices de performance
N <- N00 + N01 + N10 + N11
taux_erreurs <- (N01 + N10) / N
specificite <- N11 / (N10 + N11)
sensibilite <- N00 / (N00 + N01)

# Affichage des résultats

# Matrice de confusion

# Affichage en tableau dans la console
library(knitr)

# Création d'un data.frame pour la matrice de confusion
conf_matrix <- data.frame(
  'Prédit Non Sapin' = c(N00, N10),
  'Prédit Sapin' = c(N01, N11)
)
rownames(conf_matrix) <- c('Non Sapin', 'Sapin')

# Affichage en tableau
kable(conf_matrix, caption = "Matrice de confusion", align = 'c')


# Indices de performances
cat("Taux d'erreurs : ", round(taux_erreurs * 100, 2), "%\n")
cat("Spécificité    : ", round(specificite * 100, 2), "%\n")
cat("Sensibilité    : ", round(sensibilite * 100, 2), "%\n")

##################################### Commentaires #####################################

#Notre modèle de régression logistique récursive affiche sur le Test
#une sensibilité respectable de 81,67 % , indiquant qu'il identifie 
#bien les vrais positifs mais avec une spécificité de 71,79 %,
#il y a une marge d'amélioration dans la reconnaissance des vrais négatifs. 
#Le taux d'erreur global s'élève à 22,58 %, ce qui suggère qu'un peu plus
#d'un cinquième des prédictions sont incorrectes. 
#Ces résultats dénotent un modèle qui performe de manière adéquate en termes 
#de sensibilité mais qui pourrait bénéficier d'une amélioration de sa spécificité
#pour réduire le nombre de faux positifs.

# En explorant notre dataApp on remarque que la classe 0 fait 226817
# Tandis que la classe 1 fait 169295 , on a une différence de 57522
# Ce qui n'est pas très notable mais le modèle pourrait souffir d'un 
# déséquilibre de classe et donc il serait plus performant à réconnaitre 
# La classe minoritaire même si cela est contre intuitive car on pourrait
# s'attendre à ce qu'il identifie la classe majoritaire mais en général 
# Dans un projet de regression logistique , on penserait à rééquilibrer nos classes.

# L'autre chose qu'il faut noté aussi est qu'on a fixé le seuil à 5%.


############################### Partie complémentaire ##################################


# Recalcul de la moyenne et variance de façon récursive uniquement sur les variables
# binaires.

#Nettoyage de l’espace de travail
rm(list=ls())

# initialisation
p = 49 # Nombre de variables explicatives
num_variables = 10  # Nombre de variables numériques
moyx = 0
varx =0
n = 0
# Connexion au fichier puis boucle de lecture s´equentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  n=n+1
  zz <- scan(text=oneLine, quiet=T, sep=";")
  x=zz[1:num_variables]
  
  # Calcul récursif de la moyenne et de la variance pour les variables explicatives
  
  varx=varx +(((n-1)*((x - moyx)^2)/n) - varx)/n
  moyx=moyx+(1/n)*(x-moyx)
  
}
close(nfile)
# Calcul du vecteur des écart-types
sigmax = sqrt(varx)

round(sqrt(sum(sigmax^2)), 3)

# Nous obtenons toujours la même valeur : 2095.148

# Cela suggère que la distribution et la dispersion des variables 
#numériques parmi les classes d'intérêt ("Spruce/Fir" et "Lodgepole Pine") 
#sont globalement similaires à celles de l'ensemble de données complet.



################ re-Calcul du model de regression logistique récursif ####################

# Definition de la fonction pi(z)
pif <- function(x){
  return(1/(1+exp(-x)))
}



# Initialisation
lambda <- 1
p <- 49
Q <- lambda * diag(p+1)
theta <- rep(0, p+1)

# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  
  # Centrage-réduction des variables numériques
  
  x_num <- zz[1:10] # Variables numériques à centrer et réduire
  x_bin <- zz[11:p] # Variables binaires à laisser telles quelles
  y <- zz[p+1]
  
  x_num_norm <- (x_num - moyx) / sqrt(varx)
  
  # Mise à jour récursive de theta
  phi <- c(1, x_num_norm, x_bin)
  
  r <- sum(theta * phi) # Utilisation de sum pour le produit sclaire
  # Cela évite les problème de produits
  vec <- pif(r)
  
  an <- vec * (1 - vec)
  
  # Mise à jour de Q
  scalar <- 1 + an * sum(Q %*% phi * phi)
  Q <- Q - (an * (Q %*% phi %*% t(phi) %*% Q)) / scalar
  
  # Mise à jour de theta
  theta <- theta + (Q %*% phi) * (y - vec)
}

# Fermeture du fichier.
close(nfile)


# Norme du vecteur theta
round(sqrt(sum(theta^2)),4)

# Cette fois ci nous trouvons  : 7.5447

# A la différence de la première partie , on a une valeur très élévée
# Avant de de tirer une conclusion , il faudra aller plus loin dans l'étude

round(sum(sqrt(diag(Q))), 3)
# Cette valeur vaut : 10.802

######################################## intervalles de confiances II ####################

# Calcul de l'erreur standard des estimateurs
se <- sqrt(diag(Q))

# Quantile de la distribution normale pour un intervalle de confiance de 95%
z <- qnorm(0.975)

# Calcul des intervalles de confiance à 95%
IC.binf <- theta - z * se # Bornes inférieures
IC.bsup <- theta + z * se # Bornes supérieures

# On retire la constante
IC.binf <- IC.binf[-1]
IC.bsup <- IC.bsup[-1]

# Identification des variables significatives
# Une variable est considérée comme non significative si 0 est inclus dans son intervalle de confiance
varsignif <- (1:p)[!(IC.binf <= 0 & IC.bsup >= 0)]

# Affichage des indices des variables significatives
varsignif

#Nombe de variables significatives
length(varsignif)
# Nous obtenons 37 sur 49 variables significatives. Donc 2 fois plus que la partie où
# On centrait et réduisons également les variables binaires.

#somme de varsignif
sum(varsignif)
# Cette somme vaut : 901

#################################### Parie 3 complémenatire ##############################


library(knitr)

# Initialisation
seuil <- 0.5
N00 <- 0 # Vrais Négatifs (VN)
N01 <- 0 # Faux Positifs (FP)
N10 <- 0 # Faux Négatifs (FN)
N11 <- 0 # Vrais Positifs (VP)

# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  
  # Centrage-réduction des variables numériques
  x_num <- zz[1:10]
  x_num_norm <- (x_num - moyx) / sqrt(varx)
  # Combinaison des variables numériques normalisées et binaires
  x_combined <- c(x_num_norm, zz[11:p])
  phi <- c(1, x_combined)
  
  # Calcul de la prédiction
  proba <- pif(sum(theta * phi))
  ychap <- ifelse(proba > seuil, 1, 0) # Prédiction binaire
  
  # Vérité terrain
  y <- zz[p + 1]
  
  # Mise à jour de la table de confusion
  if (y == 0 && ychap == 0) N00 <- N00 + 1
  if (y == 0 && ychap == 1) N01 <- N01 + 1
  if (y == 1 && ychap == 0) N10 <- N10 + 1
  if (y == 1 && ychap == 1) N11 <- N11 + 1
}
close(nfile)

# Calcul des indices de performance
N <- N00 + N01 + N10 + N11
taux_erreurs <- (N01 + N10) / N
specificite <- N11 / (N10 + N11)
sensibilite <- N00 / (N00 + N01)

# Affichage des résultats

# Matrice de confusion
conf_matrix <- data.frame(
  'Prédit Non Sapin' = c(N00, N10),
  'Prédit Sapin' = c(N01, N11)
)
rownames(conf_matrix) <- c('Non Sapin', 'Sapin')
print(kable(conf_matrix, caption = "Matrice de confusion", align = 'c'))

# Indices de performances
cat("Taux d'erreurs : ", round(taux_erreurs * 100, 2), "%\n")
cat("Spécificité    : ", round(specificite * 100, 2), "%\n")
cat("Sensibilité    : ", round(sensibilite * 100, 2), "%\n")


# Les résultats en ne normalisant que les variables numériques 
# Sont les mêmes que si on normalisait.Toutes nos variables explicatives 
# dans le cadre de notre jeu de données.
# Autant sur la sensibilité que sur la spécificité mais égalementa aussi 
# Pour le taux d'erreurs , nous avons la même chose.

# La grosse différence c'est que quand on normalise toutes les varibales 
# explicatives dans le cadre de ce jeu de données , à la fin on a que 18 su 49
# variables significatifs à 95% , alors que quand on ne normalise que les 
# variables numériques, nous avons 37 significatis sur 49.

# Donc dans ce jeu de donnée , on dirait qu'on serait tenté de tout normaliser 
# Mais peut être dans le cadre d'un autre jeu de donné , normaliser le tout pourrait 
# Ne pas être un avantage dans l'apprentissage de notre modèle. Donc encore une fois 
# Tout dépendra du jeu de donnée et de la demande métier.




############################## ALgorithme du Gradient stockastique ################################


# Notons que dans cette partie nous allons centré et réduire toutes les variables 
# Au vu du résultat que nous avons eu précédement.


#Nettoyage de l’espace de travail
rm(list=ls())

# initialisation
p = 49 # Nombre de variables explicatives
moyx = 0
varx =0
n = 0
# Connexion au fichier puis boucle de lecture s´equentielle, ligne par ligne
nfile = file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n=1, warn=F)) > 0) {
  n=n+1
  zz <- scan(text=oneLine, quiet=T, sep=";")
  x=zz[1:p]
  
  # Calcul récursif de la moyenne et de la variance pour les variables explicatives
  
  varx=varx +(((n-1)*((x - moyx)^2)/n) - varx)/n
  moyx=moyx+(1/n)*(x-moyx)
  
}
close(nfile)

##################################### Algorithme #########################################

# Fonction logistique
pif <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Initialisation
n <- 1 # Le numéro de l'itération initiale
theta <- rep(0, p+1) # Initialiser theta avec des zéros
p=49
# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_app.csv", open = "r")
while(length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  x <- zz[1:p] 
  y <- zz[p+1] # La variable cible
  
  # Centrage-réduction des variables
  x_num_norm <- (x - moyx) / sqrt(varx)
  
  phi <- c(1, x_num_norm)
  
  # Calcul du taux d'apprentissage pour l'itération courante
  gamma_n <- n^(-0.9)
  
  # Mise à jour de theta en utilisant l'algorithme du gradient stochastique
  z <- sum(theta * phi)
  theta <- theta - gamma_n * phi * (pif(z) - y)
  
  n <- n + 1
}

# Fermeture du fichier
close(nfile)

# Norme du vecteur theta
round(sqrt(sum(theta^2)),4)
#val =  1.5325
# Nous obtenons une valeur proche de celle qu'on avait pour l'ago de newton stochastique

# Nous allons donc supposer que notre algorithme a converger et nous allons 
# Aller directement dans la partie évaluation dans la partie test.


################################### Partie 3 pour gradient stochastique #############################


# Initialisation
seuil <- 0.5
N00 <- 0 # Vrais Négatifs (VN)
N01 <- 0 # Faux Positifs (FP)
N10 <- 0 # Faux Négatifs (FN)
N11 <- 0 # Vrais Positifs (VP)

# Connexion au fichier puis boucle de lecture séquentielle, ligne par ligne
nfile <- file("covtype_tst.csv", open = "r")
while (length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  zz <- scan(text = oneLine, quiet = TRUE, sep = ";")
  
  # Centrage-réduction des variables explicatives
  x <- zz[1:p]
  phi <- c(1, (x - moyx) / sqrt(varx))
  
  # Calcul de la prédiction
  proba <- pif(sum(theta * phi))
  ychap <- ifelse(proba > seuil, 1, 0) # Prédiction binaire
  
  # Vérité terrain
  y <- zz[p + 1]
  
  # Mise à jour de la table de confusion
  if (y == 0 && ychap == 0) N00 <- N00 + 1
  if (y == 0 && ychap == 1) N01 <- N01 + 1
  if (y == 1 && ychap == 0) N10 <- N10 + 1
  if (y == 1 && ychap == 1) N11 <- N11 + 1
}
close(nfile)

# Calcul des indices de performance
N <- N00 + N01 + N10 + N11
taux_erreurs <- (N01 + N10) / N
specificite <- N11 / (N10 + N11)
sensibilite <- N00 / (N00 + N01)

# Affichage des résultats

# Matrice de confusion

# Affichage en tableau dans la console
library(knitr)

# Création d'un data.frame pour la matrice de confusion
conf_matrix <- data.frame(
  'Prédit Non Sapin' = c(N00, N10),
  'Prédit Sapin' = c(N01, N11)
)
rownames(conf_matrix) <- c('Non Sapin', 'Sapin')

# Affichage en tableau
kable(conf_matrix, caption = "Matrice de confusion", align = 'c')


# Indices de performances
cat("Taux d'erreurs : ", round(taux_erreurs * 100, 2), "%\n")
cat("Spécificité    : ", round(specificite * 100, 2), "%\n")
cat("Sensibilité    : ", round(sensibilite * 100, 2), "%\n")


# Le modèle a une sensibilité de 79% donc fort comparable au modèle
# De newton stochastique avec 81%. Et il fait d'ailleurs un peu mieux pour la
# spécificité avec 75% contre 71% pour newton stochastique.
# Nénamoins la différence n'est pas très forte , il faudra juste remarquer
# qu'il est moins sensible au désequilibre des classes. 
# Mais nous savons qu'il peut etre mauvais si les valeurs 
#propres de la matrice Hessienne ont un ordre de grandeur différent
# Donc là on peut dire que ce n'est pas le cas.

