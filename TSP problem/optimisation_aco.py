# optimisation_aco.py

import numpy as np
import pandas as pd

def aco_tsp_large(df, nb_fourmis=50, nb_iterations=100, alpha=1, beta=2, evaporation_rate=0.1, q=10):
    """
    Optimisation par colonie de fourmis pour le problème du voyageur de commerce.

    Arguments :
    - df : DataFrame contenant les points et les distances.
    - nb_fourmis : Nombre de fourmis simulées.
    - nb_iterations : Nombre d'itérations.
    - alpha : Importance des phéromones.
    - beta : Importance de l'attractivité.
    - evaporation_rate : Taux d'évaporation des phéromones.
    - q : Quantité de phéromones déposées par les fourmis.

    Retourne :
    - meilleur_chemin_points : Liste des points dans l'ordre optimal.
    - meilleur_cout : Coût total du chemin optimal.
    """
  
    points = pd.concat([df['ID1'], df['ID2']]).unique()
    n = len(points)
    points_dict = {point: idx for idx, point in enumerate(points)}

    # Matrice des distances et des phéromones
    distance_matrix = np.full((n, n), np.inf)
    pheromone_matrix = np.ones((n, n))

    for _, row in df.iterrows():
        i, j = points_dict[row['ID1']], points_dict[row['ID2']]
        distance_matrix[i, j] = distance_matrix[j, i] = row['Temps trajet (s)']

    # Fonction pour calculer les probabilités de transition
    def calcul_probabilite(visited, current_node):
        prob = np.zeros(n)
        non_visited = set(range(n)) - visited
        tau_eta = pheromone_matrix[current_node, list(non_visited)] ** alpha * \
                  (1 / distance_matrix[current_node, list(non_visited)]) ** beta
        prob[list(non_visited)] = tau_eta
        total = prob.sum()
        return prob / total if total > 0 else np.zeros_like(prob)

    # Fonction pour construire un chemin
    def construire_chemin():
        chemin = [np.random.randint(n)]
        visited = {chemin[-1]}
        while len(visited) < n:
            prob = calcul_probabilite(visited, chemin[-1])
            next_node = np.random.choice(range(n), p=prob)
            chemin.append(next_node)
            visited.add(next_node)
        return chemin

    # Calculer le coût d'un chemin
    def calculer_cout(chemin):
        return sum(distance_matrix[chemin[i], chemin[i + 1]] for i in range(len(chemin) - 1))

    # Mettre à jour les phéromones
    def mettre_a_jour_pheromones(chemins, couts):
        nonlocal pheromone_matrix
        pheromone_matrix *= (1 - evaporation_rate)  # Évaporation
        for chemin_f, cout_f in zip(chemins, couts):
            depot = q / cout_f if cout_f > 0 else 0
            for i in range(len(chemin_f) - 1):
                pheromone_matrix[chemin_f[i], chemin_f[i + 1]] += depot
                pheromone_matrix[chemin_f[i + 1], chemin_f[i]] += depot  # Symétrie

    # Initialisation
    meilleur_chemin = None
    meilleur_cout = float('inf')

    # Boucle principale
    for _ in range(nb_iterations):
        chemins = []
        couts = []

        for _ in range(nb_fourmis):
            chem = construire_chemin()
            c = calculer_cout(chem)
            chemins.append(chem)
            couts.append(c)

            # Mise à jour du meilleur chemin
            if c < meilleur_cout:
                meilleur_chemin = chem
                meilleur_cout = c

        # Mise à jour des phéromones
        mettre_a_jour_pheromones(chemins, couts)

    # Conversion du meilleur chemin en noms de points
    meilleur_chemin_points = [list(points_dict.keys())[idx] for idx in meilleur_chemin]
    return meilleur_chemin_points, meilleur_cout
