import itertools
import numpy as np
import pandas as pd

def generer_donnees(nb_points=200, seed=1):
    np.random.seed(seed)
    points = [f"Point_{i}" for i in range(1, nb_points + 1)]

    # Génération de toutes les combinaisons possibles de paires de points
    combinaisons = list(itertools.combinations(points, 2))

    data = {
        "ID1": [pair[0] for pair in combinaisons],
        "ID2": [pair[1] for pair in combinaisons],
        "Temps trajet (s)": np.random.randint(1, 101, len(combinaisons))
    }

    df = pd.DataFrame(data)
    return df
