# main.py

from data_simulation import generer_donnees
from optimisation_aco import aco_tsp_large
from trajet_simulation import start_simulation

def main():

    #############################
    # 1) Générer les données
    #############################

    df = generer_donnees(nb_points=10, seed=42)

    #############################
    # 2) Résoudre le TSP par ACO
    #############################
    chemin_complet, cout_opt = aco_tsp_large(df, nb_fourmis=50, nb_iterations=100)
    print("Meilleur chemin :", chemin_complet)
    print("Coût total      :", cout_opt)

    #############################
    # 3) Démarrer la simulation
    #############################
  
    start_simulation(chemin_complet, df)

if __name__ == "__main__":
    main()
