
import pygame
import threading
import time
import random
import pandas as pd
import numpy as np
import itertools
import math

#############################
#   PARAMÈTRES GLOBAUX
#############################

# Dimensions
LARGEUR, HAUTEUR = 1200, 800

# Couleurs
BLANC = (255, 255, 255)
NOIR = (0, 0, 0)
ROUGE = (255, 0, 0)
VERT = (0, 255, 0)
BLEU = (0, 0, 255)
GRIS = (200, 200, 200)

COLOR_TOP = (50, 50, 50)
COLOR_BOTTOM = (10, 10, 10)

# Bouton START
button_rect = pygame.Rect(LARGEUR // 2 - 50, HAUTEUR - 80, 100, 40)

#############################
#   VARIABLES GLOBALES
#############################
chemin_essai = []         # Liste des points à survoler
positions = {}            # Positions des points
edges_info = []           # Arêtes parcourues (pt_dep, pt_arr, cout)

#############################
#   INIT PYGAME + FOND
#############################
pygame.init()
fenetre = pygame.display.set_mode((LARGEUR, HAUTEUR))
pygame.display.set_caption("Simulation de Livraison")

background = pygame.Surface((LARGEUR, HAUTEUR))

def dessiner_fond_degrade(surface, color_top, color_bottom):
    """Dessine un dégradé vertical du haut vers le bas dans surface."""
    width = surface.get_width()
    height = surface.get_height()
    for y in range(height):
        ratio = y / float(height)
        r = int(color_top[0] * (1 - ratio) + color_bottom[0] * ratio)
        g = int(color_top[1] * (1 - ratio) + color_bottom[1] * ratio)
        b = int(color_top[2] * (1 - ratio) + color_bottom[2] * ratio)
        pygame.draw.line(surface, (r, g, b), (0, y), (width, y))

dessiner_fond_degrade(background, COLOR_TOP, COLOR_BOTTOM)

#############################
#   FONCTIONS DE POSITIONS
#############################
def distance_euclidienne(p1, p2):
    return math.dist(p1, p2)

def creer_positions(chemin, largeur, hauteur, min_distance=60, max_essais=1000):
    """
    Génère des positions aléatoires pour chaque point du 'chemin'
    """
    positions_local = {}
    for point in chemin:
        for _ in range(max_essais):
            x = random.randint(50, largeur - 50)
            y = random.randint(50, hauteur - 50)

            trop_proche = any(distance_euclidienne((x, y), positions_local[pt]) < min_distance
                              for pt in positions_local)
            if not trop_proche:
                positions_local[point] = (x, y)
                break
        else:
            positions_local[point] = (x, y)
    return positions_local

#############################
#   ARCS (COURBES) + FLÈCHES
#############################
def points_bezier_quadratique(A, B, steps=20):
    """
    Génère une liste de points (x,y) selon une courbe de Bézier quadratique
    entre A et B.
    """
    (xA, yA) = A
    (xB, yB) = B

    mx = (xA + xB) / 2
    my = (yA + yB) / 2

    offset_x = random.randint(-80, 80)
    offset_y = random.randint(-80, 80)
    cx = mx + offset_x
    cy = my + offset_y

    result = []
    for t in np.linspace(0, 1, steps):
        x_t = (1 - t) ** 2 * xA + 2 * (1 - t) * t * cx + (t ** 2) * xB
        y_t = (1 - t) ** 2 * yA + 2 * (1 - t) * t * cy + (t ** 2) * yB
        result.append((x_t, y_t))
    return result

def dessiner_fleche(surface, color, start_pos, end_pos, arrow_size=12, width=2):
    """
    Dessine une flèche directionnelle (start_pos -> end_pos).
    """
    pygame.draw.line(surface, color, start_pos, end_pos, width)
    dx = end_pos[0] - start_pos[0]
    dy = end_pos[1] - start_pos[1]
    angle = math.atan2(dy, dx)
    xF = end_pos[0]
    yF = end_pos[1]
    left_angle = angle + math.radians(150)
    right_angle = angle - math.radians(150)
    left_x = xF + arrow_size * math.cos(left_angle)
    left_y = yF + arrow_size * math.sin(left_angle)
    right_x = xF + arrow_size * math.cos(right_angle)
    right_y = yF + arrow_size * math.sin(right_angle)

    color_tete = (255, 0, 0)
    pygame.draw.polygon(surface,color_tete, [(xF, yF), (left_x, left_y), (right_x, right_y)])

#############################
#   FONCTIONS D'AFFICHAGE
#############################
def dessiner_points():
    for point, (x, y) in positions.items():
        pygame.draw.circle(fenetre, (100, 100, 255), (x, y), 10)
        pygame.draw.circle(fenetre, BLEU, (x, y), 8)
        label = point.split('_')[-1]
        font = pygame.font.SysFont("Arial", 20, bold=True)
        texte = font.render(label, True, BLANC)
        fenetre.blit(texte, (x - 10, y - 30))

def dessiner_arretes_parcourus():
    """
    Dessiner toutes les arêtes déjà parcourues
    """
    for (pt_dep, pt_arr, cout) in edges_info:
        dessiner_fleche(fenetre, VERT, pt_dep, pt_arr, arrow_size=10, width=2)

        xm = (pt_dep[0] + pt_arr[0]) // 2
        ym = (pt_dep[1] + pt_arr[1]) // 2

        font = pygame.font.SysFont("Arial", 18, bold=True)
        txt_cout = font.render(str(cout), True, BLANC)
        rect_cout = txt_cout.get_rect(center=(xm, ym))
        pygame.draw.rect(fenetre, (50, 50, 50), rect_cout)
        fenetre.blit(txt_cout, rect_cout)

def dessiner_bouton():
    """Dessiner le bouton 'START'."""
    pygame.draw.rect(fenetre, GRIS, button_rect, border_radius=5)
    font = pygame.font.SysFont("Arial", 24, bold=True)
    texte = font.render("START", True, NOIR)
    text_rect = texte.get_rect(center=button_rect.center)
    fenetre.blit(texte, text_rect)

def afficher_cout_total(total_cout):
    """Afficher le coût total dans un encadré en haut."""
    rect_cout = pygame.Rect(LARGEUR // 2 - 100, 10, 200, 50)
    pygame.draw.rect(fenetre, (80, 80, 80), rect_cout, border_radius=8)
    pygame.draw.rect(fenetre, ROUGE, rect_cout, width=2, border_radius=8)

    font = pygame.font.SysFont("Arial", 24, bold=True)
    texte = font.render(f"Coût total : {total_cout}", True, BLANC)
    text_rect = texte.get_rect(center=rect_cout.center)
    fenetre.blit(texte, text_rect)
    pygame.display.flip()

#############################
#   SIMULATION : THREAD
#############################
def simulation_livraison(df):
    """
    Simule le déplacement "camion" sur chemin_essai,
    récupère le coût dans 'df', anime le trajet (courbe Bézier)
    """
    global edges_info
    edges_info.clear()

    total_cout = 0

    for i in range(len(chemin_essai) - 1):
        depart = chemin_essai[i]
        arrivee = chemin_essai[i + 1]

        A = positions[depart]
        B = positions[arrivee]

        masque = (
            ((df["ID1"] == depart) & (df["ID2"] == arrivee))
            | ((df["ID1"] == arrivee) & (df["ID2"] == depart))
        )
        cout = df.loc[masque, "Temps trajet (s)"].values[0]
        total_cout += cout

        points_courbe = points_bezier_quadratique(A, B, steps=60)

        for idx in range(len(points_courbe)):
            fenetre.blit(background, (0, 0))
            dessiner_points()
            dessiner_arretes_parcourus()

            if idx > 0:
                dessiner_fleche(
                    fenetre, (0, 200, 0),
                    points_courbe[0],
                    points_courbe[idx],
                    arrow_size=10,
                    width=2
                )
            (x, y) = points_courbe[idx]
            pygame.draw.circle(fenetre, (255, 150, 150), (int(x), int(y)), 12)
            pygame.draw.circle(fenetre, ROUGE, (int(x), int(y)), 10)

            pygame.display.flip()
            time.sleep(0.001)

        edges_info.append((A, B, cout))

    # Affichage final
    fenetre.blit(background, (0, 0))
    dessiner_points()
    dessiner_arretes_parcourus()
    afficher_cout_total(total_cout)

#############################
#   LANCEMENT PYGAME
#############################
def start_simulation(chemin, df_externe):

    global chemin_essai, positions

    chemin_essai = chemin[:15]

    positions = creer_positions(chemin_essai, LARGEUR, HAUTEUR, min_distance=80)

    main(df_externe)

def main(df_externe=None):
    fenetre.blit(background, (0, 0))
    dessiner_points()
    dessiner_bouton()
    pygame.display.flip()

    lancer_simulation = False
    running = True

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if button_rect.collidepoint(event.pos):
                    lancer_simulation = True

        if lancer_simulation:
            simulation_thread = threading.Thread(target=simulation_livraison, args=(df_externe,))
            simulation_thread.start()
            lancer_simulation = False

    pygame.quit()
