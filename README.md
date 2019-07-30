# IGoR
Shiny interface for running R code in simple situations

IGoR (prononcer "Aië gore", pour "I go R" : je deviens R), est une interface graphique de prise en main de fonctions simples de maniuplation de tables de données sous R.
IGoR est un générateur de code qui, à partir d'éléments fournis sous une interface construite avec Shiny, délivre le code nécessaire en utilisant au maximum le tidyverse.
Autour de ce dernier, les packages **rio** et **ggformula** apportent leurs capacités de prise en main rapide des fonctions d'entré/sortie ou de production de graphiques.

IGoR a été conçu pour simplifier la prise en main de R en affrichissant l'utilisateur statisticien de la nécessité de connaître la syntaxe, les fonctions ou les packages nécessaire à la réalisation de ses tâches.
Le code produit peut être exéctuté sous IGoR et permettre le déroulement d'une analyse simple, mais peut aussi être amené ou recopié en dehors d'IGoR.
A deux exceptions près (wtd.percent et gf_lorenz) le code ne dépend pas d'IGoR mais uniquement des packages appelés par IGoR.

## Installation
L'installation d'IGoR est simple : il suffit de recopier l'ensemble des fichiers dans la hierarchie fournie ici et de d'assure que les packages nécessaires sont disponibles.
Le fichier *init.R* contient tous les élements qui ont trait à la configurations :<br>
- le chargement des packages necssaires,<br>
- la liste des emplacements des fichiers.<br>
Cette dernière a été construite dans un contexte d'installation système particulier et devra être modifiée manuellement,
soit pour l'adapter aux disques Windows accessibles, soit pour l'adapter aux chemins UNIX.

IGoR a été développé sous les versions 3.4 et 3.5 de R. Certains packages présentent des lacunes dans des versions antérieures de R.

## Lancement
