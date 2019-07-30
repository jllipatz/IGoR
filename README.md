# IGoR
"Shiny interface for running R code in simple situations"

IGoR (prononcer "Aië gore", pour "I go R" : je deviens R), est une interface graphique de prise en main de fonctions simples de manipulation de tables de données sous R.
IGoR est un générateur de code qui, à partir d'éléments fournis sous une interface construite avec Shiny, délivre le code nécessaire en utilisant au maximum le **tidyverse**.
Autour de ce dernier, les packages **rio** et **ggformula** apportent leurs capacités de prise en main rapide des fonctions d'entrée/sortie ou de production de graphiques.

IGoR a été conçu pour simplifier la prise en main de R en affranchissant l'utilisateur statisticien de la nécessité de connaître la syntaxe, les fonctions ou les packages nécessaires à la réalisation de ses tâches.
Le code produit peut être exécuté sous IGoR et permettre le déroulement d'une analyse simple, mais il peut aussi être amendé ou recopié en dehors d'IGoR. En outre IGoR garde un "log" des différentes séquences qu'il a généré.<br>
A deux exceptions près (`wtd.percent` et `gf_lorenz` qui sont définies dans *init.R*) le code ne dépend pas d'IGoR mais uniquement des packages appelés par IGoR.

IGoR est livré avec deux fichiers de test, mais n'est pas restreint à ceux ci et peut en pratique travailler sur tout fichier qui puisse être chargé dans la mémoire de RStudio. Néanmoins IGoR travaille dans l'optique du "tidy data" et ne connaît que les types de données de base.

Le code généré par IGoR peut être incorrect si les éléments fournis par l'utilisateur sont inadéquats ou si la structure des données sort d'un cadre strictement "tidy". Dans la mesure du possible les erreurs qui en sont la conséquence sont interceptées et n'interrompent pas le fonctionnement d'IGoR (sauf bugs). Mais les messages d'erreur proviennent le plus souvent de R lui même et ne sont traduits que dans les cas les plus évidents de fausse manipulation. Ceci est le lot commun des programmeurs en R et ne sera pas amélioré.

## Installation
L'installation d'IGoR est simple : il suffit de recopier l'ensemble des fichiers dans la hierarchie fournie ici et de s'assurer que les packages nécessaires sont disponibles.
Le fichier *init.R* contient tous les élements qui ont trait à la configuration :<br>
- le chargement des packages necssaires,<br>
- la liste des emplacements des fichiers.<br>
Cette dernière a été construite dans un contexte d'installation système particulier et devra être modifiée manuellement,
soit pour l'adapter aux disques Windows accessibles, soit pour l'adapter aux chemins UNIX.

IGoR a été développé sous les versions 3.4 et 3.5 de R. Certains packages présentent des lacunes dans des versions antérieures de R.

## Lancement
Le mode de fonctionnement conseillé est le lancement de l'application depuis RStudio. Les tables créées par IGoR ou utilisables par lui sont celles présentes dans l'environnement global. Un travail en alternance sous RStudio et sous IGoR est donc envisageable.<br>
Pour démarrer IGoR, il faut charger le source *app.R* et cliquer sur le bouton `run App` après s'être assuré que la ligne `run external` y est sélectionnée. Cette dernière fait tourner l'application Shiny sous le navigateur par défaut et non à l'intérieur de RStudio. Cela permet de disposer de fonctionnalités java script avancées comme l'indicateur d'opération longue en cours.<br>
L'arrêt d'IGoR peut se faire par fermeture de la fenêtre du navigateur ou à l'aide de l'option du menu ad'hoc qui, elle, ne fermera pas la fenêtre. Cet arrêt est nécessaire si on veut faire tourner du code sous RStudio, mais pour relancer IGoR il suffira de cliquer de nouveau sur le bouton `run App`.

## Structure de l'application
IGoR a été conçu afin d'en faciliter l'extension.<br>
- *app.R* ne fait rien de plus que charger l'ensemble des sources et lancer l'application Shiny,<br>
- *init.R* contient tout ce qui pourrait être modifiable par l'utilisateur,<br>
- *all.R* contient toutes les fonctions partagées entre les différentes pages de l'application,<br>
- *dashboard.R* contient les éléments définissant l'interface générale : la structure de l'application dashboard et la liste des menus,<br>
- chacune des pages est gérée par un seul fichier source contenant à la fois la définition de l'interface (l'"ui") et la définition des traitements (le "server"). La première est souvent réduite au strict nécessaire en raison de la versatilité d'IGoR : beaucoup d'éléments de l'interface graphique sont dynamiques et constituent en pratique l'essentiel de la définition des traitements.
