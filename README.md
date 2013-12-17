TRADUCTEUR - PROJET CERCLES²
============================

HOW TO USE
----------

Pour compiler : `make`
Pour nettoyer : `make clean`

L'executable porte le nom scade2b.
Pour traduire un projet, on donne en paramètre le répertoire du projet Scade :

    ./scade2b chemin/vers/projet

Les machines sont générées dans le repertoire `Machines_B`, dans le repertoire du projet.

NEW FEATURES
------------

  - Traduction du projet entier, et non d'un node individuel.
  - Création automatique de machines contenant les définitions de constantes.
  - Génération automatique des machines IMPORTS.
