# IPRF - Projet d'analyse d'un fichier text en OCaml

Dans ce readme, vous trouverez les informations pour compiler, et tester le projet.
Vous retrouverez de toute façon une grande partie de ces informations dans le rapport.

La première partie de ce projet consiste à utiliser des listes, pour récupérer les mots d'un fichier texte, pour les stocker.
Le code fourni n'a pas été modifié pour les parties 1 et 2 du sujet. En effet, les sauts de lignes et tabulations ne sont pas prises en charge.
On observe assez rapidement que les listes sont trop gourmandes, pour pouvoir stocker de gros textes. Ainsi, nous arrivons à un stack overflow en essayant de stocker de gros fichiers texte, avec des listes.
En revanche, j'ai modifié le code pour la troisième partie. Pour utilier le type définit en partie 3, et ainsi l'utiliser de façon optimale.

## Compilation

Pour compiler, placez vous à la racine du projet.
Vous pouvez utiliser la commande ```make``` pour générer l'exécutable.

## Tests

Pour essayer le code que vous aurez compilé, vous trouverez l'exécutable dans le répertoire ```test```
Dans ce répertoire se trouvent aussi les différents fichiers textes que j'utilise pour tester mes fonctions.

## Arborescence du projet

```
 ./Sujet.pdf
 ./Romain_EPIARD_Projet_IPRF_Rapport.pdf
 ./Makefile
 ./src/*.ml;*.mli
 ./_build/*.cmo;*.cmi
 ./test/IPRF_Analyseur_De_Texte_Romain_EPIARD;*.txt
```

## Auteur
- Romain EPIARD