# M06-Disque accrétion

Ce code consiste à simuler un disque de gaz en rotation autour d'un trou noir de Schwarzschild.

## Architecture projet

Le sujet du projet, ainsi que l'article associé peuvent être trouvés dans `/info/`.

Les autres fichiers utiles sont dans les répertoires :
- `/config/` : ces fichiers déterminent les paramètres d'entrée de la simulation
- `/src/` : contient le code source à compiler.
- `/python/` : contient les différents codes python pour traiter les données obtenues dans `/output/`.

## Makefile

Il faut installer **BLAS** et **LAPACK** pour pouvoir compiler.

`make` : compile la simulation principale

`make curve` : compile la simulation de courbes en S 

`make schema` : compile schéma truc

## Executables

`disk` : lance la simulation du disque d'accrétion

`scurve` : idem pour les courbes en S

`schema` : ?

## Python

...