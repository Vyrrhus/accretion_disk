# M06-Disque accrétion

Ce code consiste à simuler un disque de gaz en rotation autour d'un trou noir de Schwarzschild.

## Architecture projet

Le sujet du projet, ainsi que l'article associé peuvent être trouvés dans `/info/`.

Les autres fichiers utiles sont dans les répertoires :
- `/config/` : ces fichiers déterminent les paramètres d'entrée de la simulation.
- `/src/` : contient le code source à compiler.
- `/python/` : contient les différents codes python pour traiter les données obtenues dans `/output/`.

### Comment lancer une simulation ?

On commence par prendre soin de choisir les paramètres d'entrée `config/input.config` et de sortie `config/output.config`.

Si les fichiers de configuration ne sont pas présents, les fichiers de configuration par défaut sont copiés depuis `default/` vers `config/` avec le premier run `make`.


La simulation s'exécute avec `./disk` une fois les fichiers compilés. Pour fonctionner, il faut s'assurer que les courbes en S sont bien calculées avec les mêmes paramètres d'entrée et la même discrétisation spatiale `NX` (cf `src/modules/declarations.f90` pour ce paramètre). Le cas échéant, si les courbes ont déjà été calculées précédemment, on peut éviter de les recalculer en mettant en paramètre d'entrée COURBE EN S = 0 (cf `config/input.config`).

### Comment visualiser les résultats ?

Une fois le fichier de sortie produit, on peut visualiser les résultat en appelant le fichier python `python/disk.py [filename]` depuis le root.

A noter que le fichier de sortie peut être corrompu si jamais la simulation a été arrêtée manuellement. Le cas échéant, il suffit d'ouvrir le fichier de sortie concerné `output/*.out` et supprimer les lignes correspondant au dernier pas de temps. Une erreur de lecture peut survenir soit si le dernier bloc de variables est incomplet, soit s'il y a des NaN (ce qui n'arrivera pas dans la mesure où les paramètres d'entrée sont soigneusement choisis).


Les fichiers pythons relatifs aux courbes en S ne s'exécutent qu'en les appelant depuis le dossier `python`

## Makefile

Il faut installer **BLAS** et **LAPACK** pour pouvoir compiler.

`make` : compile la simulation principale
`make clean`: supprime les fichiers binaires & exécutables.

## Executables

`./disk` : lance la simulation du disque d'accrétion

## Python

