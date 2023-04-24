# COOP
Projet logique : COOP

Pour installer glucose : https://github.com/wadoon/glucose
Pour installer opam lp et opam lp-glpk : opam install lp et opam install lp-glpk

Pour compiler le projet, il faut avoir les packages lp et lp_glpk, puis effectuer eval $(opam env) et enfin faire make
make clean permet de supprimer les fichiers .cmo, .cmi etc..

Make va créer my_program qui prend en entrée une formule sous la forme (  a < 7  or  b > 6  or  d > 7 ) and ( c > 7 or  a > 7  or  b > 5  )
Si elle est satisfiable ou non, et une valuation si elle est satisfiable.

./my_program tests/1.txt
Formule :  (  a < 7  or  b > 6  or  d > 7 ) and ( c > 7 or  a > 7  or  b > 5  )

Voici une valuation convenant pour cette formule
a : 0.00
b : 5.00
d : 0.00
c : 0.00
a : 0.00
b : 5.00   

./my_program tests/4.txt 
Formule : ( a < 3 ) and ( a > 4 )

La formule n'est pas satisfiable


