# Lieu d'Evans

Il est parfois difficile de trouver les bons coefficients d'un regulateur [[PID]] pour reguler un systeme dynamique. En effet il faut faire varier trois coeficients en meme temps (proportionel, integrale, derivee). On peut parfois faire dependre les trois coefs du PID d'un seul parametre puis regarder le lieu des poles du systeme en boucle ferme correspondant.

Soit le systeme en boucle fermee suivant :

```    
U --->(+ -)---->[ C(k) ]--->[ R ] ------> Y
         ^                           |
         |---------------------------|
```

ou on note :
* R la fonction de transfert du systeme,
* C(k) le regulateur (par exemple PID) qui depend maintenant du seul parametre k a regler.
La fonction de transfert globale de ce systeme en boucle fermee s'ecrit : C(k) R / (1 + C(k) R)

Prenons comme exemple la fonction de transfert R = 300 / (s^2 + 1.28 s + 31) que l'on ecrira sous la forme R = g / p avec g un gain statique (g = 300) et p le polynome p = s^2 + 1.28 s + 31. On remarque que les racines de p sont stables et proches de l'axe imaginaire (-0.64 + 5.53i et -0.64 - 5.53i), et qu'il peut etre difficile de modifier les coefs d'un PID sans le rendre instable le systeme (les racines passent a droite de l'axe imaginaire). R correspond a la fonction de la balancoire du [[h4h]]. La fonction de transfert C(k) R / (1 + C(k) R) s'ecrit maintenant : H = g C(k) / (p + g C(k))

L'astuce consiste a choisir C(k) suffisamment grand pour que le polynome p soit negligable.
C(k) etant un regulateur de type PID, il s'ecrit de la facon suivante : C(k) = a(k) / s + b(k) + c(k) s ou a(k) est le gain de l'integrale, b(k) est le gain du proportionel et c(k) le gain de la derivee.
H s'ecrit maintenant : H = g (a(k) + b(k) s + c(k) s^2) / (s p + g (a(k) + b(k) s + c(k) s^2))

Les poles de H vont evolues en fonction de k. Pour k grand alors a(k), b(k) et c(k) sont grands et les poles sont voisins des racines du polynome Q = s^3 + g (a(k) + b(k) s + c(k) s^2). On peut placer les racines de ce polynome en prenant Q = (s + k) (s + k + ik) (s + k -ik) pour k grand.

**Remarque:** Ce genre de placement de pole (avec un pole sur l'axe reel negatif et deux poles a +135° et -135° comme sur la figure ci dessous) conduit classiquement aux meilleures reponses a un creneau (montee relativement raide et peu d'overshoot).

```
        ^
        |
    X   | ik
      \ |
       \|
----X------------->
   -k  /|
      / |
    X   | -ik
        |
```

Il reste a verifier que lorsqu'on fait varier k de 0 a l'infini, que le systeme reste toujours stable (cad que les poles ne traversent pas l'axe imaginaire). Ensuite, sur le banc d'essai on reglera le k de facon a ce qu'il soit le plus grand possible tout en gardant la stabilite du systeme.

**Remarque:** Des petits retards non modelises dans R peuvent limiter l'amplitude du gain k en rendant instable le systeme.

## Exemple concret : visualiser les poles selon les differentes valeurs de k

(Avec g = 1 au lieu de 300 pour simplifer)
s p + g (a(k) + b(k) s + c(k) s^2) = s^3 + s^2 (1.28 + 3k) + s (31 + 4k^2) + 2k^3

Programme Scilab associe:
```
kmax=100;
pole1=[];
pole2=[];
pole3=[];
for k = 0:kmax
  x=roots(%s^3+%s^2*(1.28+3*k)+%s*(31+4*k^2)+2*k^3);
  pole1=[pole1, x(1)];
  pole2=[pole2, x(2)];
  pole3=[pole3, x(3)];
end

xbasc();
plot2d(real(pole1), imag(pole1), 2);
plot2d(real(pole2), imag(pole2), 3);
plot2d(real(pole3), imag(pole3), 5);
```

**Remarque:** Ce programme n'est pas robuste dans le sens ou les racines x ne sont pas forcement donnees dans le bon ordre. Il faudrait tenir compte des anciens resultats et les trier par rapport aux resultat le plus proches. Ce qui n'est pas fait dans ce programme. Mais Scilab trie relativement bien les resultats pour cet exemple.

Resultat obtenu :

En prenant un k suffisamment grand, on obtient un tres bon resultat. On remarque que les poles n'ont jamais traverses l'axe imaginaire :-D

## Exemple Scicos d'un systeme en boucle ferme avec Evans

A faire: changer la valeur k definie dans le contexte de Scicos par exemple k = 3, k = 4.5, k = 10, k = 20, k = 100. On remarque bien les overshoots.
