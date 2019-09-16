// Simulation tres simplifiee d'un helicopter sous ScicosLab
clear all;

// Cross product
function b=cdot(b1, b2)
b = [ b1(2) * b2(3) - b1(3) * b2(2);
b1(3) * b2(1) - b1(1) * b2(3);
b1(1) * b2(2) - b1(2) * b2(1)];
endfunction

// Produit de quaternion
function qres=qprod(q1, q2)
b1=q1(2:4);
b2=q2(2:4);
a1=q1(1);
a2=q2(1);
qres=[a1 * a2 - b1' * b2;
a1 * b2 + a2 * b1 + cdot(b1, b2)];
endfunction

// Conjuge d'un quaternion
function qc=qconj(q)
qc=[q(1); -q(2:4)]
endfunction

// Appliquer la rotation definie par le quat R (unitaire)
// a un vecteur defini par un quat M (imaginaire)
function Mr=Xrot(R,M)
Mr=qprod(R,qprod(M,qconj(R)))
endfunction


function eul=q2euler(q)
q0=q(1); q1=q(2); q2=q(3); q3=q(4);
eul=[atan(2*(q0*q1+q2*q3), 1-2*(q1*q1+q2*q2));
     asin(2*(q0*q2-q3*q1));
     atan(2*(q0*q3+q1*q2), 1-2*(q2*q2+q3*q3))]
endfunction

// ----------
// Matrice d'inertie
I=[1, 0, 0;
0, 2, 0;
0, 0, 3];

// Couple (force de rotation)
C=[0;0;0];

//------------
// Vitesse instantanee de rotation l'objet
om = [0; 0; 0; 2 * %pi];


// Dynamique du solide inertie I lorsqu'on lui applique le couple C
function xdot=f(t, x)
q = x(1:4);
om = x(5:7);
// Vitesse des quat associee a la vitesse de rotation instantanee om
xdot(1:4) = 0.5 * qprod(q, [0; om]);
// Equation d'Euler
xdot(5:7) = I \ (cdot(I * om, om) + C);
endfunction


P = [0; 0; 1; 0];
X0 = [1; 0; 0; 0; 0.01; 1; 0.01];
h=0.1
T=100
i=1;
for t=0:h:T-h
  // Calcul la rotation
  X=ode(X0, t, t+h, f);
  // Fait tourner X
  R=X(1:4);
  Pr=Xrot(R,P);
  P=[P,Pr];
  X0=X;

  M=q2euler(P(:,i));i=i+1;
  Mx=[1 0 0; 0 cos(M(1)) -sin(M(1)); 0 sin(M(1)) cos(M(1))];
  My=[cos(M(2)) 0 sin(M(2)); 0 1 0; -sin(M(2)) 0 cos(M(2))];
  Mz=[cos(M(3)) -sin(M(3)) 0; sin(M(3)) cos(M(3)) 0; 0 0 1];

end
