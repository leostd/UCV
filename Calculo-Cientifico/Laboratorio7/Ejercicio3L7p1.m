function B = Ejercicio3L7p1(A,w)

L = tril(A,-1);
U = triu(A,1);
D = diag(diag(A));

c = 1-w;
Ds = c*D;
Us = (-w*U);
C = Ds*Us;
B =inv(w*L + D)*C;

