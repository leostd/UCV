function B = Ejercicio3L7p2(A)

L = tril(A,-1);
U = triu(A,1);
D = diag(diag(A));


B = -1*inv(D)*(L+U);