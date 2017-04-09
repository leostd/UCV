%%Ejercicio4

B = [10 5 5; 2 9 0; 6 8 8];

%Paso 1

x1 = B(:,1);
I = eye(3);
e1 = I(:,1);
y1 = e1 * norm(x1);
u1 = x1 - y1;


H1 =  I - (2/(u1'*u1))*(u1*u1');
B1 = H1*B;

%Paso 2
e2 = [ 1; 0];
x2 = [B1(2,2) ; B1(3,2)];
y2 = e2 * norm(x2);
u2 = x2 - y2;
I2 = eye(2);
H2a =  I2 - (2/(u2'*u2))*(u2*u2');

H2 = I;
H2(2,3) = H2a(1,2);
H2(2,2) = H2a(1,1);
H2(3,2) = H2a(2,1);
H2(3,3) = H2a(2,2);

R = H2*B1;

%%Paso 3

Qinv = H2*H1;
Q = Qinv';
Q*R; %%Da como resultado la misma matriz B, pero con punto flotante

%%Paso 4
[Q2,R2] = qr(B);

Q==Q2
R==R2

-Q==Q2
-R==R2

-Q
Q2

-R
R2

%%Las cifras son las mismas, difieren en el signo

