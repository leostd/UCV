function SplinesE1

x = 3:6;
y = [9/2 8 25/2 18];
scatter(x,y)
disp('Enter para continuar')
pause
%El menor grado posible de un polinomio que se construye con un par de
%puntos consecutivos es 1 (una recta)
%Con los valores de la tabla podemos formar 3 polinomios Si(x)

x2 = 3:0.1:6;
A0 = [x(1) 1; x(2) 1];
b0 = [y(1);y(2)];
A1 = [x(2) 1; x(3) 1];
b1 = [y(2);y(3)];
A2 = [x(3) 1; x(4) 1];
b2 = [y(3) ;y(4)];
s0 = A0\b0;
s1 = A1\b1;
s2 = A2\b2;
y0 = polyval(s0, [3 4])
y1 = polyval(s1, [4 5])
y2 = polyval(s2, [5 6])
plot([3 4],y0);
disp('Enter para continuar')
pause
plot([4 5],y1);
disp('Enter para continuar')
pause
plot([5 6],y2);
disp('Enter para continuar')
pause
f = x2.^2/2;
plot(f)
disp('Enter para continuar')
pause
f0 = x([1 2]).^2/2
f1 = x([2 3]).^2/2
f2 = x([3 4]).^2/2
s0
s1
s2
r0 = f0 - y0
r1 = f1 - y1
r2 = f2 - y2

%Los ri algunos son distintos de [0 0] por cuestiones de representacion
%punto flotante
