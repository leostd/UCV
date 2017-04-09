%%Verde=x; Rojo=vector ortogonal a u; Azul= vector ortogonal a u en
%%direccion opuesta; Amarillo = y normalizado

function  Ejercicio1(u,x)

ort = [-u(2); u(1)];
ort2 = [u(2); -u(1)];



a = u*u';
b = u'*u;
c = 2*(1/b);
H = eye(2) - (b*a);
y = H*x;
yn = (1/norm(y))*y;

ax=abs(x);
aort2 = abs(ort2);
aort = abs(ort);
ayn = abs(yn);
figure
hold on
ejex = max([ax(1) aort(1) aort2(1) ayn(1)]);
ejey = max([ax(1) aort(1) aort2(1) ayn(1)]);
axis([-1.5*ejex 1.5*ejex -1.5*ejey 1.5*ejey])

plot([0 u(1)], [0 u(2)],'v--');
disp('Enter para continuar')
pause
plot([0 x(1)], [0 x(2)],'g');
disp('Enter para continuar')
pause
plot([0 ort(1)], [0 ort(2)],'r');
disp('Enter para continuar')
pause
plot([0 ort2(1)], [0 ort2(2)],'b');
disp('Enter para continuar')
pause
plot([0 yn(1)], [0 yn(2)],'y');
hold off