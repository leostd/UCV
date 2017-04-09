//Primer ejercicio: Ec Normales y calcular Cholesky a las Ec Normales
//Mandar adjunto con calculos hechos en matlab

//Ejercicio 2. Los comentarios indican como hay que modificar este codigo
//para que resuelva una funcion cuadratica. 
A=[9 1; 10 1; 11 1; 20 1; 44 1] //Agregar una columna mas
b=[2; 3; 20; 12; 30]

Am = A'*A;
bm = A'*b;
[Q,R]=qr(Am);
x = R\(Q'*bm);
w = 1:0.01:50;
w1 = [9 10 11 20 44];
plot(w1,b,'*r');
hold on
plot(w,x(1)*w+x(2),'b'); //x(1)*w*w+x(2)*w+x(3)

//El lab es para el sabado de la semana que viene. 
//Mandar scripts de todos los ejercicios