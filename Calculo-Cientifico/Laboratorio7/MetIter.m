function [xnew,iter] = MetIter(A,x0,b,tol,maxit)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Implementar los m'etodos iterativos estacionarios:                   
% Jacobi, Gauss-Seidel y SOR (para este caso se necesita un par'ametro extra w))
% 
%  Par'ametros de entrada:
% A     es un matriz nxn del sistema lineal a resolver Ax=b.
% x0    es un vector n, aproximaci'on inicial a la soluci'on del sistema Ax=b.
% b     es un vector n, vector independiente del sistema Ax=b.
% tol   numero real, es la tolerancia permitida para la condici'on de parada.
% maxit entero positivo, es el m'aximo n'umero de iteraciones permitidas.
%
% Par'ametros de Salida:
% xnew  es un vector n, aproximaci'on obtenida por el metodo iterativo al sistema Ax=b.
% iter  entero positivo, numero de iteraciones realizadas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Vamos a utilizar dos vectores: 
% xold contiene el aproximado de la iteraci'on anterior.
% xnew contiene el aproximado de la iteraci'on actual.
xold = x0;
xnew = xold;
iter = 0;

%n contiene la dimensi'on de la matriz A
n = size(A,1);

for iter=1:maxit,
    for i=1:n
        xnew(i) = %Actualizar la posici'on i-'esima del nuevo vector 
                  %dependiendo del m'etodo a implementar 
                  %puede que se necesite otro ciclo interno.
    end
    xold = xnew;
    if % condicion de parada: 
       % puede ser norma del residual, 
       % norma del residual relativa, 
       % norma de la diferencia entre dos iterados consecutivos
       % etc
        break;
    end
end

