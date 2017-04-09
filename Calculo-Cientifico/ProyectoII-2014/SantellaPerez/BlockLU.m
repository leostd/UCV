function[L,U] = BlockLU2(A,N,p)
%La matriz A siempre es cuadrada
%Las matrices internas son cuadradas

%Colocamos las matrices identidad y las matrices nulas de la matriz L,
%triangular inferior y matriz U, triangular superior

Aux = A;
%Matrices identidad en las submatrices de la diagonal de L, primera fila de
%submatrices en la matriz U
for k=1:N            
    L([(p*k)-p+1:p*k], [(p*k)-p+1:p*k]) = eye(p);
end

for i=1:N
    for j=i+1:N
        Laux = zeros(N*p);
        for k=1:N
            Laux([(p*k)-p+1:p*k], [(p*k)-p+1:p*k]) = eye(p);
        end
        Laux2 = zeros(N*p);
        pivot = Aux([((i-1)*p)+1:i*p],[((i-1)*p)+1:i*p]);
        if det(pivot) == 0
            disp('El pivote es no es invertible, por lo tanto no se puede continuar. L y U seran nulos')
            L = 0;
            U = 0;
            return;
        end
        mij = Aux([((j-1)*p)+1:j*p], [((i-1)*p)+1:i*p]);
        Laux([((j-1)*p)+1:j*p], [((i-1)*p)+1:i*p]) = SELMatrix(pivot',mij')';
        L = L*Laux;
        Laux2([((j-1)*p)+1:j*p], [((i-1)*p)+1:i*p]) = -SELMatrix(Aux([((i-1)*p)+1:i*p],[((i-1)*p)+1:i*p])',Aux([((j-1)*p)+1:j*p], [((i-1)*p)+1:i*p])')';
        for k=1:N            
            Laux2([(p*k)-p+1:p*k], [(p*k)-p+1:p*k]) = eye(p);
        end
        Aux = Laux2*Aux;
    end
   
end
U = Aux;

        
