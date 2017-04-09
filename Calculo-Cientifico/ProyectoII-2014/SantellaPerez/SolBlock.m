function X = SolBlock(A,B,N,p,q)

X = zeros(size(B));
Y = zeros(size(B));
[L,U] = BlockLU2(A,N,p);

%Resolvemos el sistema de ecuaciones por bloques LW = Y con sustitucion 
%hacia adelante
for i=1:N
    Y([(i-1)*p+1:p*i],[1:q]) = B([(i-1)*p+1:p*i],[1:q]);
    for j = 1: i-1
        p1 = L([(i-1)*p+1:p*i],[(j-1)*p+1:p*j]);
        p2 = Y([(j-1)*p+1:p*j],[1:q]);
        Y([(i-1)*p+1:p*i],[1:q]) = Y([(i-1)*p+1:p*i],[1:q]) - p1*p2;
    end
end

%Resolvemos el sistema de ecuaciones por bloques UX = W por sustitucion 
%hacia atras
for i = N:-1:1
    X([(i-1)*p+1:p*i],[1:q]) = Y([(i-1)*p+1:p*i],[1:q]);
    for j= i+1:N
        p1 = U([(i-1)*p+1:p*i],[(j-1)*p+1:p*j]);
        p2 = X([(j-1)*p+1:p*j],[1:q]);
        X([(i-1)*p+1:p*i],[1:q]) =  X([(i-1)*p+1:p*i],[1:q]) - p1*p2;
    end
    p3 = U([(i-1)*p+1:p*i],[(i-1)*p+1:p*i]);
    p4 = X([(i-1)*p+1:p*i],[1:q]);
    X([(i-1)*p+1:p*i],[1:q]) = (SELMatrix( p3',p4'))';
end