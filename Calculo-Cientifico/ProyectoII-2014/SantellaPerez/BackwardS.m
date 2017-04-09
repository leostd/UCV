function X = BackwardS(U,b) 

N = size(U);
X = zeros([N(1) 1]);

for i = N(1):-1:1
    X(i) = b(i);
    for j = i+1:N(1)
        X(i) = X(i) - U(i,j)*X(j);
    end
    X(i) = X(i)/U(i,i);
end