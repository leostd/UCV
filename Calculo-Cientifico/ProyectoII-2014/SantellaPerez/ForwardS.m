function X = ForwardS(L,b)

N = size(L);
X = zeros([N(1) 1]);


for i = 1:N(1)
    X(i) = b(i); 
    for j=1:i-1
        X(i) = X(i)-L(i,j)*X(j);
    end
    X(i) = X(i)/L(i,i);
end
        
    