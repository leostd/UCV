function [c,L] = lagrange(x,y)
format rat
w = length(x);
n = w-1;
L =zeros(w,w);
for k=1:n+1
    V=1;
    for j=1:n+1
        if k~=j
            V = conv(V,poly(x(j)))/(x(k)-x(j));
        end
    end
    L(k,:) = V;    
end
c = y*L;



