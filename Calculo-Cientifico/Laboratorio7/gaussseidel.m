function [x, iter] = gaussseidel(A,b, maxiter, E)
    n = size(A,1);
    x = zeros(n,1);
    xnew = x;
    for iter = 1: maxiter
        for i = 1:n
            xnew(i) = b(i);
            for j = 1:n
                if i ~= j
                    xnew(i) = xnew(i) - A(i,j) * xnew(j);
                end
            end
            xnew(i) = xnew(i)/A(i,i);
        end
        if norm(b-A*xnew) <= E
            x=xnew;
            break;
        end
    end