function [x, iter] = sor(A,b, maxiter, E, w)
    n = size(A,1);
    x = zeros(n,1);
    xold = x;
    xnew = xold;
    for iter = 1: maxiter
        for i = 1:n
           x1 = 0;
           x2 = 0;
           for j = 1:i-1
               x1 = x1 + A(i,j)*xnew(j);
           end
           for j = i+1:n
               x2 = x2 + A(i,j)*xold(j);
           end
           xnew(i) = ((w/A(i,i))*(b(i)-x1-x2))+ (1-w)*xold(i);
        end
        xold = xnew;
        if norm(b-A*xnew) <= E
            x=xnew;
            break;
        end
    end