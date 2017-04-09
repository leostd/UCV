function X = SELMatrix(A,B)
N = size(A);
X = zeros(N(1));

[Q,R] = qr(A);


for i = 1:N(1)
    x = BackwardS(R,Q'*B(:,i));
    X(:,i) = x;
end