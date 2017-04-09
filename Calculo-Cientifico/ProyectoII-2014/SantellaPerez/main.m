function main(N,p,q)

A = ABlock(N,p);
B = BBlock(N,p,q);

X = SolBlock(A,B,N,p,q)
AxX = A*X