s = 1;
iter = 0;

while ( 1 + s ) ~= 1,
    
   s = s / 2;
   iter = iter + 1;
end
disp(s);

disp(iter);