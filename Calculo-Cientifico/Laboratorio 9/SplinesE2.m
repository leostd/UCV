x = [1965 1970 1980 1985 1990 1991];
y = [17769 24001 25961 34336 29036 33417];
pp = spline(x,y);
xa = [1962 1977 1992];
ra = ppval(pp,xa);
rr = [12380 27403 32059];
abs(ra - rr)