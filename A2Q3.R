#Assignment2 - Q3

#5.7
e = c(1000, 1005, 1010, 1008, 1006, 998)
d = c(80, 90, 95, 105, 115, 125)
x = 70:75
l = function(p){sum(e*p[1]*p[2]^(x)) - sum(d*log(e*p[1]*p[2]^(x)))}
p = nlm(l, c(.0002, 1.085))
p = p[[2]]
print(p)


e = c(70000, 66672, 68375, 65420, 61779, 66091, 68514, 69560, 65000,
      66279, 67300, 65368, 65391, 62917, 66537, 62302, 62145, 63856, 61097, 61110)
d = c(39, 43, 34, 31, 23, 50, 48, 43,  48,  47,  62,
      63,  84,  86, 120, 121, 122, 162, 151, 184)
q = d/e
x = 30:49
wls = function(p){t = log(q/(1-q)); sum(e*(t-p[1]-p[2]*x)^2)}
p = nlm(wls, c(-10, 0.01))
p = p[[2]]
print(p)

#5.9
e = c(166,  187,  218,  243,  276,  302,  347,  390,  
      430,  494,  558,  628,  701,  813,  917, 1040, 1182, 1299, 1432, 1596, 1752)
d = c(2,  2,  4 ,6,  2,  4,  7, 3,  9,  9,  8, 11, 14, 18, 18 ,24, 30, 43, 41, 54, 64)
q = d/e
qs = c(0.505, 0.570, 0.644, 0.728, 0.826, 0.930, 1.051, 1.184, 1.331, 1.492,
       1.668, 1.859, 2.065,2.287, 2.525, 2.778, 3.049, 3.339, 3.648, 3.978, 4.332)/100
x = 47:67
wls = function(p){t = p[1] + p[2]*qs; sum(e*(q-t)^2)}
p = nlm(wls, c(1e-2, 1))
p = p[[2]]
print(p)
