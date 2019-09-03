#Assignment2 - Q2

ExptoR = c(15518, 19428, 21594, 21890, 19174, 
           15775, 11414, 6993, 3276, 1096, 201)
D = c(65, 144, 219, 378, 465, 557, 685, 644, 471, 217, 67)
ED = c(73.9, 134.6, 223.9, 346.3, 468.1, 600.2, 675.5, 637.4, 458.7, 240.6, 61.4)
graduation = ED/ExptoR
q_hat = D/ExptoR
z = (D-ED)/sqrt(ExptoR*graduation*(1-graduation))

#smooth 
m = length(graduation)
m = m-3
abs(diff(diff(diff(graduation))))*7^3 < graduation[1:m]

#chi square test
chi_fit = sum(z^2)
chi_fit < qchisq(.95, length(D)-1)

##cumulative deivation test
abs(sum(D)-sum(ED))/sqrt(sum(ExptoR*graduation*(1-graduation)))<1.96

#standardised deviatios test
m = length(z)
number_expected = m * c(pnorm(-1), pnorm(0)-pnorm(-1), pnorm(0)-pnorm(-1), 1-pnorm(1))
number_observed = numeric()
number_observed[1] = length(z[z< -1])
number_observed[2] = length(z[z< 0]) - length(z[z< -1])
number_observed[3] = length(z[z< 1]) - length(z[z< 0])
number_observed[4] = length(z[z< 2]) - length(z[z< 1])

sum((number_observed-number_expected)^2/number_expected) < qchisq(0.95, 3)

#sign test
signcount = function(z){
  n = length(z)
  num = 0
  for(i in 2:n){if(z[i]*z[i-1]<0){num = num + 1}}
  signcount = (num+2-1*(z[1]<0)-(z[n]<0))/2
}
m = length(z)
n1 = signcount(z)

n1 > qbinom(0.025, m, 0.5) && n1 < qbinom(0.975, m, 0.5)
#group of signs test
n2 = signcount(-z)
#find the critical value for hypergeometric distributon
qhyg = function(n1,n2){
  k = 1
  level = 0.05
  for(tim in 1:n1+n2){
    p = 0
    for(j in 1:k){ 
      p = p + choose(n1-1,j-1)*choose(n2+1,j)/choose(n1+n2, n1)
    }  
    if(p>=level){break}
    k = k+1
  }
  qhyg = k
}
#find the number of different sign groups
group = function(z){
  n = length(z)
  k = 1
  for(i in 2 :n){
    if(z[i]*z[i-1]<0){k = k+1}
  }
  return(k)
}

group(z) > qhyg(n1,n2)

#serial correlation test
m = length(z)
v1 = z[1:m-1]; v2 = z[2:m]
mu1 = mean(v1); mu2 = mean(v2)
dv1 = v1 - mu1; dv2 = v2 - mu2
sum((dv1)*(dv2))/sqrt(sum(dv1^2)*sum(dv2^2))*sqrt(m-1) < qnorm(.95, 0, 1/(m-1))
