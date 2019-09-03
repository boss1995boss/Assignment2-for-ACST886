#Assignment2 - Q1

#calculate the year between two day considering leap years
years = function(date1, date2){
  date1 = as.Date(date1)
  date2 = as.Date(date2)
  if(date1 - date2 >0){u = date1; date1 = date2; date2 = u}
  n = as.numeric(date2 - date1)
  y1 = as.numeric(substr(date1,1,4))
  y2 = as.numeric(substr(date2,1,4))
  difd = floor((y2-y1)/4)
  
  m1 = as.numeric(substr(date1, 6,7))
  m2 = as.numeric(substr(date2, 6,7))
  if(m1 <= 2 & y1%%4 == 0){difd = difd + 1}
  if(m2 > 2 & y2%%4 == 0){difd = difd + 1}
  n = n- difd
  years = n/365            
}

#Life A, Life D and Life H. They are alive.
A_date1 = as.Date("1964-08-12"); A_date2 = as.Date("1998-06-20")
D_date1 = as.Date("1965-10-27"); D_date2 = as.Date("1998-01-4")
H_date1 = as.Date("1965-07-4"); H_date2 = as.Date("1998-02-16")
c(years(A_date2, A_date1),years(D_date2, D_date1),years(H_date2, H_date1))>30

#Life B. He dies before 30.
B_date1 = as.Date("1964-05-06"); B_date2 = as.Date("1993-06-12")
years(B_date2,B_date1)<30

#Life C and Life F. They withdrawdal 
C_date1 = as.Date("1994-08-12"); C_date2 = as.Date("1995-06-18")
F_date1 = as.Date("1995-04-18"); F_date2 = as.Date("1995-12-12")
E_C = as.numeric(C_date2 - C_date1)
E_F = as.numeric(F_date2 - F_date1)
c(E_C, E_F)

#Life E, Life G and Life J, They die.
E_date1 = as.Date("1965-01-4"); E_date2 = as.Date("1996-08-29")
G_date1 = as.Date("1965-5-20"); G_date2 = as.Date("1996-04-21")
J_date1 = as.Date("1965-12-11"); J_date2 = as.Date("1997-02-17")
c(years(E_date2, E_date1),years(G_date2, G_date1),years(J_date2, J_date1))>=30

#Life I. He withdrawal.
I_date1 = as.Date("1965-09-16"); I_date2 = as.Date("1997-02-22")
years(I_date2, I_date1)>=31

#estimate
E_30 = 365* 3 + 0 + 310 + 238 + 365* 3 + 365; E_30 = E_30/365
d_30 = 3
q_30 = d_30 / E_30; q_30

fac = 4+((310+238)/365)
q_mle = 3/(3+fac); q_mle