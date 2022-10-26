e1=function()
{
  alfa = 0.01
  population_mean = 420
  sample_mean = 418
  n = 125
  sigma = 2.75
  critical_z = qnorm(1- alfa)
  z_score = (sample_mean - population_mean)/(sigma/sqrt(n))
  critical_z
  z_score
  # In acest caz u0 va fi 418 si u va fi 420
  # u0<u deci este ip simetrica la dreapta
  # Cum Z>Z* (aici Z este z_score si z* este critical_z)
  if(population_mean > sample_mean)
  {
    if(z_score > critical_z)
      cat("Ipoteza este acceptata")
    else
      cat("Nu avem dovezi pentru asta")
  }
  if(population_mean < sample_mean)
  {
    if(z_score < critical_z)
      cat("Ipoteza este acceptata")
    else
      cat("Nu avem dovezi pentru asta")
  }
  if(population_mean == sample_mean)
    cat("Ipoteza este nula")
}
e1()




e2a=function(alfa,population_mean,sample_mean,n,s)
{
  se = s/sqrt(n)
  if(population_mean > sample_mean) #dr
    critical_t = qt(1 - alfa, n - 1)
  if(population_mean < sample_mean) #stg
    critical_t = qt(alfa, n - 1)
  else
    critical_t = qt(1 - alfa/2, n - 1)
  t_score = (sample_mean - population_mean)/se
  sw=0
  if(critical_t>t_score && population_mean > sample_mean && sw==0)
    sw=1
  if (critical_t<t_score && population_mean < sample_mean && sw==0)
    sw=1
  if(critical_t>t_score && population_mean != sample_mean && sw==0)
    sw=1
  if(sw==0)
    cat("Ipoteza nula poate fi respinsa")
  else
    cat("Ipoteza nula nu poate fi respinsa")
  
}
e2a(0.01,4.9,5.17,25,0.35)
e2a(0.05,4.9,5.17,25,0.35)


e3a=function(alfa,sample1_mean,sample2_mean,n1,n2,sigma1,sigma2)
{
  m0=0
  if(sample1_mean - sample2_mean < m0)
    critical_z = qnorm(alfa,0,1)
  if(sample1_mean - sample2_mean > m0)
    critical_z = qnorm(1-alfa,0,1)
  else 
    critical_z = qnorm(1-alfa/2,0,1)
  combined_sigma = sqrt(sigma1^2/n1 + sigma2^2/n2)
  critical_z = qnorm(1 - alfa/2)
  z_score = (sample1_mean - sample2_mean - m0)/combined_sigma
  if((sample1_mean < sample2_mean && z_score < critical_z) ||  (sample1_mean > sample2_mean && z_score > critical_z) ||  (sample1_mean != sample2_mean && z_score > critical_z))
    cat("ipoteza nula va fi respinsa ¸si se accepta ca mediile celor doua populatii sunt diferite")
  else
    cat("ipoteza nula va fi acceptata")
}

e3a(0.01,5.48,6.12,25,28,1.31,0.93)


e3b=function(alfa,sample1_mean,sample2_mean,n1,n2,sigma1,sigma2) ####DE VERIFICAT
{
  m0=0
  if(sample1_mean - sample2_mean < m0)
    critical_z = qnorm(alfa,0,1)
  if(sample1_mean - sample2_mean > m0)
    critical_z = qnorm(1-alfa,0,1)
  else 
    critical_z = qnorm(1-alfa/2,0,1)
  combined_sigma = sqrt(sigma1^2/n1 + sigma2^2/n2)
  critical_z = qnorm(1 - alfa/2)
  z_score = (sample1_mean - sample2_mean - m0)/combined_sigma
  
  critical_Fs = qf(alfa/2,n1-1,n2-1);
  critical_Fd = qf(1-alfa/2,n1-1,n2-1);
  
  if((z_score<critical_Fs )||(z_score>critical_Fd)){
    cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");
  }
  else{
    cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
  }
  cat("Scorul:", z_score," Valori critice: ", critical_Fs, critical_Fd,  "\n");
}                         

e3b(0.01,5.48,6.12,25,28,1.31,0.93)



e4a=function(n1,n2,s1,s2,alfa) ################
{
   F_score=s1^2/s2^2
   sw=0;
   critical_F= qf(1-alfa,n1-1,n2-1) 
   
   critical_F_s= qf(alfa/2, n1 - 1, n2 - 1)
   critical_F_d= qf(1 - alfa/2, n1 - 1, n2 - 1)
  if(F_score>critical_F || (F_score<critical_F_s && F_score>critical_F_d))
        sw=1;
   else
     cat("nu exista suficiente dovezi pentru a respinge ipoteza nula")
   
   if(sw==0)
     cat("nu exista suficiente dovezi pentru a respinge ipoteza nula")
  if(sw==1)
    cat("Se respinge ipoteza nula si se accepta ipoteza Ha alternativa")
  if(sw==1 && F_score<critical_F && F_score>critical_F_d )
     cat("Primul automat are dispersie mai mare.")
   if(sw==1 && F_score<critical_F && F_score<critical_F_d) 
     cat("Al doilea automat are dispersie mai mare.")  
}
e4a(25,28,1.24,0.87,0.01,"right")