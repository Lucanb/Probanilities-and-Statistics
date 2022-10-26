d1=function(n,media,dispersia,proc)
{
  alfa=1-proc
  sigma=sqrt(dispersia)
  a=media-proc*sigma/sqrt(n)
  b=media+proc*sigma/sqrt(n)
  interval=c(a,b)
  cat("Intervalul de incredere este: ", interval)
}

d2=function(alfa)
{
  count=0;
  for(k in 1:100)
  {
    n=1;
    z=c()
    x=c(0,1,2,3,4,5,6,7,8,9)
    for(i in 1:40)
    {
      y=sample(x,10)
      z[n]=y
      n=n+1
    }
  }
  k=k+1
  n=40
  s=0.4
  se=sqrt(s/n)
  critical_t=qnorm(1-alfa/2,n-1)
  a=sample_mean - critical_t*se
  b=sample_mean + critical_t*se
  interval=c(a,b)
  if(4.5>=a && 4.5 <=b)
    count=count+1
  return (count)
}

d3= function(n, p0, succese, alfa, ipoteza){
  p_prim = succese/n;
  z_score = (p_prim - p0)/(sqrt(p0*(1 - p0)/n));
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
}

d4= function(n, p0, succese, alfa, ipoteza){
  p_prim = succese/n;
  z_score = (p_prim - p0)/(sqrt(p0*(1 - p0)/n));
  if(ipoteza=='left'){
    critical_z = qnorm(alfa,0,1);
    if(z_score < critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
  if(ipoteza=='right'){
    critical_z = qnorm(1 - alfa,0,1);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
  if(ipoteza=='sim'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa");
    }
    else{
      cat("Nu se poate respinge ipoteza alternativa");
    }
  }
}

temaD=function()
{
  d1(20,300,30^2,0.9)
  cat("\n")
  d1(20,300,30^2,0.95)
  cat("\n")
  cat(d2(0.99))
  cat("\n")
  cat(d2(0.95))
  cat("\n")
  d3(1250,0.72,852,0.1,"right")
  cat("\n")
  d3(1250,0.72,852,0.05,"right")
  cat("\n")
  d4(1020,0.60,623,0.1,"right")
  cat("\n")
  d4(1020,0.60,623,0.05,"right")
}
temaD()