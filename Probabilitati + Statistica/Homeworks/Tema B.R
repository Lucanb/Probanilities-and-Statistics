b1=function(n,a,b,c,h)
{
  real=(pi*a*b*h^2)/(2*c)
  count=0
  for(i in 1:n)
  {
    x=runif(1,(-a)*sqrt(h/c),a*sqrt(h/c))
    y=runif(1,(-b)*sqrt(h/c),(b)*sqrt(h/c))
    z=runif(1,0,h)
    if(x^2/a^2+y^2/b^2<=z/c)
    {
      count=count+1;
    }
  }
  val=((4 * a^2*b^2*h^2)/(c^2) *count) / n
  valRelativa = abs( (real - val) / val)
  cat("Volumul Aproximat",val)
 
  cat("\nDiferenta :",  val-real)
  cat("\nEroarea Relativa :", valRelativa)
}

a=4
b=3
c=4
h=4
x1 = b1(1000000, a, b,c,h)
x2 = b1(2000000, a, b , c, h)
x3 = b1(5000000, a, b , c, h)


b2=function(n,a,b,c,d)
{
  count=0
  for(i in 1:n)
  {
    x=runif(1,a,b)
    y=runif(1,c,d)
    if(x>=1 && y<=2 && y<=x-1 && y>=0 && y<=7-x)
       count=count+1;
  }
  return (abs((b-a)*(d-c)*count)/n)
}
  
b2(200000, 0, 3, 0, 3)
  
fa=function(x)
{
 return ((x)/(x^2+2)^3)
}
fb=function(x)
{
  return(1/(x^2+9))
}
fc=function(x)
{
  return(x*exp(-x^2))
}
b3a=function(n,a,b)
{
  sum=0
  for(i in 1:n)
  {
    x=runif(1,a,b)
    sum=sum+fa(x)
  }
  aria=((b-a)*sum)/n
  cat("Aria aproximativa intre :" , a ,"si", b ," : ",aria,"\n")
  cat("Valoarea exacta intre 1 si 2 :" , 1/48)  
}
b3a(20000 ,1,2)

b3b=function(n,a,b)
{
  sum=0;
  for(i in 1:n)
  {
    x=runif(1,a,b)
    sum=sum+fb(x)
  }
  aria=(b-a)*sum/n
  cat("Aria aproximata intre :" ,a, "si",b,":",aria,"\n")
  cat("Valoarea exacta intre -3 si 3 este :" ,pi/6 )
}

b3b(2000,-3,3)

b3c=function(n,a)
{
  sum=0;
  for(i in 1:n)
  {
    u = rexp(1,a)
    sum=sum+fc(u)/(a*exp(-a*u))
  }
  aria=(a)*sum/n
  cat("Aria aproximata intre :" ,a, "si",b,":",aria,"\n")
  cat("Valoarea exacta intre 0 si +inf este :" ,1/2 )
}

b3c(2000,1)

b4=function(n)
{
  sum=0
  for(i in 1:n)
  {
    x=rexp(1,4)
    s1=rgamma(1,shape=4,scale=3)
    s2=rgamma(1,shape=4,scale=2)
    s3=rgamma(1,shape=5,scale=2)
    s4=rgamma(1,shape=5,scale=3)
    u=runif(1,0,1)
    
    if(u<=0.25)
      sum=sum+s1
    else
      if(u>0.25 & u<0.5)
        sum=sum+s2
    else
      if(u>=0.5 && u<0.8)
        sum=sum+s3
    else
      sum=sum+s4
    
    sum=sum+x;
  }
  
  return (sum/n)
}

b4(20000)

b5a=function(n,p)
{
  x=vector()
  count = 0
  for(i in 1:50)
    x[i]=0
  x[1]=1
  infected=1
  while(n>0)
  {
    for(i in 1:(p*(50-infected) ) )
    {
      y=runif(1,1,50)
      if(x[y]==0)
      {
        x[y]=1
        infected=infected+1
      }
      else
        i=i-1
    }
    infected=infected-8
    n=n-1
  }
  for(i in 1:50)
    if(x[i]==1)
      count=count+1
  return (count/50)
}

b5a(20,0.1)
b5a(20,0.2)
b5a(20,0.05)

b5b=function(n,p)
{
  x=vector()
  count = 0
  for(i in 1:50)
    x[i]=0
  x[1]=1
  infected=1
  while(n>0)
  {
    for(i in 1:(p*(50-infected) ) )
    {
      y=runif(1,1,50)
      if(x[y]==0)
      {
        x[y]=1
        infected=infected+1
      }
      else
        i=i-1
    }
    infected=infected-8
    n=n-1
  }
  for(i in 1:50)
    if(x[i]==1)
      count=count+1
  return (count/50)
}

p=0.4#asta e asa de test ca functia lucreaZa cu orice p luat la general.
b5b(7,p)


b5c=function(p,epsilon)
{
 alfa =1-p
 z=qnorm(alfa/2)
 p=b5b(7,p);
 Nmin = p*(1 - p)*(z/epsilon)^2
 return (Nmin)
}
b5c(0.99,0.01)
