a1=function(lambda,p,k,n)
{
  po=dpois(0:n,lambda)
  geo=dgeom(0:n,p)
  binom=dbinom(0:n,n+1,p);
  par(mfrow=c(4,1))
  hist(po[k:n+k])
  hist(geo[k:n+k])
  hist(binom[k:n+k])
}

a2a=function(x)
{
  y=vector()
  y[1]=median(x)
  y[2]=mean(x)
  y[3]=sd(x)
  y[4]=as.vector(quantile(x))[1+1]
  y[5]=as.vector(quantile(x))[1+2]
  y[6]=as.vector(quantile(x))[1+3]
  return (y)
}

a2b=function(x)
{
  n=length(x)
  medie=sum(x)/n
  s=sd(x)
  left=medie-s*2
  right=medie+s*2
  y=vector()
  l=0
  for(i in 1:n) 
    if(x[i]-left>0 || x[i]-right<0) 
    {
      l=l+1
      y[l]=x[i]
    }
  return (y)
}

a2c=function(y) 
{
  x=a2b(y)
  interval=seq(0,80,10)
  interval[1]=0
  hist(x,breaks=interval,freq=T,right=T)
}

temaA=function()
{
  a1(5,0.25,10,100)
  x=scan("input.txt")
  
  cat(a2a(x))

  cat(a2b(x))
  
  a2c(x)
}
temaA()

