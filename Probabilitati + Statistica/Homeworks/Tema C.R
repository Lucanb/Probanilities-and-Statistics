
tree_eval = function(i, leaves) 
{
  a = runif(1, 0, 1) 
  len = length(leaves)
  if(log(i,3) >= log(len,3) - 1) 
  {
    if(a <= 1/3) 
    {
      if(leaves[3*i - len + 1] == 0)
        return(leaves[3*i +1 -len + 1])
      return(1)
    }
    else {
      if(leaves[3*i + 1 -len + 1] == 0)
        return(leaves[3*i -len + 1])
      return(1)
      else
        if(leaves[3*i + 2 -len + 1] == 0)
          return(leaves[3*i+1 -len + 1])
    }
  }
  if((floor(log(i,3))%% 2 == 0)) 
  { 
    if(a <= 1/3) 
    {
      if(tree_eval (3*i, leaves) == 1)
        return(tree_eval (3*i + 1, leaves))
      return(0)
    }
    else 
    {
      if(tree_eval (3*i +1, leaves) == 1)
        return(tree_eval (3*i, leaves))
      else
      {
        if(tree_eval (3*i +2, leaves) == 1)
        return(tree_eval (3*i+1, leaves))
        return (0)
      }
      return(0)
    }
  }
  if((floor(log(i,3))%% 2 == 1)) 
  { 
    if(a <= 1/3) 
    {
      if(tree_eval (3*i, leaves) == 0)
        return(tree_eval (3*i + 1, leaves))
      return(1)
    }
    else 
    {
      if(tree_eval (3*i +1, leaves) == 0)
        return(tree_eval (3*i, leaves))
      else
      {
        if(tree_eval (3*i +2, leaves) == 0)
          return(tree_eval (3*i+1, leaves))
        return(0)
      }
      return(1)
    }
  }
  if((floor(log(i,3))%% 2 == 2))
  {
    if(a <= 1/3) 
    {
      if(tree_eval(3*i, leaves) == 0)
        return(tree_eval(3*i + 1, leaves))
      return(0)
    }
    else 
    {
      if(tree_eval(3*i +1, leaves) == 0)
        return(tree_eval (3*i, leaves))
      else
      {
        if(tree_eval(3*i +2, leaves) == 0)
          return(tree_eval (3*i+1, leaves))
        return(0)
      }
      return(0)
    }
  }
}

c1=function(leaves)
{
  return(tree_eval(1, leaves))
}

a=c(1,0,1,0,1,1,0,1,1)
c1(a)

#first=1 -> barbatii propun primii, first=2 -> femeile
c2 <- function(m, n, preference.row, preference.col, first)
{
  min.n <- function(x,n,value=TRUE){
    s <- sort(x, index.return=TRUE)
    if(value==TRUE){s$x[n]} else{s$ix[n]}}
  max.n <- function(x,n,value=TRUE){
    s <- sort(x, decreasing=TRUE, index.return=TRUE)
    if(value==TRUE){s$x[n]} else{s$ix[n]}}
  s <- NULL
  test_s <-NULL
  loop <- 2
  step.1 <- matrix(0,ncol=n, nrow=m)
  step.2 <- matrix(0,ncol=n, nrow=m)
  store <- NULL
  r <- NULL
  if (first==1)
  {
    step.1 <- matrix(0,ncol=n, nrow=m)
    for (i in 1:n)
    {
      step.1[i,][preference.row[i,]==min.n(preference.row[i,],n=1)] <- 1
    }
    for (i in 1:n){s[i] <- sum(step.1[,i])}
    test_s <- s>1
    while (any(test_s==TRUE)==TRUE)
    {
      if (any(test_s==TRUE)==TRUE) {
        position1 <- which(s>1)
        position2 <- vector('list')
        position3 <- vector('list')
        position4 <- NULL
        position5 <- 1:m
        for (k in 1:length(position1)){position2[[k]] <- which(step.1[,position1[k]]==1)
        position3[[k]] <- which(preference.col[,position1[k]]>min(preference.col[position2[[k]],position1[k]]))
        x <- which(position3[[k]]%in%position2[[k]])
        position3[[k]] <- position3[[k]][x]
        step.1[position3[[k]],position1[k]] <- 0}
        for (t in 1:n){position4[t] <-
          if(sum(step.1[,t])==0){0}else{which(step.1[,t]==1)}}
        position4 <- position4[position4 >0]
        position5 <- position5[-position4]
        store <- append(position5, store)
        r <- rle(sort(store))
        for (j in
             position5){step.1[j,][preference.row[j,]==r$lengths[r$values==j]+1] <- 1}
        for (i in 1:n){s[i] <- sum(step.1[,i])}
        test_s <- s>1
      }else{
        step.1 <- matrix(0,ncol=m, nrow=n)
        for (i in 1:m){step.1[i,][preference.row[i,]==min(preference.row[i,])] <-
          1}
        return(step.1)}
      loop <- loop + 1
    }
  }
  if (first==2)
  {
    step.2 <- matrix(0,ncol=n, nrow=m)
    for (i in 1:n)
    {
      step.2[,i][preference.col[,i]==min.n(preference.col[,i],n=1)] <- 1
    }
    for (i in 1:n){s[i] <- sum(step.2[i,])}
    test_s <- s>1
    while (any(test_s==TRUE)==TRUE)
    {
      if (any(test_s==TRUE)==TRUE) {
        position1 <- which(s>1)
        position2 <- vector('list')
        position3 <- vector('list')
        position4 <- NULL
        position5 <- 1:m
        for (k in 1:length(position1)){position2[[k]] <-
          which(step.2[position1[k],]==1)
        position3[[k]] <-
          which(preference.row[position1[k],]>min(preference.row[position1[k],position2[[k]]]))
        x <- which(position3[[k]]%in%position2[[k]])
        position3[[k]] <- position3[[k]][x]
        step.2[position1[k],position3[[k]]] <- 0}
        for (t in 1:n){position4[t] <-
          if(sum(step.2[t,])==0){0}else{which(step.2[t,]==1)}}
        position4 <- position4[position4 >0]
        position5 <- position5[-position4]
        store <- append(position5, store)
        r <- rle(store)
        for (j in position5){step.2[,j][preference.col[,j]==r$lengths[r$values==j]+1] <- 1}
        for (i in 1:n){s[i] <- sum(step.2[i,])}
        test_s <- s>1
      }
      else
      {
        step.2 <- matrix(0,ncol=m, nrow=n)
        for (i in 1:m){step.2[i,][preference.col[,i]==min(preference.col[,i])] <-1
        }
        step.2
      }
      loop <- loop + 1
    } 
  }
  if (first==1) 
    print(step.1)
  if (first==2) 
    print(step.2)
}
m1 <- c(1,2,3); m2 <- c(3,1,2); m3 <- c(2,3,1)
n1 <- c(3,1,2) ;n2 <- c(2,3,1); n3 <- c(1,2,3)
preference.row <- matrix(c(m1, m2, m3), ncol=3, byrow=TRUE)
preference.col <- matrix(c(n1, n2, n3), ncol=3)
c2(m = 3, n = 3, preference.row = preference.row, preference.col = preference.col, first=1)
c2(m = 3, n = 3, preference.row = preference.row, preference.col = preference.col, first=2)



prime=function(x)
{
  if(x<2 || (x>2 && x%%2==0))
    return (0)
  d=3
  while(d<=x/2)
  {
    if(x%%d==0)
      return (0)
    d=d+2
  }
  return(1)
}
c3=function(U,g,n,m)
{
  k=nn
  p=runif(1,1,k)
  while(prime(p)==0)
    p=runif(1,1,k)
  numar=0
  for(i in 1:n)
    numar=numar+g[i](2^(i-1))
  numar=numar%%p
  v=c(0,0,0,0,0,0)
  for(i in 1:m)
  {
    v[i]=0
    for(j in 1:n)
      v[i]=v[i]+U[i,j]*(2^(j-1))
    v[i]=v[i]%%p
  }
  ok=0
  for(i in 1:m)
    if (numar==v[i])
      ok=1
  if(ok==1)
    return (TRUE)
  else
    return (FALSE)
}
x1=c(1,0,0)
x2=c(0,1,1)
x3=c(0,0,1)
U= matrix(c(x1, x2, x3), ncol=3,nrow=3, byrow=TRUE)
u1=c(0,1,0)
u2=c(1,1,0)
n=3
m=3
c3(U,u1,n,m)
c3(U,u2,n,m)