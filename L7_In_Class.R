Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

X = list(x=seq(0,6000,100),y=seq(0,9000,100))
Z = Outer(P,X)

### In class notes ###

obj = function(x){(10*x[1]^(0.6)*x[2]^(0.4))*-1}

# Countour plot of worksheet #3 

X = list(x=seq(0,20,.1),y=seq(0,20,.1))
Z = Outer(obj,x)
contour(x=X$x,y=Y$y,z=-z,lwd=2)
abline(a=10,b=-5/3,col="red",lwd=2)


contour(x=X$x,y=Y$y,z=-z,lwd=2,
        levels=c(20,37.55,60,80,100,120,140,160,180))
abline(a=10,b=-5/3,col="red",lwd=2)

library(MASS);library(NlcOptim) #Need to download this
x0=c(3,5)
Aeq = matrix(c(50,30),nrow=1)
Beq = matrix(300)
ans=solnl(x0,obj,Aeq=Aeq,Beq=Beq)
print(ans) #you will  know x, y, and lambda from this

# part 1A of worksheet

obj = function(x){(1/2*(x[2]-32)*x[1]^2)*(-1)}
con = function(x){
  f = NULL #clears memory of any previous functions defined as f  
  f = rbind(f,x[2]^2*x[1]-10000) #subtracting 10000 sets  it equal to zero
  return(list(ceq=f,c=NULL))
}
x0 = c(1,100)
solnl(x0,objfun=obj,confun=con)
