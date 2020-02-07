### Programs to call ####################################################################################

fprime = function(f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

bisection = function(f,a,b,tol=0.0001){
  if(f(a)*f(b)>0){
    return("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while(abs(f(middle))>tol){
      middle = (a+b)/2
      if(f(middle)*f(a)>0) (a = middle)
      else(b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return(middle)
  }
}

library(MASS);library(NlcOptim)
solnl(X = NULL, objfun = NULL, confun = NULL, A = NULL, B = NULL,
      Aeq = NULL, Beq = NULL, lb = NULL, ub = NULL, tolX = 1e-05,
      tolFun = 1e-06, tolCon = 1e-06, maxnFun = 1e+07, maxIter = 4000)

## Outer function to produce matrix needed for plotting ##
# f is the function
# x is a list with two vectors x, y
# returns a matrix of f(x,y)
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

## Example of a simple 2D plot ###################################################################################
profit = function (x){
  return((0.65-0.01*x)*(200+5*x)-.45*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o")
print(profit(8))


## Example of how to find a root of the function ###################################################################################
f = function (x){x^3-5} #sinlge line function definition
zero=bisection (f,-10,20)
x=seq(-1,2,0.01)
plot(x,f(x),"l")
abline(h=0,col="red")
abline(v=zero,col="blue")
print(zero)

## Using fprime and bisection in the conxtext of above ###################################################################################
dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)

## Sensitivity Analysis Example ###################################################################################
p = seq(0.008,0.012,0.001)
ans = array(0,length(p))
for (i in 1:length(p)){
  profit = function (x){
    return((0.65-p[i]*x)*(200+5*x)-.45*x)
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,0,20,0.0001)
}
print(ans)
plot(p,ans,"o",xlab="p($/day)",ylab="x(Days to Sell)")
title("Sensitivity of Falling Price of Pig")

## Example of a piecewise function ###################################################################################
x = seq(-5,5,0.01)
f = function(x)(x<=-2)*1+(x>-2)*(x<3)*(x)+(x>=3)*(-x) #1,x,-x are the "left sides" with x<=2,-2<x<3, and x>=3 are the right side
f = Vectorize(f)
plot(x,f(x),type="l")

## Basic plot commands ###################################################################################
plot() #Generic plotting function
boxplot() #Create a box plot
hist() #Create a histogram
qqnorm() #Create a quantile-quantile plot
curve() #Graph a function
points() #Add points
lines() #Add lines
abline() #Add a straight line
segments() #Add line segments
polygon() #Add a closed polygon
text() #Add text

## Example of "Plot, type, xlab, ylab, xlim, ylim" ###################################################################################
x = seq(1,10,.01)
f = function(x) x^2
y=f(x)
plot(x,y,
     type="l",
     xlab="x title",
     ylab="y title",
     main = "The Title",
     xlim = c(0,5),
     ylim = c(-5,50))

## Example of Quasi-Netwon BFGS Method ###################################################################################
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
x=c(500,500) #starting point for optimization
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)
paste(c("Profits are maximized by selling:",round(ans$par[1])," 19 in TVs and ",
        round(ans$par[2])," 21 in TVs. Resulting in a profit of: $",
        format(round(-P(ans$par)),big.mark=",")),collapse="")
paste(c("Average Selling Price of 19 in set is: $",round(339-0.01*ans$par[1]-0.003*ans$par[2],2)),collapse="")
paste(c("Average Selling Price of 21 in set is: $",round(399-0.004*ans$par[1]-0.01*ans$par[2],2)),collapse="")

## Another Sensitivity Analysis ###################################################################################
P = function(a){
  f = function(x){((339-a*x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*-1
  }
  x = c(4700,7042)
  ans = optim(x,f,method="BFGS")
  ## Choose which of these that we will return ##
  print(ans$par)
  print(ans$value)
}
P(0.01)

# Produce a data frame for the sensitivity analysis
P = function(a){
  f = function(x){((339-a*x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*-1
  }
  x = c(4700,7042)
  ans = optim(x,f,method="BFGS")
  ## Choose which of these that we will return ##
  #print(ans$par)
  #print(ans$value)
  return(ans)
}
a = seq(0.002,0.02,0.001)
ans.x1=0
ans.x2=0
ans.profit=0
for(i in 1:length(a)){
  ans = P(a[i])
  ans.x1[i]=ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.profit[i] = -ans$value
}
result = data.frame(a=a,x1=ans.x1,x2=ans.x2,profit=format(ans.profit,big.mark=","))
print(result)
plot(a,ans.x1,xlim=c(0,0.02),ylim=c(0,10000),type="l",ylab="optimum x1/x2",main="Figure 2.5/2.6 Book",
     xaxs="i", yaxs="i",col="blue")
lines(a,ans.x2,col="red")
legend("topleft",legend=c("x1","x2"),lty=1,col=c("blue","red"))
grid(lwd=2, nx=4, ny=10)

## Example of 3D plots #################################################################################
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

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
X = list(x=seq(3000,6000,100),y=seq(6000,8000,100))
Z = Outer(P,X)
contour(x=X$x,y=X$y,z=-Z)
persp(x=X$x,y=X$y,Z,theta=40,ticktype = "detailed",shade=0.01)


## Example of 'greater than' constraints' #################################################################################
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5000,8000,10000,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)


# Graph feasible region
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
## This feasible region is an easy one to draw ###
contour(x=X$x,y=X$y,z=-Z,lwd=3)
abline(h=8000,col="red",lwd=3) #line at y=8000
abline(v=5000,col="red",lwd=3) #line at x=5000
abline(h=0,col="red",lwd=3) #line at y=0
abline(v=0,col="red",lwd=3) #line at x=0
abline(a=10000,b=-1,col="red",lwd=3) #line with y=bx+a; b is the slope, a is the y-intercept
pts = list(x=c(0,0,2000,5000,5000),y=c(0,8000,8000,5000,0)) #we need the intersection points for shading
polygon(pts$x,pts$y,density = 20,col="red") #this shades the feasible region red

## Example 2.3 - Meerschart ##
obj=function(x){
  return(-(x[1]+2*x[2]+3*x[3]))
}
#constraint function#
con=function(x){
  f=NULL
  f=rbind(f,x[1]^2+x[2]^2+x[3]^2-3)
  return(list(ceq=f,c=NULL))
}
x0=c(1,1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans)

## Example 2.4 - Meerschaert ####
obj=function(x){
  return(-(x[1]+2*x[2]+3*x[3]))
}
#constraint function
con=function(x){
  f=NULL
  f=rbind(f,x[1]^2+x[2]^2+x[3]^2-3)
  f=rbind(f,x[1]-1)
  return(list(ceq=f,c=NULL))
}
x0=c(1,1,1)
ans=solnl(x0,objfun=obj,confun=con)
print(ans$par)
print(-ans$fn)
print(ans$lambda)

## Single variable optimization #################################################################################
options(warn=-1)
f = function(x){((0.65−0.01*x)*(200+5*x)−0.45*x)*(-1)}
x=1 #starting value
ans=optim(x,f)
print(ans)

## Multi variable unconstrained optimization #################################################################################
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
x=c(500,500) #starting point for optimization
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)

## Multi variable constrained optimization / nonlinear equality constraint #################################################################################
obj = function(x){((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1)}

library(ma391laporte)
X = list(x=seq(100,500),y=seq(100,500))
Z = Outer(obj,X)
## contour plot ##
contour(X$x,X$y,-Z)

## Parametric Equations for the constraint ##
r = function(t){c(200*sin(t)+300,200*cos(t)+300)}
r = Vectorize(r)

## Get the points for the function and plot the points on the graph
t = seq(0,2*pi,0.01)
Xt = list(x=r(t)[1,],y=r(t)[2,])
points(Xt$x,Xt$y,type="l",col="red",lwd=3)

library(MASS);library(NlcOptim)
x0 = c(300,500)
## Single nonlinear constraint ##
con = function (x){
  f = NULL
  f = rbind(f,(x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(ceq=f,c=NULL))
}
ans=solnl(x0,objfun=obj,confun=con)
print(ans)


contour(X$x,X$y,-Z)
points(Xt$x,Xt$y,type="l",col="red",lwd=3)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)



## Multi variable constrained optimization / nonlinear equality constraint (2) #################################################################################

obj = function(x){((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1)}
X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(obj,X)
## contour plot ##
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3) # see the notes from class on the abline

Aeq = matrix(c(3,-5,2,7,2,12,2,-1),nrow=2,byrow=TRUE)
print(Aeq)
Beq = matrix(c(300,200))
print(Beq)

## Replot the contour plot so that we can put the answer on it.
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3)

## Aeq and Beq are the Aeq*x = Beq ##
Aeq = matrix(c(12,5),nrow=1)
Beq = matrix(5000)
x0=c(0,500)
ans=solnl(x0,objfun = obj,Aeq=Aeq,Beq=Beq)
print(ans)
## Put the answer on the contour plot as a point in green
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)


A = matrix(c(12,5,-1,0,0,-1),nrow=3,byrow=T)
B = matrix(c(5000,0,0),nrow=3)
print(A)
print(B)


obj = function(x){((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*-1}
X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(obj,X)
contour(X$x,X$y,Z)
abline(a=1000,b=-12/5,col="red",lwd=3)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
A = matrix(c(12,5,-1,0,0,-1),nrow=3,byrow=T)
B = matrix(c(5000,0,0),nrow=3)
ans = solnl(x0,objfun = obj,A=A,B=B)
print(ans)
points(ans$par[1],ans$par[2],pch=21,bg="yellow",cex=2)


## Sensitivity Analysis and Shadow Price #################################################################################

## a = 0.01 ##
options(warn=-1)
library(MASS);library(NlcOptim);library(ma391laporte)
## Example 2.2 - Meerschaert ##
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5000,8000,10000,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)

## Change a and see what it does to the objective function ###
f = function(a){
  P = function(x){
    return(((339-a*x[1]-0.003*x[2])*x[1]
            +(399-0.004*x[1]-0.01*x[2])*x[2]
            -(400000+195*x[1]+225*x[2]))*-1)
  }
  x0=c(1000,1000)
  A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
  B = matrix(c(5000,8000,10000,0,0),nrow=5)
  ans = solnl(x0,objfun=P,A=A,B=B)
  return(ans)
}
ans.x1=0
ans.x2=0
ans.profit=0
sa = seq(0.001,0.025,0.001)
for (i in 1:length(sa)){
  res = f(sa[i])
  ans.x1[i]=res$par[1]
  ans.x2[i]=res$par[2]
  ans.profit[i]= -res$fn
}
result = data.frame(a = sa,x1=ans.x1,x2=ans.x2,profit = ans.profit)
print(result)

## Perturbed problem to denote what happens when production capacity changes.
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(5000,8000,10001,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)

############################################
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
###Linear Inequality Constraints##
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) #defining this matrix is not intuitive - make sure we talk in class
B = matrix(c(3000,8000,10000,0,0),nrow=5)
solnl(x0,objfun=P,A=A,B=B)
