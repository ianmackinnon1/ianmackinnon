install.packages("MASS")
install.packages("NlcOptim")

### Worksheet 1B ###

library(MASS);library(NlcOptim)

obj=function(x){

  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+1/2*(x[3]-32)*x[1]^2)*(-1)) # Problem 1b

}

x = c(5.49,1.33,42.67) #solution for part a
print(obj(x))

# Constraints
con=function(x){
  f=NULL
  f=rbind(f,x[3]^2*x[1]-10000)
  f=rbind(f,(-32*x[2]+(x[3]-32)*x[1]))
  return(list(cew=f,c=NULL))
}

solnl(x,objfun=obj,confun=con)


### Board Problem ###

library(MASS);library(NlcOptim)

obj=function(x){((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1)} # Problem 1b

x0=c(1,1)
optim(x,obj)
X = list(x=seq(100,500),y=seq(100,500))
Z = Outer(obj,X)
contour(x=X$x,y=X$y,z=-Z)


# Constraints
con=function(x){
  f=NULL
  f=rbind(f,(x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(cew=f,c=NULL))
}

solnl(x,objfun=obj,confun=con)

###############

library(MASS);library(NlcOptim)
obj=function(x){
  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+
            1/2*(x[3]-32)*x[1]^2)*(-1)) # Problem 1b
}
x = c(5.49,1.33,42.67) #solution for part a
print(obj(x))
con = function(x){
  f = NULL
  f = rbind(f,x[3]^2*x[1]-10000)
  f = rbind(f,-32*x[2]+(x[3]-32)*x[1])
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun = con)
### Problem #1 ###
obj = function(x){((600-3*x[1]+x[2])*x[1]+
                     (800-2*x[2]+x[1])*x[2])*-1}
x0=c(1,1)
optim(x0,obj)
X = list(x=seq(100,500),y=seq(100,500))
Z = Outer(obj,X)
contour(x=X$x,y=X$y,z=-Z)
help(Outer)










