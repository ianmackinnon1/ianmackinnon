

library(MASS);library(NlcOptim)
solnl(X = NULL, objfun = NULL, confun = NULL, A = NULL, B = NULL,
      Aeq = NULL, Beq = NULL, lb = NULL, ub = NULL, tolX = 1e-05,
      tolFun = 1e-06, tolCon = 1e-06, maxnFun = 1e+07, maxIter = 4000)

### Chapter 2, Question 6 ###
f = function(x){((x*(950-dis)*(dis/100 * 1.5)*(ads/10000 *200))-((700*x)+(50000+ads)))*-1}
ads = c(50000,100000,10000)
dis = c(0,250,10)


### Chapter 2, Question 8 ###

# Part A (Optimize prices)
f = function(x){(((80000-((x[1]-1.5)/(0.1/5000))+((x[2]-250)/(100/1000)))*x[1])+((350-((x[2]-250)/(100/50)))*x[2]))*-1}#multiply by -1 to turn into maximizing function
x= c(1.5,250) #iteration starting point
ans=optim(x,f)
print(ans$par)
print(ans$value*-1)
