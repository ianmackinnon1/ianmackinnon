### Chapter 1 Question 4 ###

## Define the bisection and fprime command - eventually save to a library

fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}

## Part A
## Define the function using boolean statements such as >, <=, == 
r = 5/7
x = seq(0,50,1)
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
fT = Vectorize(fT)
plot(x,fT(x),type="o")
optimize(fT, interval=c(0,50),maximum=FALSE) ## returns 11.28 and $508k which is correct


##In class - "better" ans then optimization b/c you can't have a decimal down payment

dfT = function(x){fprime(fT,x)}=
  bisection(dfT,0,20) ##should return 11.28

cost = fT(x)
ans = data.frame(crews=x,cost=cost)
which(ans$cost==min(ans$cost)) ##outputs number of index
ans[which(ans$cost==min(ans$cost)),]

## In class - sensitivity analysis on size and duration of the fine
curr.fine=10000;curr.crews=11.28;curr.cost=508333.3
ans.crew=0;ans.cost=0;ans.days=0
r=5/7
finevalues=seq(5000,15000,1000)
for(i in 1:length(finevalues)){
  fine=finevalues[i]
  fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(fine*(200/(r*(x+1))-14))}
  dT = function(x){fprime(fT,x)}
  ans.crew[i] = bisection(dT,0,50)
  ans.cost[i] = fT(ans.crew[i])
  ans.days[i] = 200/(r*(ans.crew[i]+1))
}
result = data.frame(fine = finevalues,crew=ans.crew,cost=ans.cost,days=ans.days)
print(result) ## shows in a table that days it takes to clean decreases as size of fine increases

## Can copy and paste the sensitivity analysis above but change "14" instead of "10000" to see how the duration of the fine changes the time it takes to clean up


### Chapter 1 Question 5 ###

## Part A
## Define Variables
r = 0.08
K = 400000

## Enter Function with range from current to max population
effort = function (x){
  return((r*x*(1-x/K))/0.00001*x)
}
x = seq(70000,400000,10000)
plot(x,effort(x),type="o")

## Stabilization rate is at local max because derivative is zero
optimize(effort, interval=c(70000,400000),maximum=TRUE)

## Part B (Sensitivity to intrinsic growth rate)
## Define range for sensative variable
r = seq(0.05,0.11,.01)
ans = array(0,length(r))
for(i in 1:length(r)){
  effort = function(x){
    return((r*x*(1-x/K))/0.00001*x)
  }
  dEffort = function(x){fprime(effort,x,)}
  ans[i] = bisection(dEffort,70000,400000)
}
print(ans)
plot(r,ans,"o",xlab="r",ylab="Effort (E)")
title("Sensitivity of Intrinsic Growth Rate")

## Part C (sensitivity to maximum sustainable population)
K = seq(200000,600000,10000)
ans = array(0,length(K))
for(i in 1:length(K)){
  effort = function(x){
    return((r*x*(1-x/K))/0.00001*x)
  }
  dEffort = function(x){fprime(effort,x,)}
  ans[i] = bisection(dEffort,70000,400000)
}
print(ans)
plot(r,ans,"o",xlab="r",ylab="Effort (E)")
title("Sensitivity of maximum population")

### Chapter 1 Question 6 ###

## Part A
K=400000
r=0.08
x = function(E){return((0.00001*E*K-r) / -r)}
profit = function (E){
  return(6000*(0.00001*E*x(E))-(500*E))
}
E = seq(0,10000000,10000)
x(E) = seq(70000,400000,10000) ##Don't know where to set this parameter (current and max population)
plot(E,profit(E),type="l")

optimize(profit, interval=c(0,10000000),maximum=TRUE)

