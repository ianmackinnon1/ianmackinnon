### Maximize f (same as hand work in note)

f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*-1} ## call x "x[1]" and y "x[2]" so that it creates an array instead of two variables
x = c(0,0) ## Starting point for iteration to use
ans = optim(x,f) ## defaults to minimization solving methods, so multiply function by -1 to make it a maximization function. YYou can multiply final value by -1 to get it back to right answer
print(ans$par)
print(ans$val)

### Chapter 2 Question 3 ###
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])} ## Harvest rate of blue whales
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])} ## Harvest rate of fin whales

Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) + 6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)} ## Revenue function we want to optimize

x=c(50000,50000) # starting point for iteration
ans = optim(x,Rev,method="L-BFGS-B") ## Note the -1 in the function equation used to turn it into a maximization answer
Blue(ans$par) # optimal harvest rate of blue whales
Fin(ans$par) # optimal harvest rate of fin whales

# Sensitivity Analysis of r2#
R2 = function(r2){
  fr2=function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) + 6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)} #change given r2 value of 0.08 into a generic r2 variable
  ans = optim(x,Rev,method="L-BFGS-B")
  return(ans)
}

R2(0.08) #use this to check and make sure it returns the original value to verify that this works. It does.
r2 = seq(0.06,0.1,0.01) #now that we know it works, input a range to test the sensitivity
ans.x1=0
ans.x2=0
ans.rev=0
for (i in 1:length(r)){
  ans=R2(r2[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i]=ans$par[2]
  ans.rev[i]=-ans$value
}

result = data.frame(growth_rate=r2,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)


##############################

f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*(-1)}
x = c(0,0)
ans = optim(x,f)
print(ans$val)
#### Number 3 ##
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     6*(0.08*x[2]*(1-x[2]/
                                     400000)-1/100000000*x[1]*x[2]))*(-1)}
x = c(50000,50000)
ans=optim(x,Rev,method="L-BFGS-B")
Blue(ans$par)
Fin(ans$par)
R2 = function(r2){
  fr2=function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans=optim(x,Rev,method="L-BFGS-B")
  return(ans)
}
R2(0.08)
r = seq(0.06,.1,.01)
ans.x1=0
ans.x2=0
ans.rev=0
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i]= ans$par[2]
  ans.rev[i]=-ans$value
}
print(ans.x1)
result = data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,rev=ans.rev)
print(result)
