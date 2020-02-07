
###  Setting up example 2.1 ###
f = function(x){((((339-0.01*x[1]-0.003*x[2])*x[1]) + ((399-0.004*x[1]-0.01*x[2])*x[2]) - (400000+195*x[1]+225*x[2]))*-1)} #multiply by -1 to turn into maximizing function
x= c(1000,1000) #iteration starting point
ans1=optim(x,f)
print(ans1$par)
original = print(ans1$value*-1) #-1 turns final value into a positive

###  Chapter 2 Question 5 ###
# Part A
ftar = function(x){((339-0.01*x[1]-0.003*x[2])*x[1] + (339-0.004*x[1]-0.01*x[2])*x[2] - (400000+195*x[1]+225*x[2]+25*x[1]+25*x[2]))*(-1)} #add $25 to cost (tariff) for each kind of TV
x=c(1000,1000) #iteration starting point
ans=optim(x,ftar)
print(ans$par)
with_tariff = print(ans$value*-1) #-1 turns final value into a positive

tariff_cost = original - with_tariff #how much the company loses after the tariff
print(tariff_cost)
gov_nets = sum(ans$par * c(25,25)) #how much of the lose goes to the government
print(gov_nets)
lost_sales = tariff_cost-gov_nets #how much of the lose is from lost sales
print(lost_sales)

# Part B (Assumed domestic plant profit0)
f2 = function(x){(200000+(339-0.01*x[1]-0.003*x[2])*x[1] + (399-0.004*x[1]-0.01*x[2])*x[2] - (550000+400000+195*x[1]+225*x[2]))*-1} #$200k profit and $550000 cost
x= c(1000,1000) #iteration starting point
ans2=optim(x,f2)
print(ans2$par)
domestic_profit = print(ans2$value*-1) #-1 turns final value into a positive
print(domestic_profit) #its worth it if this number is greater than "with_tariff"

# Part C (minimum tariff needed to get company to US)
domestic = function(x){(200000+(339-0.01*x[1]-0.003*x[2])*x[1] + (399-0.004*x[1]-0.01*x[2])*x[2] - (550000+400000+195*x[1]+225*x[2]))*-1}

f3 = function(tar){
  fdom=function(x){((339-0.01*x[1]-0.003*x[2])*x[1] + (339-0.004*x[1]-0.01*x[2])*x[2] - (400000+195*x[1]+225*x[2]+tar*x[1]+tar*x[2]))*(-1)}
  ans = optim(x,fdom,method="L-BFGS-B")
  return(ans)
}

tar = seq(0,10,1)
ans.x1=0
ans.x2=0
ans.domestic=0
for (i in 1:length(tar)){
  ans = f3(tar[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.domestic[i]=-ans$value
}

result = data.frame(tarriff=tar,x1=ans.x1,x2=ans.x2,domestic=ans.domestic)
print(result)

####################################################

### Chapter 2 Question 7 ###

# Part A (Optimize prices)
f = function(x){(((80000-((x[1]-1.5)/(0.1/5000))+((x[2]-250)/(100/1000)))*x[1])+((350-((x[2]-250)/(100/50)))*x[2]))*-1}#multiply by -1 to turn into maximizing function
x= c(1.5,250) #iteration starting point
ans=optim(x,f)
print(ans$par)
print(ans$value*-1)

# Part B (sensitivity of 5,000 subs lost per 10 cents)
profit = function(x){(((80000-((x[1]-1.5)/(0.1/5000))+((x[2]-250)/(100/1000)))*x[1])+((350-((x[2]-250)/(100/50)))*x[2]))*-1}

F2 = function(subs){
  fsubs=function(x){(((80000-((x[1]-1.5)/(0.1/subs))+((x[2]-250)/(100/1000)))*x[1])+((350-((x[2]-250)/(100/50)))*x[2]))*-1}
  ans=optim(x,fsubs,method="BFGS")
  return(ans)
}

subs = seq(2000,8000,1000)
ans.x1=0
ans.x2=0
ans.profit=0
for(i in 1:length(subs)){
  ans=F2(subs[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.profit[i]=-ans$value
}

result = data.frame(subscribers=subs,x1=ans.x1,x2=ans.x2,profit=ans.profit)
print(result)

# Part D (Direct Mail Option)
# Not sure what it's asking but I assume it just raises the price start point from $250 to $500...
f = function(x){(((80000-((x[1]-1.5)/(0.1/5000))+((x[2]-500)/(100/1000)))*x[1])+((350-((x[2]-500)/(100/50)))*x[2]))*-1}#multiply by -1 to turn into maximizing function
x= c(1.5,250) #iteration starting point
ans=optim(x,f)
print(ans$par)
print(ans$value*-1)
