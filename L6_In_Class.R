Units = function(x){c((600-3*x[1]+x[2]),(800-2*x[2]+x[1]))}

Profit = function(x){((x[1]*(600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1])-(200*(600-3*x[1]+x[2])+300*(800-2*x[2]+x[1]))))*-1}

x=c(100,100)
ans = optim(x,Profit)
print(ans)

P = function(c1){
  fone=function(x){((x[1]*(600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1])-(c1*(600-3*x[1]+x[2])+300*(800-2*x[2]+x[1]))))*-1}

  x=c(100,100)
  ans = optim(x,Profit)
  return(ans)
}


cost1 = seq(200,600,20)

#########
Units = function(p){c((600-3*p[1]+p[2]),(800-2*p[2]+p[1]))}
P = function(p){(p[1]*Units(p)[1]+p[2]*Units(p)[2]-
                   (200*Units(p)[1]+300*Units(p)[2]))*(-1)}
x = c(100,100)
Units(c(100,100))
ans=optim(x,P)
Units(ans$par)
cost1 = seq(200,600,20)
Profit = function(c1){
  print(c1)
  Units = function(p){c((600-3*p[1]+p[2]),(800-2*p[2]+p[1]))}
  P = function(p){(p[1]*Units(p)[1]+p[2]*Units(p)[2]-
                     (c1*Units(p)[1]+300*Units(p)[2]))*(-1)}
  x=c(100,100)
  ans = optim(x,P)
  return(ans)
}
Profit(205)
ans.profit = 0
ans.x1=0
ans.x2=0
for(i in 1:length(cost1)){
  ans = Profit(cost1[i])
  ans.profit[i] = -ans$value
  ans.x1[i] = Units(ans$par)[1]
  ans.x2[i] = Units(ans$par)[2]
}
result = data.frame(cost1=cost1,profit=ans.profit,x1=ans.x1,x2=ans.x2)
print(result)
# There is an issue around cost of product1 ~ 300 (x1 turns negative)
