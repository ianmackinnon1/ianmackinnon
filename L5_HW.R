### Example from course notebook

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


### Chapter 2 Question 1 (answer is always -5 and -2 from start point / doesn't iterate properly)
P=function(x){
  return((0.05*x[1]*(1-x[1]/150000)-0.00000001*x[1]*x[2])+(0.08*x[2]*(1-x[2]/400000)-0.00000001*x[1]*x[2]))
}
x=c(700,700)
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)
paste(c("New whales born each year are maximized with: ",round(ans$par[1])," blue whales ",
        round(ans$par[2])," fin whales. Resulting in a total of ",
        format(round(-P(ans$par)),big.mark=",")),collapse="")

### Chapter 2 Question 3 (Need to add non negativity constraint)
P=function(x){
  return((12000*(0.05*x[1]*(1-x[1]/150000)-0.00000001*x[1]*x[2]))+(6000*(0.08*x[2]*(1-x[2]/400000)-0.00000001*x[1]*x[2]))
}
x=c(700,700)
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)
paste(c("New whales born each year are maximized with: ",round(ans$par[1])," blue whales ",
        round(ans$par[2])," fin whales. Resulting in a total of $ ",
        format(round(-P(ans$par)),big.mark=",")),collapse="")