###############################
# Simulations of random variables
###############################


# ���� 1: Linear congruental generator (LCG)
# ���������� ������-��������� ������������������. 
# �������� �������� ����������. ������ ������� �� ����������� a, c, m 
# � seed.


lcg <- function(a,c,m,run_length,seed) {
  x <- rep(0,run_length)
  x[1] <- seed
  for (i in 1:(run_length-1)) {
    x[i+1] <- (a * x[i] + c) %% m
  }
  U <- x/m 
  return(list(x=x,U=U))
}

z <- lcg(6,7,23,20,15)
z
# ����� ������ = 11
hist(z$x,col="red")
hist(z$U,col="purple")


z <- lcg(16,7,225,1000,2)
z
# ����� ������ = 225 (�� ������� Hull&Dobell'1962)
hist(z$x,col="red")
hist(z$U,col="purple")

install.packages("randtests")
library(randtests)

z1 <- lcg(16,7,225,1000,2) # "Bad" LCG 

plot( z1$U[1:(length(z1$U)-1)], z1$U[2:length(z1$U)] , main="Bad")

ks.test(z1$U, "punif") # �������� ����������-����������������

#(�������� ������������� � �������������������������)
runs.test(z1$U) # �������� ������-���������� = �������� ����� 

cox.stuart.test(z1$U) # �������� �����-������� ��� ����������� 
# ������ � ������������������


#### ������� LCG

z2 <- lcg(1093,18257,86436,2000,12) # "Good" LCG
# z2 <- lcg(16,7,225,1000,7) # "Good" LCG
plot( z2$U[1:(length(z2$U)-1)], z2$U[2:length(z2$U)] , main="Good")

ks.test(z2$U, "punif")
runs.test(z2$U)  # �������� ������-���������� = �������� ����� 
cox.stuart.test(z2$U) # Cox Stuart Trend Test 
#(��� ����������� ������ � ������������������)
# ������� �������� ����������� (�.�. ���� ������������� � �������������)






# ���� 2 :������������� ������������ ������������� �������� runif


#RNGkind("Wich")
#RNGkind("Super")


Nsim=1000 #number of random numbers
set.seed(100)
x=runif(Nsim) 
# ����� set.seed, ������ ��������� ������������������
hist(x,main="Uniform distribution")

# ���������� ����
x1=x[-Nsim] #vectors to plot
x2=x[-1] #adjacent pairs
#par(mfrow=c(1,3))

plot(x1,x2)


#���� 3: ������������� ����������������� �������������

set.seed(20)


Nsim=10^4 #number of random variables
U=runif(Nsim)
X=-1/3*log(U) #transforms of uniforms
Y=rexp(Nsim, 3) #exponentials from R
par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Exp from Uniform")
hist(Y,freq=F,main="Exp from R")


#���� 4: ������������� �������������� �������������
Nsim=10^4; lambda=3
t=seq(0, 100,1)
prob=ppois(t, lambda)

X=rep(0,Nsim)
for (i in 1:Nsim){
  u=runif(1)
  X[i]=sum(prob<u) 
  X[i]
}
par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Poisson from Uniform")
Y=rpois(Nsim, lambda) #Poisson from R
hist(Y, main="Poisson from R")


Nsim=10^4; lambda=100
spread=3*sqrt(lambda)
t=round(seq(max(0,lambda-spread),lambda+spread,1))
prob=ppois(t, lambda)
X=rep(0,Nsim)
for (i in 1:Nsim){
  u=runif(1)
  X[i]=t[1]+sum(prob<u)}
Y=rpois(Nsim, lambda) #exponentials from R

par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Poisson from Uniform")
hist(Y,freq=F,main="Poisson from R")






# ���� 5: ����� ������� � ����������� ��� ������������� ���� �������������
alpha<-1
beta<-4
xmode<-(alpha-1)/(alpha+beta-2)
dmax<-xmode^(alpha-1)*(1-xmode)^(beta-1)*
  gamma(alpha+beta)/(gamma(alpha)*gamma(beta))
y<-runif(1000)
x<-na.omit(ifelse(runif(1000)<=dbeta(y,alpha,beta)/dmax, y, NA))

b<-rbeta(500,alpha,beta)

par(mfrow=c(1,2)) #plots
hist(x,freq=F,main="Beta with Accept/Reject")
hist(b,freq=F,main="Beta from R")

# x <- c(6:-4)
# sqrt(x)  #- gives warning
# sqrt(ifelse(x >= 0, x, NA))  # no warning





