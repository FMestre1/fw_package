##############################################################################
# Plotting the degree distribution.
#-----------------------------------------------------------------------------
# Assign the matrix to be analized
#-----------------------------------------------------------------------------
mat<-as.matrix(nch_lab)
#-----------------------------------------------------------------------------
# Degree
#-----------------------------------------------------------------------------
# Animals
na<-length(t(mat)[,1])		# the number of animal species
ka<-colSums(mat)		# Vector of degree values for animals
#-----------------------------------------------------------------------------
# Plants
np<-length(mat[,1])  	# the number of plant species
kp<-rowSums(mat) 		# Vector of degree values for plants
#-----------------------------------------------------------------------------
#xx <- hist(colSums(mat),0:(na+1), right=FALSE, plot=FALSE)
xx <- hist(colSums(mat),0:(na+1), plot=FALSE)
xx <- t(xx$counts)
#yy <- hist(rowSums(mat),0:(np+1), right=FALSE, plot=FALSE)
yy <- hist(rowSums(mat),0:(np+1), plot=FALSE)
yy <- t(yy$counts)

KA <- c(0:na)
KP <- c(0:np)
X3 <- t(t(xx)*KA)
X3.Freq <- X3/rowSums(X3)
X4 <- t(t(yy)*KP)		
X4.Freq <- X4/rowSums(X4)

Cum.Anim <- matrix(0,2,np+1)
Cum.Plant <- matrix(0,2,na+1)
Cum.Anim1 <- cumsum(rev(X3.Freq))
Cum.Anim <- as.matrix(t(rbind(rev(X3.Freq),Cum.Anim1,Cum.Anim1,rev(0:na))))
Cum.Anim[,2:3] <- ifelse(Cum.Anim[,1]==0,NA,Cum.Anim[,2:3]) 
Cum.Anim[,4] <- ifelse(Cum.Anim[,1]==0,NA,Cum.Anim[,4]) 
Cum.Plant <- cumsum(rev(X4.Freq))
Cum.Plant <- t(rbind(rev(X4.Freq),Cum.Plant,Cum.Plant,rev(0:np)))
Cum.Plant [,2:3] <- ifelse(Cum.Plant[,1]==0,NA,Cum.Plant[,2:3])
Cum.Plant[,4] <- ifelse(Cum.Plant[,1]==0,NA,Cum.Plant[,4]) 

### Degree distribution histograms
par(mfrow=c(1,2))
### ANIMALS
ax<-Cum.Anim[,4]
ay<-rowSums(Cum.Anim[,2:3],na.rm = TRUE)/2
n.regions<-dim(mat)[2]
degree.dist<-colSums(mat)
hist(ka,xlab="Degree (k), Animals", ylab="Number of species",
     main=NULL,col=1,border=8,,breaks=c(0:max(ka)),plot=T)
hist(kp,xlab="Degree (k), Plants", ylab="Number of species",
     main=NULL,col=1,border=8,breaks=c(0:max(kp)),plot=T)
#-----------------------------------------------------------------------------
### Cumulative degree distribution plots with function fits
par(mfrow=c(2,2))
# Animals
nmax<-max(degree.dist)
tmp<-hist(degree.dist,breaks=c(0:nmax),plot=F)
cum.dist<-1-cumsum(tmp$counts)/n.regions 
# cumulative distribution of degree.dist
d<-fitting(degree.dist,nmax)
exp.trace<-exp(-d$mu*(0:nmax))
# cumulative distribution of the exponential law
power.trace<-(1:(nmax+1))^(-d$gamma+1)
# cumulative distribution of the power law
gamma.trace<-1-pgamma((0:nmax),shape=d$alpha,scale=d$beta) 
# cumulative distribution of the truncated power law

# Log-log plot on arithmetic labels
plot(1:(nmax),cum.dist,pch=3,xlab="k",ylab="P(k), cumulative",
     ylim=c(0.001,1.1),main="Animals",log="xy")
lines(1:(nmax+1),exp.trace,lty=3,lwd=2)      # Exponential, dotted
lines(1:(nmax+1),power.trace,lty=2,lwd=2)    # Power-law, dashed
lines(1:(nmax+1),gamma.trace,lty=1,lwd=2)    # Tr.power-law, continuous
text(c(2,2,2,2),c(0.2,0.15,0.12,0.09),labels=c("+ data","-- power law",
        ".. exponential law","- truncated power law"),pos=4)
#-----------------------------------------------------------------------------
# Saving parameters to file
x<-ka
x<-x[x>0]
n<-length(x)
fn<-function(p) -(-n*p*log(sum(x)/(n*p))-n*log(gamma(p))+
    (p-1)*sum(log(x))-n*p)
# - log likelihood for the truncated power law with only one parameter

out <- nlm(fn, p = 1, hessian = TRUE) # Minimisation of fn
alpha<-out$estimate
beta<-sum(ka)/(n.regions*alpha)
# parameters of the truncated power law

# Computation of AIC
AIC.exp<--2*(n.regions*log(d$mu)-d$mu*sum(kp))+2
AIC.pow<--2*(n.regions*log(d$gamma-1)-d$gamma*sum(log(x)))+2
AIC.trunc<--2*(-out$minimum)+2
### AIC
fitting.a<-"#### Fitting parameters, Animals"
fitting.a<-paste(fitting.a,"Animals",sep="\n")
fitting.a<-paste(fitting.a,"Parameter of the exponential law, mu =",sep="\n")
fitting.a<-paste(fitting.a,d$mu,sep=" ")
fitting.a<-paste(fitting.a,"Parameter of the power law ,gamma = ",sep="\n")
fitting.a<-paste(fitting.a,d$gamma,sep=" ")
fitting.a<-paste(fitting.a,"Parameters of the truncated power law, alpha = ",
                 sep="\n")
fitting.a<-paste(fitting.a,d$alpha,sep=" ")
fitting.a<-paste(fitting.a,"beta = ",sep="\n")
fitting.a<-paste(fitting.a,d$beta,sep=" ")
fitting.a<-paste(fitting.a,"AIC exp = ",sep="\n")
fitting.a<-paste(fitting.a,AIC.exp,sep=" ")
fitting.a<-paste(fitting.a,"AIC pow = ",sep="\n")
fitting.a<-paste(fitting.a,AIC.pow,sep=" ")
fitting.a<-paste(fitting.a,"AIC trunc = ",sep="\n")
fitting.a<-paste(fitting.a,AIC.trunc,sep=" ")
fitting.a<-paste(fitting.a,"Plot saved to file Rplots.pdf ",sep="\n\n")
#--------------------------
### Plants
n.regions<-dim(mat)[1]
# For Plants
degree.dist<-rowSums(mat)
hist(degree.dist,xlab="Degree (k)", ylab="Number of species",
     main=NULL,col=1,border=8,plot=F)

nmax<-max(degree.dist)
tmp<-hist(degree.dist,breaks=c(0:nmax),plot=F)
cum.dist<-1-cumsum(tmp$counts)/n.regions 
# cumulative distribution of degree.dist

d<-fitting(degree.dist,nmax)

exp.trace<-exp(-d$mu*(0:nmax))
# cumulative distribution of the exponential law

power.trace<-(1:(nmax+1))^(-d$gamma+1)
# cumulative distribution of the power law

gamma.trace<-1-pgamma((0:nmax),shape=d$alpha,scale=d$beta) 
# cumulative distribution of the truncated power law

# Log-log plot on arithmetic labels
plot(1:(nmax),cum.dist,pch=3,xlab="k",ylab="P(k), cumulative",
     ylim=c(0.001,1.1),main="Plants",log="xy")
lines(1:(nmax+1),exp.trace,lty=3,lwd=2)      # Exponential, dotted
lines(1:(nmax+1),power.trace,lty=2,lwd=2)    # Power-law, dashed
lines(1:(nmax+1),gamma.trace,lty=1,lwd=2)    # Tr.power-law, continuous
text(c(2,2,2,2),c(0.2,0.15,0.12,0.09),labels=c("+ data","-- power law",
        ".. exponential law","- truncated power law"),pos=4)
#-----------------------------------------------------------------------------
# Saving parameters to file
x<-kp
x<-x[x>0]
n<-length(x)
fn<-function(p) -(-n*p*log(sum(x)/(n*p))-n*log(gamma(p))+
            (p-1)*sum(log(x))-n*p)
# - log likelihood for the truncated power law with only one parameter

out <- nlm(fn, p = 1, hessian = TRUE) # Minimisation of fn
alpha<-out$estimate
beta<-sum(ka)/(n.regions*alpha)
# parameters of the truncated power law

# Computation of AIC
AIC.exp<--2*(n.regions*log(d$mu)-d$mu*sum(kp))+2
AIC.pow<--2*(n.regions*log(d$gamma-1)-d$gamma*sum(log(x)))+2
AIC.trunc<--2*(-out$minimum)+2

### AIC
fitting.p<-"#### Fitting parameters, Plants"
fitting.p<-paste(fitting.p,"Plants",sep="\n")
fitting.p<-paste(fitting.p,"Parameter of the exponential law, mu =",sep="\n")
fitting.p<-paste(fitting.p,d$mu,sep=" ")
fitting.p<-paste(fitting.p,"Parameter of the power law ,gamma = ",sep="\n")
fitting.p<-paste(fitting.p,d$gamma,sep=" ")
fitting.p<-paste(fitting.p,"Parameters of the truncated power law, alpha = ",
                 sep="\n")
fitting.p<-paste(fitting.p,d$alpha,sep=" ")
fitting.p<-paste(fitting.p,"beta = ",sep="\n")
fitting.p<-paste(fitting.p,d$beta,sep=" ")
fitting.p<-paste(fitting.p,"AIC exp = ",sep="\n")
fitting.p<-paste(fitting.p,AIC.exp,sep=" ")
fitting.p<-paste(fitting.p,"AIC pow = ",sep="\n")
fitting.p<-paste(fitting.p,AIC.pow,sep=" ")
fitting.p<-paste(fitting.p,"AIC trunc = ",sep="\n")
fitting.p<-paste(fitting.p,AIC.trunc,sep=" ")
fitting.p<-paste(fitting.p,"Plot saved to file Rplots.pdf ",sep="\n\n")

# file with all the value of the fittings
write.table(rbind(fitting.a,fitting.p),"fitting.txt",row.names=FALSE,
col.names=FALSE,quote=FALSE)
