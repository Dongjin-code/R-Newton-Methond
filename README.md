# R-Newton-Methond
--02/06/2025
func=function(x){
y=100*(x[1]^2-x[2])^2+(x[1]-1)^2
return(y)
}
x<-c(3,1);x
h=func(x);h

--03/06/2025
--01
gfunc=function(x){
g=vector(length=2)
g[1]=400*x[1]*(x[1]^2-x[2])+2*(x[1]-1)
g[2]=200*(x[2]-x[1]^2)
return(g)
}
x<-c(3,1);x
h=gfunc(x);h

--04/06/2025
--01
maxk=500
rho=0.55
sigma=0.4
x0=c(-1.2,1)
d=length(x0)
Hk=diag(d);Hk
m=0
mk=0
x0=matrix(x0)
gk=gfunc(x0);gk
dk=-Hk%*%gk;dk
k=0
result=vector()


while(m<20){
  newy=func(x0+rho^m*dk);newy
  oldy=func(x0);oldy
  if(newy<oldy+sigma*rho^m*t(gk)%*%dk){
    mk=m
    break
  }
  m=m+1
}


newy=func(x0+rho^m*dk);newy
oldy=func(x0);oldy
sigma*rho^m*t(gk)%*%dk
oldy+sigma*rho^m*t(gk)%*%dk
newy<oldy+sigma*rho^m*t(gk)%*%dk


x=x0+rho^m*dk
yk=gfunc(x)-gk


while(m<20){
  newy=func(x0+rho^m*dk);newy
  oldy=func(x0);oldy
  if(newy<oldy+sigma*rho^m*t(gk)%*%dk){
    mk=m
    break
  }
  m=m+1
}


while (k<maxk){
        gk=gfunc(x0)
	dk=-Hk%*%gk
	m=0
	mk=0  
        while(m<20){
  		newy=func(x0+rho^m*dk);newy
  		oldy=func(x0);oldy
  		if(newy<oldy+sigma*rho^m*t(gk)%*%dk){
    			mk=m
    		break
  		}
  		m=m+1
	}
        
	x=x0+rho^mk*dk
	sk=x-x0
	yk=gfunc(x)-gk
        if (t(sk)%*%yk>0){
            Hk=Hk-(Hk%*%yk%*%t(yk)%*%Hk)/(t(yk)%*%Hk%*%yk)[1,1]+(sk%*%t(sk))/(t(sk)%*%yk)[1,1]
	}
        
        k=k+1
        x0=x
        result=append(result,func(x0))
}

DFP=function(x0){
result=vector()
maxk=500
rho=0.55
sigma=0.4
d=length(x0)
Hk=diag(d)
k=0
while (k<maxk){
        gk=gfunc(x0)
	dk=-Hk%*%gk
	m=0
	mk=0  
        while(m<20){
  		newy=func(x0+rho^m*dk);newy
  		oldy=func(x0);oldy
  		if(newy<oldy+sigma*rho^m*t(gk)%*%dk){
    			mk=m
    		break
  		}
  		m=m+1
	}
        
	x=x0+rho^mk*dk
	sk=x-x0
	yk=gfunc(x)-gk
        if (t(sk)%*%yk>0){
            Hk=Hk-(Hk%*%yk%*%t(yk)%*%Hk)/(t(yk)%*%Hk%*%yk)[1,1]+(sk%*%t(sk))/(t(sk)%*%yk)[1,1]
	}
        
        k=k+1
        x0=x
        result=append(result,func(x0))
}
return(result)
}

--02
func=function(x){
y=100*(x[1]^2-x[2])^2+(x[1]-1)^2
return(y)
}

gfunc=function(x){
g=vector(length=2)
g[1]=400*x[1]*(x[1]^2-x[2])+2*(x[1]-1)
g[2]=200*(x[2]-x[1]^2)
return(g)
}

DFP=function(x0){
result=vector()
maxk=500
rho=0.55
sigma=0.4
d=length(x0)
Hk=diag(d)
k=0
while (k<maxk){
        gk=gfunc(x0)
	dk=-Hk%*%gk
	m=0
	mk=0  
        while(m<20){
  		newy=func(x0+rho^m*dk);newy
  		oldy=func(x0);oldy
  		if(newy<oldy+sigma*rho^m*t(gk)%*%dk){
    			mk=m
    		break
  		}
  		m=m+1
	}
        
	x=x0+rho^mk*dk
	sk=x-x0
	yk=gfunc(x)-gk
        if (t(sk)%*%yk>0){
            Hk=Hk-(Hk%*%yk%*%t(yk)%*%Hk)/(t(yk)%*%Hk%*%yk)[1,1]+(sk%*%t(sk))/(t(sk)%*%yk)[1,1]
	}
        
        k=k+1
        x0=x
        result=append(result,func(x0))
}
return(result)
}

x0=c(-1.2,1)
DFP(x0)
plot(DFP(x0))

x0=c(7,3)
DFP(x0)
plot(DFP(x0))
plot(DFP(x0)[10:500])












