
#Functions in R
##functions
### Graph of simulated data
ts.graph1<-function(x){
        library(ggplot2)
        df<-data.frame(time=1:length(x),value=x)
        p1<-ggplot(df, aes(x=time, y=value)) +theme_classic()+
                geom_hline(yintercept = mu)+
                geom_line(colour="black")+ xlab("Periods")+ylab(NULL)
        return(p1)
}


##### Irf
irf.graph1<-function(a,m,h)
{
        library(ggplot2)
        
        irf11<-ARMAtoMA(a,m,h)
        irf12<-c(1,irf11)
        ma.irf<-data.frame(x=0:h,y=irf12)
        
        p1<-ggplot(ma.irf,aes(x,y))+
                geom_line()+
                theme_classic()+
                ylab("IR")+xlab(NULL)+
                geom_point(aes(x=x,y=y))+
                geom_hline(yintercept = 0)+
                scale_x_continuous(breaks =seq(from = 1, to = h, by = 4))+
                geom_vline(xintercept = 1,color="grey")+
                geom_point(aes(x=0,y=1),colour="red")+
                annotate("text", x = 0.8, y = 1.1, label = "Shock",col='red')
        p1
}

cirf.graph1<-function(a,m,h){
        library(ggplot2)
        irf11<-ARMAtoMA(a,m,h)
        cirf11<-as.vector(1:h)
        cirf11[1]<-irf11[1]
        for(i in 2:h) cirf11[i] <- irf11[i]+cirf11[i-1]
        ma.cirf<-data.frame(x=1:h,y=cirf11)
        
        p2<- ggplot(ma.cirf,aes(x,y))+
                geom_line()+
                theme_classic()+
                geom_point(aes(x=x,y=y))+
                scale_x_continuous(breaks =seq(from = 1, to = h, by = 4))+
                geom_hline(yintercept = 0)+
                ylab("Accumulated IR")+ 
                xlab("Period after the shock")
        
        p2        
}
#### Theoretical ACF

acf.graph1<-function(a,m,l){
        
        ma11.acf1 <- ARMAacf(ar=a, ma=m, lag.max=l)
        ma11.acf<-ma11.acf1[-1]
        lags<-1:l
        
        df.acf.p<-data.frame(x=lags,y=ma11.acf)
        
        
        p2<- ggplot(df.acf.p,aes(x,y))+
                theme_classic()+
                geom_segment(aes(x=x, y=y,xend=x,yend=0),color="bisque4",size=2)+ 
                scale_x_continuous(breaks =seq(from = 1, to = l, by = 4))+
                geom_point(aes(x=x,y=y),size=3)+
                geom_hline(yintercept = 0)+
                ylab("ACF")+xlab("Period(lags)")
        p2
        
}



pacf.graph1<-function(a,m,l){
        
        lags<-1:l
        
        
        ma11.pacf <- ARMAacf(ar=a, ma=m, lag.max=l,pacf=T)
        df.pacf<-data.frame(x=lags,y=ma11.pacf)
        
        p2<- ggplot(df.pacf,aes(x,y))+
                theme_classic()+
                geom_segment(aes(x=x, y=y,xend=x,yend=0),color="bisque4",size=2)+ 
                scale_x_continuous(breaks =seq(from = 1, to = l, by = 4))+
                geom_point(aes(x=x,y=y),size=3)+
                geom_hline(yintercept = 0)+
                ylab("PACF")+xlab("Period(lags)")
        p2
        
}

sacf.graph1<-function(x1,n){
        
        sacf <- acf(x1, plot = F,lag.max =n )
        sacfdf <- data.frame(lags=c(0:n),sacf$lag, sacf$acf)
        sacfdf <- sacfdf[-1,]
        pci<-2/sqrt(length(x1))
        nci<- -2/sqrt(length(x1))
        ggplot(sacfdf, aes(lags, sacf.acf)) + 
                geom_bar(stat = "identity", position = "identity")+
                scale_x_continuous(breaks =seq(from = 1, to = n, by = 2))+
                geom_hline(yintercept=c(pci, nci), linetype="dashed", col="red")+
                ylab("ACF")+geom_hline(yintercept=0)
}

spacf.graph1<-function(x1,n){
        
        sacf <- pacf(x1, plot = F,lag.max =n )
        sacfdf <- data.frame(lags=c(1:n),sacf$lag, sacf$acf)
        pci<-2/sqrt(length(x1))
        nci<- -2/sqrt(length(x1))
        ggplot(sacfdf, aes(lags, sacf.acf)) + 
                geom_bar(stat = "identity", position = "identity")+
                scale_x_continuous(breaks =seq(from = 1, to = n, by = 2))+
                geom_hline(yintercept=c(pci, nci), linetype="dashed",col="red")+
                ylab("PACF")+geom_hline(yintercept=0)
}

roots_arma<-function(pm){
x <- seq(-1,1,length=1000)
y1 <- sqrt(1 - x^2)
y2 <- -sqrt(1 - x^2)
unitCircle <- ggplot(data.frame(x=x,y1=y1,y2=y2)) + #Using the results from earlier
        geom_hline(aes(yintercept=0)) +
        geom_vline(aes(xintercept=0)) +
        geom_line(aes(x=x,y=y1)) + # top of circle
        geom_line(aes(x=x,y=y2)) + # bottom of circle
        geom_point(aes(x=Real, y=Imaginary), 
                   data.frame(Real=Re(pm), # Using the same code as above
                              Imaginary=Im(pm))) +
        scale_y_continuous("Imaginary Part", limits=c(-1,1)) +
        scale_x_continuous("Real Part", limits=c(-1,1))
return(unitCircle)
}