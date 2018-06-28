# set up a function to solve the equations
MedQual <- function(t, X, parameters) {
  
  with(as.list(c(X, parameters)), {
    
    S <- X[index_X[-1] == "Sindex"] 
    T1 <- X[index_X[-1] == "T1index"]
    T2 <- X[index_X[-1] == "T2index"] 
    T3 <- X[index_X[-1] == "T3index"] 
    Tp <- X[index_X[-1] == "Tpindex"] 
    Tr <- X[index_X[-1] == "Trindex"] 
    IC1 <- X[index_X[-1] == "IC1index"] 
    IA1 <- X[index_X[-1] == "IA1index"] 
    IU1 <- X[index_X[-1] == "IU1index"] 
    P <- X[index_X[-1] == "Pindex"] 
    R <- X[index_X[-1] == "Rindex"]
    CumInc <-X[index_X[-1] == "CumIncindex"] 
    Fail <-X[index_X[-1] == "Failindex"] 
    positiveDay3up <-X[index_X[-1] == "positiveDay3upindex"] 
    positiveDay1up <-X[index_X[-1] == "positiveDay1upindex"] 
    
    
    R0 <- parameters$R0
    q <- parameters$q
    c1max <-parameters$c1max
    c2max <-parameters$c2max
    c3max <-parameters$c3max
    cpmax <-parameters$cpmax
    precmax <-parameters$precmax
    kRes2Sens <-parameters$kRes2Sens
    lam   <-parameters$lam
    
    # define variables
    N <- (S+T1+T2+T3+Tp+Tr+IC1+IA1+IU1+P+R)
    seas <- 1+amp*cos(2*pi*(t-phi))
    nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
    I <- T1+T2+T3+Tp+Tr+IC1+IA1+IU1
    beta<-R0*(muo+nu)*seas
    lam <- beta*seas*(IC1+kA*(IA1+T1+T2+T3+Tp)+kU*(IU1+Tr))/N
    
    
    nuD <- nuDmin/(q+(0.001/365))
    nup <- q*nupmax
    
    c1 <- q*c1max
    c2 <- q*c2max
    c3 <- q*c3max
    cp <- q*cpmax
    prec<- q*precmax+(1-q)*precmin
    
    theta<-1/(1+(3/365)*nuC)
    
    treat <- (t>=t_treat)*365/(wait_treat)
    
    
    tf<-7/365 # 7 days after treatment
    rf <- 52 # weekly follow up
    fw <- ((t>tf)*rf)*f
    
    #############################################################
    # sensitive Out-Cmplete Resistance WhenNoDrug  INFLUENCES
    ## 1) the present of sensitive parasites will prevent infection by resistant parasites
    lam <-lam
    lam[2] <- lam[2]*(1-(1/N[2])*((nuD[1]/(365+nuD[1]))*T1[1]+(2*nuD[1]/(365+2*nuD[1]))*T2[1]+(3*nuD[1]/(365+3*nuD[1]))*T3[1]+IC1[1]+IA1[1]+IU1[1]+Tr[1]))
    # lam[2] <- lam[2]*(1-(1/N[2])*(IC1[1]+IA1[1]+IU1[1]+Tr[1]))                   
    
    ## 2) if a resistance is co-infected with a sensitive infection, then the sensitive infection replaces the resistance one.
    kRes2Sens[1] <- 0
    kRes2Sens[2] <- ((S[1]+R[1])/N[1])*lam[1]
    
    
    #############################################################
    # rate of change
    dS <- mui*N-muo*S+omega*R -lam*S
    
    dT1 <- -muo*T1+treat*IC1-nu1*T1 -(nuD[1]/(365+nuD[1]))*kRes2Sens*T1
    dT2 <- -muo*T2+(1-c1)*nu1*T1-nu2*T2 -(2*nuD[1]/(365+2*nuD[1]))*kRes2Sens*T2
    dT3 <- -muo*T3+(1-c2)*nu2*T2-nu3*T3 -(3*nuD[1]/(365+3*nuD[1]))*kRes2Sens*T3
    dTp <- -muo*Tp+(1-theta)*(1-c3)*nu3*T3-(nuD+nup)*Tp-fw*Tp
    
    dTr <- -muo*Tr+prec*c1*nu1*T1+prec*c2*nu2*T2+prec*c3*nu3*T3-rho*Tr+prec*cp*(nuD+nup)*Tp -kRes2Sens*Tr
    
    dIC1 <- -muo*IC1 +lam*ps*S+lam*pr*R -treat*IC1-nuC*IC1 -sensC*fw*IC1 +rho*Tr +theta*(1-c3)*nu3*T3 -kRes2Sens*IC1
    dIA1 <- -muo*IA1 +lam*(1-ps)*S+lam*(1-pr)*R   +(1-prec)*(1-cp)*(nuD+nup)*Tp+nuC*IC1 -nuA*IA1-sensA*fw*IA1 -kRes2Sens*IA1
    dIU1 <- -muo*IU1 +nuA*IA1 -nuU*IU1 -sensU*fw*IU1 -kRes2Sens*IU1
    
    dP <- -muo*P -nuD*P +(1-prec)*c1*nu1*T1+(1-prec)*c2*nu2*T2+(1-prec)*c3*nu3*T3+(1-prec)*cp*(nuD+nup)*Tp
    dR <- -muo*R +nuU*IU1+nuD*P -omega*R-lam*R +kRes2Sens*(IC1+IA1+IU1+Tr)+(((nuD[1]/(365+nuD[1]))*kRes2Sens*T1)+((2*nuD[1]/(365+2*nuD[1]))*kRes2Sens*T2)+((3*nuD[1]/(365+3*nuD[1]))*kRes2Sens*T3))
    
    dCumInc <- treat*IC1
    dFail <- fw*Tp+sensC*fw*IC1+sensA*fw*IA1+sensU*fw*IU1
    dpositiveDay3up <-T3+Tp
    dpositiveDay1up <-T1+T2+T3+Tp
    
    #  percentage of all treated infections which either recrudesce or don't clear  
    
    # return the rate of change
    list(c(dS, 
           dT1, dT2, dT3, dTp, dTr, 
           dIC1, dIA1, dIU1, 
           dP, dR, dCumInc, dFail, dpositiveDay3up, dpositiveDay1up
    ))
  }
  )
}