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
    # INFLUENCES
    #cross-immunity
    ps<-ps*R0/R0
    ps[1]<-ps[1]#*(1-R[2]/N[2])+pr*(R[2]/N[2])
    ps[2]<-ps[2]#*(1-R[1]/N[1])+pr*(R[1]/N[1])
    
    # type replacement - no treatment
    # already infected with resistant and then get infected with sensitive
    s2r_tna <- R0/R0
    s2r_tna[1]<- 0
    s2r_tna[2]<- lam[1]*(S[1]+R[1])/N[1]
    # already infected with sensitive and then get challenged with resistant
    s2r_tnb <- R0/R0
    s2r_tnb[1]<- 0
    s2r_tnb[2]<- (IC1[1]+IA1[1]+IU1[1])/N[1]
    
    # type replacement upon treatment
    r2s_tya <- R0/R0
    r2s_tya[1]<- (IC1[2]+IA1[2]+IU1[2])/N[2]
    r2s_tya[2]<- 0
    
    
    
    
    
    
    
    
    # # haven't finished this yet   
    #   # blocking infection due to drug [think selective window]
    #   # for T3[1] Tp[1] and Tr[1] -> P[1] instead of IC1[1]
    #   muts2r_Ca <- R0/R0
    #   muts2r_Ca[1]<-gammaC*(S[2]+R[2])/N[2]
    #   muts2r_Ca[2]<-0
    #   # for S[2] and R2 -> IC1[2] new flow rate based on flows out of the Ts
    #   muts2r_Cb <- R0/R0
    #   muts2r_Cb[1]<-0
    #   muts2r_Cb[2]<-gammaC*(xx*T3[1]+xx*Tp[1]+rho*Tr[1])/N[1]
    #   
    #   # repeat for IA
    #   # incude gammaC and gammaA in parameter list
    #   # they will be linked to PK and to sens vs res
    #   # longer runs and IC for res not necessary
    #  
    #     # haven't finished this yet   
    
    
    # mutation on treatment failure [think selective window]
    # ie a proportion of senstive failures become resistant
    
    
    
    #############################################################
    
    
    
    # rate of change
    dS <- mui*N-muo*S+omega*R -lam*S
    
    dT1 <- -muo*T1+treat*IC1+treat*r2s_tya*(IC1+IA1+IU1)-nu1*T1
    dT2 <- -muo*T2+(1-c1)*nu1*T1-nu2*T2
    dT3 <- -muo*T3+(1-c2)*nu2*T2-nu3*T3
    dTp <- -muo*Tp+(1-theta)*(1-c3)*nu3*T3-(nuD+nup)*Tp-fw*Tp
    
    dTr <- -muo*Tr+prec*c1*nu1*T1+prec*c2*nu2*T2+prec*c3*nu3*T3-rho*Tr+prec*cp*(nuD+nup)*Tp
    
    dIC1 <- -muo*IC1 +lam*(1-s2r_tnb)*ps*S+lam*(1-s2r_tnb)*pr*R-treat*(1-r2s_tya)*IC1-nuC*IC1 -sensC*fw*IC1 +rho*Tr+theta*(1-c3)*nu3*T3-s2r_tna*IC1-treat*r2s_tya*IC1
    dIA1 <- -muo*IA1 +lam*(1-s2r_tnb)*(1-ps)*S+lam*(1-s2r_tnb)*(1-pr)*R   +(1-prec)*(1-cp)*(nuD+nup)*Tp+nuC*IC1 -nuA*IA1-sensA*fw*IA1-s2r_tna*IA1-treat*r2s_tya*IA1
    dIU1 <- -muo*IU1 +nuA*IA1 -nuU*IU1 -sensU*fw*IU1-s2r_tna*IU1-treat*r2s_tya*IU1
    
    dP <- -muo*P -nuD*P +(1-prec)*c1*nu1*T1+(1-prec)*c2*nu2*T2+(1-prec)*c3*nu3*T3+(1-prec)*cp*(nuD+nup)*Tp+treat*r2s_tya*IC1
    dR <- -muo*R +nuU*IU1+nuD*P -omega*R-lam*(1-s2r_tnb)*R+s2r_tna*(IC1+IA1+IU1)+lam*s2r_tnb*S
    
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