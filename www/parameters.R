# size of array
A <- 2

# model times
maxt <- 15
times <- seq(-10, maxt, by = 1/12)
nt <- length(times)


# Model parameters ----------
parameters <- list(
  N=1000,              # population size
  mui=1/50,            # birth
  muo=1/50,            # death
  R0=rep(2.5, A),      # basic reproduction number
  
  ps = 0.9,            # proportion of non-immune cases that become clinical
  pr = 0.1,            # proportion of all immune cases that become clinical
  wait_treat = 1,      # average wait time before treatment after clinical symptoms (days)
  omega=1/2,           # rate of loss of immunity = 1/(average duration of immunity)
  nuC=365/10,          # rate of loss of clinical symptoms
  nuA=365/60,          # rate of loss of detectibility by microscopy
  nuU=365/120,         # rate of recovery from sub-patent infection
  t_treat = 5,      # year treatment starts for the population
  
  c1max=c(0.99,0.00),          # proprtion who cure on day 1 of treatment
  c2max=c(0.99,0.00),          # proprtion who cure on day 2 of treatment
  c3max=c(0.99,0.00),          # proprtion who cure on day 3 of treatment
  cpmax=c(0.99,0.60),          # proprtion who cure during partner drug treatment
  nupmax=365/2,                    # clearance by partner drug after day 3 of treatment
  
  rho = 365/20,        # rate of recrudescence
  nu1 = 365/1,         # day 1 to day 2
  nu2 = 365/1,         # day 2 to day 3
  nu3 = 365/1,         # day 3 to day p
  
  precmax = c(0.05,0.5),          # proportion who recrudesce
  precmin = 0.05,      # proportion who recrudesce
  
  thetamax = 0.77,     # proportion who are clinical on early failure
  nuDmin=365/27,       # rate of elimination of perfect drug after 3 days of artmeisinin ACT
  
  q=1.0,               # quality of drug
  
  amp=0.7,             # relative amplitude of seasonal forcing
  phi=0,               # week of peak in seasonal forcing
  sensC=0.95,          # sensitivity of diganostic test to identify clinical malaria
  sensA=0.50,          # sensitivity of diganostic test to identify asym malaria
  sensU=0.00,          # sensitivity of diganostic test to identify undet malaria
  f=0,                 # switch on follow up
  kA=0.7,              # relative infectiousness of asymptomatics compared to clinicals
  kU=0.3,              # relative infectiousness of undetected compared to clinicals
  kRes2Sens=rep(0, A), # relative sensitive parasite out-complete resistance parasite when no drug
  lam=rep(0, A)
  
)


# define the indices for each parameter ---------------------------------------
index_parameters <- c("Nindex", "muiindex", "muoindex", rep("R0index", A),
                      "wait_treatindex", "omegaindex", "nuCindex", "nuAindex", "nuUindex", "t_treatindex",
                      rep("c1maxindex", A), rep("c2maxindex", A), rep("c3maxindex", A), rep("cpmaxindex", A),
                      "nupmaxindex", "rhoindex", "nu1index", "nu2index", "nu3index", rep("precmaxindex", A),
                      "precminindex", "thetamaxindex", "nuDminindex", "qindex", "ampindex", "phiindex",
                      "sensCindex", "sensAindex", "sensUindex", "findex", "KAindex", "KUindex","kRes2Sensindex", "lamindex")


# initial values for the ODE system
X <- c("initS" = rep(0.9, A) * parameters$N, 
       "initT1" = rep(0, A), 
       "initT2" = rep(0, A), 
       "initT3" = rep(0, A), 
       "initTp" = rep(0, A), 
       "initTr" = rep(0, A), 
       "initIC1" = rep(0, A), 
       "initIA1" = rep(0.05, A) * parameters$N, 
       "initIU1" = rep(0, A),
       "initP" = rep(0, A), 
       "initR" = rep((1 - 0.9 - 0.05), A) * parameters$N, 
       "initCumInc" = rep(0, A), 
       "initFail" = rep(0, A), 
       "initpositiveDay3up" = rep(0, A), 
       "initpositiveDay1up" = rep(0, A))


# define the indices for each variable of the main vector
index_X <- c("t", rep(c("Sindex", "T1index", "T2index", "T3index", "Tpindex",
                        "Trindex", "IC1index", "IA1index", "IU1index", "Pindex",
                        "Rindex", "CumIncindex", "Failindex", "positiveDay3upindex",
                        "positiveDay1upindex"), each = A))