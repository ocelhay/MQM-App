# Extract and process results
t <- out[, 1]
S <- out[, index_X == "Sindex"]
T1 <- out[, index_X == "T1index"]
T2 <- out[, index_X == "T2index"]
T3 <- out[, index_X == "T3index"]
Tp <- out[, index_X == "Tpindex"]
Tr <- out[, index_X == "Trindex"]
IC1 <- out[, index_X == "IC1index"]
IA1 <- out[, index_X == "IA1index"]
IU1 <- out[, index_X == "IU1index"]
P <- out[, index_X == "Pindex"]
R <- out[, index_X == "Rindex"]
CumInc <- out[, index_X == "CumIncindex"]
Fail <- out[, index_X == "Failindex"]
positiveDay3up <- out[, index_X == "positiveDay3upindex"]
positiveDay1up <- out[, index_X == "positiveDay1upindex"]
pop <- S + T1 + T2 + T3 + Tp + Tr + IC1 + IA1 + IU1 + P + R

# incidence
# TODO: check if it's 2 or -more plausibly- A
inc_month <- matrix(NA, nrow = nt, ncol = 2)
inc_month[1, ] <- c(0, 0)
inc_month[2:nt, ] <- CumInc[2:nt, ] - CumInc[1:(nt-1), ]
inc_month <- 1000 * inc_month / pop
totinc_month <- rowSums(inc_month) - inc_month[,1] * inc_month[,2] / 1000
presinc_month <- 100 * inc_month[,2] / totinc_month

# prevalence
prev <- 100*(T1 + T2 + T3 + Tp + parameters$sensC*IC1 + parameters$sensA*IA1)/pop
totprev <- rowSums(prev) - prev[,1] * prev[,2] / 100
prevr <- prev[,2] - prev[,1] * prev[,2] / 100
presprev <- 100 * prev[,2] / totprev