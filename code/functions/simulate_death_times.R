simulate_death_times <- function(pop_size, tau, h0, interval_hazards){
  DT = tau[2:length(tau)] - TAU[1:(length(tau)-1)]
  h0 <- exp(h0)
  G <- c(1, exp(interval_hazards))
  LD <- matrix(0,nrow=length(tau), ncol=(length(tau)-1))
  LD[lower.tri(LD)]<-1
  LS <- log(1-runif(pop_size))
  LSM <- -h0 * as.vector(LD %*% (G * DT))
  
  t1 <- rep(NA,pop_size)
  for (i in 1:length(tau)) {
      t1 <- ifelse(LSM[i]>=LS & LS>LSM[i+1], tau[i] + (LSM[i] - LS)/h0/G[i], t1) 
  }
  t1 <- t1[!is.na(t1)]
  return(t1)
}