# estimate how long the outbreak lasts - this will be used as time axis max
sir_t_bound = function(beta,gamma,vac=0,tol=sir_init_i/2,tmax=1000,step=1) {
  # get a quick solution out to long time
  R0 = beta/gamma
  if(R0 <= 1 | vac >= 1-1/R0) {
    return(1000)
  }
  df     = sir(beta,gamma,vac,tmax=tmax,step=step)
  df_cut = df[df$I >= tol,]
  return( df_cut[nrow(df_cut),]$time )
}

sir_system=function(t, x, parms){
  S = x[1]
  I = x[2]
  R = x[3]
  beta  = parms["beta"]
  mu    = parms["mu"]    # we're not using this here
  gamma = parms["gamma"]
  N     = parms["N"]
  dS  = mu*(N-S)  - beta*S*I/N
  dI  = beta*S*I/N - I*(mu + gamma)
  dR  = gamma*I - mu*R
  res = c(dS,dI,dR)
  list(res)
}

# vac is the vaccination fraction - it is removed from S but not added to R
sir = function(beta=3, gamma=2, vac=0, tmax=20, step=sir_system_time_step) {
  time  = seq(0, tmax, by=step)
  parms = c(mu=0, N=1, beta=beta, gamma=gamma)
  start = c(S=1-sir_init_i-vac, I=sir_init_i, R=0)
  out   = ode(y     = start, 
              times = time, 
              func  = sir_system, 
              parms = parms)
  out   = cbind(out,data.frame(R0=beta/gamma,vac=vac))
  return(as.data.frame(out))
}

report_timing = function(t0,t1) {
  if(do_timing) {
    print(sprintf("%.3f seconds",t1-t0))
  }
}