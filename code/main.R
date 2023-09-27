###############################################################################
#####      Run Posterior Adequacy Checks
###############################################################################
test <- FALSE
source("./samplers.R")

samples <- readRDS(file="../data/posterior-test-eta-omega-cddm.RDS")
posterior.list <- samples$BUGSoutput$sims.list  # Isolate posteriors

data <- read.csv("../data/orientation.csv")

trial_type <- list("speed_id" = 1,
                   "difficulty_id" = 1,
                   "deflection_id" = 1)
nSamples = 5000
specific.sub = NA
samplesPerTrialType <- function(nSamples, 
                                posterior.list,  # samples$BUGSoutput$sims.list
                                trial_type,
                                specific.sub = NA, # Choose a specific subject
                                ){
  speed_id = trial_type$speed_id
  difficulty_id = trial_type$difficulty_id
                     
  # Determine whether a single subject vs all subjects will be examined
  if(is.na(specific.sub)){ 
        sub = 1:ncol(posterior.list$delta)
  }else{ sub = specific.sub  }
  
  # Isolate relevant posterior chains according to trial type
  post.delta <- posterior.list$delta[,sub,difficulty_id]   
  post.eta <- posterior.list$eta[,sub,speed_id]
  post.t0 <- posterior.list$t0[,sub]
  post.omega <- posterior.list$omega[,sub,deflection_id]
  post.cue.var <- posterior.list$beta_var_cue[,sub]
  post.true.var <- posterior.list$var_pos[,sub,difficulty_id]
  
  # Extract nSamples out of posterior chains, with replacement
  random.samples = sample(1:nrow(post.delta),nSamples,replace = TRUE)
  delta = post.delta[random.samples,]
  eta = post.eta[random.samples,]
  t0 = post.t0[random.samples,]
  omega = post.omega[random.samples,]
  beta = post.cue.var[random.samples,]
  var = post.true.var[random.samples,]
  
  # Identify the cue deflection value
  cues_available = c(-70,-50,-20,0,20,50,70)
  this.cue = cues_available[deflection_id]
  # We don't have posterior samples for theta, so we'll sample them
  empty.matrix = matrix(NA,nrow=nSamples,ncol=length(sub))
  z = empty.matrix
  theta.true = empty.matrix
  theta.cue = empty.matrix
  for(i in sub){   # For each subject...
      # Sample an indicator value per omega sampled
      z[,i] = rbinom(nSamples,1,omega[,i])  
      # drift angles centered at true
      theta.true[,i] = rnorm(nSamples,0,sqrt(var[,i])) 
      # drift angles centered at cue
      tau = (1/var[,i])*beta[,i]   # Beta is a scale on the precision, so we transform it
      theta.cue[,i] = rnorm(nSamples,this.cue,sqrt(1/tau))
  }
  # Transform back from degrees to radians
  theta.cue = degToRad(theta.cue)
  theta.true = degToRad(theta.true)
  # Use indicator variable to indicate which theta to use per sample
  theta = (theta.cue*z)+theta.true
  
  TOTAL = length(sub)*nSamples
  count = 1
  getSamples = array(NA,dim=c(nSamples,3,length(sub)))
  for(i in sub){
      for(j in 1:nSamples){
          seed <- count
          par = list("drift" = delta[j,i],
                     "theta" = theta[j,i],
                     "tzero" = t0[j,i],
                     "boundary" = eta[j,i])
          x = c(NA,NA)
          while(is.na(x[1])){
            x = sample.MCMC.cddm(n=1, par, max.RT = 2.5, seed = seed)
            seed = seed+1
          }
          getSamples[j,1:2,i] = x
          getSamples[j,3,i] = seed
          cat("Run", count, "of ", TOTAL, "\n")
          count = count +1
      }
  }
  output <- getSamples[,1:2]
  colnames(output) <- c("choice","RT")
  return(getSamples)
}


  







