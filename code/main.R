###############################################################################
#####      Run Posterior Adequacy Checks
###############################################################################
test <- FALSE
source("./samplers.R")

samples <- readRDS(file="../posterior-test-eta-omega-cddm.RDS")
posterior.list <- samples$BUGSoutput$sims.list  # Isolate posteriors


# Argument dictionary:
# speed_id = 1 ('speed') vs 0 ('accuracy')
# difficulty_id = 
# deflection_id = 1, 2, ..., 6 

speed_id = 1
difficulty_id = 1
deflection_id = 1
sub = NA

getSamples <- function(posterior.list,
                       nSamples,
                       speed_id,
                       difficulty_id,
                       deflection_id,
                       specific.sub = NA){
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
  
  
  
  
}

  







