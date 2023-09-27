###############################################################################
#####      Run Posterior Adequacy Checks
###############################################################################
test <- FALSE
source("./samplers.R")

samples <- readRDS(file="../posterior-test-eta-omega-cddm.RDS")
posterior.list <- samples$BUGSoutput$sims.list  # Isolate posteriors


# Argument dictionary:
# speed_id = 1, 2
# difficulty_id =  
# deflection_id = 1, 2, ..., 6 

speed_id = 1
difficulty_id = 1
deflection_id = 1
specific.sub = NA
nSamples = 5000

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
  
  random.samples = sample(1:nrow(post.delta),nSamples,replace = TRUE)
  delta = post.delta[random.samples,]
  eta = post.eta[random.samples,]
  t0 = post.t0[random.samples,]
  omega = post.omega[random.samples,]
  var.cue = post.cue.var[random.samples,]
  var = post.true.var[random.samples,]
}

  







