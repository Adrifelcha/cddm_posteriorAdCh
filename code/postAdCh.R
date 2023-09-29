###############################################################################
#####      Functions to run Posterior Adequacy Check
###############################################################################

# Load posterior samples
samples <- readRDS(file="../data/posterior-test-eta-omega-cddm.RDS")
posterior.list <- samples$BUGSoutput$sims.list  # Isolate posteriors
# Load data set
rawData <- read.csv("../data/orientation.csv")
trial_type <- list("speed_id" = 1, 
                   "difficulty_id" = 1,
                   "cue_id" = 1)
nPosteriorSamples = 1500
specific.sub = NA

# Function 1: Take raw data and clean irrelevant columns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
cleanData <- function(data){
  colnames(data)
  table(data$cue_condition)
  keep.columns <- c("id", "speed_condition", "difficulty_id", "cue_deflections_id", 
                    "position", "cue_position", "response", "response_time","difference")
  data <- data[,keep.columns]
  colnames(data) <- c("sub","speed_id","difficulty_id","cue_id","true_mean","cue","choice","rt", "diff")
  return(data)
}

# Function 2:  Locate rows pertaining to a specific trial type (Cue*Diff*Speed)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
locate_trials <- function(data,trial_type){
  this.speed <- data$speed_id == (trial_type$speed_id - 1)
  this.diff <- data$difficulty_id == trial_type$difficulty_id
  this.cue <- data$cue_id == trial_type$cue_id
  keep.rows <- which(this.speed & this.diff & this.cue)
  return(keep.rows)
}

data <- cleanData(rawData)
keep <- locate_trials(data,trial_type)
counts <- table(data[keep,]$sub)

nPosteriorPredictions <- rep(0,6)
nPosteriorPredictions[as.numeric(names(counts))] <- as.numeric(counts)

# Function 3:  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
samplesPerTrialType <- function(nPosteriorSamples,     # No. of values sampled from posterior
                                nPosteriorPredictions, # No. of predicted data points to sample
                                posterior.list,    # samples$BUGSoutput$sims.list
                                trial_type,        # List(speed_id, difficulty_id, cue_id)
                                specific.sub = NA, # Specify what subjects to include
                                max.RT = 3,        # Maximum RT in dataset
                                track = TRUE       # T/F print progress
                                ){
  # Identify trial properties
  speed_id <- trial_type$speed_id             # Speed/Accuracy instruction (1 or 2)
  difficulty_id <- trial_type$difficulty_id   # Difficulty level (1, 2, 3)
  cue_id <- trial_type$cue_id                 # Cue deflection used (1, 2, ... 7)
                     
  # Determine whether a single subject vs all subjects will be examined
  if(is.na(specific.sub)){ 
        sub = 1:ncol(posterior.list$delta)
        if(length(nPosteriorPredictions)!=1){
           print("Replicating data size (i.e., number of predicted data points per sampled posterior parameter set)")
        }
  }else{     sub = specific.sub     }
  
  # Isolate relevant posterior chains according to trial type
  post.delta <- posterior.list$delta[,sub,difficulty_id]   
  post.eta <- posterior.list$eta[,sub,speed_id]
  post.t0 <- posterior.list$t0[,sub]
  post.omega <- posterior.list$omega[,sub,cue_id]
  post.cue.var <- posterior.list$beta_var_cue[,sub]
  post.true.var <- posterior.list$var_pos[,sub,difficulty_id]
  
  # Extract nPosteriorSamples out of posterior chains, with replacement
  random.samples = sample(1:nrow(post.delta),nPosteriorSamples,replace = TRUE)
  delta = post.delta[random.samples,]
  eta   = post.eta[random.samples,]
  t0    = post.t0[random.samples,]
  omega = post.omega[random.samples,]
  beta  = post.cue.var[random.samples,]
  var   = post.true.var[random.samples,]
  
  # Identify the cue deflection value
  cues_available <- degToRad(c(-70,-50,-20,0,20,50,70))
  this.cue <- cues_available[cue_id]
  # We don't have posterior samples for theta, so we'll sample them
  empty.matrix <- matrix(NA,nrow=nPosteriorSamples,ncol=length(sub))
  theta <- empty.matrix
  for(i in sub){   # For each subject...
      # Sample an indicator value per omega sampled
      all.z <- rbinom(nPosteriorSamples,1,omega[,i])  
      z <- as.numeric(names(table(all.z)[which.max(table(all.z))]))
      # drift angles centered at true
      theta.true <- rnorm(nPosteriorSamples,0,sqrt(var[,i])) 
      # drift angles centered at cue
      tau <- (1/var[,i])*beta[,i]   # Beta is a scale on the precision, so we transform it
      theta.cue <- rnorm(nPosteriorSamples,this.cue,sqrt(1/tau))
      theta[,i] = (theta.cue*z)+(theta.true*(1-z))
  }
  theta <- theta %% (2*pi)
  
  TOTAL <- length(sub)*nPosteriorSamples
  count <- 1
  getSamples = array(NA,dim=c(max(nPosteriorPredictions),4,nPosteriorSamples))
  for(i in sub){
      for(j in 1:nPosteriorSamples){
          seed <- count
          par = list("drift" = delta[j,i],  "theta" = theta[j,i],
                     "tzero" = t0[j,i],     "boundary" = eta[j,i])
          x = c(NA,NA)
          while(is.na(x[1])){
            x = sample.MCMC.cddm(n=nPosteriorPredictions[i], par, max.RT = max.RT, seed = seed)
            seed = seed+1
          }
          getSamples[,1,j] = i
          getSamples[,c(2,3),j] = x
          getSamples[,4,j] = seed
          if(track){    cat("Run", count, "of ", TOTAL, "\n")   }
          count = count +1
      }
  }
  output <- getSamples[,1:2]
  colnames(output) <- c("choice","RT")
return(getSamples)
}


  






