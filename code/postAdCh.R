###############################################################################
#####      Functions to run Posterior Adequacy Check
###############################################################################

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


# Function 4: Order data per Sub x Speed x Diff x Cue
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
orderData <- function(data){
  sub <- unique(data$sub)
  speed <- unique(data$speed_id)
  diff <- unique(data$difficulty_id)
  cues <- unique(data$cue_id)
  
  for(s in speed){
      for(d in diff){
        for(c in cues){
           trial_type <- list("speed_id" = s, 
                              "difficulty_id" = d,
                              "cue_id" = c)
           keep <- locate_trials(data,trial_type)
           }
        }
     }
  
  
  table(data$cue_condition)
  keep.columns <- c("id", "speed_condition", "difficulty_id", "cue_deflections_id", 
                    "position", "cue_position", "response", "response_time","difference")
  data <- data[,keep.columns]
  colnames(data) <- c("sub","speed_id","difficulty_id","cue_id","true_mean","cue","choice","rt", "diff")
  return(data)
}


# Function 4:  
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
  difficulty_id <- trial_type$difficulty_id   # Difficulty level (1, 2, 3)
  speed_id      <- trial_type$speed_id        # Speed/Accuracy instruction (1 or 2)
  cue_id        <- trial_type$cue_id          # Cue deflection used (1, 2, ... 7)
  # Identify the cue deflection value in radians
  cues_available <- degToRad(c(-70,-50,-20,0,20,50,70)) 
  this.cue <- cues_available[cue_id]
                     
  # Determine which subjects will be examined
  nSub = ncol(posterior.list$delta)  # Total no. of subjects
  if(is.na(specific.sub)){   
        sub = 1:nSub                 # If subject is not specified, we do everyone
  }else{     
        sub = specific.sub
  }
  
  # Make sure nPosteriorPredictions is a vector of length nSub
  nPP = length(nPosteriorPredictions)  # Different no. of datapoints sampled per subject
  if(nPP < nSub){   # If vector doesn't have nSub elements                                    
     temp = rep(0,nSub)                   # Assign a default value of 0 per subject
     temp[sub] = nPosteriorPredictions    # Fill in the nPosteriorPrediction values provided
     nPosteriorPredictions = temp         # Replace
    if(nPP != length(sub) & nPP < nSub){  # Defensive coding
        cat("Please specify subject ID")
        break
    }
  }
  
  # Isolate relevant posterior chains according to trial type and extract nPosteriorSamples
  random.iterations = sample(1:nrow(posterior.list$delta),nPosteriorSamples,replace = TRUE)
  delta <- posterior.list$delta[random.iterations,sub,difficulty_id]   
  eta <- posterior.list$eta[random.iterations,sub,speed_id]
  t0 <- posterior.list$t0[random.iterations,sub]
  omega <- posterior.list$omega[random.iterations,sub,cue_id]
  beta <- posterior.list$beta_var_cue[random.iterations,sub]
  var <- posterior.list$var_pos[random.iterations,sub,difficulty_id]
  
  # We don't have posterior samples for theta, so we'll sample them
  theta <- matrix(NA,nrow=nPosteriorSamples,ncol=length(sub))
  for(i in sub){   # For each subject...
      # Sample an indicator value per omega sampled
      all.z <- rbinom(nPosteriorSamples,1,omega[,i])  
      # Keep mode
      z <- as.numeric(names(table(all.z)[which.max(table(all.z))]))
      # drift angles centered at true
      theta.true <- rnorm(nPosteriorSamples,0,sqrt(var[,i])) 
      # drift angles centered at cue
      tau <- (1/var[,i])*beta[,i]   # Beta is a scale on the precision, so we transform it
      theta.cue <- rnorm(nPosteriorSamples,this.cue,sqrt(1/tau))
      # keep theta value associated with z
      theta[,i] = ((theta.cue*z)+(theta.true*(1-z))) %% (2*pi)
  }
  
  max.iterations <- length(sub)*nPosteriorSamples
  count <- 1
  getSamples <- array(NA,dim=c(sum(nPosteriorPredictions),4,nPosteriorSamples))
  row.index <- 0
  for(i in sub){
      n.DataPoints <- nPosteriorPredictions[i]
      if(n.DataPoints==0){ 
              next 
      }
      rows <- (row.index+1):(row.index+n.DataPoints)
      for(j in 1:nPosteriorSamples){
          seed <- count
          par = list("drift" = delta[j,i],  "theta" = theta[j,i],
                     "tzero" = t0[j,i],     "boundary" = eta[j,i])
          x = matrix(NA,nrow=n.DataPoints,ncol=2)
          while(0 < sum(is.na(x[,1]))){
              x = sample.MCMC.cddm(n=n.DataPoints, par, max.RT, plot=FALSE, seed=seed)
              seed  = seed+1
          }
          getSamples[rows,c(2,3),j] = x
          getSamples[rows,1,j] = i
          getSamples[rows,4,j] = seed
          if(track){    cat("Run", count, "of ", max.iterations, "\n")   }
          count = count + 1
          }
      row.index <- row.index+n.DataPoints
      }
  colnames(getSamples) <- c("sub","choice","RT","seed")
  return(getSamples)
}








