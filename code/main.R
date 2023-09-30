###############################################################################
#####      Run Posterior Adequacy Checks
###############################################################################
test <- FALSE
source("./samplers.R")
source("./postAdCh.R")
source("./dCDDM.R")

# Load posterior samples
samples <- readRDS(file="../data/posterior-test-eta-omega-cddm.RDS")
posterior.list <- samples$BUGSoutput$sims.list  # Isolate posteriors
# Load data set
rawData <- read.csv("../data/orientation.csv")
data <- cleanData(rawData)  # Keep relevant columns
data <- orderData(data)

# General settings
nPosteriorSamples = 1500
specific.sub = NA
max.RT = max(data$rt)
track = TRUE
trial_type <- list("speed_id" = 1, 
                   "difficulty_id" = 1,
                   "cue_id" = 1)

keep <- locate_trials(data,trial_type)
counts <- table(data[keep,]$sub)
nPosteriorPredictions <- rep(0,6)
nPosteriorPredictions[as.numeric(names(counts))] <- as.numeric(counts)

x <- samplesPerTrialType(nPosteriorSamples, nPosteriorPredictions, posterior.list,
                         trial_type, specific.sub = NA, max.RT = max.RT, track = TRUE)