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
data.0 <- keepCols(rawData)  # Keep relevant columns
data <- orderData(data.0, show.missing=TRUE)


# General settings
nPosteriorSamples = 1000

out <- posterior_predictions(data, posterior.list, nPosteriorSamples)



##################
# Plot example
# Trial type 1
trial_type = list("speed_id" = 1, "difficulty_id" = 2, "cue_id" = 4)
locate <- locate_trials(data,trial_type, sub=6)
obs.choice <- data[locate,]$choice %% (2*pi)
obs <- cbind(obs.choice, data[locate,]$rt)

plot(obs, pch=4, col="red", xlab="Choice", ylab="RT",
     xlim=c(0,2*pi),ylim=c(0,max(data$rt)))
for(a in 1:dim(out)[3]){
    pred <- cbind(out[locate,"choice",a],out[locate,"RT",a])
    points(pred,col=rgb(0.6,0.5,0.1,0.09))
}

