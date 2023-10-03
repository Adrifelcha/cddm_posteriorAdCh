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