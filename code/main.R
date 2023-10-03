
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
speed <- rep(c(0,1), each=42/2)
diff <- rep(rep(c(1,2,3),2), each=42/6)
cues <- rep(c(1:7), 6)
p.col <- seq(0,1,length.out=6)
#for(p in 1:6){
p <- 2
par(pty="s")     
par(mfrow=c(7,6),
    mar = c(0.85, 0.85, 0.85, 0.85)) 
    for(i in 1:42){
          trial_type = list("speed_id" = speed[i], "difficulty_id" = diff[i], "cue_id" = cues[i])
          locate <- locate_trials(data,trial_type, sub=p)
          nT <-  length(locate)
          obs.choice <- data[locate,]$diff
          obs <- cbind(obs.choice, data[locate,]$rt)
          
          max.RT <- max(data$rt)
          rt.labels <- round(seq(0,max.RT,length.out=5),2)
          plot(obs, pch=4, col="red", xlab="Choice", ylab="RT",
               xlim=c(-pi,pi),ylim=c(0,max.RT), ann=F, axes = F)
          axis(1, c(-pi,0,pi), c(expression(-pi),0,expression(pi)))
          axis(2, rt.labels, rt.labels, las=2)
          choice.vector <- c()
          rt.vector <- c()
          for(a in 1:dim(out)[3]){
              out.choice.0 <- out[locate,"choice",a]
              out.choice.1 <- ifelse(out.choice.0 > pi, 
                             yes = out.choice.0-(2*pi), 
                             no = out.choice.0)
              pred <- cbind(out.choice.1,out[locate,"RT",a])
              choice.vector <- append(choice.vector,out.choice.1)
              rt.vector <- append(rt.vector, out[locate,"RT",a])
              points(pred,col=rgb(p.col[p],0.75,0.1,0.1), pch=16)
              
          }
          
          if(nT>0){
          legend("topright", paste("n =", length(locate)),bty="n")
          }else{
          text(0,max.RT/2,"N/A")    
            }
              
          points(obs,pch=4,col="red")
          
          mtext(paste("Participant", p), side = 3, line =2, outer = TRUE, f=2, col="green")
    }
#}