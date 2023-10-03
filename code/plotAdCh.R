###############################################################################
#####      Run Posterior Adequacy Checks
###############################################################################

##################
# Plot example
# Trial type 1
trial_type = list("speed_id" = 0, "difficulty_id" = 1, "cue_id" = 2)
locate <- locate_trials(data,trial_type, sub=4)
obs.choice <- data[locate,]$diff
obs <- cbind(obs.choice, data[locate,]$rt)

plot(obs, pch=4, col="red", xlab="Choice", ylab="RT",
     xlim=c(-pi,pi),ylim=c(0,max(data$rt)))
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
    points(pred,col=rgb(0.4,0.75,0.1,0.1), pch=16)
}
points(obs,pch=4,col="red")


library(MASS)
z <- MASS::kde2d(choice.vector,rt.vector)
z.obs <- MASS::kde2d(data$diff,data$rt)
cbind(choice.vector,rt.vector)

plot(x, y, pch = 19)
contour(z, lwd = 2, add = TRUE, col = hcl.colors(10, "Spectral"))

filled.contour(z$x,z$y,z$z, plot.axes = {
  contour(z.obs, add=TRUE)
})


filled.contour(z.obs$x,z.obs$y,z.obs$z)