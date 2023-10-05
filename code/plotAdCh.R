###############################################################################
#####     Plot 1:  Simple jittery scatter plot
###############################################################################
speed <- rep(c(0,0,0,1,1,1), 7)
diff <- rep(c(1,2,3,1,2,3), 7)
cues <- rep(c(7:1), each=6)
p.col <- rep(seq(0,1,length.out=3),4)
left.side <- c(1,7,13,19,25,31,37)
right.side <- left.side+5

###############################################################################
#####     Plot 1:  Simple jittery scatter plot
###############################################################################
for(p in 1:6){
  fileName <- paste("./simplePlot-0",p,".pdf",sep="")
  pdf(file = fileName, width = 10, height = 11)
  par(mfrow=c(7,6),
      mar = c(1, 0.85, 0.85, 0.85), 
      omi = c(0.2,0.2,0.5,0.3)) 
    for(i in 1:42){
      trial_type = list("speed_id" = speed[i], "difficulty_id" = diff[i], "cue_id" = cues[i])
      locate <- locate_trials(data,trial_type, sub=p)
      nT <-  length(locate)
      obs.choice <- data[locate,]$diff
      obs <- cbind(obs.choice, data[locate,]$rt)
      
      max.RT <- max(data$rt)
      plot(obs, pch=4, col="red", xlab="Choice", ylab="RT",
           xlim=c(-pi,pi),ylim=c(0,max.RT), ann=F, axes = F)
      axis(1, c(-pi,0,pi), c("","",""))
      axis(2, c(0,max.RT), c("",""), las=2)
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
        points(pred,col=rgb(p.col[p],p.col[p+1],p.col[p+2],0.05), pch=16)
        
      }
      
      if(nT>0){
        legend("topright", paste("n = ", length(locate),sep=""),bty="n")
      }else{
        text(0,max.RT/2,"N/A")    
      }
      
      points(obs,pch=4,col="black")
      
      mtext(paste("Participant", p), side = 3, line =2, outer = TRUE, f=2, cex=1.2)
      if(i==2){mtext("Accuracy", side = 3, line =2, f=2)}
      if(i==5){mtext("Speed", side = 3, line =2, f=2)}
      if(i<7){mtext(paste("Difficulty", diff[i]), side = 3, line =0.6, f=2, cex=0.7)}
      if(i %% 6 == 0){mtext(paste("Cue", cues[i]), side = 4, line = 0.5, f=2, cex=0.9)}
      if(i > 36){axis(1, c(-pi,0,pi), c(expression(-pi),0,expression(pi)))}
      if(i %in% left.side){axis(2, c(0,2.5), c("0","2.5"))}
      if(i %in% left.side){mtext("RT", side=2, cex=0.8, line=0.5)}
    }
   dev.off()
}

###############################################################################
#####     Plot 2:  
###############################################################################
for(p in 1:6){
  fileName <- paste("./simplePlot-0",p,".pdf",sep="")
  pdf(file = fileName, width = 10, height = 11)
  par(mfrow=c(7,6),
      mar = c(1, 0.85, 0.85, 0.85), 
      omi = c(0.2,0.2,0.5,0.3)) 
  for(i in 1:42){
    trial_type = list("speed_id" = speed[i], "difficulty_id" = diff[i], "cue_id" = cues[i])
    locate <- locate_trials(data,trial_type, sub=p)
    nT <-  length(locate)
    obs.choice <- data[locate,]$diff
    obs <- cbind(obs.choice, data[locate,]$rt)
    
    max.RT <- max(data$rt)
    plot(obs, pch=4, col="red", xlab="Choice", ylab="RT",
         xlim=c(-pi,pi),ylim=c(0,max.RT), ann=F, axes = F)
    axis(1, c(-pi,0,pi), c("","",""))
    axis(2, c(0,max.RT), c("",""), las=2)
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
      points(pred,col=rgb(p.col[p],p.col[p+1],p.col[p+2],0.05), pch=16)
      
    }
    
    if(nT>0){
      legend("topright", paste("n = ", length(locate),sep=""),bty="n")
    }else{
      text(0,max.RT/2,"N/A")    
    }
    
    points(obs,pch=4,col="black")
    
    mtext(paste("Participant", p), side = 3, line =2, outer = TRUE, f=2, cex=1.2)
    if(i==2){mtext("Accuracy", side = 3, line =2, f=2)}
    if(i==5){mtext("Speed", side = 3, line =2, f=2)}
    if(i<7){mtext(paste("Difficulty", diff[i]), side = 3, line =0.6, f=2, cex=0.7)}
    if(i %% 6 == 0){mtext(paste("Cue", cues[i]), side = 4, line = 0.5, f=2, cex=0.9)}
    if(i > 36){axis(1, c(-pi,0,pi), c(expression(-pi),0,expression(pi)))}
    if(i %in% left.side){axis(2, c(0,2.5), c("0","2.5"))}
    if(i %in% left.side){mtext("RT", side=2, cex=0.8, line=0.5)}
  }
  dev.off()
}