rawData <- read.csv("../data/orientation.csv")
#The 7 values of cue deflection don't match the description
table(rawData$cue_position-rawData$position)  # And there is some variability
table(round(rawData$cue_position-rawData$position,3))
table(round(rawData$position-rawData$cue_position,3))
all.cues <- c(-70,-50,-20,0,20,50,70)
degToRad(all.cues)



# Every trial type appears at least 6 times
for(d in 1:3){
  for(s in 1:2){
    for(c in 1:7){
      trial_type <- list("speed_id" = s-1,
                         "difficulty_id" = d,
                         "cue_id" = c)
      x <- locate_trials(data,trial_type)
      print(length(x))
    }
  }
}