#####################################################
#                                                   #
# Function to trim Yes, No labels for graph display #
#                                                   #
#####################################################

label.sort <- function(x) {
  
  # vector is sorted from smallest to largest value
  # labels are sorted accordingly
  # Yes, No string extracted from the labels
  # trailing empty spaces are removed
  
  tags <- trimws(substring(names(sort(x)),1,3),"both")
  
  return(tags)
  
}

#################################
#                               #
# Effects Size ANOVA Extraction #
#                               #
#################################

# get matrix of eta square, partial eta square and squared omega for each design factor

aov.effects <- function(aov.matrix) {
  
  # select columns with df and sum of squares
  
  effect.values <- summary(aov.matrix)[[1]][,1:2]
  
  # eta square definition
  # column of sum of squares in relative terms
  
  effect.values[,3]<-effect.values[,2]/sum(effect.values[,2])
  
  # partial eta square definition
  
  residual.row <- dim(effect.values)[1] # get row with residual values
  
  effect.values[,4]<-effect.values[,2]/(effect.values[,2]+effect.values[residual.row,2])
  
  # omega squared
  
  residual.mse <- summary(aov.matrix)[[1]][residual.row,3]
  
  effect.values[,5] <- (effect.values[,2]-(effect.values[,1]*residual.mse))/(sum(effect.values[,2])+residual.mse)
  
  # column names assignment
  
  colnames(effect.values)[3:5] <- c("Eta^2",
                                    "Partial Eta^2",
                                    "Omega^2")
  
  return(effect.values)

}

######################################################################
#                                                                    #
# set color instructions for barplots to reflect strength of effects #
#                                                                    #
######################################################################

omega2.palette <- function(x) {
  
  coloring <- c("grey25", "grey50", "grey75") # assign grey shading for ease of print, from strongest to weakest effect
  
  color.marker <- (x >= 0.14)*1 + ((x < 0.14)*(x >= 0.06))*2 + (x < 0.06)*3 # categorical vector
  
  color.set <- NULL
  
  # loop to assign colors as function of strength
  
  for (count in 1:length(x)) { 
    
    color.set <- c(color.set,coloring[color.marker[count]])
    
  }
  
  return(color.set)
  
}

