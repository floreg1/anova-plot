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
