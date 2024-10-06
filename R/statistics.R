library(afex)

customANOVA <- function(depvar='precision', indvars=c('dominant', 'matching_hand_seen')) {
  
  aov_data <- getMatchingDescriptor(grid.variables = indvars, descriptor = depvar)
  
  aov_data$group <- as.factor(aov_data$group)
  
  afex::aov_ez(id = 'participant',
               dv = depvar,
               data = aov_data,
               between = 'group',
               within = indvars)
  
}