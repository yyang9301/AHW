###############################################################################
## DESCRIPTION: This function subsets a data.frame (data) with a premade list of sites (sites) extracting the sites of interest and rearranging them in the correct west to east order.
## USAGE: You must be in the project root directory to use this function. 
## ARGUMENTS: The (data) must have columns labeled "site" and "src"
## DETAILS:
## VALUE:
## AUTHORS(S):
## REFERENCE(S):
## EXAMPLE(S)
##############################################################################

seq.sites <- function(data, sites = data$site, src = data$src, ...){
  load("metadata/site_list_v4.1.Rdata")
  data$index <- as.factor(paste(data$site, data$src, sep = "/ "))
  site_list <- droplevels(subset(site_list, (index %in% data$index)))
  data <- droplevels(subset(data, (index %in% site_list$index)))
  
  # Add order tag
  data <- data %>%
    group_by(index) %>%
    mutate(order = site_list$order[site_list$index == index][1])
  data <- data.frame(data)
  
  # Order and clean up
  data <- data[order(data$order),]
  data$index <- reorder(data$index, data$order)
  data$order <- NULL
  rownames(data) <- NULL
  data <- data[,c(1:2,5,3:4)]
  return(data)
}
