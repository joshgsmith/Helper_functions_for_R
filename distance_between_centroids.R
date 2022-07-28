#By Joshua G. Smith
#July 27, 2022

#This function calculates the distance between centroids in high dimensional space. It works best when year is the grouping variable of interest (distance from 
#one year to the next). However, it can be applied to any grouping variable. 
#Requires usedist::dist_between_centroids()
#Replace 'year' with centroid groupings. 

#group = data frame containing grouping variables. Must be same length as 'x'.
# c = column specified from grouping vars dataframe 'X' that contains centroid grouping variables. 
# x = distance matrix. Must be same length as 'group'. 


cenfun <- function(group, c, x) {
  
  c <- as.factor(c)
  levels(c)
  n <- nlevels(c)
  start <- levels(c)[1:(n - 1)]
  end <- levels(c)[2:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(c == .x)
    idx2 <- which(c == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} 


Example: 

require(vegan)
require(usedist)

dune <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.spe.txt', row.names = 1)       #biological data
dune.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.env.txt', row.names = 1)   #group vars

data(dune) 
data(dune.env)

dune_dist <- vegdist(dune, method="bray")  #calculate distmat

cenfun(dune.env,dune.env$Use,dune_dist) #apply centroid distance function

# A tibble: 2 × 3
  centroid_1 centroid_2 distance
  <chr>      <chr>         <dbl>
1 Hayfield   Haypastu      0.327
2 Haypastu   Pasture       0.210







