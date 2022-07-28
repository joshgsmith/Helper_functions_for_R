#By Joshua G. Smith
#July 27, 2022

#This function calculates the distance between centroids in high dimensional space. 
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
ccfrp <- cenfun(group=CCFRP_group_vars,c=CCFRP_group_vars$year, x=CCFRP_distmat)

# A tibble: 13 × 3
   centroid_1 centroid_2 distance
   <chr>      <chr>         <dbl>
 1 2007       2008         0.0908
 2 2008       2009         0.141 
 3 2009       2010         0.103 
 4 2010       2011         0.157 
 5 2011       2012         0.0595
 6 2012       2013         0.181 
 7 2013       2014         0.0949
 8 2014       2015         0.100 
 9 2015       2016         0.154 
10 2016       2017         0.0975
11 2017       2018         0.0867
12 2018       2019         0.0921
13 2019       2020         0.0654



