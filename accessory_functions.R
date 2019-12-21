### Generate Comparison function ####
### 

weights <- tibble(item = c("car", "medium missile", "armored car", "large meteor",
                           "helicopter", "school bus", "space shuttle",
                           "Statue of Liberty"),
                  weight = c(4000, 2600, 25000, 5000, 14000, 14000,
                             220000, 450000)
)


  weights_filt <- subset(weights, weight <= total_weight)
  comp <- weights_filt[sample(1:nrow(weights_filt), 1), ]
  
  num <- total_weight / comp$weight
  
  final <- paste("You lifted a ", comp$item, " ", num, " times!")

