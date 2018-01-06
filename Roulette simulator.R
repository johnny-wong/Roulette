simulate_roulette <- function(sim_num = 10000,
                              num_numbers = 38,
                              num_zeros = 2,
                              start_money = 1000,
                              goal_multiplier = 1.1,
                              start_bid = 50){
  
  won <- 0
  for (simulation in 1:sim_num){
    current_money <- start_money

    while((current_money < start_money * goal_multiplier) && (current_money>0)){
      bid <- max(start_bid, start_money - current_money)
      if (runif(1) < (num_numbers - num_zeros)/2/num_numbers){
        outcome <- 1
      } else {
        outcome <- -1
      }
      current_money <- current_money + outcome * bid
    }
    
    if (current_money > 0){
      won <- won + 1
    }
  }
  
  print(paste("You have won ", won/sim_num*100,"% of the games", sep = ""))
  
  return(won/sim_num*100)
}