welch_t_test <- function(outcome, group) {
  if (!is.numeric(outcome)) {
    stop("The outcome must be numeric.")
  }
  test <- t.test(outcome ~ group)
  
  list(
    statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value,
    confidence_interval = test$conf.int,
    #The difference order absolutely matters for conf. int.
    mean_difference = unname(test$estimate[1] - test$estimate[2]),
    group_means = test$estimate
  )
  #test for equivalent means 
  #no assumption of equal variance
  #R defaults to this when 2 samples are independent
}


