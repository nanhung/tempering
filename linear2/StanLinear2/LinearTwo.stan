/* ****************************************************
  LinearTwo.stan

  read LinearTwo.html or LinearTwo.nb.html for more details
**************************************************** */

data {
  int<lower=0>    nb       ;
  real            x   [nb] ;
  real            y1  [nb] ;
  real            y2  [nb] ;
}

parameters {
  real beta       ;
  real log_sigma1 ;
  real log_sigma2 ;
}

transformed parameters {
  real sigma1 ;
  real sigma2 ;
  
  sigma1 = exp(log_sigma1) ;
  sigma2 = exp(log_sigma2) ;
}

model {
  beta   ~ uniform(-10, 10) ;
  log_sigma1 ~ uniform( log(0.001) , log(100));
  log_sigma2 ~ uniform( log(0.001) , log(100));
  
  for(i in 1:nb){
    y1[i] ~ normal(beta * x[i], sigma1) ;
    y2[i] ~ normal(beta * x[i], sigma2) ;    
  }
}
