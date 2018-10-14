# Black-Scholes formula for the price of european options without dividends
BS_Price <-function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  d2 <- d1 - Sigma*sqrt(Maturity)
  
  if(opt_type == 'c')
  {
    return(S0*exp(-div*Maturity)*pnorm(d1, 0, 1) - K*exp(-r*Maturity)*pnorm(d2, 0, 1))
  }
  else
  {
    return(-S0*exp(-div*Maturity)*pnorm(-d1, 0, 1) + K*exp(-r*Maturity)*pnorm(-d2, 0, 1))
  }
}


Delta <- function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  
  if(opt_type == 'c')
  {
    return(exp(-div*Maturity)*pnorm(d1, 0, 1))
  }
  else
  {
    return(exp(-div*Maturity)*(pnorm(d1, 0, 1)-1))
  }
}

Gamma <- function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  
  return(exp(-div*Maturity)*dnorm(d1, 0, 1)/(S0*Sigma*sqrt(Maturity)))
}

Theta <- function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  d2 <- d1 - Sigma*sqrt(Maturity)
  
  if(opt_type == 'c')
  {
    return(-S0*exp(-div*Maturity)*dnorm(d1, 0, 1)*Sigma/(2*sqrt(Maturity))
           +div*S0*exp(-div*Maturity)*pnorm(d1, 0, 1)
           -r*K*exp(-r*Maturity)*pnorm(d2, 0, 1))
  }
  else
  {
    return(-S0*exp(-div*Maturity)*dnorm(d1, 0, 1)*Sigma/(2*sqrt(Maturity))
           -div*S0*exp(-div*Maturity)*pnorm(-d1, 0, 1)
           +r*K*exp(-r*Maturity)*pnorm(-d2, 0, 1))
  }
}

Vega <- function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  
  return(S0*exp(-div*Maturity)*sqrt(Maturity)*dnorm(d1, 0, 1))
}

Rho <- function(data, K, Sigma, r, div, opt_type)
{
  S0 = data['Var2']
  Maturity = data['Var1']
  
  d1 <- (log(S0/K)+(r - div + 0.5*Sigma^2)*Maturity)/(Sigma*sqrt(Maturity))
  d2 <- d1 - Sigma*sqrt(Maturity)
  
  if(opt_type == 'c')
  {
    return(K*Maturity*exp(-r*Maturity)*pnorm(d2, 0, 1))
  }
  else
  {
    return(-K*Maturity*exp(-r*Maturity)*pnorm(-d2, 0, 1))
  }
}