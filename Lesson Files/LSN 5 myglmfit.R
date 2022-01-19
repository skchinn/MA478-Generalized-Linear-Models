pfamily <- poisson()

names(pfamily)

myglmfit <- function(y, X, family, tol=1e-8, maxit=50){
  
  # Set initial values
  mu <- y
  oldbeta <- rep(0, ncol(X))
  beta <- rep(1, ncol(X))
  it <- 1
  
  while(it < maxit && max(abs((1-beta/oldbeta))) > tol){
    
    # Set oldbeta to incumbent
    oldbeta <- beta
    
    # Compute the linear predictor, eta
    eta <- family$linkfun(mu)
    
    # Calculate d(mu)/d(eta)
    mm <- family$mu.eta(eta)
    
    # Form the Adjusted Dependent Variable (ADV)
    adjdev <- eta + (y-mu)/mm
    
    # Form the weight
    w <- c(1 / (mm^2 * family$variance(mu)))
    
    # Re-estimate to get beta
    beta <- lm.wfit(X, adjdev, w)$coefficients
    
    # Compute the new mu
    mu <- family$linkinv(X %*% beta)
    
    # Update the iteration
    it <- it + 1
  }
  beta
}

X <- model.matrix(~wool*tension, data = warpbreaks)

y <- warpbreaks$breaks

myglmfit(y, X, poisson())

coef(glm(breaks~wool*tension, data=warpbreaks, family=poisson))
