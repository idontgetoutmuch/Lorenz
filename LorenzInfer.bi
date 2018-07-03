model LorenzInfer {
  const rho = 45.92
  const beta = 4

  param mu, sigma
  state X, Y, Z
  noise alpha
  obs X_obs

  sub parameter {
    mu ~ uniform(12.00, 20.00)
    sigma ~ uniform(0.0, 0.5)
  }

  sub initial {
    X ~ log_normal(log(1.0), 0.2)
    Y ~ log_normal(log(1.0), 0.2)
    Z ~ log_normal(log(1.0), 0.2)
  }

  sub transition(delta = 0.0001) {
    alpha ~ gaussian(mu, sigma)
    ode {
      dX/dt = alpha * (Y - X)
      dY/dt = X * (rho - Z) - Y
      dZ/dt = X * Y - beta * Z
    }
  }

  sub observation {
    X_obs ~ log_normal(X, 0.2)
  }
}
