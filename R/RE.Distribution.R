djohnson <- function(x, gamma, eta, epsilon, lambda) {
    z <- (x - epsilon) / lambda
    y <- gamma + eta * log(z / (1 - z))

    if (lambda > 0 && eta > 0) { # SL form
        density <- (eta / (x * (1 - x))) * dnorm(y)
    } else if (lambda > 0) { # SU form
        density <- eta / ((1 + exp(-y)) * (1 - exp(-y))) * dnorm(y)
    } else if (eta > 0) { # SB form
        density <- eta / ((1 + exp(y)) * (1 - exp(y))) * dnorm(y)
    } else {
        stop("Invalid parameters for Johnson distribution")
    }

    return(density)
}
