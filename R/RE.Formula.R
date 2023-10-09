RE.Formula <- function(method = c("SB", "SL", "SU"), use_values = NULL) {
    if (is.null(use_values)) {
        gamma <- "\u03B3"
        eta <- "\u03B7"
        lambda <- "\u03BB"
        epsilon <- "\u03B5"
    } else {
        gamma <- round(use_values$gamma, 5)
        eta <- round(use_values$eta, 5)
        lambda <- round(use_values$lambda, 5)
        epsilon <- round(use_values$epsilon, 5)
    }

    if (method == "SB") {
        formula <- paste0(gamma, " + ", eta, " * log((X - ", epsilon, ") / (", lambda, " + ", epsilon, " - X))")
    } else if (method == "SL") {
        formula <- paste0(gamma, " + ", eta, " * log(X - ", epsilon, ")")
    } else if (method == "SU") {
        formula <- paste0(gamma, " + ", eta, " * asinh((X - ", epsilon, ") / ", lambda, ")")
    } else {
        stop("method must be one of 'SB', 'SL', or 'SU'")
    }

    return(formula)
}
