#' Compare estimated means
#' 
#' Conduct Significance, Equivalence, Non-inferiority and Superiorty tests between means.
#' 
#' @param object model
#' @param specs character
#' @param upper_margin numeric
#' @param method c("sd","eq","noi","sup")
#' @param level numeric
#' @param make_plot logical TRUE
#' 
#' @return em data.frame
#' @export 
#' @examples 
#' data(CO2)
#' fm <- lm(uptake ~ Plant + conc, data = CO2)
#' compare_means(fm, "Plant", 15.0,
#'     method = c("sd", "eq"),
#'     level = 0.99)
#' 
compare_means <- function(object, specs,
    upper_margin, lower_margin = -upper_margin,
    method = c("sd", "eq"),
    level = 0.99,
    make_plot = TRUE) {
    em <- emmeans::emmeans(object, specs,  method = "mvt")
    pc <- pairs(em)
    ci <- confint(pc, level = 0.99)
    em <- as.data.frame(em)
    if ("sd" %in% method) {
        em$sd <- calcSD(ci)
    }
    if ("eq" %in% method) {
        em$eq <- calcEQ(ci, upper_margin, lower_margin, make_plot)
    }
    if ("noi" %in% method) {
        em$noi <- calcNOI(ci, upper_margin, lower_margin, make_plot)
    }
    if ("sup" %in% method) {
        em$sd <- calcSUP(ci, upper_margin, lower_margin, make_plot)
    }
    return(em)
}


calcSD <- function(ci, make_plot=TRUE) {
    # 1
    comps = ci$contrast
    labels <- do.call(rbind, strsplit(comps, " - "))
    lower.CL = ci$lower.CL
    upper.CL = ci$upper.CL
    # 3
    sdH1  = lower.CL > 0 | upper.CL < 0
    # 4
    H1 <- sdH1
    V <- unique(c(labels))
    E <- c(t(labels[!H1,]))
    isolates <- setdiff(V, unique(c(E)))
    sdGr <- igraph::graph(E, isolates = isolates, directed = F)
    if (make_plot) plot(sdGr)
    mc <- igraph::max_cliques(sdGr)
    out <- character(length(V))
    for(i in seq_along(mc)) {
        out <- paste0(out, ifelse(V %in% names(mc[[i]]), letters[i], ""))
    }
    return(out)
}

calcEQ <- function(ci, upper_margin, lower_margin, make_plot=TRUE) {
    # 1
    comps = ci$contrast
    labels <- do.call(rbind, strsplit(comps, " - "))
    lower.CL = ci$lower.CL
    upper.CL = ci$upper.CL
    # 3
    eqH1  = (lower_margin < lower.CL) & (upper.CL < upper_margin)
    # 4
    H1 <- eqH1
    V <- unique(c(labels))
    E <- c(t(labels[H1,]))
    isolates <- setdiff(V, unique(c(E)))
    sdGr <- igraph::graph(E, isolates = isolates, directed = F)
    if (make_plot) plot(sdGr)
    mc <- igraph::max_cliques(sdGr)
    out <- character(length(V))
    for(i in seq_along(mc)) {
        out <- paste0(out, ifelse(V %in% names(mc[[i]]), letters[i], ""))
    }
    return(out)
}

calcNOI <- function(ci, upper_margin, lower_margin) {
    stop("Non inferiority test to be implemented")
}

calcSUP <- function(ci, upper_margin, lower_margin) {
    stop("Superiority test to be implemented")
}
