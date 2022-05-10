#' @name quadratic_plateau
#' @title Quadratic-plateau response function
#' @description This function helps to fit a quadratic-plateau response model and to
#' estimate a critical soil test values (CSTV) above which yield response becomes flat.
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the stv and ry data, Default: NULL
#' @param stv name of the vector containing soil test values (-) of type `numeric`.
#' @param ry name of the vector containing relative yield values (%) of type `numeric`.
#' @param target `numeric` value of relative yield target (e.g. 90 for 90%) to estimate the CSTV.
#' The target needs to be < plateau, otherwise, target = plateau.
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a data.frame, FALSE returns a list (default).
#' @param resid logical operator (TRUE/FALSE) to plot residuals analysis, Default: FALSE
#' @param plot logical operator (TRUE/FALSE) to plot the quadratic-plateau model, Default: FALSE
#' @param x selfstart vector for independent variable, Default: NULL
#' @param intercept selfstart arg. for intercept Default: NULL
#' @param slope selfstart arg. for slope Default: NULL
#' @param Xc selfstart arg. for critical value Default: NULL
#' @rdname quadratic_plateau
#' @return returns an object of type `ggplot` if plot = TRUE.
#' @return returns a residuals plot if resid = TRUE.
#' @return returns an object of class `data.frame` if tidy = TRUE, 
#' @return returns an object of class `list` if tidy = FALSE.
#' @details See [online-documentation](https://adriancorrendo.github.io/soiltestcorr/articles/quadratic_plateau_tutorial.html) for additional details.
#' @references
#' Bullock, D.G. and Bullock, D.S. (1994) 
#' Quadratic and Quadratic-Plus-Plateau Models for Predicting Optimal Nitrogen Rate of Corn: A Comparison. 
#' _Agron. J., 86: 191-195._ \doi{10.2134/agronj1994.00021962008600010033x}
#' @examples 
#' \donttest{
#' # Example dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example_qp <- quadratic_plateau(data = dat, 
#'  ry = ry, stv = stv, resid = TRUE, plot = FALSE)
#'  fit_example_qp
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[minpack.lm]{nlsLM}}
#'  \code{\link[stats]{AIC}},\code{\link[stats]{lm}},\code{\link[stats]{optim}},\code{\link[stats]{coef}},\code{\link[stats]{predict}}
#'  \code{\link[AICcmodavg]{AICc}}
#'  \code{\link[modelr]{model-quality}}
#'  \code{\link[nlstools]{nlsResiduals}}
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_rug}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggpp]{annotate}}
#' @note For extended reference, we recommend to visit 
#' <https://gradcylinder.org/quad-plateau/> & <https://github.com/austinwpearce/SoilTestCocaCola>
#' by Austin Pearce.
#' Self-start function code adapted from nlraa package by F. Miguez <https://github.com/femiguez/nlraa>
#' @export
#' @importFrom rlang eval_tidy quo
#' @importFrom minpack.lm nlsLM
#' @importFrom stats sortedXyData AIC lm optim coef predict
#' @importFrom AICcmodavg AICc
#' @importFrom modelr rsquare
#' @importFrom nlstools nlsResiduals confint2
#' @importFrom dplyr bind_cols %>% mutate
#' @importFrom ggplot2 ggplot aes geom_rug geom_point geom_vline geom_hline geom_path annotate scale_y_continuous labs theme_bw theme unit rel element_blank element_text
#' @importFrom stats lm AIC optim coef predict
#' 
NULL


QP_init <- function(mCall, LHS, data, ...){
  
  ### STAGE 1 ====================================================================
  # Self start functions (adapted from nlraa package by Fernando Miguez)
  # Original source: https://github.com/femiguez/nlraa
  
  # Initialization function
  
  xy <- sortedXyData(mCall[["x"]], LHS, data)
  if(nrow(xy) < 3){
    stop("Too few distinct input values to fit a quadratic-platueau-3-Xc.")
  }
  ## Guess for a, b and Xc is to fit a quadratic regression to all the data
  fit <- lm(xy[,"y"] ~ xy[,"x"] + I(xy[,"x"]^2))
  a <- coef(fit)[1]
  b <- coef(fit)[2]
  c <- coef(fit)[3]
  Xc <- -0.5 * b/c
  ## If I fix a and b maybe I can try to optimze Xc only
  value <- c(a, b, Xc)
  names(value) <- mCall[c("intercept","slope","Xc")]
  value
}

#' @rdname quadratic_plateau
#' @return QP_f: vector of the same length as x using the quadratic-plateau function
#' @export
#' 
QP_f <- function(x, intercept, slope, Xc){
  
  .value <- (x <= Xc) * (intercept + slope * x + (-0.5 * slope/Xc) * x^2) + (x > Xc) * (intercept + (-slope^2)/(4 * -0.5 * slope/Xc))
  
  ## Derivative with respect to intercept
  .exp1 <- 1 
  
  ## Derivative with respect to slope
  ## .exp2 <- deriv(~ intercept + slope * x + (-0.5 * slope/Xc) * x^2, "slope")
  ## .exp2slope <- deriv(~ intercept + (-slope^2)/(4 * -0.5 * slope/Xc), "slope")
  .expr2 <- -slope^2
  .expr4 <- 4 * -0.5
  .expr6 <- .expr4 * slope/Xc
  .exp2 <- ifelse(x <= Xc, x - 0.5/Xc * x^2, -(2 * slope/.expr6 + .expr2 * (.expr4/Xc)/.expr6^2))
  
  ## Derivative with respect to Xc
  ## .exp2 <- deriv(~ (intercept + slope * x + (-0.5 * slope/Xc) * x^2), "Xc")
  ## .exp2slope <- deriv(~ a + (-slope^2)/(4 * -0.5 * slope/Xc), "Xc")
  .expr4 <- -0.5 * slope
  .expr6 <- x^2
  .expr2 <- -slope^2
  .expr5 <- 4 * -0.5 * slope
  .expr7 <- .expr5/Xc
  .exp3 <- ifelse(x <= Xc, -(.expr4/Xc^2 * .expr6), .expr2 * (.expr5/Xc^2)/.expr7^2)
  
  .actualArgs <- as.list(match.call()[c("intercept","slope","Xc")])
  
  ##  Gradient
  if (all(unlist(lapply(.actualArgs, is.name)))) {
    .grad <- array(0, c(length(.value), 3L), list(NULL, c("intercept","slope","Xc")))
    .grad[, "intercept"] <- .exp1
    .grad[, "slope"] <- .exp2
    .grad[, "Xc"] <- .exp3
    dimnames(.grad) <- list(NULL, .actualArgs)
    attr(.value, "gradient") <- .grad
  }
  .value
}

#' @rdname quadratic_plateau
#' @return SS_QP: selfStart object to pass into the quadratic_plateau fit
#' @export 
SS_QP <- stats::selfStart(QP_f, initial = QP_init, c("intercept","slope","Xc"))

#' @rdname quadratic_plateau
#' @return quadratic_plateau: function
#' @export 
quadratic_plateau <- function(data = NULL,
                                stv,
                                ry,
                                target = NULL,
                                tidy = FALSE,
                                plot = FALSE,
                                resid = FALSE
                                ) {
  if (missing(stv)) {
    stop("Please specify the variable name for soil test values using the `stv` argument")
  }
  
  if (missing(ry)) {
    stop("Please specify the variable name for relative yields using the `ry` argument")
  }
  
  # Re-define x and y from stv and ry
  x <- rlang::eval_tidy(data = data, rlang::quo({{stv}}) )
  
  y <- rlang::eval_tidy(data = data, rlang::quo({{ry}}) )
  
  # Create data.frame if it doesn't exist yet (data from vectors)
  test.data <- data.frame(x=x, y=y)
  
  # Error message for insufficient sample size
  if (length(x) < 4) {
    stop("Too few distinct input values to fit LP. Try at least 4.")
  }
  
  # Extreme values
  minx <- min(test.data$x)
  maxx <- max(test.data$x)
  miny <- min(test.data$y)
  maxy <- max(test.data$y)
  
  # Run the model combining minpack.lm::nlsLM + defined selfStart (SS_QP)
  qpmodel <-
    try(minpack.lm::nlsLM(y ~ SS_QP(x, b0, b1, CSTV), data = test.data))
  
  if (inherits(qpmodel, "try-error")) {
    stop("Quadratic-plateau model did not converge. Please, consider other models.")
  } else {
    qpmodel <- qpmodel
  }
  
  # Find AIC and pseudo R-squared
  # AIC 
  # It makes sense because it's a sort of "simulation" (using training data) to 
  # test what would happen with out of sample data
  AIC <- round(stats::AIC(qpmodel), 0)
  AICc <- round(AICcmodavg::AICc(qpmodel), 0)
  # R2
  R2 <- round(modelr::rsquare(qpmodel, test.data), 2)
  
  # get model coefficients
  b0 <- stats::coef(qpmodel)[[1]]
  b1 <- stats::coef(qpmodel)[[2]]
  b2 <- -0.5 * b1 / stats::coef(qpmodel)[[3]]
  #CSTV <- round(stats::coef(qpmodel)[[3]], 1)
  
  # CSTV for plateau or for target
  
  if (!is.null(target)) { 
    if (target >= stats::coef(qpmodel)[[3]]) {
      warning("You have specified a relative yield target equal or greater than the CSTV for plateau. 
          The CSTV estimations have been changed for the plateau level", 
              call. = FALSE) 
    }
  }
  
  CSTV <- ifelse(is.null(target), 
                 round(stats::coef(qpmodel)[[3]], 1),
                 ifelse(target >= stats::coef(qpmodel)[[3]],
                        (-b1 + sqrt((b1 ^ 2) - (4 * b2 * (b0 - target) ))) / (2 * b2),
                        (-b1 + sqrt((b1 ^ 2) - (4 * b2 * (b0 - target) ))) / (2 * b2) )  )
  
  # confidence interval for CSTV
  qpmodel.confint <- nlstools::confint2(qpmodel)
  CSTV_lower <- qpmodel.confint[[3,1]]
  CSTV_upper <- qpmodel.confint[[3,2]]
  plateau <- b0 + b1 * stats::coef(qpmodel)[[3]] + b2 * (stats::coef(qpmodel)[[3]])^2
  
  # have to make a line because the SS_QP doesn't plot right
  qp_line <- data.frame(x = seq(minx, maxx, by = maxx/200)) %>%
                      dplyr::mutate(y = ifelse(x < stats::coef(qpmodel)[[3]], 
                                        b0 + b1 * x + b2*x^2,
                                        b0 + b1 * stats::coef(qpmodel)[[3]] + b2*stats::coef(qpmodel)[[3]]^2))
  
  equation <- paste0(round(b0, 1), " + ",
                     round(b1, 2), "x + ",
                     round(b2, 2), "x^2 if x<CSTV")
  ## STAGE 3 ====================================================================
  
  ## Outputs
  # Table output =================================================
  if (plot == FALSE) {
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(qpmodel), which = 0)
    }
    results <- data.frame(
      intercept = round(b0, 2),
      slope = round(b1, 2),
      equation,
      plateau = round(plateau, 1),
      target = ifelse(!is.null(target),
                      target,
                      round(plateau, 1)),
      CSTV = round(CSTV, 1),
      LL_cxp = round(CSTV_lower,1),
      UL_cxp = round(CSTV_upper,1),
      AIC,
      AICc,
      R2
      )
    
    # Decide type of output
    if (tidy == TRUE) {results <- results}
    
    if (tidy == FALSE) {results <- as.list(results)}
    
    return(results)
    
  } else {
    # Residual plots and normality
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(qpmodel), which = 0)
    }
    
    # Generate predicted values
    predicted <- stats::predict(qpmodel, newdata = test.data) %>%
      as.data.frame() %>%
      dplyr::bind_cols(test.data)
    
    # GGPLOT output =================================================   
    qp_plot <- predicted %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
      # Data points
      ggplot2::geom_point(shape = 21, size = 3, alpha = 0.75, fill = "#e09f3e") +
      # CSTV
      ggplot2::geom_vline(xintercept = CSTV, alpha = 1, color = "#13274F", 
                          size = 0.5, linetype = "dashed") +
      # CI
      { if (is.null(target)) 
            geom_vline(xintercept = CSTV_lower, col = "grey25", size = 0.25, linetype = "dotted") } +
      { if (is.null(target))
            geom_vline(xintercept = CSTV_upper, col = "grey25", size = 0.25, linetype = "dotted") } +
      # Plateau
      ggplot2::geom_hline(yintercept = plateau, alpha = 0.2) +
      # LP Curve
      ggplot2::geom_path(data = qp_line, ggplot2::aes(x=x,y=y), color="grey15", size = 1.5) +
      # Text annotations
      ggplot2::annotate("text",label = paste("CSTV =", round(CSTV,1), "ppm"),
                        x = CSTV, y = 0, angle = 90, hjust = 0, vjust = 1.5, col = "grey25") +
      ggplot2::annotate("text",label = paste0("Plateau = ", round(plateau, 1), "%"),
                        x = maxx, y = plateau, hjust = 1,vjust = 1.5, col = "grey25") +
      ggplot2::annotate("text", col = "grey25",
                        label = paste0("y = ", equation,
                                       "\nn = ", nrow(test.data),
                                       "\npseudo-R2 = ", R2,
                                       "\nAICc = ", AICc,
                                       "\nCI = [", round(CSTV_lower,1)," - ", round(CSTV_upper,1),"]"),
                        x = maxx, y = 0, vjust = 0, hjust = 1) +
      ggplot2::scale_y_continuous(limits = c(0, maxy),breaks=seq(0,maxy*2,10)) +
      ggplot2::labs(x = "Soil test value (units)", y = "Relative yield (%)",
                    title = "Quadratic-plateau")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
    
    return(qp_plot)
  }
  
}
