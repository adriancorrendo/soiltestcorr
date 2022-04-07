#' @name linear_plateau
#' @title Linear Plateau
#' @description This function helps to fit a linear-plateau model in order to
#' estimate critical soil test values (CSTV)
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the STV and RY data, Default: NULL
#' @param STV name of the vector containing soil test values (-) of type `numeric`.
#' @param RY name of the vector containing relative yield values (%) of type `numeric`.
#' @param resid boolean (TRUE/FALSE) to plot residuals analysis, Default: FALSE
#' @param plot boolean (TRUE/FALSE) to plot the linear-plateau model, Default: FALSE
#' Default: FALSE
#' @rdname linear_plateau
#' @return returns a `data.frame` if plot = FALSE, if plot = TRUE
#' @details This functions fits a linear-plateau model using selfStart initial values
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # Example dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example_lp <- linear_plateau(data = dat, RY = ry, STV = stv, resid = TRUE, plot = FALSE)
#'  fit_example_lp
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[minpack.lm]{nlsLM}}
#'  \code{\link[stats]{AIC}},\code{\link[stats]{lm}},\code{\link[stats]{optim}},\code{\link[stats]{coef}},\code{\link[stats]{predict}}
#'  \code{\link[AICcmodavg]{AICc}}
#'  \code{\link[modelr]{model-quality}}
#'  \code{\link[nlstools]{nlsResiduals}}
#'  \code{\link[tidyr]{reexports}}
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_rug}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggpp]{annotate}}
#' @export
#' @importFrom rlang eval_tidy quo
#' @importFrom minpack.lm nlsLM
#' @importFrom stats sortedXyData AIC lm optim coef predict
#' @importFrom AICcmodavg AICc
#' @importFrom modelr rsquare
#' @importFrom nlstools nlsResiduals confint2
#' @importFrom dplyr bind_cols %>%
#' @importFrom ggplot2 ggplot aes geom_rug geom_point geom_vline geom_hline geom_path annotate scale_y_continuous labs theme_bw theme unit rel element_blank element_text
#' @importFrom stats lm AIC optim coef predict
#' 
NULL


LP_init <- function(mCall, LHS, data, ...){
  
  ### STAGE 1 ====================================================================
  # Self start functions (adapted from nlraa package by Fernando Miguez)
  # Original source: https://github.com/femiguez/nlraa
  # Define the linear-plateau function (LP_f) 
  
  # Initialize function
  
  xy <- stats::sortedXyData(mCall[["x"]], LHS, data)
  if(nrow(xy) < 4){
    stop("Too few distinct input values to fit a linear-plateau model")
  }
  
  # Start with a guess from simple lm to half the data
  xy1 <- xy[1:floor(nrow(xy)/2),]
  fit1 <- stats::lm(xy1[,"y"] ~ xy1[,"x"])
  
  ## Optimization of starting values
  objfun <- function(cfs){
    pred <- LP_f(xy[,"x"], intercept=cfs[1], slope=cfs[2], Xc=cfs[3])
    ans <- sum((xy[,"y"] - pred)^2)
    ans
  }
  # optimization
  cfs <- c(coef(fit1),mean(xy[,"x"]))
  op <- try(stats::optim(cfs, objfun, method = "L-BFGS-B",
                         upper = c(Inf, Inf, max(xy[,"x"])),
                         lower = c(-Inf, -Inf, min(xy[,"x"]))), silent = TRUE)
  
  if(class(op) != "try-error"){
    intercept <- op$par[1]
    slope <- op$par[2]
    Xc <- op$par[3]
  } else {
    ## If it fails I use the mean for the CSTV (Xc)
    ## and initial values guess by fiting a lm() to half the data
    
    intercept <- stats::coef(fit1)[1]
    slope <- stats::coef(fit1)[2]
    Xc <- mean(xy[,"x"])
  }
  
  value <- c(intercept, slope, Xc)
  names(value) <- mCall[c("intercept","slope","Xc")]
  value
}

#' @rdname linear_plateau
#' @return LP_f: vector of the same length as x using the linear-plateau function
#' @export
#' 
LP_f <- function(x, intercept, slope, Xc){
  
  .asym <- intercept + slope * Xc
  .value <- (x < Xc) * (intercept + slope * x) + (x >= Xc) * .asym
  
  ## Derivative with respect to a when (x < Xc)
  .exp1 <- 1 ## ifelse(x < Xc, 1, 1)
  ## Derivative with respect to slope
  .exp2 <- ifelse(x < Xc, x, Xc)
  ## Derivative with respect to Xc
  .exp3 <- ifelse(x < Xc, 0, slope)
  
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

#' @rdname linear_plateau
#' @return SS_LP: selfStart object to pass into the linear_plateau fit
#' @export 
SS_LP <- stats::selfStart(LP_f, initial = LP_init, c("intercept","slope","Xc"))

#' @rdname linear_plateau
#' @return linear_plateau: function
#' @export 
linear_plateau <- function(data = NULL,
                                STV,
                                RY,
                                resid = FALSE,
                                plot = FALSE) {
  
  # Re-define x and y from STV and RY
  x <- rlang::eval_tidy(data = data, rlang::quo({{STV}}) )
  
  y <- rlang::eval_tidy(data = data, rlang::quo({{RY}}) )
  
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
  
  # Run the model combining minpack.lm::nlsLM + defined selfStart (SS_LP)
  lp_model <-
    try(minpack.lm::nlsLM(y ~ SS_LP(x, b0, b1, CSTV), data = test.data))
  
  if (inherits(lp_model, "try-error")) {
    stop("Linear-plateau model did not converge. Please, consider other models.")
  } else {
    lp_model <- lp_model
  }
  
  # Find AIC and pseudo R-squared
  # AIC 
  # It makes sense because it's a sort of "simulation" (using training data) to 
  # test what would happen with out of sample data
  AIC <- round(stats::AIC(lp_model), 0)
  AICc <- round(AICcmodavg::AICc(lp_model), 0)
  # R2
  R2 <- 1-round(modelr::rsquare(lp_model, test.data), 2)
  
  # get model coefficients
  b0 <- stats::coef(lp_model)[[1]]
  b1 <- stats::coef(lp_model)[[2]]
  CSTV <- round(stats::coef(lp_model)[[3]], 1)
  # confidence interval for CSTV
  lp_model.confint <- nlstools::confint2(lp_model)
  CSTV_lower <- lp_model.confint[[3,1]]
  CSTV_upper <- lp_model.confint[[3,2]]
  plateau <- b0 + b1 * CSTV
  
  # have to make a line because the SS_LP doesn't plot right
  lp_line <- data.frame(x = c(minx, CSTV, maxx),
                    y = c(b0 + b1 * minx, plateau, plateau))
  
  equation <- paste0(round(b0, 1), " + ",
                     round(b1, 2), "x")
  
  ## STAGE 3 ====================================================================
  ## Outputs
  # Table output =================================================
  if (plot == FALSE) {
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(lp_model), which = 0)
    }
    data.frame(
      intercept = round(b0, 2),
      slope = round(b1, 2),
      equation,
      CSTV = round(CSTV, 1),
      LL = round(CSTV_lower,1),
      UL = round(CSTV_upper,1),
      plateau = round(plateau, 1),
      AIC,
      AICc,
      R2
      
    )
  } else {
    # Residual plots and normality
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(lp_model), which = 0)
    }
    
    # Generate predicted values
    predicted <- stats::predict(lp_model, newdata = test.data) %>%
      as.data.frame() %>%
      dplyr::bind_cols(test.data)
    
    # GGPLOT output =================================================   
    lp_plot <- predicted %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
      # Data points
      ggplot2::geom_point(shape = 21, size = 3, alpha = 0.75, fill = "#e09f3e") +
      # CSTV
      ggplot2::geom_vline(xintercept = CSTV, alpha = 1, color = "#13274F", 
                          size = 0.5, linetype = "dashed") +
      # CI
      geom_vline(xintercept = CSTV_lower, col = "grey25", size = 0.25, linetype = "dotted")+
      geom_vline(xintercept = CSTV_upper, col = "grey25", size = 0.25, linetype = "dotted")+
      # Plateau
      ggplot2::geom_hline(yintercept = plateau, alpha = 0.2) +
      # LP Curve
      ggplot2::geom_path(data = lp_line, ggplot2::aes(x=x,y=y), color="grey15", size = 1.5) +
      # Text annotations
      ggplot2::annotate("text",label = paste("CSTV =", CSTV, "ppm"),
                        x = CSTV, y = 0, angle = 90, hjust = 0, vjust = 1.5, col = "grey25") +
      ggplot2::annotate("text",label = paste0("Plateau = ", round(plateau, 1), "%"),
                        x = maxx, y = plateau, hjust = 1,vjust = 1.5, col = "grey25") +
      ggplot2::annotate("text", col = "grey25",
                        label = paste0("y = ", equation, ", n = ", nrow(test.data),
                                       "\npseudo-R2 = ", R2,
                                       "\nAICc = ", AICc,
                                       "\nCI = [", round(CSTV_lower,1)," - ", round(CSTV_upper,1),"]"),
                        x = maxx, y = 0, vjust = 0, hjust = 1) +
      ggplot2::scale_y_continuous(limits = c(0, maxy),breaks=seq(0,maxy*2,10)) +
      ggplot2::labs(x = "Soil test value (units)", y = "Relative yield (%)",
                    title = "Linear-plateau")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
    
    return(lp_plot)
  }
  
}
