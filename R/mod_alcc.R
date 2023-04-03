#' @name mod_alcc
#' @title Modified Arcsine-Log Calibration Curve
#' @description This function runs the modified arcsine-log calibration curve to
#' estimate critical soil test values (CSTV) following Correndo et al. (2017)
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the stv and ry data, Default: NULL
#' @param stv name of the vector containing soil test values of type `numeric`.
#' @param ry name of the vector containing relative yield values (%) of type `numeric`.
#' @param target `numeric` value of relative yield target (e.g. 90 for 90%) to estimate the CSTV.
#' @param confidence `numeric` value of confidence level (e.g. 0.95 for 
#' significance = 0.05)
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a tidy data frame or tibble (default), FALSE returns a list.
#' @param plot logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a ggplot,
#' FALSE returns either a list (tidy == FALSE) or a data.frame (tidy == TRUE).
#' @param object the "object" is the output data frame from approx with resid column
#' @param n sample size for the bootstrapping Default: 500
#' @param ... when running bootstrapped samples, the `...` (open arguments) allows to add grouping variable/s (factor or character) Default: NULL  
#' @rdname mod_alcc
#' @return returns an object of type `ggplot` if plot = TRUE.
#' @return returns an object of class `data.frame` if tidy = TRUE, 
#' @return returns an object of class `list` if tidy = FALSE.
#' @details See [online-documentation](https://adriancorrendo.github.io/soiltestcorr/articles/mod_alcc_tutorial.html) for additional details.
#' @references 
#' Correndo et al. (2017).
#' A modification of the arcsine–log calibration curve for analysing soil test value–relative yield relationships. 
#' _Crop and Pasture Science, 68(3), 297-304._ \doi{10.1071/CP16444}
#' @examples 
#' \donttest{
#'  # Example 1 dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example <- mod_alcc(data = dat, ry = ry, stv = stv, target=90, confidence = 0.95)
#'  fit_example
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[stats]{TDist}},\code{\link[stats]{cor}},\code{\link[stats]{cor.test}},\code{\link[stats]{sd}}, \code{\link[stats]{approx}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{geom_rug}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggpp]{annotate}}
#' @note 
#' For extended reference, we recommend to visit \doi{10.7910/DVN/NABA57} and 
#' <https://github.com/adriancorrendo/modified-ALCC> by Adrian Correndo.
#' @export 
#' @importFrom rlang eval_tidy quo enquo
#' @importFrom stats qt cor cor.test sd AIC BIC approx
#' @importFrom dplyr bind_cols filter %>% select group_by mutate slice_sample as_tibble ungroup
#' @importFrom tidyr nest unnest expand_grid
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual geom_rug geom_hline geom_vline geom_path scale_y_continuous annotate labs theme_bw theme annotate
#' @importFrom purrr map possibly
#' @importFrom smatr sma

mod_alcc <- function(data=NULL, 
                     ry, 
                     stv, 
                     target, 
                     confidence = 0.95, 
                     tidy = TRUE,
                     plot = FALSE
){
  
  if (missing(stv)) {
    stop("Please specify the variable name for soil test values using the `stv` argument")
  }
  
  if (missing(ry)) {
    stop("Please specify the variable name for relative yield using the `ry` argument")
  }
  
  if (missing(target)) {
    stop("Please specify the relative yield target to estimate the critical soil test value using the
         the `target` argument (e.g. target = 90)")
  }
  
  if (missing(confidence)) {warning("You have not specified the confidence level. 
                                Please, modify if your desired confidence is different than the default (0.95)", 
                                call. = FALSE) 
  }
  
  ### STAGE 1 ====================================================================
  # 1.1. Add a function to cap if there are RY values > 100
  RY <- rlang::eval_tidy(data = data, rlang::quo(ifelse({{ry}} > 100, 100, as.double({{ry}})) ))
  
  # 1.2. Re-define STV as stv
  STV <- rlang::eval_tidy(data = data, rlang::quo({{stv}}))
  maxx <- max(STV)
  
  # 1.3. Extract values for estimation of SMA parameters
  n <- length(RY) # Sample size
  df <- n - 2 # Degrees of freedom
  prob <- 1-((1-confidence)/2) # Probability for t-dist
  tvalue <- stats::qt(p=prob, df = df) # Student-t value
  arc_RY <- asin(sqrt(RY/100)) - asin(sqrt(target/100)) # RY transformation (centered to target)
  ln_STV <- log(STV) # STV natural log transformation
  r <- stats::cor(ln_STV, arc_RY, method = "pearson") # Pearson correlation (r)
  p_value <- stats::cor.test(ln_STV,arc_RY, method = "pearson")$p.value # p-value of r
  slope <- stats::sd(ln_STV)/stats::sd(arc_RY) # SMA slope for ln_STV ~ arc_RY
  intercept <- mean(ln_STV) - (mean(arc_RY)*slope) # Intercept
  SMA_line <- intercept + slope * arc_RY # Fitted ln_STV for observed RY
  
  # 1.4. Critical Soil Test Value (CSTV) and confidence interval
  target <- target # Target RY to show on summary
  confidence <- confidence # Confidence level to show on summary
  CSTV <- exp(intercept) # Critical STV for specified RY-target and confidence (1-alpha)
  MSE <- sum((SMA_line-ln_STV)^2)/df # Mean Square Error of ln_STV
  SSx <- sum((mean(arc_RY)-arc_RY)^2)  # Sum of Squares of arc_RY
  SE_int <- sqrt(MSE*((1/n)+ ((mean(arc_RY)^2)/SSx)))  # Standard Error intercept
  CSTV_lower <- exp(intercept - (tvalue * SE_int))  # Lower limit of CSTV
  CSTV_upper <- exp(intercept + (tvalue * SE_int)) # Upper limit of CSTV
  
  # 1.5. Create vectors for plots of ALCC curve and SMA regression
  new_RY <- seq(min(RY),100, by=0.2) # New RY vector up to %100 to fit curve
  new_arc_RY <- asin(sqrt(new_RY/100)) - asin(sqrt(target/100)) # Transforming new_RY vector
  fitted_Line <- intercept + slope * new_arc_RY # Fitted ln_STV for curve plot
  fitted_STV <- exp(fitted_Line) # Fitted ln_STV for new_RY
  residuals <- ln_STV - SMA_line # Residuals of SMA Regression
  fitted_axis <- ln_STV + slope * arc_RY # Fitted axis to check SMA residuals
  
  # 1.6. Goodness of fit of underlying model (SMA)
  sma_model <- smatr::sma(formula = ln_STV ~ arc_RY, method = "SMA")
  AIC_sma <- stats::AIC(sma_model)
  BIC_sma <- stats::BIC(sma_model)
  
  # 1.7. Goodness of fit of ALCC curve (on original scale)
  ## Predicted RY values of ALCC on original scale
  RY_curve <- 
    100 * sin((ln_STV -(intercept -(slope*asin(sqrt(target/100))))) / slope )^2
  
  ## Residuals of ALCC curve on original scale
  resid_alcc <- RY - RY_curve 
  
  ## RMSE for ALCC model to predict RY on original scale
  RMSE_alcc <- sqrt(sum((resid_alcc)^2)/n)
  
  ## AIC on original scale of the data
  ### Create a dataframe 
  resid_alcc_df <- data.frame(stv = STV, ry = RY,
                              fitted_ry = RY_curve,
                              resid = resid_alcc)
  ## class(approx_tbl) <- c("alcc", "data.frame")
  
  ### Degrees of freedom
  dfs_alcc <- attributes(logLik_alcc(resid_alcc_df))$df
  ### AIC estimation
  AIC_alcc <- -2 * as.numeric(logLik_alcc(resid_alcc_df)) + 2 * dfs_alcc
  
  
  # 1.8. Critical STV for RY = 90 & 100
  arc_RY_100 <- asin(sqrt(RY/100)) - asin(sqrt(1)) 
  cstv.100 <- exp(mean(ln_STV) - (mean(arc_RY_100)*(sd(ln_STV)/sd(arc_RY_100))))
  arc_RY_90 <- asin(sqrt(RY/100)) - asin(sqrt(90/100)) 
  cstv.90 <- exp(mean(ln_STV) - (mean(arc_RY_90)*(sd(ln_STV)/sd(arc_RY_90))))
  
  ## Count cases with STV > x2 cstv90 and STV > cstv100
  n.90x2 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{stv}} > (2*cstv.90))) ) )
  n.100 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{stv}} > cstv.100)) ) )
  
  # AIC on original scale of the data
  
  ### NOTE: with the RY_curve (L119), we actually don't need the approx function of the ALCC curve
  # approx_fit <- stats::approx(x = fitted_STV,
  #                      y = new_RY,
  #                      xout = STV, rule = 2)
  # resid <- RY - approx_fit$y
  # fttd <- approx_fit$y
  
  ### STAGE 2 ====================================================================
  # Outputs
  results <- 
    dplyr::bind_cols(
      # Data frame with summary
      dplyr::bind_cols(dplyr::as_tibble(list("n" = n, 
                                             "r" = r,
                                             "RMSE_alcc" = RMSE_alcc,
                                             "AIC_alcc" = AIC_alcc,
                                             "AIC_sma" = AIC_sma,
                                             "BIC_sma" = BIC_sma,
                                             "p_value" = p_value,
                                             "confidence" = confidence,
                                             "target" = target,
                                             "CSTV" = CSTV,
                                             "LL" = CSTV_lower,
                                             "UL" = CSTV_upper,
                                             "CSTV90" = cstv.90, 
                                             "n.90x2" = n.90x2,
                                             "CSTV100" = cstv.100,
                                             "n.100" = n.100 )),
                       # Data frame with Curve
                       dplyr::as_tibble(list("RY.fitted" = new_RY, 
                                             "STV.fitted" = fitted_STV)) %>%
                         tidyr::nest(Curve = c("RY.fitted", "STV.fitted")),
                       # Data frame with SMA residuals
                       dplyr::as_tibble(list("ln_STV" = ln_STV, 
                                             "arc_RY" = arc_RY,
                                             "SMA_line" = SMA_line,
                                             "residuals" = residuals,
                                             "fitted_axis" = fitted_axis)) %>%
                         tidyr::nest(SMA =  c("ln_STV", "arc_RY", "SMA_line","residuals", 
                                              "fitted_axis") ) ) 
    )
  
  # Decide type of output
  if (tidy == TRUE) {results <- dplyr::as_tibble(results)}
  
  if (tidy == FALSE) {results <- as.list(results)}
  
  # WARNINGS
  rlang::eval_tidy(data = data, rlang::quo(
    # RY > 100%
    if (max({{ry}}) > 100) {warning("One or more original RY values exceeded 100%. All RY values greater 
          than 100% have been capped to 100%.", call. = FALSE) } ) )
  # Sample size
  if (results$n <= 8) {warning(paste0("n =",n,". Limited sample size. Consider adding more 
                                      observations for a reliable calibration"), call. = FALSE) }
  # Correlation level
  if (results$r <= 0.2) {warning(paste0("r =", r, "p-value =", p_value,". Low correlation level between variables. 
                                        Please, interpret results with caution"), call. = FALSE) }
  # STV data points
  if (results$n.100 > 0) {warning(paste0(n.100," STV points exceeded the CSTV for 100% of RY.
  Risk of leverage. You may consider a sensitivity analysis by removing extreme points, 
  re-run the mod_alcc(), and check results."), call. = FALSE) }
  
  if (results$n.90x2 > 0) {warning(paste0(n.90x2," STV points exceeded two-times (2x) 
  the CSTV for 90% of RY. Risk of leverage. You may consider a sensitivity analysis by 
  removing extreme points, re-run the mod_alcc(), and check results."), call. = FALSE) }
  
  
  
  ### STAGE 3 ====================================================================
  # add this conditional check to prevent the function from
  # creating plot if just discarding it for table results
  if (plot == TRUE){
  
  # Plot
  datapoints <- data.frame(STV=STV, RY=RY)
  curve <- data.frame(fitted_STV=fitted_STV, new_RY = new_RY)
  
  modalcc.ggplot <- datapoints %>% 
    ggplot2::ggplot(ggplot2::aes(x=STV, y=RY)) +
    # Data points
    ggplot2::geom_point(shape = 21, size = 3, alpha = 0.75, fill = "#e09f3e") +
    # Highlight potential leverage points >2xCSTV90
    { if (length(STV[STV > 2*cstv.90]) > 0)
      ggplot2::geom_point(data = datapoints %>% 
                            dplyr::filter(STV > 2*cstv.90),
                          aes(x=STV, y=RY, shape = ">2xCSTV90"), 
                          col = "#CE1141", size = 3, alpha = 0.5) } +
    # Highlight potential leverage points >2xCSTV90
    { if (length(STV[STV > cstv.100]) > 0)
      ggplot2::geom_point(data = datapoints %>% 
                            dplyr::filter(STV > cstv.100),
                          aes(x=STV, y=RY, shape = ">CSTV100"), 
                          col = "#CE1141", size = 3, alpha = 0.5) } +
    ggplot2::scale_shape_manual(name = "", values = c(15,8))+
    ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
    # RY target
    ggplot2::geom_hline(yintercept = target, alpha = 0.2) +
    # CSTV
    ggplot2::geom_vline(xintercept = CSTV, alpha = 1, color = "grey25", 
                        linewidth = 0.5, linetype = "dashed") +
    # CI
    geom_vline(xintercept = CSTV_lower, col = "grey25", linewidth = 0.25, linetype = "dotted")+
    geom_vline(xintercept = CSTV_upper, col = "grey25", linewidth = 0.25, linetype = "dotted")+
    # ALCC curve
    ggplot2::geom_path(data = curve, ggplot2::aes(x=fitted_STV,y=new_RY),
                       color="grey15", linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(0, max(RY)),breaks=seq(0,max(RY)*2, 10)) +
    # Giving more flexibility to x scale
    ggplot2::scale_x_continuous(
      breaks = seq(0, maxx,
                   by = ifelse(maxx >= 300, 50,
                         ifelse(maxx >= 200, 20,
                          ifelse(maxx >= 100, 10, 
                           ifelse(maxx >= 50, 5,
                            ifelse(maxx >= 20, 2,
                             ifelse(maxx >= 10, 1,
                              ifelse(maxx >= 5, 0.5,
                               ifelse(maxx >= 1, 0.2, 
                                      0.1))))))))) ) +
    # Text annotations
    ggplot2::annotate("text",label = paste("CSTV =", round(CSTV,1), "ppm"),
                      x = CSTV, y = 0, angle = 90, hjust = 0, vjust = 1.5, col = "grey25") +
    ggplot2::annotate("text",label = paste0("Target = ", round(target, 1), "%"),
                      x = max(max(STV),max(fitted_STV)), 
                      y = target, hjust = 1,vjust = 1.5, col = "grey25") +
    ggplot2::annotate("text", col = "grey25",
                      label = paste0("n = ", length(STV),
                                     # AIC on original scale
                                     "\nAIC = ", round(AIC_alcc),
                                     "\nCI = [", round(CSTV_lower,1)," - ", round(CSTV_upper,1),"]"),
                      x = max(max(STV),max(fitted_STV)), y = 0, vjust = 0, hjust = 1) +
    # Shade
    #ggpp::annotate(geom = "rect", xmin = CSTV_lower, xmax = CSTV_upper,
    #              ymin = min(ry), ymax = 100, alpha = 0.3, fill = "#13274F")+
    ggplot2::labs(x = "Soil test value (units)", y = "Relative yield (%)",
                  title = "Modified arcsine-log calibration curve")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
  
  return(modalcc.ggplot)
  
  } else {
    return(results)
  }
}

#' @rdname mod_alcc
#' @return logLik_alcc: AIC on original scale function
#' @export

logLik_alcc <- function (object,
                         #REML = FALSE,
                         ...){
  
  # if (REML)
  #   stop("cannot calculate REML log-likelihood for \"nls\" objects")
  res <- object$resid ## Extract residuals from ALCC object
  N <- length(res)
  if (is.null(w <- object$weights))
    w <- rep_len(1, N)
  zw <- w == 0
  N <- sum(!zw)
  val <- -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw))/N + log(sum(res^2)))/2
  
  attr(val, "df") <- 1L + 2L ## For the alcc the number of parameters might be fixed?
  attr(val, "nobs") <- attr(val, "nall") <- N
  class(val) <- "logLik"
  val
  
}

#' @rdname mod_alcc
#' @return boot_mod_alcc: bootstrapping function
#' @export 
boot_mod_alcc <- 
  function(data, ry, stv, n = 500, target = 90, confidence = 0.95, ... ) {
    # Allow customized column names
    x <- rlang::enquo(stv)
    y <- rlang::enquo(ry)
    
    # Empty global variables
    boot_id <- NULL
    boots <- NULL
    model <- NULL
    Curve <- NULL
    SMA <- NULL
    LL <- NULL
    UL <- NULL
    approx <- NULL
    quiet <- NULL
    
    data %>%  
      dplyr::select(!!y, !!x, ...) %>%
      tidyr::expand_grid(boot_id = seq(1, n, by = 1)) %>%
      dplyr::group_by(boot_id, ...) %>%
      tidyr::nest(boots = c(!!x, !!y)) %>% 
      dplyr::mutate(boots = boots %>% 
                      purrr::map(function(boots) 
                        dplyr::slice_sample(boots, 
                                            replace = TRUE, n = nrow(boots))) ) %>% 
      dplyr::mutate(
        model = map(boots, purrr::possibly(
           .f = ~ soiltestcorr::mod_alcc(data = ., ry = !!y, stv = !!x,
                                    target = target, 
                                    confidence = confidence))),
                                otherwise = NULL, quiet = TRUE) %>%
      dplyr::select(-boots) %>% 
      tidyr::unnest(cols = model) %>% 
      dplyr::select(-confidence, -LL, -UL, -Curve, -SMA, -quiet) %>%
      dplyr::ungroup()
  }

