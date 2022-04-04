
#' @title modALCC
#' @description This function runs the modified arcsine-log calibration curve to
#' estimate critical soil test values (CSTV) following Correndo et al. (2017)
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the STV and RY data, Default: NULL
#' @param RY name of the vector containing relative yield values (%) of type `numeric`.
#' @param STV name of the vector containing soil test values (-) of type `numeric`.
#' @param target `numeric` value of relative yield target (e.g. 90 for 90%) to estimate the CSTV.
#' @param confidence `numeric` value of confidence level (e.g. 0.95 for 
#' significance = 0.05)
#' @param tidy `boolean` to decide the type of return. TRUE returns a data.frame, 
#' FALSE returns a list (default). 
#' @return an object of type `data.frame` if tidy == TRUE, otherwise, it returns a list
#' @details See Correndo et al. (2017)
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # Example 1 dataset
#'  data_1 = data.frame("RY" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                      "STV" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example1 = modALCC(data = data_1, RY = RY, STV = STV, target=90, confidence = 0.95)
#'  fit_example1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[tidyr]{nest}}
#' @rdname modALCC
#' @importFrom rlang eval_tidy quo
#' @importFrom dplyr bind_cols `%>%`
#' @importFrom tidyr nest
#' @importFrom stats cor cor.test sd qt
#' @export 

modALCC <- function(data=NULL, RY, STV, target, confidence, tidy = FALSE){
  
  # Add a function to cap if there are RY values > 100
  ry <- rlang::eval_tidy(data = data, rlang::quo(ifelse({{RY}} > 100, 100, as.double({{RY}})) ))
  n <- length(ry) # Sample size
  df <- n - 2 # Degrees of freedom
  prob <- 1-((1-confidence)/2) # Probability for t-dist
  tvalue <- stats::qt(p=prob, df = df) # Student-t value
  arc_RY <- asin(sqrt(ry/100)) - asin(sqrt(target/100)) # RY transformation (centered to target)
  ln_STV <- rlang::eval_tidy(data = data, rlang::quo(log({{STV}}) ) ) # STV natural log transformation
  r <- stats::cor(ln_STV, arc_RY, method = "pearson") # Pearson correlation (r)
  p_value <- stats::cor.test(ln_STV,arc_RY, method = "pearson")$p.value # p-value of r
  slope <- stats::sd(ln_STV)/stats::sd(arc_RY) # SMA slope for ln_STV ~ arc_RY
  intercept <- mean(ln_STV) - (mean(arc_RY)*slope) # Intercept
  SMA_line <- intercept + slope * arc_RY # Fitted ln_STV for observed RY
  CSTV <- exp(intercept) # Critical STV for specified RY-target and confidence (1-alpha)
  MSE <- sum((SMA_line-ln_STV)^2)/df # Mean Square Error of ln_STV
  SSx <- sum((mean(arc_RY)-arc_RY)^2)  # Sum of Squares of arc_RY
  SE_int <- sqrt(MSE*((1/n)+ ((mean(arc_RY)^2)/SSx)))  # Standard Error intercept
  CSTV_lower <- exp(intercept - (tvalue * SE_int))  # Lower limit of CSTV
  CSTV_upper <- exp(intercept + (tvalue * SE_int)) # Upper limit of CSTV
  new_RY <- seq(min(ry),100, by=0.2) # New RY vector up to %100 to fit curve
  new_arc_RY <- asin(sqrt(new_RY/100)) - asin(sqrt(target/100)) # Transforming new_RY vector
  fitted_Line <- intercept + slope * new_arc_RY # Fitted ln_STV for curve plot
  fitted_STV <- exp(fitted_Line) # Fitted ln_STV for new_RY
  residuals <- ln_STV - SMA_line # Residuals of SMA Regression
  fitted_axis <- ln_STV + slope * arc_RY # Fitted axis to check SMA residuals
  target <- target # Target RY to show on summary
  confidence <- confidence # Confidence level to show on summary
  # Critical STV for RY = 90 & 100
  arc_ry_100 <- asin(sqrt(ry/100)) - asin(sqrt(1)) 
  cstv.100 <- exp(mean(ln_STV) - (mean(arc_ry_100)*(sd(ln_STV)/sd(arc_ry_100))))
  arc_ry_90 <- asin(sqrt(ry/100)) - asin(sqrt(90/100)) 
  cstv.90 <- exp(mean(ln_STV) - (mean(arc_ry_90)*(sd(ln_STV)/sd(arc_ry_90))))
  # Count cases with STV > x2 cstv90 and STV > cstv100
  n.90x2 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{STV}} > (2*cstv.90))) ) )
  n.100 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{STV}} > cstv.100)) ) )
  # Outcome
  results <- 
    dplyr::bind_cols(
      # Data frame with summary
      dplyr::bind_cols(as.data.frame(list("n" = n, 
                                          "r" = r, 
                                          "target" = target,
                                          "CSTV" = CSTV,
                                          "LL" = CSTV_lower,
                                          "UL" = CSTV_upper,
                                          "confidence" = confidence,
                                          "p_value" = p_value,
                                          "CSTV90" = cstv.90, 
                                          "n.90x2" = n.90x2,
                                          "CSTV100" = cstv.100,
                                          "n.100" = n.100)),
                       # Data frame with Curve
                       as.data.frame(list("RY.fitted" = new_RY, 
                                          "STV.fitted" = fitted_STV)) %>%
                         tidyr::nest(Curve = c("RY.fitted", "STV.fitted"))),
      # Data frame with SMA residuals
      as.data.frame(list("ln_STV" = ln_STV, "arc_RY" = arc_RY,
                         "SMA_line" = SMA_line,
                         "residuals" = residuals,
                         "fitted_axis" = fitted_axis)) %>%
        tidyr::nest(SMA =  c("ln_STV", "arc_RY", "SMA_line","residuals", "fitted_axis") ) )
  
  if (tidy == TRUE) {results <- results}
  
  if (tidy == FALSE) {results <- as.list(results)}
  
  # WARNINGS
  rlang::eval_tidy(data = data, rlang::quo(
    # RY > 100%
    if (max({{RY}}) > 100) {warning("One or more original RY values exceeded 100%. All RY values greater 
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
  re-run the modALCC(), and check results."), call. = FALSE) }
  
  if (results$n.90x2 > 0) {warning(paste0(n.90x2," STV points exceeded two-times (2x) 
  the CSTV for 90% of RY. Risk of leverage. You may consider a sensitivity analysis by 
  removing extreme points, re-run the modALCC(), and check results."), call. = FALSE) }
  
  # IF END
  return(results) }
