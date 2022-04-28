
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
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a data.frame, FALSE returns a list (default).
#' @param plot logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a ggplot,
#' FALSE returns either a list (tidy == FALSE) or a data.frame (tidy == TRUE). 
#' @return an object of type `data.frame` if tidy == TRUE, otherwise, it returns a list
#' @details See Correndo et al. (2017)
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # Example 1 dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example <- mod_alcc(data = dat, RY = ry, STV = stv, target=90, confidence = 0.95)
#'  fit_example
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[stats]{TDist}},\code{\link[stats]{cor}},\code{\link[stats]{cor.test}},\code{\link[stats]{sd}}
#'  \code{\link[dplyr]{bind}},\code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{nest}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{geom_rug}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggpp]{annotate}}
#' @rdname mod_alcc
#' @export 
#' @importFrom rlang eval_tidy quo
#' @importFrom stats qt cor cor.test sd
#' @importFrom dplyr bind_cols filter %>%
#' @importFrom tidyr nest
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual geom_rug geom_hline geom_vline geom_path scale_y_continuous annotate labs theme_bw theme annotate 

mod_alcc <- function(data=NULL, 
                    ry, 
                    stv, 
                    target, 
                    confidence = 0.95, 
                    tidy = FALSE,
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
  # Add a function to cap if there are RY values > 100
  RY <- rlang::eval_tidy(data = data, rlang::quo(ifelse({{ry}} > 100, 100, as.double({{ry}})) ))
  # Re-define STV as stv
  STV <- rlang::eval_tidy(data = data, rlang::quo({{stv}}))
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
  CSTV <- exp(intercept) # Critical STV for specified RY-target and confidence (1-alpha)
  MSE <- sum((SMA_line-ln_STV)^2)/df # Mean Square Error of ln_STV
  SSx <- sum((mean(arc_RY)-arc_RY)^2)  # Sum of Squares of arc_RY
  SE_int <- sqrt(MSE*((1/n)+ ((mean(arc_RY)^2)/SSx)))  # Standard Error intercept
  CSTV_lower <- exp(intercept - (tvalue * SE_int))  # Lower limit of CSTV
  CSTV_upper <- exp(intercept + (tvalue * SE_int)) # Upper limit of CSTV
  new_RY <- seq(min(RY),100, by=0.2) # New RY vector up to %100 to fit curve
  new_arc_RY <- asin(sqrt(new_RY/100)) - asin(sqrt(target/100)) # Transforming new_RY vector
  fitted_Line <- intercept + slope * new_arc_RY # Fitted ln_STV for curve plot
  fitted_STV <- exp(fitted_Line) # Fitted ln_STV for new_RY
  residuals <- ln_STV - SMA_line # Residuals of SMA Regression
  fitted_axis <- ln_STV + slope * arc_RY # Fitted axis to check SMA residuals
  target <- target # Target RY to show on summaRY
  confidence <- confidence # Confidence level to show on summaRY
  # Critical STV for RY = 90 & 100
  arc_RY_100 <- asin(sqrt(RY/100)) - asin(sqrt(1)) 
  cstv.100 <- exp(mean(ln_STV) - (mean(arc_RY_100)*(sd(ln_STV)/sd(arc_RY_100))))
  arc_RY_90 <- asin(sqrt(RY/100)) - asin(sqrt(90/100)) 
  cstv.90 <- exp(mean(ln_STV) - (mean(arc_RY_90)*(sd(ln_STV)/sd(arc_RY_90))))
  # Count cases with STV > x2 cstv90 and STV > cstv100
  n.90x2 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{stv}} > (2*cstv.90))) ) )
  n.100 <- rlang::eval_tidy(data=data, rlang::quo(length(which({{stv}} > cstv.100)) ) )
  
  ### STAGE 2 ====================================================================
  # Outputs
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

# Decide type of output
  if (tidy == TRUE) {results <- results}
  
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
                        size = 0.5, linetype = "dashed") +
    # CI
    geom_vline(xintercept = CSTV_lower, col = "grey25", size = 0.25, linetype = "dotted")+
    geom_vline(xintercept = CSTV_upper, col = "grey25", size = 0.25, linetype = "dotted")+
    # ALCC curve
    ggplot2::geom_path(data = curve, ggplot2::aes(x=fitted_STV,y=new_RY),
                       color="grey15", size = 1.5) +
    ggplot2::scale_y_continuous(limits = c(0, max(RY)),breaks=seq(0,max(RY)*2,10)) +
    # Text annotations
    ggplot2::annotate("text",label = paste("CSTV =", round(CSTV,1), "ppm"),
                      x = CSTV, y = 0, angle = 90, hjust = 0, vjust = 1.5, col = "grey25") +
    ggplot2::annotate("text",label = paste0("Target = ", round(target, 1), "%"),
                      x = max(max(STV),max(fitted_STV)), 
                      y = target, hjust = 1,vjust = 1.5, col = "grey25") +
    ggplot2::annotate("text", col = "grey25",
                   label = paste0("n = ", length(STV),
                                  "\nr = ", round(r, 2),
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
  
  
  if (plot == TRUE){
    return(modalcc.ggplot)
  } else {
    return(results)
    }
  }
