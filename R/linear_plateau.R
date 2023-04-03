#' @name linear_plateau
#' @title Linear-plateau response function
#' @description This function helps to fit a linear-plateau model in order to
#' estimate critical soil test values (CSTV) above which yield response becomes flat.
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the soil test value (STV) and relative yield (RY) data, Default: NULL
#' @param stv name of the vector containing soil test values (-) of type `numeric`.
#' @param ry name of the vector containing relative yield values (%) of type `numeric`.
#' @param target `numeric` value of relative yield target (e.g. 90 for 90%) to estimate the CSTV.
#' The target needs to be < plateau, otherwise, target = plateau.
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a tidy data frame or tibble (default), FALSE returns a list.
#' @param resid logical operator (TRUE/FALSE) to plot residuals analysis, Default: FALSE
#' @param plot logical operator (TRUE/FALSE) to plot the linear-plateau model, Default: FALSE
#' @param x selfstart arg. for explanatory variable in SSlinp Default: NULL
#' @param a selfstart arg. for intercept Default: NULL
#' @param b selfstart arg. for slope Default: NULL
#' @param xs selfstart arg. for break/join point in SSlinp Default: NULL
#' @param n sample size for the bootstrapping Default: 500
#' @param ... when running bootstrapped samples, the `...` (open arguments) allows to add grouping variable/s (factor or character) Default: NULL
#' @rdname linear_plateau
#' @return returns an object of type `ggplot` if plot = TRUE.
#' @return returns a residuals plot if resid = TRUE.
#' @return returns an object of class `data.frame` if tidy = TRUE, 
#' @return returns an object of class `list` if tidy = FALSE.
#' @details See [online-documentation](https://adriancorrendo.github.io/soiltestcorr/articles/linear_plateau_tutorial.html) for additional details.
#' @references
#' Anderson, R. L., and Nelson, L. A. (1975). 
#' A Family of Models Involving Intersecting Straight Lines and Concomitant Experimental Designs Useful in Evaluating Response to Fertilizer Nutrients. 
#' _Biometrics, 31(2), 303–318._ \doi{10.2307/2529422}
#' @examples 
#' \donttest{
#'  # Example dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example_lp <- linear_plateau(data = dat, 
#'  ry = ry, stv = stv, resid = TRUE, plot = FALSE)
#'  fit_example_lp
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[minpack.lm]{nlsLM}}
#'  \code{\link[nlraa]{SSlinp}}
#'  \code{\link[stats]{AIC}},\code{\link[stats]{lm}},\code{\link[stats]{optim}},\code{\link[stats]{coef}},\code{\link[stats]{predict}}
#'  \code{\link[AICcmodavg]{AICc}}
#'  \code{\link[modelr]{model-quality}}
#'  \code{\link[nlstools]{nlsResiduals}}
#'  \code{\link[dplyr]{bind}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_rug}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_path}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{theme}}
#'  \code{\link[ggpp]{annotate}}
#' @note For extended reference, we recommend to visit: 
#' https://gradcylinder.org/post/linear-plateau/ by Austin Pearce.
#' Self-start function code adapted from nlraa package by F. Miguez <https://github.com/femiguez/nlraa>
#' @export
#' @importFrom rlang eval_tidy quo enquo
#' @importFrom nlraa SSlinp
#' @importFrom minpack.lm nlsLM
#' @importFrom AICcmodavg AICc
#' @importFrom modelr rsquare rmse
#' @importFrom nlstools nlsResiduals confint2
#' @importFrom dplyr bind_cols mutate select slice_sample group_by tibble as_tibble ungroup
#' @importFrom ggplot2 ggplot aes geom_rug geom_point geom_vline geom_hline geom_path annotate scale_y_continuous labs theme_bw theme unit rel element_blank element_text
#' @importFrom stats lm AIC optim coef predict anova BIC
#' @importFrom tidyr nest unnest expand_grid
#' @importFrom purrr map possibly
#' 
NULL



#' @rdname linear_plateau
#' @return SS_LP: selfStart function to pass into the linear_plateau fit
#' @export 
SS_LP <- nlraa::SSlinp


#' @rdname linear_plateau
#' @return linear_plateau: function
#' @export 
linear_plateau <- function(data = NULL,
                           stv,
                           ry,
                           target = NULL,
                           tidy = TRUE,
                           plot = FALSE,
                           resid = FALSE) {
  
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
  test.data <- data.frame(x = as.numeric(x), 
                          y = as.numeric(y))
  
  # Error message for insufficient sample size
  if (length(x) < 4) {
    stop("Too few distinct input values to fit LP. Try at least 4.")
  }
  
  # Extreme values
  minx <- min(test.data$x)
  maxx <- max(test.data$x)
  miny <- min(test.data$y)
  maxy <- max(test.data$y)
  
  # Run the model combining minpack.lm::nlsLM + selfStart LP)
  lp_model <-
    try(minpack.lm::nlsLM(y ~ SS_LP(x, a, b, jp), data = test.data))
  
  if (inherits(lp_model, "try-error")) {
    stop("Linear-plateau model did not converge. Please, consider other models.")
  } else {
    lp_model <- lp_model
  }
  
  # Get p-value of model vs. null, Pr(>F)
  null_model <- stats::lm(y ~ 1, data = test.data)
  pvalue <- round(stats::anova(lp_model, null_model)[,"Pr(>F)"][[2]], 4)
  # if (pvalue >= 0.001) {pvalue <- round(pvalue, 3)} else{pvalue <- "<0.001"}
  # Find AIC and pseudo R-squared
  # AIC 
  # It makes sense because it's a sort of "simulation" (using training data) to 
  # test what would happen with out of sample data
  
  AIC <- round(stats::AIC(lp_model), 2)
  AICc <- round(AICcmodavg::AICc(lp_model), 2)
  BIC <- round(stats::BIC(lp_model), 2)
  # R2
  R2 <- round(modelr::rsquare(lp_model, test.data), 2)
  # RMSE
  RMSE <- round(modelr::rmse(lp_model, test.data), 2)
  
  
  # get model coefficients
  a <- stats::coef(lp_model)[[1]]
  b <- stats::coef(lp_model)[[2]]
  jp <- stats::coef(lp_model)[[3]]
  plateau <- a + b * jp
  
  
  # CSTV rounded for plot display only (don't round for bootstraps)
  CSTV <- jp
  CSTVr <- ifelse(jp > 10, round(jp), round(jp, 1))
  
  # Wald confidence interval for CSTV
  # not as reliable as bootstrapping but sometimes useful
  lp_model.confint <- nlstools::confint2(lp_model, level = 0.95)
  lowerCL <- lp_model.confint[[3,1]]
  upperCL <- lp_model.confint[[3,2]]
  
  # STVt remains at join point (jp) if > plateau
  if (!is.null(target)){
    if (target > plateau) {
    warning("You have specified a relative yield target > plateau. The STVt therefore remains at the join point at the plateau level", 
          call. = FALSE) 
    }
  }
  
  # STVt is not the same as CSTV unless at join point
  # CI for STVt require bootstrapping 
  STVt <- ifelse(is.null(target), 
                 CSTV,
                 ifelse(target >= plateau,
                        (plateau - a) / b,
                        (target - a) / b))
  
  
  # have to make a line because seq() doesn't plot clean break point
  lp_line <- dplyr::tibble(
    x = c(minx, jp, maxx),
    y = c(a + b * minx, plateau, plateau))
  
  equation <- paste0(round(a, 1), " + ",
                     round(b, 2), "x when x < ", CSTVr)
  
  ## STAGE 3 ====================================================================
  ## Outputs
  # Table output =================================================
  if (plot == FALSE) {
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(lp_model), which = 0)
    }
    
    results <- dplyr::tibble(
      intercept = a,
      slope = b,
      equation,
      plateau,
      CSTV,
      lowerCL = round(lowerCL, 1),
      upperCL = round(upperCL, 1),
      CI_type = "Wald, 95%",
      target = ifelse(!is.null(target),
                      target,
                      round(plateau, 1)),
      STVt = round(STVt, 1),
      AIC,
      AICc,
      BIC,
      R2,
      RMSE,
      pvalue
      )
    
  # Decide type of output
    if (tidy == TRUE) {
      
      return(dplyr::as_tibble(results))
    
    } else if (tidy == FALSE) {
    
      return(as.list(results))
      
    }
    
  } else {
    # Residual plots and normality
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(lp_model), which = 0)
    }
    
    # GGPLOT output =================================================   
    lp_plot <- test.data %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
      # Data points
      ggplot2::geom_point(shape = 21, size = 3, alpha = 0.75, fill = "#e09f3e") +
      # LP Curve
      ggplot2::geom_line(data = lp_line, ggplot2::aes(x, y),
                         color = "grey15", linewidth = 1) +
      # CSTV for break point
      {if (is.null(target))
        ggplot2::geom_vline(xintercept = jp, alpha = 1, color = "#13274F", 
                            linewidth = 0.5, linetype = "dashed") } +
      # annotation
      {if (is.null(target))
        ggplot2::annotate(
          "text", label = paste("CSTV =", CSTVr, "ppm"),
          x = jp, y = 0, angle = 90, hjust = 0, vjust = 1.5, color = "grey25") } +
      # STV for TARGET
      {if (!is.null(target))
        ggplot2::geom_vline(xintercept = STVt, alpha = 1, color = "#13274F", 
                            linewidth = 0.5, linetype = "dashed") } +
      # CSTV annotation
      {if (!is.null(target))
        ggplot2::annotate(
          "text", label = paste(ifelse(target < plateau, "STVt =", "CSTV ="),
                                round(STVt, 1),"ppm"),
          x = STVt, y = 0, angle = 90, hjust = 0, vjust = 1.5, color = "grey25") } +
      # CI
      { if (is.null(target)) 
        geom_vline(xintercept = c(lowerCL, upperCL),
                   color = "grey25", linewidth = 0.25, linetype = "dotted") } +
      # Plateau
      { if(is.null(target))
        ggplot2::geom_hline(yintercept = plateau, alpha = 0.2) } +
      
      { if(!is.null(target))
        ggplot2::geom_hline(
          yintercept = ifelse(target < plateau, target, plateau), alpha = 0.2) } +
      # Text annotations
      # Target = NULL = CSTV @ join point
      { if(is.null(target))
        ggplot2::annotate(
          "text", label = paste0("Plateau = ", round(plateau, 0), "%"),
          x = maxx, y = plateau, hjust = 1,vjust = 1.5, color = "grey25")  } +
      # Target if not null
      {  if(!is.null(target))
        ggplot2::annotate(
          "text",label = paste0(
            ifelse(target < plateau, "Target = ", "Plateau = "),
            round(ifelse(target < plateau, target, plateau), 0), "%"),
          x = maxx, y = ifelse(target < plateau, target, plateau),
          hjust = 1,vjust = 1.5, color = "grey25")     } +
      
      ggplot2::annotate("text", color = "grey25",
                        label = paste0(
                          "n = ", nrow(test.data),
                          "\ny = ", equation, 
                          "\npseudo-R2 = ", R2,
                          "\nAICc = ", AICc,
                          "\nCSTV CI = [", ifelse(is.null(target),
                                             round(lowerCL,1),
                                             NA)," - ",
                          ifelse(is.null(target), round(upperCL,1), NA), "]"),
                        x = maxx, y = 0, vjust = 0, hjust = 1) +
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
      ggplot2::scale_y_continuous(limits = c(0, maxy),
                                  breaks = seq(0, maxy * 2,10)) +
      ggplot2::labs(x = "Soil test value (units)",
                    y = "Relative yield (%)",
                    title = "Linear-plateau")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
    
    return(lp_plot)
  }
}



#' @rdname linear_plateau
#' @return boot_linear_plateau: bootstrapping function
#' @export 

boot_linear_plateau <-
  function(data, stv, ry, n = 1000, target = NULL, ...) {
    # Allow customized column names
    x <- rlang::enquo(stv)
    y <- rlang::enquo(ry)
    
    # Empty global variables
    boot_id <- NULL
    boots <- NULL
    model <- NULL
    equation <- NULL
    CI_type <- NULL
    lowerCL <- NULL

    output_df <- data %>%
      dplyr::select(!!y, !!x, ...) %>%
      tidyr::expand_grid(boot_id = seq(1, n, by = 1)) %>%
      dplyr::group_by(boot_id, ...) %>%
      tidyr::nest(boots = c(!!x, !!y)) %>%
      dplyr::mutate(boots = boots %>%
                      purrr::map(function(boots)
                        dplyr::slice_sample(boots,
                                            replace = TRUE, n = nrow(boots)))) %>%
      dplyr::mutate(
        model = map(boots, purrr::possibly(
          .f = ~ soiltestcorr::linear_plateau(
            data = ., ry = !!y, stv = !!x, target = target)),
          otherwise = NULL, quiet = TRUE)) %>%
      dplyr::select(-boots) %>%
      tidyr::unnest(cols = model) %>% 
      # irrelevant columns
      dplyr::select(-equation, -(lowerCL:CI_type)) %>% 
      dplyr::ungroup()
    
    return(output_df)
  }

#Testing for potential future release
# boot_linear_plateau2 <-
#   function(data, stv, ry, n = 500, target = NULL, ....) {
#     # Allow customized column names
#     x <- rlang::enquo(stv)
#     y <- rlang::enquo(ry)
#     by <- rlang::enquo(.by)
# 
#     output_df <- data %>%
#       #dplyr::select(!!x, !!y, ...) %>%
#       dplyr::group_by(...) %>% 
#       tidyr::nest() %>% 
#       mutate(nested_boots = map(data, modelr::bootstrap, n = 5)) %>%
#       unnest(nested_boots)
#       mutate(boots = map(strap, dplyr::as_tibble),
#              model = map(boots, purrr::possibly(
#                .f = ~ soiltestcorr::linear_plateau(
#                data = ., stv = !!x, ry = !!y, target = target),
#              otherwise = NULL))) %>%
#       dplyr::select(-(data:boots)) %>%
#       tidyr::unnest(model) %>%
#       dplyr::ungroup() %>%
#       # irrelevant columns
#       dplyr::select(-equation, -(lowerCL:CI_type))
#       
#     return(output_df)
#   }
