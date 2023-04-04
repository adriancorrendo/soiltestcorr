#' @name mitscherlich
#' @title Mitscherlich response function
#' @description This function helps to fit a Mitscherlich-style exponential
#' response model for relative yield (ry) as a function of soil test values (stv).
#' @param data Optional argument to call and object of type data.frame or data.table 
#' containing the stv and ry data, Default: NULL
#' @param stv name of the vector containing soil test values (-) of type `numeric`.
#' @param ry name of the vector containing relative yield values (%) of type `numeric`.
#' @param target `numeric` value of relative yield target (e.g. 90 for 90%) to estimate the CSTV.
#' Default: NULL
#' @param type string or number that indicates the type of Mitscherlich model to fit. Default: 1. 
#' For model with 'no restrictions' use `type = 1`, `type = "no restriction"`, or `type = "free"`; 
#' For model with 'asymptote = 100' use `type = 2`, `type = "asymptote 100"`, or `type = "100"`; 
#' For model with 'asymptote = 100 and xintercept = 0'" `type = 3`, `type = "asymptote 100 from 0"`, or `type = "fixed"`. 
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a tidy data frame or tibble (default), FALSE returns a list.
#' @param resid logical operator (TRUE/FALSE) to plot residuals analysis, Default: FALSE
#' @param plot logical operator (TRUE/FALSE) to plot the Mitscherlich model, Default: FALSE
#' @param a selfstart arg. for asymptote parameter, Default: NULL
#' @param b selfstart arg. for b parameter (b = -X_intercept) Default: NULL
#' @param c selfstart arg. for curvature parameter Default: NULL
#' @param x selfstart vector. for model fit Default: NULL
#' @param n sample size for the bootstrapping Default: 500
#' @param ... when running bootstrapped samples, the `...` (open arguments) allows to add grouping variable/s (factor or character) Default: NULL
#' @rdname mitscherlich
#' @return returns an object of type `ggplot` if plot = TRUE.
#' @return returns a residuals plot if resid = TRUE.
#' @return returns an object of class `data.frame` if tidy = TRUE, 
#' @return returns an object of class `list` if tidy = FALSE.
#' @details See [online-documentation](https://adriancorrendo.github.io/soiltestcorr/articles/mitscherlich_tutorial.html) for additional details.
#' @references
#' Melsted, S.W. and Peck, T.R. (1977). 
#' The Mitscherlich-Bray Growth Function. 
#' _In Soil Testing (eds T. Peck, J. Cope and D. Whitney)._ \doi{10.2134/asaspecpub29.c1}
#' @examples 
#' \donttest{
#'  # Example dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example_mits <- mitscherlich(data = dat, type = 1, 
#'  ry = ry, stv = stv, resid = TRUE, plot = FALSE)
#'  
#'  fit_example_mits
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
#' @note For extended reference, we recommend to visit: 
#' <https://github.com/austinwpearce/SoilTestCocaCola> by Austin Pearce.
#' @export
#' @importFrom rlang eval_tidy quo enquo
#' @importFrom minpack.lm nlsLM
#' @importFrom stats sortedXyData AIC lm optim coef predict
#' @importFrom AICcmodavg AICc
#' @importFrom modelr rsquare
#' @importFrom nlstools nlsResiduals confint2
#' @importFrom dplyr bind_cols %>% mutate select slice_sample group_by
#' @importFrom ggplot2 ggplot aes geom_rug geom_point geom_vline geom_hline geom_path annotate scale_y_continuous labs theme_bw theme unit rel element_blank element_text
#' @importFrom stats lm AIC optim coef predict anova
#' @importFrom tidyr nest unnest expand_grid
#' @importFrom purrr map possibly
#' @export 
#' 
NULL
#' @rdname mitscherlich
#' @return Mitscherlich type 1 formula
#' @export
mits_formula_1 <- function(x, a, b, c){ a * (1 - exp(-c * (x + b))) }

#' @rdname mitscherlich
#' @return Mitscherlich type 2 formula
#' @export
mits_formula_2 <- function(x, b, c){ 100 * (1 - exp(-c * (x + b))) }

#' @rdname mitscherlich
#' @return Mitscherlich type 3 formula
#' @export
mits_formula_3 <- function(x, c){ 100 * (1-exp(-c * x)) }

#' @rdname mitscherlich
#' @return mitscherlich: function
#' @export 
#' 
mitscherlich <- function(data = NULL,
                         stv,
                         ry,
                         type = 1,
                         target = 95,
                         tidy = TRUE,
                         plot = FALSE,
                         resid = FALSE
) {
  if (!(type %in% c(1,2,3,"no restriction", "free", "asymptote 100", "100", 
                    "asymptote 100 from 0", "fixed"))) {
    stop("Please specify the type of Mitscherlich function using the `type` argument.
          Options: 
          Type = 1 for 'no restriction' or 'free' model; 
          Type = 2 for 'asymptote 100' or '100' model;
          Type = 3 for 'asymptote 100 from 0' or 'fixed' model")
  }
  
  
  if (missing(stv)) {
    stop("Please specify the variable name for soil test values using the `stv` argument")
  }
  
  if (missing(ry)) {
    stop("Please specify the variable name for relative yields using the `ry` argument")
  }
  
  if (is.null(target)) {
    stop("Please specify the target RY at which the soil test value with be determined. Must be target <= asymptote.")
  }
  
  # Re-define x from stv argument
  x <- rlang::eval_tidy(data = data, rlang::quo({{stv}}) )
  
  # Error message for insufficient sample size
  if (length(x) < 4) {
    stop("Too few distinct input values to fit LP. Try at least 4.")
  }
  
  # Re-define y from ry argument
  y <- rlang::eval_tidy(data = data, rlang::quo({{ry}}) )
  
  # Error message for soil test correlation data on ratio scale
  # Modeling steps depend on percentage scale
  if (max(y) < 2) {
    stop("The reponse variable does not appear to be on a percentage scale. Please, double check the units of RY.")
  }
  
  # Create data.frame if it doesn't exist yet (data from vectors)
  test.data <- data.frame(x = as.numeric(x),
                          y = as.numeric(y))
  
  # Extreme values
  minx <- min(test.data$x)
  maxx <- max(test.data$x)
  miny <- min(test.data$y)
  maxy <- max(test.data$y)
  # slope of the data divided by 10 gives reasonable starting value for c
  start_c <- (maxy - miny) / (maxx - minx) / 10
  
  # Run the model combining minpack.lm::nlsLM + defined selfStart
  # Type 1, no restrictions
  if (type == "no restriction" | type == "free" | type == 1) {
    mitsmodel <-
      try(minpack.lm::nlsLM(formula = y ~ mits_formula_1(x, a, b, c),
                            data = test.data,
                            start = list(a = maxy, b = 0, c = start_c),
                            lower = c(a = miny, b = -maxx, c = 1e-7),
                            upper = c(a = Inf, b = 1e3, c = 10)))
  }
  # Type 2
  if (type == "asymptote 100" | type == "100" | type == 2) {
    mitsmodel <-
      try(minpack.lm::nlsLM(formula = y ~ mits_formula_2(x, b, c),
                            data = test.data,
                            start = list(b = 0, c = start_c),
                            lower = c(b = -maxx, c = 1e-7),
                            upper = c(b = 1e3, c = 10)))
  }
  # Type 3
  if (type == "asymptote 100 from 0" | type == "fixed" | type == 3) {
    mitsmodel <-
      try(minpack.lm::nlsLM(formula = y ~ mits_formula_3(x, c),
                            data = test.data,
                            start = list(c = start_c),
                            lower = c(c = 1e-7),
                            upper = c(c = 10)))
  }
  
  if (inherits(mitsmodel, "try-error")) {
    stop("Mitscherlich model did not converge. Please try another `type = ` argument, or consider other models.")
  } else {
    mitsmodel <- mitsmodel
  }
  
  # Get p-value of model vs. null, Pr(>F)
  null_model <- stats::lm(y ~ 1, data = test.data)
  pvalue <- round(stats::anova(mitsmodel, null_model)[,"Pr(>F)"][[2]], 4)
  # if (pvalue >= 0.001) {pvalue <- round(pvalue, 3)} else{pvalue <- "<0.001"}
  # Find AIC and pseudo R-squared
  # AIC 
  # It makes sense because it's a sort of "simulation" (using training data) to 
  # test what would happen with out of sample data
  AIC <- round(stats::AIC(mitsmodel), 2)
  AICc <- round(AICcmodavg::AICc(mitsmodel), 2)
  BIC <- round(stats::BIC(mitsmodel), 2)
  # R2
  R2 <- round(modelr::rsquare(mitsmodel, test.data), 2)
  RMSE <- round(modelr::rmse(mitsmodel, test.data), 2)
  
  # get model coefficients
  if (type == "no restriction" | type == "free" | type == 1) {
    a <- stats::coef(mitsmodel)[[1]] # Asymptote
  }
  if (type == "asymptote 100" | type == "100" | type == 2 | 
      type == "asymptote 100 from zero" | type == "fixed" |  type == 3) {
    a <- 100 # Asymptote = 100
  }
  # X_Intercept (b = - X_intercept)
  if (type == "no restriction" | type == "free" |  type == 1) {
    b <- stats::coef(mitsmodel)[[2]] # Intercept
  }
  
  if (type == "asymptote 100" | type == "100" |  type == 2) {
    b <- stats::coef(mitsmodel)[[1]] # Intercept
  }
  
  if (type == "asymptote 100 from 0" | type == "fixed" |  type == 3) {
    b <- 0 # Intercept
  }
  
  # Curvature
  if (type == "no restriction" | type == "free" |  type == 1) {
    c <- stats::coef(mitsmodel)[[3]] # curvature
  }
  
  if (type == "asymptote 100" | type == "100" |  type == 2) {
    c <- stats::coef(mitsmodel)[[2]] # curvature
  }
  
  if (type == "asymptote 100 from 0" | type == "fixed" |  type == 3) {
    c <- stats::coef(mitsmodel)[[1]] # curvature
  }
  
  ## Derived vars
  if (target > a) {
    warning("Target RY > asymptote. Please choose a target <= asymptote.")
  }
  
  CSTV <- (log(1 - (target/a)) / -c) - b
  
  # for displaying rounded CSTV on plots
  CSTVr <- ifelse(CSTV > 10, round(CSTV), round(CSTV, 1))
  
  #  CSTV_target <- log((target - a) / (b-a)) / -c
  # There are no confidence interval for CSTV as it is not a parameter
  
  # have to make a line because the SS_mits doesn't plot right
  if (type == "no restriction" | type == "free" |  type == 1) {
    mits_line <-
      data.frame(x = seq(minx, maxx, by = maxx/200)) %>%
      dplyr::mutate(y = mits_formula_1(x, a, b, c) ) }
  
  if (type == "asymptote 100" | type == "100" |  type == 2) {
    mits_line <-
      data.frame(x = seq(minx, maxx, by = maxx/200)) %>%
      dplyr::mutate(y = mits_formula_2(x, b, c) ) }
  
  if (type == "asymptote 100 from zero" | type == "fixed" |  type == 3) {
    mits_line <-
      data.frame(x = seq(minx, maxx, by = maxx/200)) %>%
      dplyr::mutate(y = mits_formula_3(x, c) ) }
  
  
  # Equation
  if (b > 0) { 
    equation <- paste0(round(a, 1), "(1-e^(-", 
                       round(c, 3), "(x+",
                       round(b, 1), "))")
  }
  if (b == 0) { 
    equation <- paste0(round(a, 1), "(1-e^(-", 
                       round(c, 3), "x))")
  }
  if (b < 0) { 
    equation <- paste0(round(a, 1), "(1-e^(-", 
                       round(c, 3), "(x",
                       round(b, 1), "))")
  }
  
  # Y-intercept
  y_intercept = a * (1 - exp(-c * b))
  
  ## STAGE 2 ====================================================================
  ## Outputs
  # Table output =================================================
  if (plot == FALSE) {
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(mitsmodel), which = 0)
    }
    results <- dplyr::tibble(
      asymptote = a,
      b, 
      curvature = c,
      equation,
      y_intercept,
      target,
      CSTV,
      AIC,
      AICc,
      BIC,
      R2,
      RMSE,
      pvalue
    )
    
    # Decide type of output
    if (tidy == TRUE) {
      
      return(results)
    } else if (tidy == FALSE) {
      
      return(as.list(results))
    }
    
  } else {
    # Residual plots and normality
    {
      if (resid == TRUE)
        plot(nlstools::nlsResiduals(mitsmodel), which = 0)
    }
    
    # Generate predicted values
    predicted <- stats::predict(mitsmodel, newdata = test.data) %>%
      as.data.frame() %>%
      dplyr::bind_cols(test.data)
    
    # GGPLOT output =================================================   
    mits_plot <- predicted %>%
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
      # Data points
      ggplot2::geom_point(shape = 21, size = 3, alpha = 0.75, fill = "#e09f3e") +
      # Mits Curve
      ggplot2::geom_path(data = mits_line, ggplot2::aes(x, y),
                         color = "grey15", linewidth = 1) +
      # CSTV (if target defined)
      {
        if(!is.na(CSTV))
          ggplot2::geom_vline(xintercept = CSTV, alpha = 1, color = "#13274F", 
                              linewidth = 0.5, linetype = "dashed") 
      } +
      # Target
      ggplot2::geom_hline(yintercept = target, alpha = 0.2) +
      # Text annotations
      # CSTV
      {
        if(!is.na(CSTV))
          ggplot2::annotate("text",
                            label = paste("CSTV =", CSTVr, "ppm"),
                            x = CSTV, y = 0,
                            angle = 90, hjust = 0, vjust = 1.5, col = "grey25") 
      } +
      # Target if null
      {
        if(target == a)
          ggplot2::annotate("text",
                            label = paste0("Asym = ", round(target, 1), "%"),
                            x = maxx, y = target,
                            hjust = 1,vjust = 1.5, col = "grey25") 
      } +
      # Target if target == a
      {
        if(target != a)
          ggplot2::annotate("text",
                            label = paste0("Target = ", round(target, 1), "%"),
                            x = maxx, y = target,
                            hjust = 1,vjust = 1.5, col = "grey25") 
      } +
      ggplot2::annotate("text", col = "grey25",
                        label = paste0("y = ", equation,
                                       "\nn = ", nrow(test.data),
                                       "\npseudo-R2 = ", R2,
                                       "\nAICc = ", AICc
                        ),
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
      # Y-axis scale
      ggplot2::scale_y_continuous(limits = c(0, maxy),
                                  breaks = seq(0, maxy * 2, 10)) +
      ggplot2::labs(x = "Soil test value (units)",
                    y = "Relative yield (%)",
                    title = "Mitscherlich-type Exponential Response")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
    
    return(mits_plot)
  }
  
}

#' @rdname mitscherlich
#' @return boot_mitscherlich: bootstrapping function
#' @export 
boot_mitscherlich <- 
  function(data, stv, ry, type = 1, n = 999, target = 95, ...) {
    # Allow customized column names
    x <- rlang::enquo(stv)
    y <- rlang::enquo(ry)
    
    # Empty global variables
    boot_id <- NULL
    boots <- NULL
    model <- NULL
    equation <- NULL
    
    output_df <- data %>%  
      dplyr::select(!!x, !!y, ...) %>%
      tidyr::expand_grid(boot_id = seq(1, n, by = 1)) %>%
      dplyr::group_by(boot_id, ...) %>%
      tidyr::nest(boots = c(!!x, !!y)) %>% 
      dplyr::mutate(boots = boots %>% 
                      purrr::map(function(boots) 
                        dplyr::slice_sample(boots, 
                                            replace = TRUE, n = nrow(boots)))) %>% 
      dplyr::mutate(
        model = map(boots, purrr::possibly(
          .f = ~ soiltestcorr::mitscherlich(
            data = ., ry = !!y, stv = !!x,
            type = type, target = target)), 
          otherwise = NULL, quiet = TRUE)) %>%
      dplyr::select(-boots) %>% 
      tidyr::unnest(cols = model) %>%
      # Irrelevant columns
      dplyr::select(-equation) %>% 
      dplyr::ungroup()
    
    return(output_df)
  }