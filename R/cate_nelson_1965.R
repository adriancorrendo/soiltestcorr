#' @name cate_nelson_1965
#' @title Cate & Nelson quadrants analysis (graphical)
#' @description This function runs the quadrants analysis suggested by Cate and Nelson (1965)
#' @param data argument to call a data.frame or data.table containing the data
#' @param stv argument to call the vector or column containing the soil test value (stv) data
#' @param ry argument to call the vector or column containing the relative yield (ry) data
#' @param target argument to specify the ry target (numeric) to estimate the critical stv for
#' @param tidy logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a tibble, FALSE returns a list. Default: TRUE.
#' @param plot logical operator (TRUE/FALSE) to decide the type of return. TRUE returns a ggplot,
#' FALSE returns either a list (tidy == FALSE) or a tibble (tidy == TRUE).
#' @param n sample size for the bootstrapping Default: 500
#' @param ... when running bootstrapped samples, the `...` (open arguments) allows to add grouping variable/s (factor or character) Default: NULL
#' @rdname cate_nelson_1965
#' @return returns an object of type `ggplot` if plot = TRUE.
#' @return returns an object of class `data.frame` if tidy = TRUE, 
#' @return returns an object of class `list` if tidy = FALSE.
#' @details
#' See [online-documentation](https://adriancorrendo.github.io/soiltestcorr/articles/cate_nelson_1965_tutorial.html) for additional details. 
#' @references
#' Cate & Nelson (1965). 
#' A rapid method for correlation of soil test analysis with plant response data. 
#' _North Carolina Agric. Exp. Stn., International soil Testing Series l. No. 1._
#' @examples 
#' \donttest{
#'  # Example 1 dataset
#'  dat <- data.frame("ry" = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
#'                    "stv" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#'  # Run
#'  fit_example_cn_1965 <- cate_nelson_1965(data = dat, 
#'  ry = ry, stv = stv, target = 90, tidy=FALSE, plot=FALSE)
#'  
#'  fit_example_cn_1965
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[stats]{lm}},\code{\link[stats]{anova}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{theme}}
#' @note
#' This code was adapted from 
#' Mangiafico, S. S. (2013). Cate-Nelson Analysis for Bivariate Data Using R-project.
#' _The Journal of Extension, 51(5), Article 33._ <https://tigerprints.clemson.edu/joe/vol51/iss5/33/> 
#' @export 
#' @importFrom rlang eval_tidy quo enquo
#' @importFrom dplyr %>% select group_by mutate slice_sample ungroup as_tibble
#' @importFrom stats lm anova AIC BIC
#' @importFrom modelr rmse
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual scale_color_manual labs geom_vline geom_hline annotate theme_bw theme
#' @importFrom tidyr nest unnest expand_grid
#' @importFrom purrr map possibly
#' 
cate_nelson_1965 <- function(data=NULL, stv, ry, target, tidy = TRUE, plot = FALSE){
  
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

  x <- rlang::eval_tidy(data = data, rlang::quo({{stv}}) )
  
  y <- rlang::eval_tidy(data = data, rlang::quo({{ry}}) )
  
  n <- length(x) 
  
  ##-----order by x and create critical.x variable for calculation---
  xgroup <- c('a','b')
  ygroup <- c('c','d')
  for (i in c(2:n))
  { xgroup[i] <-  c('b')
  ygroup[i] <-  c('d') }
  dataset <- data.frame(x = x, y = y, xgroup = as.factor(xgroup),
                        ygroup = as.factor(ygroup))
  dataset$ObsNo <- 1:n
  dataset <- dataset[with(dataset, order(x, y)), ]
  
  ## Define the Target of Y variable
  
  for (i in c(1:n))
  {dataset$ygroup[i] <- if(dataset$y[i] < target)  'c' else 'd'}
  
  ##-order by y, add target variable, and determine final critical-y-
  dataset <- dataset[with(dataset, order(y, x)), ]
  dataset$critical.x[1] <- 0
  for(k in c(2:n))
  { dataset$critical.x[k] <- (dataset$x[k]+dataset$x[k-1])/2 }
  dataset$critical.x[1] <- min(dataset$critical.x[2:n])
  for(j in c(1:n))
  { for (i in c(1:n))
  { (dataset$xgroup[i]
     <- if(dataset$x[i] < dataset$critical.x[j]) 'a' else 'b')}
    for (i in c(1:n))
    { dataset$q.i[i] <- with(dataset, ifelse
                             (dataset$xgroup[i]=='a' & dataset$ygroup[i]=='d', 1, 0))
    dataset$q.ii[i] <- with(dataset, ifelse
                            (dataset$xgroup[i]=='b' & dataset$ygroup[i]=='d', 1, 0))
    dataset$q.iii[i] <- with(dataset, ifelse
                             (dataset$xgroup[i]=='b' & dataset$ygroup[i]=='c', 1, 0))
    dataset$q.iv[i] <- with(dataset, ifelse
                            (dataset$xgroup[i]=='a' & dataset$ygroup[i]=='c', 1, 0)) }
    dataset$q.err[j] <- (  sum(dataset$q.i) + sum(dataset$q.iii)) }
  
  min.qerr <- min(dataset$q.err)
  dataset3 <- subset(dataset, dataset$q.err == min.qerr)
  CSTV <- dataset3$critical.x[1]
  dplyr::`%>%`
  
  # Rewrite xgroups
  dataset <- dataset %>% dplyr::mutate(xgroup = dplyr::case_when(x < CSTV ~ "a",
                                                          x >= CSTV ~ "b")) %>% 
    dplyr::mutate(
      q = dplyr::case_when((dataset$x<CSTV)&(dataset$y>=target) ~ "I",
                    (dataset$x>=CSTV)&(dataset$y>=target) ~ "II",
                    (dataset$x>=CSTV)&(dataset$y<target) ~ "III",
                    (dataset$x<CSTV)&(dataset$y<target)~ "IV"),
      Quadrant = dplyr::case_when((dataset$x<CSTV)&(dataset$y>=target) ~ "negative",
                           (dataset$x>=CSTV)&(dataset$y>=target) ~ "positive",
                           (dataset$x>=CSTV)&(dataset$y<target) ~ "negative",
                           (dataset$x<CSTV)&(dataset$y<target)~ "positive") )
  
  # Counts by quadrant
  quadrants.summary <- dataset %>% 
    dplyr::summarise(q.I = sum(q=="I"),
                     q.II = sum(q=="II"),
                     q.III = sum(q=="III"),
                     q.IV = sum(q=="IV"),
                     positive = sum(dataset$Quadrant == "positive"),
                     negative = sum(dataset$Quadrant == "negative"))
  
  ## ---------- chi-square -----------
  
  q.I <- quadrants.summary$q.I[[1]]
  q.II <- quadrants.summary$q.II[[1]]
  q.III <- quadrants.summary$q.III[[1]]
  q.IV <- quadrants.summary$q.IV[[1]]
  row.1 <- c(q.I, q.II)
  row.2 <- c(q.IV, q.III)
  X2.test <- stats::chisq.test(data.frame(row.1,row.2))
  
  ## Performance of the model, coefficient of determination (R2) ----------------
  aov.model <- stats::lm(y ~ xgroup, data=dataset)
  anova.model <- stats::anova(aov.model)
  R2.model <- anova.model[1,2]/sum(anova.model[,2])
  AIC <- stats::AIC(aov.model)
  BIC <- stats::BIC(aov.model)
  RMSE <- modelr::rmse(model = aov.model, data = dataset)
  
  ## --------- final plot --------------
  
  max.x <- max(dataset$x)
  max.y <- max(dataset$y)
  min.x <- min(dataset$x)
  min.y <- min(dataset$y)
  Quadrant_ <- dataset$Quadrant
  
  # ggplot %>% 
  cn65.ggplot <- 
    ggplot2::ggplot(data = dataset, ggplot2::aes(x=x, y=y))+
    ggplot2::geom_rug(alpha = 0.2, length = ggplot2::unit(2, "pt")) +
    ggplot2::geom_point(aes(color = Quadrant_, shape = Quadrant_), alpha = 0.75)+
    ggplot2::scale_shape_manual(name = "", values = c(4,16))+
    ggplot2::scale_color_manual(name = "", values = c("#800f2f","#355070"))+
    ggplot2::geom_vline(xintercept = CSTV, col = "dark red", linetype = "dashed")+
    ggplot2::geom_hline(yintercept = target, col = "dark red", linetype = "dashed")+
    # CSTV
    ggplot2::annotate(geom = "text", label = paste("CSTV =", CSTV, "ppm"),
                      x = CSTV+1, y = 0, angle = 90, hjust = 0, vjust = 1, col = "grey25") +
    # RY target
    ggplot2::annotate(geom = "text", label = paste0("RY = ", round(target, 0), "%"),
                      x = max(dataset$x), y = target-2, angle = 0, hjust = 1, vjust = 1, 
                      col = "grey25")+
    # Quadrants
    ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.01, y =  min.y+(max.y-min.y)*0.99, 
                      label = "I", size = 3, color = "#800f2f")+
    ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.99, y =  min.y+(max.y-min.y)*0.99, 
                      label = "II", size = 3, color = "#355070")+
    ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.99, y =  min.y+(max.y-min.y)*0.01, 
                      label = "III", size = 3, color = "#800f2f")+
    ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.01, y =  min.y+(max.y-min.y)*0.01, 
                      label = "IV", size = 3, color = "#355070")+
    # Giving more flexibility to x scale
    ggplot2::scale_x_continuous(
      breaks = seq(0, max.x,
                   by = ifelse(max.x >= 300, 50,
                         ifelse(max.x >= 200, 20,
                          ifelse(max.x >= 100, 10, 
                           ifelse(max.x >= 50, 5,
                            ifelse(max.x >= 20, 2,
                             ifelse(max.x >= 10, 1,
                              ifelse(max.x >= 5, 0.5,
                               ifelse(max.x >= 1, 0.2, 
                                      0.1))))))))) ) +
    # Y-axis scale
    ggplot2::scale_y_continuous(limits = c(0, max.y),
                                breaks = seq(0, max.y * 2, 10)) +
    ggplot2::labs(x="Soil test value", y="Relative yield (%)",
                  title = "Cate & Nelson (1965)")+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position="none",
                   panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
## Outputs
  results <- list("n" = n, 
              "CRYV" = target,
              "CSTV" = CSTV,
              "R2" = R2.model,
              "AIC" = AIC,
              "BIC" = BIC,
              "RMSE" = RMSE,
              "quadrants" = quadrants.summary,
              "X2" = X2.test,
              "anova" = anova.model) 
  
  if (tidy == TRUE) {
    results <- dplyr::as_tibble(results[c(1:8)])
  } else {
    results <- results
  }

  if (plot == TRUE){
    return(cn65.ggplot)
  } else {
    return(results)
  }
}

#' @rdname cate_nelson_1965
#' @return boot_cn_1965: bootstrapping function
#' @export 
boot_cn_1965 <- 
  function(data, ry, stv, target = 90, n=5, ...) {
    # Allow customized column names
    x <- rlang::enquo(stv)
    y <- rlang::enquo(ry)
    # Empty global variables
    boot_id <- NULL
    boots <- NULL
    model <- NULL
    
    data %>%  
      dplyr::select(!!y, !!x, ...) %>%
      tidyr::expand_grid(boot_id = seq(1, n, by = 1)) %>%
      dplyr::group_by(boot_id, ...) %>%
      tidyr::nest(boots = c(!!x, !!y)) %>% 
      dplyr::mutate(boots = boots %>% 
                      map(function(boots) 
                        dplyr::slice_sample(boots, 
                                            replace = TRUE, n = nrow(boots))) ) %>% 
      dplyr::mutate(model = map(boots,
                                purrr::possibly(
                                  .f = ~soiltestcorr::cate_nelson_1965(
                                    data = ., ry = !!y, stv = !!x, 
                                    target = target),
                                  otherwise = NULL, quiet = TRUE)) ) %>%
      dplyr::select(-boots) %>% 
      dplyr::mutate(model = map(model, ~dplyr::as_tibble(.))) %>% 
      tidyr::unnest(cols = model) %>% 
      dplyr::ungroup()
  }