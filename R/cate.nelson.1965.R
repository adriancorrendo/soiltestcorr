# CODE adapted from Mangiafico, S. S. (2013). Cate-Nelson Analysis for Bivariate Data Using R-project.
# The Journal of Extension, 51(5), Article 33. https://tigerprints.clemson.edu/joe/vol51/iss5/33

#' @title Cate & Nelson Quadrants Analysis, 1965
#' @description This function runs the quadrants analysis suggested by Cate and Nelson (1965)
#' @param data argument to call a data.frame or data.table containing the data
#' @param STV argument to call the vector or column containing the soil test value (STV) data
#' @param RY argument to call the vector or column containing the relative yield (RY) data
#' @param target argument to specify the RY target (numeric) to estimate the critical STV
#' @return it returns an object of type list containing the main results plus a ggplot object with the figure display
#' @details See Cate, R. B. Jr., and L. A. Nelson. 1965. A rapid method for correlation of soil test analysis
#' with plant response data. North Carolina Agric. Exp. Stn., International soil Testing Series l. No. 1.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{eval_tidy}},\code{\link[rlang]{defusing-advanced}}
#'  \code{\link[stats]{lm}},\code{\link[stats]{anova}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{annotate}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{theme}}
#' @rdname cate.nelson.1965
#' @export 
#' @importFrom rlang eval_tidy quo
#' @importFrom stats lm anova
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual scale_color_manual labs geom_vline geom_hline annotate theme_bw theme
cate.nelson.1965 <- function(data, STV, RY, target){

x <- rlang::eval_tidy(data = data, rlang::quo(STV) )

y <- rlang::eval_tidy(data = data, rlang::quo(RY) )

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

dataset$critical.y[1] <- 0
for(k in c(2:n))
{dataset$critical.y[k] <- (dataset$y[k] + dataset$y[k-1])/2}
dataset$critical.y[1] <- min(dataset$critical.y[2:n])

## Define the Target of Y variable
critical.y <- target
for (i in c(1:n))
{dataset$ygroup[i] <- if(dataset$y[i] < critical.y)  'c' else 'd'}

##-order by y, add critical.y variable, and determine final critical-y-
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
dataset3 <- subset(dataset, q.err == min.qerr)
critical.x <- dataset3$critical.x[1]
critical.x

## Reset y-grouping for final grouping 
#--------------------------------------------------------------
for (i in c(1:n))
{ dataset$q.i[i] <- with(dataset, ifelse
                         (dataset$xgroup[i]=='a' & dataset$ygroup[i]=='d', 1, 0))
dataset$q.ii[i] <- with(dataset, ifelse
                        (dataset$xgroup[i]=='b' & dataset$ygroup[i]=='d', 1, 0))
dataset$q.iii[i] <- with(dataset, ifelse
                         (dataset$xgroup[i]=='b' & dataset$ygroup[i]=='c', 1, 0))
dataset$q.iv[i] <- with(dataset, ifelse
                        (dataset$xgroup[i]=='a' & dataset$ygroup[i]=='c', 1, 0)) }

## ---------- results and perform chi-square -----------
q.I <- sum(dataset$q.i)
q.II <- sum(dataset$q.ii)
q.III <- sum(dataset$q.iii)
q.IV <- sum(dataset$q.iv)
row.1 <- c(q.I, q.II)
row.2 <- c(q.IV, q.III)
X2.test <- stats::chisq.test(data.frame(row.1,row.2))

## Performance of the model, coefficient of determination (R2) ----------------
aov.model <- stats::lm(y ~ xgroup, data=dataset)
anova.model <- stats::anova(aov.model)
R2.model <- anova.model[1,2]/sum(anova.model[,2])

## --------- final plot and clean up data set --------------
for(i in c(1:n))
{ dataset$Quadrant[i] <- "positive"}
dataset$Quadrant[(dataset$x<critical.x)&(dataset$y<critical.y)] <- "positive"
dataset$Quadrant[(dataset$x>=critical.x)&(dataset$y>=critical.y)] <- "positive"
dataset$Quadrant[(dataset$x>=critical.x)&(dataset$y<critical.y)] <- "negative"
dataset$Quadrant[(dataset$x<critical.x)&(dataset$y>=critical.y)] <- "negative"

max.x <- max(dataset$x)
max.y <- max(dataset$y)
min.x <- min(dataset$x)
min.y <- min(dataset$y)


final.plot <- 
ggplot2::ggplot(data = dataset, ggplot2::aes(x=x, y=y))+
  ggplot2::geom_point(aes(color = Quadrant, shape = Quadrant))+
  ggplot2::scale_shape_manual(name = "", values = c(4,16))+
  ggplot2::scale_color_manual(name = "", values = c("dark red","steelblue"))+
  ggplot2::labs(x="Soil test value", y="Relative yield (%)")+
  ggplot2::geom_vline(xintercept = critical.x, col = "dark red", linetype = "dashed")+
  ggplot2::geom_hline(yintercept = critical.y, col = "dark red", linetype = "dashed")+
  # Critical STV
  ggplot2::annotate(geom = "text", label = paste("CSTV =", critical.x, "ppm"),
    x = critical.x+1, y = 0, angle = 90, hjust = 0, vjust = 1, col = "grey25") +
  # RY target
  ggplot2::annotate(geom = "text", label = paste0("RY = ", round(critical.y, 0), "%"),
           x = max(dataset$x), y = critical.y-2, angle = 0, hjust = 1, vjust = 1, 
           col = "grey25")+
  # Quadrants
  ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.01, y =  min.y+(max.y-min.y)*0.99, 
           label = "I", size = 3, color = "dark red")+
  ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.99, y =  min.y+(max.y-min.y)*0.99, 
           label = "II", size = 3, color = "navy")+
  ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.99, y =  min.y+(max.y-min.y)*0.01, 
           label = "III", size = 3, color = "dark red")+
  ggplot2::annotate(geom = "label", x = min.x+(max.x-min.x)*0.01, y =  min.y+(max.y-min.y)*0.01, 
           label = "IV", size = 3, color = "navy")+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "top")

return(list("n" = n, 
            "target" = target,
            "CSTV" = critical.x,
            "quadrants" = as.data.frame(list("I" = q.I, "II" = q.II, 
                                        "III" = q.III, "IV" = q.IV)), 
            "X2" = X2.test,
            "anova" = anova.model,
            "R2" = R2.model,
            "plot" = final.plot))

}

