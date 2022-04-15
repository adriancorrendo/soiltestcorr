
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soiltestcorr: Soil Test Correlation and Calibration in R

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/adriancorrendo/soiltestcorr?branch=main&svg=true)](https://ci.appveyor.com/project/adriancorrendo/soiltestcorr)
[![R-CMD-check](https://github.com/adriancorrendo/soiltestcorr/workflows/R-CMD-check/badge.svg)](https://github.com/adriancorrendo/soiltestcorr/actions)

<!-- badges: end -->

<img src="man/figures/soiltestcorr_logo.png" align="right" height="200" style="float:right; height:200px;">
<br/>

You can install the development version of soiltestcorr from
[GitHub](https://github.com/adriancorrendo/soiltestcorr) with:

``` r
# install.packages("devtools")
devtools::install_github("adriancorrendo/soiltestcorr")
```

## For more details, check the [TUTORIAL Documentation](https://adriancorrendo.github.io/soiltestcorr/articles/Introduction_to_soiltestcorr.html) <br/>

# Description <br/>

The goal of `soiltestcorr` is to assist users on the analysis of
relationships between relative yield (RY) and soil test values (STV)
following different approaches. <br/>

Available functions (version 1.0.3, 04-08-2022): <br/>

## 1. Modified Arcsine-Log Calibration Curve <br/>

The first calibration method available is the Modified Arcsine-log
Calibration Curve (`mod_alcc()`) originally described by Dyson and
Conyers (2013) and modified by Correndo et al. (2017). This function
produces the estimation of critical soil test values (CSTV) for a target
relative yield (RY) with confidence intervals at adjustable confidence
levels. <br/>

<b> mod_alcc() </b> <br/>

Instructions <br/>

1.  Load your dataframe with soil test value (STV) and relative yield
    (RY) data. <br/>

2.  Specify the following arguments into the function -mod_alcc()-:
    <br/>

(a). `data` (optional), <br/>

(b). `STV` (soil test value) and `RY` (relative yield) columns or
vectors, <br/>

(c). `target` of relative yield (e.g. 90%), <br/>

(d). desired `confidence` level (e.g. 0.95 for 1 - alpha(0.05)). Used
for the estimation of critical soil test value (CSTV) lower and upper
limits. <br/>

(e). `plot` TRUE (produces a ggplot as main output) or FALSE -default-
(no plot, only results as list or data.frame), <br/>

(f). `tidy` TRUE (produces a data.frame with results) or FALSE-default-
(store results as list), <br/>

3.  Run and check results. <br/>

4.  Check residuals plot, and warnings related to potential leverage
    points. <br/>

5.  Adjust curve plots as desired. <br/>

Example of mod_alcc() output
<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## 2. Cate & Nelson Quadrants Analysis (1965) <br/>

`soiltestcorr` also allows users to implement the quadrants analysis
approach, also known as the Cate-Nelson analysis. There are two versions
of the Cate-Nelson technique: <br/>

Thus, the second alternative is based on Cate and Nelson (1965)
(`cate_nelson_1965()`). The first step of this method is to apply an
arbitrarily fixed value of RY as a target (y-axis) that divides the data
into two categories (below & equal or above RY target). In a second
stage, it estimates the CSTV (x-axis) as the minimum STV that divides
the data into four quadrants (target RY level combined with STV lower or
greater than the CSTV) maximizing the number of points under
well-classified quadrants (II, STV \>= CSTV & RY \>= RY target; and IV,
STV \< CSTV & RY \< RY target). This is also known as the “graphical”
version of the Cate-Nelson approach. <br/>

<b> cate_nelson_1965() </b> <br/>

Instructions <br/>

1.  Load your dataframe with soil test value (STV) and relative yield
    (RY) data. <br/>

2.  Specify the following arguments into the function
    -cate_nelson_1965()-: <br/>

(a). `data` (optional), <br/>

(b). `STV` (soil test value) and `RY` (relative yield) columns or
vectors, <br/>

(c). `plot` TRUE (produces a ggplot as main output) or FALSE (no plot,
only results as list or data.frame), <br/>

(d). `tidy` TRUE (produces a data.frame with results) or FALSE (store
results as list), <br/>

3.  Run and check results. <br/>

4.  Adjust plot as desired. <br/>

Example of cate_nelson_1965() output
<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## 3. Cate & Nelson Quadrants Analysis (1971) <br/>

The third alternative is based on Cate and Nelson (1971)
(`cate_nelson_1971()`). The first step of this alternative version is to
estimates the CSTV (x-axis) as the minimum STV that minimizes the
residual sum of squares when dividing data points in two classes (lower
or greater than the CSTV) without using a fixed RY. This refined version
does not constrains the model performance (measured with the coefficient
of determination -R2-) but the user has no control on the RY level for
the CSTV. This is also known as the “statistical” version of the
Cate-Nelson approach. <br/>

<b> cate_nelson_1971() </b> <br/>

Instructions <br/>

1.  Load your dataframe with soil test value (STV) and relative yield
    (RY) data. <br/>

2.  Specify the following arguments into the function
    -cate_nelson_1965()-: <br/>

(a). `data` (optional), <br/>

(b). `STV` (soil test value) and `RY` (relative yield) columns or
vectors, <br/>

(c). `plot` TRUE (produces a ggplot as main output) or FALSE (no plot,
only results as list or data.frame), <br/>

(d). `tidy` TRUE (produces a data.frame with results) or FALSE (store
results as list), <br/>

3.  Run and check results. <br/>

4.  Adjust plot as desired. <br/>

Example of cate_nelson_1971() output
<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## 4. Linear-plateau Regression </b> <br/>

The next calibration method available is the linear-plateau model
(`linear_plateau()`). This function fits the classical regression
response model that follows two phases: i) a first linear phase
described as y = b0 + b1 x, and ii) a second phase were the RY response
to increasing STV becomes NULL (flat), described as plateau = y = b0 +
b1 Xc, where Xc represents the CSTV. The function works automatically
with self starting initial values to facilitate the model to converge.
<br/>

<b> linear_plateau() </b> <br/>

Instructions <br/>

1.  Load your dataframe with soil test value (STV) and relative yield
    (RY) data. <br/>

2.  Specify the following arguments into the function
    -linear_plateau()-: <br/>

(a). `data` (optional), <br/>

(b). `STV` (soil test value) and `RY` (relative yield) columns or
vectors, <br/>

(c). `plot` TRUE (produces a ggplot as main output) or FALSE (no plot,
only results as data.frame), <br/>

(d). `resid` TRUE (produces plots with residuals analysis) or FALSE (no
plot), <br/>

3.  Run and check results. <br/>

4.  Check residuals plot, and warnings related to potential limitations
    of this model. <br/>

5.  Adjust curve plots as desired. <br/>

Example of linear_plateau() output
<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

<b> MORE FUNCTIONS COMING SOON…. </b> <br/>

<b> References </b> <br/>

*Correndo, A.A., Salvagiotti, F., García, F.O. and Gutiérrez-Boem, F.H.,
2017. A modification of the arcsine–log calibration curve for analysing
soil test value–relative yield relationships. Crop and Pasture Science,
68(3), pp.297-304. <https://doi.org/10.1071/CP16444> * <br/>

*Cate, R.B. Jr., and Nelson, L.A., 1965. A rapid method for correlation
of soil test analysis with plant response data. North Carolina Agric.
Exp. Stn., International soil Testing Series Bull. No. 1. * <br/>

*Cate, R.B. Jr., and Nelson, L.A., 1971. A simple statistical procedure
for partitioning soil test correlation data into two classes. Soil Sci.
Soc. Am. Proc. 35:658-659 * <br/>

*Dyson, C.B., Conyers, M.K., 2013. Methodology for online biometric
analysis of soil test-crop response datasets. Crop & Pasture Science 64:
435–441. <https://doi.org/10.1071/CP13009> * <br/>

*Warton, D.I., Wright, I.J., Falster, D.S., Westoby, M., 2006. Bivariate
line-fitting methods for allometry. Biol. Rev. Camb. Philos. Soc. 81,
259–291. <https://doi.org/10.1017/S1464793106007007> * <br/>
