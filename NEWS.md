# soiltestcorr 2.2.1

Updates of the new version:

1) Fixing duplicated ORCID on DESCRIPTION file.

2) Include the "quadratic" term estimate as an output of the quadratic_plateau() function. This term is useful to understand the curvature of the quadratic model.

3) Fixing the names of the vignettes to display correctly on the CRAN website.

####################
PREVIOUS VERSIONS
####################

# soiltestcorr 2.2.0

* Apr 7th, 2023. <br/>
Updates of this version:

1) Bootstrapping:
  - New bootstrapping functions for all models to better approximate distributions of CSTVs and parameters or derived quantities of interest. Functions include: (i) `boot_cn_1965()`, (ii) `boot_cn_1971()`, (iii) `boot_mod_alcc()`, (iv) `boot_linear_plateau()`, (v) `boot_quadratic_plateau()`, and (vi) `boot_mitscherlich()`.

2) P-value for non-linear models: 
  - Including `p-value` estimation for non-linear models using an F-test approach comparing the non-linear model vs. a null-model (intercept-only).
  
3) `cate_nelson_1965()` and `cate_nelson_1971()`:
  - tidy = TRUE as default
  - Including AIC, BIC, and RMSE of anova.model results
  - Add flexibility to STV scales
  - `boot...()` output including quadrants count

4) `mitscherlich()` updates: 
  - Adding RMSE, and BIC outputs
  - default type = 1
  - target default now 95 and should not be NULL (prevent CSTV = Inf)
  - tidy = TRUE default (output is tibble)
  - alternative names for type: "free" to "no restriction", "100" to "asymptote 100", and "fixed" to "asymptote 100 from 0".
  - Do not round in results table (rounding throws off bootstrap distributions), round only in display equations and plot
  - add stop if target = NULL
  - add warning if target > asymptote
  - make results into tidy tibble
  - use return() explicitly to return bootstrap data frame
  - in keeping with ggplot2 updates, line `sizes` are now line `linewidth`
  - add scale_x_continuous to attempt auto rescaling
  - corrected description of b parameter (as -x_intercept)
  - add y_intercept to output table (for comparing to y_intercept of LP and QP)
  - corrected XY order of STV and RY in boot_mitscherlich
  - added set.seed to bootstrapping section of vignette
  - added na.rm = TRUE on quantile calc

5) `linear_plateau()` and `quadratic_plateau()` updates:
  - Adding RMSE, and BIC outputs
  - default to tidy = TRUE
  - join point is now referred to as jp throughout the internal code - equation now shows JP value instead of text "CSTV"
  - simplified code repetition for readibility concerning the coef(lp_model)[[3]] by saving join point to `jp` variable instead
  - return tibble instead of dataframe (dplyr has tibble and as_tibble so no new dependencies)
  - if target = NULL, defaults to join point (promote the use of join point)
  - section CSTV for plateau or for target mistakenly checked if target is greater than CSTV, but it should be greater than plateau. Now it reports that targets greater than plateau will result in CSTV at join point.
  - STVt should be reported right after target in the table
  - Use lowerCL and upperCL instead of LL_cstv and UL_cstv
  - replace "Wald Conf. Interval" with "Wald, 95%"
  - Always calculate and report Wald CI for CSTV (join point parameter) now that we have a STVt to find a soil value at the target. The STVt doesn't represent CSTV
  - removed code for creating "predicted" data.frame as the qp_line is being used instead
  - cleaned up plotting code (line wraps, vlines with c(lower, upper) instead of two vlines)
  - move LP line up so it is underneath vlines
  - change size to linewidth arg in the line geoms
  - removed extra irrelevant columns in bootstrap output like Wald CI and equation for neater tibble
  - simplified vignette: changing group_map to group_modify to preserve tibble over lists

6) `mod_alcc()` update including:
  - including Root Mean Square Error (RMSE_alcc) of the ALCC curve with respect to observed RY values as a potential indicator of model's quality.
  - including the estimation of Akaike Information Criteria (AIC_sma), and Bayesian Information Criteria (BIC_sma) for the "underlying" bivariate SMA models.
  - using "tibble" instead of "dataframe".
  - implementing tidy=TRUE as default.
  - using linewidth instead of "size" for geom_line().
  - reordering variables of the output (e.g. goodness of fit indicators first)
  - Add AIC for ALCC model to predict RY in terms of the original units following and approximation of the logLik of the ALCC curve (Dr. Fernando Miguez contribution).
  - add scale_x_continuous to attempt auto rescaling on ggplot objects

7) New dependencies:
  - `nlraa` is now included as a dependency to facilitate self-start of non-linear models.
  - `smatr` is a new dependency (to get AIC and BIC of SMA models).

8) Documentation:
  - Updated documentation and vignettes including bootstrapping functions for each of the models.
  - New Bootstrapping vignette.
  - Add a `CITATION` file into "inst/" folder following `bibentry` format.
  
9) Code of Conduct and Contribution:
  - Including code of conduct and contribution guidelines.
  
  
10) Plots:
  - Add more flexibility to STV scales for default plots.
  
  
# soiltestcorr 2.1.2

* June 11th, 2022. <br/>

General maintenance update.
- Documentation updating authors' contributions, and mention main research projects supporting the development of the package (FRST and SIIL).
- Fixing minor bugs on the CSTV estimation (and plot) when using the "target" argument within linear_plateau() and quadratic_plateau() functions. When "target" is activated for these functions, output is STVt (stv-target) instead of CSTV (which corresponds to the plateau level)
- Minor typos on the vignettes.  

## Previous versions

# soiltestcorr 2.1.1

* First CRAN release, accepted on May 11th, 2022.

# soiltestcorr 2.1.1

* May, 10, 2022. <br/>

Maintenance update fixing minor details in documentation after feedback from CRAN submission of v2.1.0. 
Changes include: i) adding references in DESCRIPTION, functions, and vignettes; and ii) removing brackets from examples (replacing 'dontrun' by 'donttest').

# soiltestcorr 2.1.0

* May, 06, 2022. <br/>

Maintenance update fixing minor typos on vignettes (linear_plateau(), quadratic_plateau(), mitscherlich()), fixed bugs on examples within functions cate_nelson_1965(), cate_nelson_1971(), quadratic_plateau(), and mitscherlich().

* May, 05, 2022. <br/>

Soiltestcorr v2.0.2 is maintenance update fixing minor bugs related to checks before CRAN submission (e.g. spelling check, links, data source URLs, DOIs, etc.)

## Previous versions

* May, 04, 2022. <br/>

Major: soiltestcorr v2.0.1 fixes the versioning of the package (this version was previously named as v1.0.7); it 
introduces the Mitscherlich response model, and it introduces the the "target" argument into linear_plateau() and quadratic_plateau() functions, allowing to estimate critical soil test values for relative yields different than the plateau level. <br/>

Minor: added 'Date' field on DESCRIPTION file, removed unused Imports-packages from Description file, fixed DOI source of 'data_test.Rd' and 'freitas.Rd'. <br/>

Patch: fixes some minor typos in the comments within the functions. <br/>

* Apr, 28, 2022. The soiltestcorr v1.0.6 uses consistent arguments across all functions using lowercase (e.g. stv, ry) and includes warning and stop messages to improve users experience.

* Apr, 18, 2022. The soiltestcorr v1.0.5 includes the option for fitting self-starting quadratic-plateau models with the quadratic_plateau() function.

* Apr, 14, 2022. The soiltestcorr v1.0.4 updated its name (previously soiltestR) in order to follow good coding practices using all lowercase letters. For the same reason, the old modALCC() function was renamed as mod_alcc().

* The soiltestcorr v1.0.3 includes the option for fitting linear-plateau models, includes the 'plot' arg (Boolean) in all functions, and the 'tidy' arg for choosing outputs as type list (tidy = FALSE) or data.frame (tidy = TRUE).

* The soiltestcorr v1.0.2 includes the option for running the quadrants approach described by Cate & Nelson (1971).

* The soiltestcorr v1.0.1 includes the option for running the quadrants approach described by Cate & Nelson (1965).

* The soiltestcorr v1.0.0 includes the option for running the modified arcsine-log calibration curve described Correndo et al. (2017).
