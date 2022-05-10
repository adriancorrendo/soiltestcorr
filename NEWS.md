# soiltestcorr 2.1.1

* May, 10, 2022. <br/>

Maintenance update fixing minor details in documentation after feedback from CRAN submission of v2.1.0. 
Changes include: i) adding references in DESCRIPTION, functions, and vignettes; and ii) removing brackets from examples (replacing 'dontrun' by 'donttest').

## Previous versions

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
