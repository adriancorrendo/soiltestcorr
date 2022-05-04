# soiltestcorr 2.0.1

* May, 04, 2022. <br/>

Major: soiltestcorr v2.0.1 fixes the versioning of the package (this version was previously named as v1.0.7); it 
introduces the Mitscherlich response model, and it introduces the the "target" argument into linear_plateau() and quadratic_plateau() functions, allowing to estimate critical soil test values for relative yields different than the plateau level. <br/>

Minor: added 'Date' field on DESCRIPTION file, removed unused Imports-packages from Description file, fixed DOI source of 'data_test.Rd' and 'freitas.Rd'. <br/>

Patch: fixes some minor typos in the comments within the functions. <br/>

## Previous versions

* Apr, 28, 2022. The soiltestcorr v1.0.6 uses consistent arguments across all functions using lowercase (e.g. stv, ry) and includes warning and stop messages to improve users experience.

* Apr, 18, 2022. The soiltestcorr v1.0.5 includes the option for fitting selfs-starting quadratic-plateau models with the quadratic_plateau() function.

* Apr, 14, 2022. The soiltestcorr v1.0.4 updated its name (previously soiltestR) in order to follow good coding practices using all lowercase letters. For the same resason, the old modALCC() function was renamed as mod_alcc().

* The soiltestcorr v1.0.3 includes the option for fitting linear-plateau models, includes the 'plot' arg (boolean) in all functions, and the 'tidy' arg for choosing outputs as type list (tidy = FALSE) or data.frame (tidy = TRUE).

* The soiltestcorr v1.0.2 includes the option for running the quadrants approach described by Cate & Nelson (1971).

* The soiltestcorr v1.0.1 includes the option for running the quadrants approach described by Cate & Nelson (1965).

* The soiltestcorr v1.0.0 includes the option for running the modified arcsine-log calibration curve described Correndo et al. (2017).
