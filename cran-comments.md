# soiltestcorr 2.1.1 

* This is a re-submission. Precedent version (v2.1.0, SHA: c7346613269713327ed29dee65b24cb36de72226)


###########################################################

## 1. Local R CMD check results 

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

###########################################################

## 2. WIN-DEVEL. Results devtools::check_win_devel()

Build log: https://win-builder.r-project.org/EbqGOB2VxucR

Status: 1 NOTE

New submission

Possibly misspelled words in DESCRIPTION:
  Cate (13:374, 13:406, 13:448, 13:480)
  Correndo (13:310)
  Melsted (13:779)
  Mitscherlich (13:754)
  al (13:322)
  arcsine (13:279)
  et (13:319)

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2134/agronj1994.00021962008600010033x
    From: man/quadratic_plateau.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.2134/asaspecpub29.c1
    From: man/mitscherlich.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.2136/sssaj1971.03615995003500040048x
    From: man/cate_nelson_1971.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.2307/2529422
    From: man/linear_plateau.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2134/agronj1994.00021962008600010033x
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
  DOI: 10.2134/asaspecpub29.c1
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
  DOI: 10.2136/sssaj1971.03615995003500040048x
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
  DOI: 10.2307/2529422
    From: DESCRIPTION
    Status: Forbidden
    Message: 403


###########################################################

# 3. R-hub check, results from devtools::check_rhub()

## Test environments
- R-hub windows-x86_64-devel (r-devel), build log: https://builder.r-hub.io/status/soiltestcorr_2.1.1.tar.gz-fc3530a292c1427790ca08c2a7463981

- R-hub ubuntu-gcc-release (r-release), build log: https://builder.r-hub.io/status/soiltestcorr_2.1.1.tar.gz-50db37e4acf641f7992fd285701b4c97

- R-hub fedora-clang-devel (r-devel), build log:
https://builder.r-hub.io/status/soiltestcorr_2.1.1.tar.gz-3f95bcee52374b9893332c657ef74808

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Adrian A. Correndo <correndo@ksu.edu>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Cate (13:374, 13:406, 13:448, 13:480)
    Correndo (13:310)
    Melsted (13:779)
    Mitscherlich (13:754)
    al (13:322)
    arcsine (13:279)
    et (13:319)
  
  Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.2134/agronj1994.00021962008600010033x
      From: man/quadratic_plateau.Rd
      Status: 503
      Message: Service Unavailable
    URL: https://doi.org/10.2134/asaspecpub29.c1
      From: man/mitscherlich.Rd
      Status: 503
      Message: Service Unavailable
    URL: https://doi.org/10.2136/sssaj1971.03615995003500040048x
      From: man/cate_nelson_1971.Rd
      Status: 503
      Message: Service Unavailable
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.2134/agronj1994.00021962008600010033x
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503
    DOI: 10.2134/asaspecpub29.c1
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503
    DOI: 10.2136/sssaj1971.03615995003500040048x
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors √ | 0 warnings √ | 2 notes x

###########################################################

*Author comments to NOTES: 

Mentioned words are not misspelled: Cate, Correndo, Melsted, and Mitscherlich are last names, arcsine is a trigonometry definition for the "inverse of sine function", and et al are just the latin of "and col" for citation purposes.
All links marked as (possibly) invalid are actually working fine and using secured address.
The 'lastMiKTeXException' note only appears on this check. 

