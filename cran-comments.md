# soiltestcorr 2.1.2 

* This is an update of the package. Precedent version (v2.1.1)


###########################################################

## 1. Local R CMD check results ─────────────────────── soiltestcorr 2.1.2 ────
Duration: 1m 51.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

###########################################################

## 2. WIN-DEVEL. Results devtools::check_win_devel()

Build log: https://win-builder.r-project.org/YRINbhxvJ9T8/

Platform: x86_64-w64-mingw32 (64-bit)

Status: 1 NOTE

Package update submission

Possibly misspelled words in DESCRIPTION:
  FRST (15:955)
  SIIL (15:1055)

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
- R-hub Windows Server 2022, R-devel, 64 bit, build log: https://builder.r-hub.io/status/soiltestcorr_2.1.2.tar.gz-57892e480c7243119e99688243d8c871

- R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC, build log: https://builder.r-hub.io/status/soiltestcorr_2.1.2.tar.gz-388ca33cd5c74d24b6b091254ba4e8d0

- R-hub Fedora Linux, R-devel, clang, gfortran, build log:
https://builder.r-hub.io/status/soiltestcorr_2.1.2.tar.gz-590ef7eb85f3421191601dc6748175dc

#################
# SUMMARY
#################

## R CMD check results
> On Windows Server 2022, R-devel, 64 bit

0 errors √ | 0 warnings √ | 2 notes x
  
  Possibly misspelled words in DESCRIPTION:
  FRST (15:955)
  SIIL (15:1055)

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
    From: DESCRIPTION
    Status: Forbidden
  DOI: 10.2307/2529422
    Message: 403

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

###########################################################
> On Ubuntu Linux 20.04.1 LTS, R-release, GCC

0 errors √ | 0 warnings √ | 1 note x
  
  Possibly misspelled words in DESCRIPTION:
  FRST (15:955)
  SIIL (15:1055)

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
    From: DESCRIPTION
    Status: Forbidden
  DOI: 10.2307/2529422
    Message: 403

###########################################################
> On Fedora Linux, R-devel, clang, gfortran
  
0 errors √ | 0 warnings √ | 1 note x

  Possibly misspelled words in DESCRIPTION:
  FRST (15:955)
  SIIL (15:1055)

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
    From: DESCRIPTION
    Status: Forbidden
  DOI: 10.2307/2529422
    Message: 403

###########################################################

*Author comments to NOTES: 

Mentioned words are not misspelled: FRST (Fertilizer Recommendation System Tool) and SIIL (Sustainable Intensification and Innovation Lab) stand for the name of two projects supporting related research. All the DOIs marked as possibly invalid work correctly. 
The 'lastMiKTeXException' note only appears on the Windows check. 

