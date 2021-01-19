# R API client for ODK API

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


## Overview

This is an almost complete implementation of the [ODK Central REST API](https://odkcentral.docs.apiary.io/#reference/forms-and-submissions) for R. I currently cannot continue work on it because of other priorities, so I make it available at an early alpha stage. 

### Features
* Uses R6 classes to implement token-controlled access to the API
* Always returns data frames
* For a slim footprint, it does not use any function of the _tidyverse_ , but only a the absolute necessary subset`jsonlite, httr, R6, glue`.  I like tidyverse function for short-lived reports, but have been bitten very often its frequent depreciations when maintaining a package on CRAN that uses the _tidyverse_ functions. The _tidyverse_ has well-tested functions to unnest data used in ruODK; for odkapi, it is planned to use XSLT for nested data, but these functions are not yet complete because my XSLT is rusty.
* Naming is close to the function names of the online [API](https://odkcentral.docs.apiary.io/#reference/forms-and-submissions) which has excellent documentation that should be used at the current state. 
* I have tried to make the functions orthogonal to those in Florian Mayer's [ruODK](https://github.com/ropensci/ruODK) ; this package is a lower-level complement to ruODK - see below for a comparison.
* odkapi has a full sets of tests against the online API, and against a mock version of the API using `httptest`, and against a live server. The `testlive`functions requires a working server; random projects named `testXXX` and users will be generated and deleted, so these should not interfere with your working project, but better use a dedicated test installation - there is no guarantee that nothing goes wild.
* It can generate QR codes; you could steal the code for your own use.

### When to use and not to use

* When you want to batch-retrieve and analyze data, use the higher-level implementation of [ruODK](https://github.com/ropensci/ruODK). Most importantly, it is well tested with geo-data and images - this package was tested with  text/numeric fields only.  I also do not have tested `odkapi`  for deeply nested data, and the  XSLT-base transformation of is not yet working - XSLT specialists are welcome to work on the stubs.
* ODK is very limited when you want to retrieve subsets of data, it was build mainly for batch processing; see this [discussion](https://forum.getodk.org/t/filter-data-in-odk-central/30204/11). I have written this package as an interface to mirror the data in a local warehouse database, so that retrieving individual records with searches can be implemented. 
* My application is a system for managing forms and data entry in a clinic. Since the web interface is very nerdy - even after using it for several months I need half a dozen of mis-clicks to find the place where the QR code is generated - I use the REST API to create a simplified GUI with Shiny that follows the  end-user workflow. 
* In theory, `odkapi` might be a bit faster than `ruODK`because it uses tokens instead of passwords to connect to the API server. I have not tested this, though, because it was of minor importance for me. 
* Cetero censeo, to keep the discussion up: ODK was build for access via Internet and it must use a public certificate, e.g. from letsencrypt. Using it in an Intranet setting with firewall stopping all external traffic as required by European Law for patient data is messy and [not supported](https://forum.getodk.org/t/selfsign-certificate-with-latest-central/25466/4?u=dmenne).  It requires the tricky installation of a [local certificate authority](https://forum.getodk.org/t/use-easy-rsa-to-set-up-a-self-signed-certificate-authority-for-central/29016/5) and in some cases a custom-built version of ODK collect (I did it!). It is surprising that the ODK team stresses the importance of security, but on the other hand needed quite some time to be convinced that a Google-only backup is not what the General Data Protection Regulation (GDPR) suggests;  thank to @Mattew_White for having implemented in 1.1 of the server.

### Missing

* Documentation and Examples. Use the ODK documentation in the [API](https://odkcentral.docs.apiary.io/#reference/forms-and-submissions). The test code in `tests/testlive` shows usage examples and has almost complete coverage.  While the API documentation is good, the returned mock data are sometimes incorrect; the live tests are more reliable.
* Data returned via XSLT transformations (`test-FormsApi_Apiary.R`). Tests of these functions fail currently.

### Thanks to

A first draft of the API was generated by converting the Apiary docu the [swagger-codegen](https://github.com/swagger-api/swagger-codegen) project.  This gave a basic framework for the interface, but the generated output was unusable and had to be totally reworked extensively.

@florianm for his great work on ruODK; I use it heavily for batch reporting.
