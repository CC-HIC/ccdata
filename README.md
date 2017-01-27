[![Build Status](https://travis-ci.com/UCL-HIC/ccdata.svg?token=tpqYy2kGKwjiyqQSznFy&branch=master)](https://travis-ci.com/UCL-HIC/ccdata)
[![codecov](https://codecov.io/gh/CC-HIC/ccdata/branch/master/graph/badge.svg)](https://codecov.io/gh/CC-HIC/ccdata)
[![AUR](https://img.shields.io/aur/license/yaourt.svg)]()

`ccdata` is an R package for working with the Critical Care Health Informatics
Collaborative's data set. Since 2014 data from the critical care units at
Cambridge, Guys/Kings/St Thomas', Imperial, Oxford, and University College
London has been extracted and stored securely in a standardised format. 

The purpose of the project is to enable researchers to answer clinical
questions that are important to patients, but which are normally too difficult
because data is unstandardised, siloed, and inaccessible. 

These data are crucially needed by healthcare professionals for the delivery
and continuity of care; by administrators for audit, planning and service
improvement; and by academic and industry researchers for the translation of
scientific progress into patient benefit. Through this process, CC-HIC can
improve patient outcomes, reduce the costs of care, and accelerate the pace of
translational health research. 

The physical database is held at the [UCL
IDHS](http://www.ucl.ac.uk/isd/itforslms/services/handling-sens-data/tech-soln)
within the Information Services Division of University College London (UCL).
UCL manage and ensure that the database and the surrounding governance
structures are appropriate for holding identifiable and sensitive NHS data. The
safe haven is compliant to NHS Information Governance Toolkit Level 2 and
operates to the ISO 27001, the Safe Haven already holds identifiable, sensitive
NHS data for secondary purpose. The Critical Care HIC management group will
maintain oversight and be the point of contact for researchers wishing to
access data.

The security put in place to ensure the safety of this resource inevitably
creates challenges for the researchers. We have therefore created a three sided
tool to make CCHIC _research ready_.

- this shared code library
- an anonymised development data set 
- a virtual machine for simulating work within the safe haven


## Required packages
* R (>= 3.1.0),
* XML,
* data.table,
* yaml,
* pander,
* Rcpp,
* methods


## How to install the R package
### Mac & Linux
```
git clone git@github.com:UCL-HIC/ccdata.git
R CMD INSTALL ccdata # "sudo R CMD INSTALL ccdata" if root access is required.
```
### RStudio
* Download the `tar` file from ccdata Github page.
* In the `package` panel click the button `install`.
* select `Package Archive File (.tgz, .tar.gz)` for the `select from` tab.
* click install

## How to contribute
The `ccdata` package is currently underdevelopment. We wellcome users using,
commenting about the code on the master branch. If you have any question, you
can just raise an isssue on Github or contact the developers via email
(s.shi@ucl.ac.uk). Please let us know if you also want make contribution to the
code development. 
