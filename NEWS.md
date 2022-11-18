# ARDL 0.2.1

### New features

* The `bounds_f_test` and `bounds_t_test` functions now have a new returned 
value `PSS2001parameters`, if `alpha` is one of the 0.1, 0.05, 0.025 or 0.01, 
`exact = FALSE` and k <= 10. These are the critical value bounds presented in 
Pesaran et al. (2001) but are less precise. Previously, only these were reported
in `parameters`.  
e.g. In the following marginal case, according to the critical values of 
Pesaran et al. (2001), H0 is rejected but according to the p-value is not.

> m1 <- uecm(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
           order = c(5, 1, 5, 5, 5), data = PSS2001, start = c(1972, 01))  
> btt <- bounds_t_test(m1, case = 3, alpha = 0.05)}  
> btt$tab  
> btt$PSS2001parameters

### Documentation update

* Added `BugReports` in the `DESCRIPTION`.

* Updated the references in `README` and `CITATION`.

* Added codecov and badges.

### Bug fix

* `multipliers()` was giving an error when trying to estimate short-run and
interim multipliers using a model without a constant term.

### Automated tests

* Added automated tests.

### New data

* Added the data used in Pesaran et al. (2001)

### Minor changes

* Added dependency on R >= 3.5.0 because serialized objects in serialize/load 
version 3 cannot be read in older versions of R. File(s) containing such 
objects: ‘ARDL/data/PSS2001.rda’

---

# ARDL 0.2.0

### Documentation update

* Add DOI and references of the publication where the package is used and 
successfully replicates the results of the methodology it implements, in the 
`DESCRIPTION` and `README.Rmd` files.

* Properly changed the citation info in the `CITATION` and `ardl-package.R`files.

---

# ARDL 0.1.1

### New features

* The `multipliers()` function now supports the calculation of short-run and
interim multipliers.

### Documentation improvements

* Corrected the example about the `search_type` argument in the help file of
`auto_ardl()`. 

* Minor correction in the help file of `auto_ardl()`.

* Corrected the mathematical formula of the Long-Run multipliers in the help
file of `multipliers()`.

* Corrected the mathematical formula of the bounds F-test in the help file of
`bounds_f_test()`.

* Added the mathematical formulas of the short-run and interim multipliers in 
the help file of `multipliers()`.

* Properly changed the citation info in the `CITATION`, `ardl-package.R` and  
`DESCRIPTION` files.

* Changed the designer's contact info in the logo.

* Added a small example about short-run multipliers in the `README.md` file.

* Corrected the release date of version 0.1.0 in the `NEWS.md` file.

---

# ARDL 0.1.0  (10 Apr 2020)

## Released to CRAN
