test_that("case 1", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(5,4,5,1,6))
    bounds_test <- bounds_f_test(ardl_model, case = 1)
    expected_tab <- data.frame(statistic=4.5677603, p.value=0.0082357202)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # bounds_t_test
    bounds_test <- bounds_t_test(ardl_model, case = 1)
    expected_tab <- data.frame(statistic=1.1355325, p.value=0.99210825)
    row.names(expected_tab) <- "t"
    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))
})

test_that("case 4", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    bounds_test <- bounds_f_test(ardl_model, case = 4)
    expected_tab <- data.frame(statistic=4.6919878, p.value=0.014067839)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))
})

test_that("test = 'Chisq'", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    bounds_test <- bounds_f_test(ardl_model, case = 3, test = "Chisq")
    expected_tab <- data.frame(statistic=27.750442, p.value=0.0044907254)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))
})

test_that("uecm input", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    bounds_test <- bounds_f_test(uecm(ardl_model), case = 3)
    expected_tab <- data.frame(statistic=5.5500885, p.value=0.0044907254)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # bounds_t_test
    bounds_test <- bounds_t_test(uecm(ardl_model), case = 3)
    expected_tab <- data.frame(statistic=-3.914273, p.value=0.06086908)
    row.names(expected_tab) <- "t"
})

test_that("exact = FALSE & kx <= 10 & alpha %in% c(0.1, 0.05, 0.025, 0.01)", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    bounds_test <- bounds_f_test(ardl_model, case = 3, alpha = 0.05)
    expected_tab <- data.frame(statistic=5.5500885,
                               "Lower-bound I(0)" = 2.8942417, "Upper-bound I(1)" = 4.0273194,
                               alpha = 0.05, p.value=0.0044907254)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # & PSS2001 critical values
    expected_critvals <- c("Lower-bound I(0)" = 2.86, "Upper-bound I(1)" = 4.01)
    expect_equal(bounds_test$PSS2001parameters, expected_critvals)

    # bounds_t_test
    bounds_test <- bounds_t_test(ardl_model, case = 3, alpha = 0.05)
    expected_tab <- data.frame(statistic=-3.9142734,
                               "Lower-bound I(0)" = -2.8649811, "Upper-bound I(1)" = -4.0002457,
                               alpha = 0.05, p.value=0.060869085)
    row.names(expected_tab) <- "t"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # & PSS2001 critical values
    expected_critvals <- c("Lower-bound I(0)" = -2.86, "Upper-bound I(1)" = -3.99)
    expect_equal(bounds_test$PSS2001parameters, expected_critvals)

    # & test = "Chisq" & case %in% c(1, 3, 5)
    bounds_test <- bounds_f_test(ardl_model, case = 3, alpha = 0.1, test = "Chisq")

    expected_tab <- data.frame(statistic=27.750442,
                               "Lower-bound I(0)" = 12.310873, "Upper-bound I(1)" = 17.6785096,
                               alpha = 0.1, p.value=0.0044907254)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # & PSS2001 critical values
    expected_critvals <- c("Lower-bound I(0)" = 12.25, "Upper-bound I(1)" = 17.60)
    expect_equal(bounds_test$PSS2001parameters, expected_critvals)

    # & test = "Chisq" & case %in% c(2, 4)
    bounds_test <- bounds_f_test(ardl_model, case = 2, alpha = 0.01, test = "Chisq")

    expected_tab <- data.frame(statistic=66.391527,
                               "Lower-bound I(0)" = 20.0125226, "Upper-bound I(1)" = 26.16848,
                               alpha = 0.01, p.value=1e-06)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # & PSS2001 critical values
    expected_critvals <- c("Lower-bound I(0)" = 19.74, "Upper-bound I(1)" = 26.22)
    expect_equal(bounds_test$PSS2001parameters, expected_critvals)
})

test_that("exact = FALSE & kx <= 10 & alpha %in% c(0.005 0.075 0.150 0.200) & test = Chisq", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    bounds_test <- bounds_f_test(ardl_model, case = 5, alpha = 0.15, test="Chisq")
    expected_tab <- data.frame(statistic=19.567343,
                               "Lower-bound I(0)" = 13.83472, "Upper-bound I(1)" = 18.6088492,
                               alpha = 0.15, p.value=0.118128143)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # bounds_t_test (not test = Chisq, only t-test)
    bounds_test <- bounds_t_test(ardl_model, case = 5, alpha = 0.15)
    expected_tab <- data.frame(statistic=-2.63792694,
                               "Lower-bound I(0)" = -2.9359784, "Upper-bound I(1)" = -3.816559,
                               alpha = 0.15, p.value=0.61754341)
    row.names(expected_tab) <- "t"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))

    # case %in% c(2, 4)
    bounds_test <- bounds_f_test(ardl_model, case = 4, alpha = 0.15, test="Chisq")
    expected_tab <- data.frame(statistic=28.151927,
                               "Lower-bound I(0)" = 14.7611036, "Upper-bound I(1)" = 19.5069829,
                               alpha = 0.15, p.value=0.0140678391)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=1000))
})

test_that("NOT exact = FALSE & kx <= 10 & alpha %in% c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.150, 0.200)", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    set.seed(2022)
    bounds_test <- bounds_f_test(ardl_model, case = 3, alpha = 0.06, exact = TRUE, R= 10000)
    expected_tab <- data.frame(statistic=5.5500885,
                               "Lower-bound I(0)" = 2.8971731, "Upper-bound I(1)" = 4.0672186,
                               alpha = 0.06, p.value=0.007782627)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=104))

    # bounds_t_test
    set.seed(2022)
    bounds_test <- bounds_t_test(ardl_model, case = 3, alpha = 0.06, exact = TRUE, R= 10000)
    expected_tab <- data.frame(statistic=-3.9142734,
                               "Lower-bound I(0)" = -2.8126763, "Upper-bound I(1)" = -3.9143047,
                               alpha = 0.06, p.value=0.060005925)
    row.names(expected_tab) <- "t"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=104))

    # test = Chisq
    set.seed(2022)
    bounds_test <- bounds_f_test(ardl_model, case = 3, alpha = 0.06, exact = TRUE,
                                 R= 10000, test = "Chisq")
    expected_tab <- data.frame(statistic=27.750442,
                               "Lower-bound I(0)" = 14.4858655, "Upper-bound I(1)" = 20.336093,
                               alpha = 0.06, p.value=0.007782627)
    row.names(expected_tab) <- "Chisq"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=104))

    # Clearly rejects H0 (p.value=1e-06)
    set.seed(2022)
    bounds_test <- bounds_f_test(ardl_model, case = 2, alpha = 0.06, exact = TRUE, R= 10000)
    expected_tab <- data.frame(statistic=11.0652544,
                               "Lower-bound I(0)" = 2.59362986, "Upper-bound I(1)" = 3.57422109,
                               alpha = 0.06, p.value=1e-06)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab, ignore_attr = TRUE)
    expect_equal(bounds_test$null.value, c(k=4, T=104))
})

test_that("exact = TRUE", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    set.seed(2022)
    bounds_test <- bounds_f_test(ardl_model, case = 3, exact = TRUE, R = 10000)
    expected_tab <- data.frame(statistic=5.5500885, p.value=0.007782627)
    row.names(expected_tab) <- "F"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=104))

    # bounds_t_test
    set.seed(2022)
    bounds_test <- bounds_t_test(ardl_model, case = 3, exact = TRUE, R = 10000)
    expected_tab <- data.frame(statistic=-3.9142734, p.value=0.060005925)
    row.names(expected_tab) <- "t"

    expect_equal(bounds_test$tab, expected_tab)
    expect_equal(bounds_test$null.value, c(k=4, T=104))
})

test_that("is.null(alpha) & (pvalue == FALSE) causes expected error", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_error(bounds_f_test(ardl_model, case = 3, pvalue = FALSE),
                 "Specify an 'alpha' level, set 'pvalue' = TRUE or both")
    # bounds_t_test
    expect_error(bounds_t_test(ardl_model, case = 3, pvalue = FALSE),
                 "Specify an 'alpha' level, set 'pvalue' = TRUE or both")
})

test_that("alpha is acceptable value", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_error(bounds_f_test(ardl_model, case = 3, alpha = 0.00000005),
                 paste0("alpha must be either 'NULL' or one of the numbers produced by the following code:\n",
                        "c\\(seq\\(0.001, 0.1, by = 0.001\\), seq\\(0.11, 0.99, by = 0.01\\)\\)"))
    # bounds_t_test
    expect_error(bounds_t_test(ardl_model, case = 3, alpha = 0.00000005),
                 paste0("alpha must be either 'NULL' or one of the numbers produced by the following code:\n",
                        "c\\(seq\\(0.001, 0.1, by = 0.001\\), seq\\(0.11, 0.99, by = 0.01\\)\\)"))
})

test_that("case %in% c(2, 4) causes expected error", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    expect_error(bounds_t_test(ardl_model, case = 2),
                 "The t-bounds test applies only when 'case' is either 1, 3 or 5")
})
