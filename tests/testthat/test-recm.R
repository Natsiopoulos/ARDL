test_that("correct results using ardl or uecm inputs", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    uecm_model <- uecm(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    ardl_model_c <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                         data = PSS2001, start = c(1972, 01),
                         order=c(6,1,5,4,5))
    uecm_model_c <- uecm(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                         data = PSS2001, start = c(1972, 01),
                         order=c(6,1,5,4,5))
    ardl_model_ct <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                          data = PSS2001, start = c(1972, 01),
                          order=c(6,1,5,4,5))
    uecm_model_ct <- uecm(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                          data = PSS2001, start = c(1972, 01),
                          order=c(6,1,5,4,5))

    expect_equal(recm(ardl_model, case=1), recm(uecm_model, case=1))

    expect_equal(recm(ardl_model_c, case=2), recm(ardl_model_c, case=2))
    expect_equal(recm(ardl_model_c, case=3), recm(ardl_model_c, case=3))

    expect_equal(recm(uecm_model_ct, case=4), recm(uecm_model_ct, case=4))
    expect_equal(recm(uecm_model_ct, case=5), recm(uecm_model_ct, case=5))

    # check that ect is correct
    ect_uecm <- uecm_model$coefficients[1]
    names(ect_uecm) <- "ect"
    expect_equal(recm(ardl_model, case=1)$coefficients["ect"], ect_uecm)

    ect_uecm <- uecm_model_c$coefficients[2]
    names(ect_uecm) <- "ect"
    expect_equal(recm(ardl_model_c, case=2)$coefficients["ect"], ect_uecm)
    expect_equal(recm(ardl_model_c, case=3)$coefficients["ect"], ect_uecm)

    ect_uecm <- uecm_model_ct$coefficients[3]
    names(ect_uecm) <- "ect"
    expect_equal(recm(ardl_model_ct, case=4)$coefficients["ect"], ect_uecm)
    expect_equal(recm(ardl_model_ct, case=5)$coefficients["ect"], ect_uecm)
})
