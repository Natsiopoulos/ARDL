test_that("correct results using ardl or direct inputs", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    uecm_model <- uecm(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_equal(uecm_model, uecm(ardl_model))

    # check if non time-series data work
    ardl_model_df <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                          data = data.frame(PSS2001), start = 9,
                          order=c(6,1,5,4,5))
    arduecm_model_df <- uecm(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                             data = data.frame(PSS2001), start = 9,
                             order=c(6,1,5,4,5))

    expect_equal(uecm_model$coefficients, uecm(ardl_model_df)$coefficients)
    expect_equal(uecm_model$coefficients, arduecm_model_df$coefficients)
})
