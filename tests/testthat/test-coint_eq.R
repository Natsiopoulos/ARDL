test_that("correct results", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_equal(coint_eq(ardl_model, case = 2), coint_eq(uecm(ardl_model), case = 2))
    expect_equal(coint_eq(ardl_model, case = 3), coint_eq(recm(ardl_model, case = 3)))
})

test_that("case 1", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_equal(coint_eq(ardl_model, case = 1), coint_eq(uecm(ardl_model), case = 1))
})

test_that("case 4 & case 5 & linear time trend", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    ardl_model_scale <- ardl(w ~ Prod + UR + Wedge + Union + trend(w, scale=FALSE) | D7475 + D7579,
                             data = PSS2001, start = c(1972, 01),
                             order=c(6,1,5,4,5))
    ardl_model_time <- ardl(w ~ Prod + UR + Wedge + Union + time(w) | D7475 + D7579,
                            data = ts(PSS2001), start = 9,
                            order=c(6,1,5,4,5))

    expect_equal(coint_eq(ardl_model, case = 4), coint_eq(uecm(ardl_model), case = 4))
    expect_equal(coint_eq(ardl_model, case = 5), coint_eq(uecm(ardl_model), case = 5))
    expect_equal(coint_eq(ardl_model_scale, case = 4), coint_eq(uecm(ardl_model_scale), case = 4))
    expect_equal(coint_eq(ardl_model_time, case = 4), coint_eq(uecm(ardl_model_time), case = 4))
})
