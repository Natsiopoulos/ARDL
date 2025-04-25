test_that("verify prediction results all cases", {
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    # Case 1: No intercept and no trend
    case_i <- ardl(w ~ Prod + UR + Wedge + Union - 1 | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # Case 2 & 3: Restricted intercept and no trend / Unrestricted intercept and no trend
    case_ii_iii <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # Case 4 & 5: Unrestricted intercept and restricted trend / Unrestricted intercept and unrestricted trend
    case_iv_v <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # Alternative trend methods
    case_iv_v_time <- ardl(w ~ Prod + UR + Wedge + Union + time(w) | D7475 + D7579, order =c(3,0,1,4,3), data = ts(PSS2001))
    case_iv_v_trend_scale <- ardl(w ~ Prod + UR + Wedge + Union + trend(w, scale = FALSE) | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    result_case_i <- predict.ardl(case_i, newdata)
    result_case_ii_iii <- predict.ardl(case_ii_iii, newdata)
    result_case_iv_v <- predict.ardl(case_iv_v, newdata)
    result_case_iv_v_time <- predict.ardl(case_iv_v_time, newdata)
    result_case_iv_v_trend_scale <- predict.ardl(case_iv_v_trend_scale, newdata)
    # These are externally calculated
    case_i <- setNames(c(-13.1721023693382, -40.3134240474061, -57.8052029786566), seq(1, 3))
    case_ii_iii <- setNames(c(-7.40819925882942, -24.8395014334613, -29.1887358923117), seq(1, 3))
    case_iv_v <- setNames(c(-6.61455842285156, -23.5381169607463, -27.919945003499), seq(1, 3))
    # Note expect equal ignores small numeric differences
    expect_equal(case_i, result_case_i, tolerance=1e-3)
    expect_equal(case_ii_iii, result_case_ii_iii, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v_time, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v_trend_scale, tolerance=1e-3)
})

test_that("verify equivalence of different newdata types", {
    library(zoo)
    model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # dataframe
    newdata_df <- data.frame(
        "Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
        "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    # ts object, no timestamp
    newdata_ts <- ts(newdata_df)
    result_df <- predict.ardl(model, newdata_df)
    result_ts <- predict.ardl(model, newdata_ts)
    expect_equal(result_ts, result_df)
    # ts object with timestamp
    newdata_ts_timestamp <- ts(newdata_df, start = c(1998, 1))
    result_ts_timestamp <- predict.ardl(model, newdata_ts_timestamp)
    expect_equal(result_ts_timestamp, result_df)
    # zoo object
    newdata_zoo <- zoo(newdata_df)
    result_zoo <- predict.ardl(model, newdata_zoo)
    expect_equal(result_zoo, result_df)
    # zooreg object
    newdata_zooreg <- zooreg(newdata_df, start = c(1998, 1), frequency = 4)
    result_zooreg <- predict.ardl(model, newdata_zooreg)
    expect_equal(result_zooreg, result_df)
})

test_that("check integrity of newdata", {
    # NA values
    newdata <- data.frame("Prod" = c(NA,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    expect_error(predict.ardl(ardl_model, newdata))
    # Empty newdata
    expect_error(predict.ardl(ardl_model))
    # Not ARDL model
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    uecm_model <- uecm(ardl_model)
    expect_error(predict.ardl(uecm_model, newdata))
    # Missing cols
    newdata <- data.frame("UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    expect_error(predict.ardl(ardl_model, newdata))
    # Give newdata in another order
    newdata <- data.frame("UR"= c(4,5,6), "Prod" = c(1,2,3), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    expect_error(predict.ardl(ardl_model, newdata))
    # Give newdata with non-consecutive times
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    newdata_ts <- ts(newdata, start = c(2000, 1))
    predict.ardl(ardl_model, newdata_ts)
    # This should ignore different timestamp and be equivalent
    expect_equal(predict.ardl(ardl_model, newdata_ts),
                 setNames(c(-7.40819925882942, -24.8395014334613, -29.1887358923117), seq(1, 3)))
})






