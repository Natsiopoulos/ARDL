test_that("verify prediction results all cases", {
    # Using data.frame as the test case, time series objects tested separately
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
    # Note: Ignores small numeric differences
    expect_equal(case_i, result_case_i, tolerance=1e-3)
    expect_equal(case_ii_iii, result_case_ii_iii, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v_time, tolerance=1e-3)
    expect_equal(case_iv_v, result_case_iv_v_trend_scale, tolerance=1e-3)
})

test_that("unsupported newdata object", {
    newdata <- matrix(c(1,2,3,  4,5,6,  7,8,9,  11,22,33,  0,1,0,  1,0,0),
                      nrow = 3,
                      ncol = 6
                      )
    case_i <- ardl(w ~ Prod + UR + Wedge + Union - 1 | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    expect_error(predict.ardl(case_i, newdata))
})

test_that("verify equivalence of different newdata types", {
    library(zoo)
    model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # dataframe
    newdata_df <- data.frame(
        "Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
        "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    result_df <- predict.ardl(model, newdata_df)
    # setting frequency and start timestamp as expected for time series objects
    # ts object
    newdata_ts_timestamp <- ts(newdata_df, start = c(1998, 1), frequency = 4)
    result_ts_timestamp <- predict.ardl(model, newdata_ts_timestamp)
    expect_equal(unname(result_df), c(unname(result_ts_timestamp)))
    # zoo object
    dates <- as.yearqtr(c("1998 1", "1998 2", "1998 3"), format = "%Y %q")
    newdata_zoo_timestamp <- zoo(newdata_df, dates)
    result_zoo_timestamp <- predict.ardl(model, newdata_zoo_timestamp)
    expect_equal(unname(result_df), c(coredata(result_zoo_timestamp)))
    # zooreg object
    newdata_zooreg_timestamp <- zooreg(newdata_df, start = c(1998, 1), frequency = 4)
    result_zooreg_timestamp <- predict.ardl(model, newdata_zooreg_timestamp)
    expect_equal(unname(result_df), c(coredata(result_zooreg_timestamp)))
})

test_that("check integrity of newdata and ardl object", {
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
})

test_that("check time series objects", {
    PSS2001_ts <- ts(PSS2001, start = c(1970, 1), frequency = 4)
    PSS2001_zoo <- as.zoo(window(PSS2001, start = as.yearqtr("1970 1", format = "%Y %q"), end = as.yearqtr("1997 4", format = "%Y %q")))
    PSS2001_zooreg <- zooreg(PSS2001, start = c(1970, 1), frequency = 4)
    
    ardl_ts <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001_ts)
    ardl_zoo <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001_zoo)
    ardl_zooreg <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001_zooreg)
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    # Check frequency is the same
    # PSS2001 has frequency = 4
    newdata_ts <- ts(newdata, start = c(1998, 1), frequency = 3)
    expect_error(predict.ardl(ardl_ts, newdata_ts))
    newdata_zoo <- zoo(newdata, frequency = 1)
    expect_error(predict.ardl(ardl_zoo, newdata_zoo))
    newdata_zooreg <- zooreg(newdata, start = c(1998, 1), frequency = 5)
    expect_error(predict.ardl(ardl_zooreg, newdata_zooreg))
    # Check time series are consecutive
    newdata_ts <- ts(newdata, start = c(1997, 3), frequency = 4)
    expect_error(predict.ardl(ardl_ts, newdata_ts))
    dates <- as.yearqtr(c("1998 2", "1998 3", "1998 4"), format = "%Y %q")
    newdata_zoo <- zoo(newdata, dates)
    expect_error(predict.ardl(ardl_zoo, newdata_zoo))
    newdata_zooreg <- zooreg(newdata, start = c(1995, 2), frequency = 4)
    expect_error(predict.ardl(ardl_zooreg, newdata_zooreg))
    # As a double check we'll carry out basic check on monthly frequency
    PSS2001_monthly <- ts(coredata(PSS2001), start = c(1970, 1), frequency = 12)
    ardl_monthly <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001_monthly)
    # Incorrect frequency
    newdata_ts <- ts(newdata, start = c(1979, 5), frequency = 3)
    expect_error(predict.ardl(ardl_monthly, newdata_ts))
    # Not consecutive
    newdata_ts <- ts(newdata, start = c(1980, 1), frequency = 4)
    expect_error(predict.ardl(ardl_monthly, newdata_ts))
})