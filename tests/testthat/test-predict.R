test_that("verify prediction results all cases - df", {
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    # Case 1: No intercept and no trend
    case_i <- ardl(w ~ Prod + UR + Wedge + Union - 1 | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # Case 2 & 3: Restricted intercept and no trend / Unrestricted intercept and no trend
    case_ii_iii <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    # Case 4 & 5: Unrestricted intercept and restricted trend / Unrestricted intercept and unrestricted trend
    case_iv_v <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    result_case_i <- predict.ardl(case_i, newdata)
    result_case_ii_iii <- predict.ardl(case_ii_iii, newdata)
    result_case_iv_v <- predict.ardl(case_iv_v, newdata)
    # These are externally calculated
    case_i_y1 <- -13.1721023693382
    case_i_y2 <- -40.3134240474061
    case_i_y3 <- -57.8052029786566
    case_ii_iii_y1 <- -7.40819925882942
    case_ii_iii_y2 <- -24.8395014334613
    case_ii_iii_y3 <- -29.1887358923117
    case_iv_v_y1 <- -6.61455842285156
    case_iv_v_y2 <- -23.5381169607463
    case_iv_v_y3 <- -27.919945003499
    # Note expect equal ignores small numeric differences
    expect_equal(c(case_i_y1, case_i_y2, case_i_y3), result_case_i)
    expect_equal(c(case_ii_iii_y1, case_ii_iii_y2, case_ii_iii_y3), result_case_ii_iii)
    expect_equal(c(case_iv_v_y1, case_iv_v_y2, case_iv_v_y3), result_case_iv_v)
})

test_that("verify prediction results all cases - kn", {
    newdata <- data.frame("Prod" = c(1,2,3), "UR"= c(4,5,6), "Wedge"=c(7,8,9), "Union" = c(11,22,33),
                          "D7475"= c(0,1,0), "D7579"=c(1,0,0))
    # Case 1: No intercept and no trend
    case_i <- ardl(w ~ Prod + UR + Wedge + Union - 1 | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    case_i_y1 <- t(matrix(case_i$coefficients)) %*% matrix(c(3.541190, 3.528882, 3.525881,
                                                1,
                                                4, 1.898443,
                                                7, -0.3640409, -0.3627026, -0.3582229, -0.3523980,
                                                11, -0.6529655, -0.6529655, -0.6529655,
                                                0, 1))
    #T+1
    #-13.1721
    case_i_y2 <- t(matrix(case_i$coefficients)) %*% matrix(c(-13.1721, 3.541190, 3.528882,
                                                2,
                                                5, 4,
                                                8, 7, -0.3640409, -0.3627026, -0.3582229,
                                                22, 11, -0.6529655, -0.6529655,
                                                1, 0))
    #T+2
    #-40.31342
    case_i_y3 <- t(matrix(case_i$coefficients)) %*% matrix(c(-40.31342, -13.1721, 3.541190,
                                                3,
                                                6, 5,
                                                9, 8, 7, -0.3640409, -0.3627026,
                                                33, 22, 11, -0.6529655,
                                                0, 0))
    # Case 2 & 3: Restricted intercept and no trend / Unrestricted intercept and no trend
    case_ii_iii <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    case_ii_iii_y1 <- t(matrix(case_ii_iii$coefficients)) %*% matrix(c(1,
                                                3.541190, 3.528882, 3.525881,
                                                1,
                                                4, 1.898443,
                                                7, -0.3640409, -0.3627026, -0.3582229, -0.3523980,
                                                11, -0.6529655, -0.6529655, -0.6529655,
                                                0, 1))
    #T+1
    #-7.408199
    case_ii_iii_y2 <- t(matrix(case_ii_iii$coefficients)) %*% matrix(c(1,
                                                -7.408199, 3.541190, 3.528882,
                                                2,
                                                5, 4,
                                                8, 7, -0.3640409, -0.3627026, -0.3582229,
                                                22, 11, -0.6529655, -0.6529655,
                                                1, 0))
    #T+2
    #-24.8395
    case_ii_iii_y3 <- t(matrix(case_ii_iii$coefficients)) %*% matrix(c(1,
                                                -24.8395, -7.408199, 3.541190,
                                                3,
                                                6, 5,
                                                9, 8, 7, -0.3640409, -0.3627026,
                                                33, 22, 11, -0.6529655,
                                                0, 0))
    #T+3
    #-29.18874
    
    # Case 4 & 5: Unrestricted intercept and restricted trend / Unrestricted intercept and unrestricted trend
    case_iv_v <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579, order =c(3,0,1,4,3), data = PSS2001)
    
    case_iv_v_y1 <- t(matrix(case_iv_v$coefficients)) %*% matrix(c(1, 28.25,
                                                3.541190, 3.528882, 3.525881,
                                                1,
                                                4, 1.898443,
                                                7, -0.3640409, -0.3627026, -0.3582229, -0.3523980,
                                                11, -0.6529655, -0.6529655, -0.6529655,
                                                0, 1))
    #T+1
    #-6.616179
    case_iv_v_y2 <- t(matrix(case_iv_v$coefficients)) %*% matrix(c(1, 28.50,
                                                -6.616179, 3.541190, 3.528882,
                                                2,
                                                5, 4,
                                                8, 7, -0.3640409, -0.3627026, -0.3582229,
                                                22, 11, -0.6529655, -0.6529655,
                                                1, 0))
    #T+2
    #-23.54048
    case_iv_v_y3 <- t(matrix(case_iv_v$coefficients)) %*% matrix(c(1, 28.75,
                                                -23.54048, -6.616179, 3.541190,
                                                3,
                                                6, 5,
                                                9, 8, 7, -0.3640409, -0.3627026,
                                                33, 22, 11, -0.6529655,
                                                0, 0))
    #T+3
    #-27.92307
    #####
    result_case_i <- predict.ardl(case_i, newdata)
    result_case_ii_iii <- predict.ardl(case_ii_iii, newdata)
    result_case_iv_v <- predict.ardl(case_iv_v, newdata)
    expect_equal(c(case_i_y1, case_i_y2, case_i_y3), result_case_i)
    expect_equal(c(case_ii_iii_y1, case_ii_iii_y2, case_ii_iii_y3), result_case_ii_iii)
    expect_equal(c(case_iv_v_y1, case_iv_v_y2, case_iv_v_y3), result_case_iv_v)
})




