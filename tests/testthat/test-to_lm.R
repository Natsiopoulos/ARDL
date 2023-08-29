test_that("From ardl to lm", {
    ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
    ardl_3132_lm <- to_lm(ardl_3132)

    expect_equal(summary(ardl_3132)$coefficients,
                 summary(ardl_3132_lm)$coefficients, ignore_attr = TRUE)
    expect_equal(ardl_3132$residuals, ardl_3132_lm$residuals)
    expect_equal(ardl_3132$effects, ardl_3132_lm$effects)
    expect_equal(ardl_3132$rank, ardl_3132_lm$rank)
    expect_equal(ardl_3132$fitted.values, ardl_3132_lm$fitted.values)
    expect_equal(ardl_3132$assign, ardl_3132_lm$assign)
    expect_equal(ardl_3132$qr, ardl_3132_lm$qr, ignore_attr= TRUE)
    expect_equal(ardl_3132$df.residual, ardl_3132_lm$df.residual)
    expect_equal(ardl_3132$xlevels, ardl_3132_lm$xlevels)
    expect_equal(ardl_3132$model, ardl_3132_lm$model, ignore_attr= TRUE)

    # Check data_class = "ts"
    ardl_3132_lm_ts <- to_lm(ardl_3132, data_class = "ts")
    expect_equal(ardl_3132_lm$coefficients, ardl_3132_lm_ts$coefficients)
})

test_that("From uecm to lm", {
    uecm_3132 <- uecm(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
    uecm_3132_lm <- to_lm(uecm_3132)

    expect_equal(summary(uecm_3132)$coefficients,
                 summary(uecm_3132_lm)$coefficients, ignore_attr = TRUE)
    expect_equal(uecm_3132$residuals, uecm_3132_lm$residuals)
    expect_equal(uecm_3132$effects, uecm_3132_lm$effects)
    expect_equal(uecm_3132$rank, uecm_3132_lm$rank)
    expect_equal(uecm_3132$fitted.values, uecm_3132_lm$fitted.values)
    expect_equal(uecm_3132$assign, uecm_3132_lm$assign)
    expect_equal(uecm_3132$qr, uecm_3132_lm$qr, ignore_attr= TRUE)
    expect_equal(uecm_3132$df.residual, uecm_3132_lm$df.residual)
    expect_equal(uecm_3132$xlevels, uecm_3132_lm$xlevels)
    expect_equal(uecm_3132$model, uecm_3132_lm$model, ignore_attr= TRUE)

    # fix_names = TRUE
    uecm_3132_lm_names <- to_lm(uecm_3132, fix_names = TRUE)
    names_FALSE <- c("d.LRM", "L(LRM, 1)", "L(LRY, 1)", "L(IBO, 1)", "L(IDE, 1)",
                     "d(L(LRM, 1))", "d(L(LRM, 2))", "d(LRY)", "d(IBO)",
                     "d(L(IBO, 1))", "d(L(IBO, 2))", "d(IDE)", "d(L(IDE, 1))")
    names_TRUE <- c("d.LRM", "L.LRM.1", "L.LRY.1", "L.IBO.1", "L.IDE.1",
                    "d.L.LRM.1", "d.L.LRM.2", "d.LRY", "d.IBO", "d.L.IBO.1",
                    "d.L.IBO.2", "d.IDE", "d.L.IDE.1")
    expect_equal(names(uecm_3132_lm$model), names_FALSE)
    expect_equal(names(uecm_3132_lm_names$model), names_TRUE)
})

test_that("Exclude intercept when -1", {
    ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE -1, data = denmark, order = c(3,1,3,2))
    ardl_3132_lm <- to_lm(ardl_3132)

    expect_equal(summary(ardl_3132)$coefficients,
                 summary(ardl_3132_lm)$coefficients, ignore_attr = TRUE)
    expect_equal(ardl_3132$residuals, ardl_3132_lm$residuals)
    expect_equal(ardl_3132$effects, ardl_3132_lm$effects)
    expect_equal(ardl_3132$rank, ardl_3132_lm$rank)
    expect_equal(ardl_3132$fitted.values, ardl_3132_lm$fitted.values)
    expect_equal(ardl_3132$assign, ardl_3132_lm$assign)
    expect_equal(ardl_3132$qr, ardl_3132_lm$qr, ignore_attr= TRUE)
    expect_equal(ardl_3132$df.residual, ardl_3132_lm$df.residual)
    expect_equal(ardl_3132$xlevels, ardl_3132_lm$xlevels)
    expect_equal(ardl_3132$model, ardl_3132_lm$model, ignore_attr= TRUE)

    # check trend
    ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM), data = denmark, order = c(3,1,3,2))
    ardl_3132_lm <- to_lm(ardl_3132)

    expect_equal(summary(ardl_3132)$coefficients,
                 summary(ardl_3132_lm)$coefficients, ignore_attr = TRUE)

    # check trend and no intercept
    ardl_3132 <- ardl(LRM ~ LRY + IBO + IDE + trend(LRM,scale=FALSE) -1, data = denmark, order = c(3,1,3,2))
    ardl_3132_lm <- to_lm(ardl_3132)

    expect_equal(summary(ardl_3132)$coefficients,
                 summary(ardl_3132_lm)$coefficients, ignore_attr = TRUE)
})

test_that("From recm to lm", {
    # Case 2
    recm_3132 <- recm(uecm(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2)), case = 2)
    recm_3132_lm <- to_lm(recm_3132)

    expect_equal(summary(recm_3132)$coefficients,
                 summary(recm_3132_lm)$coefficients, ignore_attr = TRUE)
    expect_equal(recm_3132$residuals, recm_3132_lm$residuals)
    expect_equal(recm_3132$effects, recm_3132_lm$effects)
    expect_equal(recm_3132$rank, recm_3132_lm$rank)
    expect_equal(recm_3132$fitted.values, recm_3132_lm$fitted.values)
    expect_equal(recm_3132$assign, recm_3132_lm$assign)
    expect_equal(recm_3132$qr, recm_3132_lm$qr, ignore_attr= TRUE)
    expect_equal(recm_3132$df.residual, recm_3132_lm$df.residual)
    expect_equal(recm_3132$xlevels, recm_3132_lm$xlevels)
    expect_equal(recm_3132$model, recm_3132_lm$model, ignore_attr= TRUE)

    # Case 3
    recm_3132 <- recm(uecm(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2)), case = 3)
    recm_3132_lm <- to_lm(recm_3132)

    expect_equal(summary(recm_3132)$coefficients,
                 summary(recm_3132_lm)$coefficients, ignore_attr = TRUE)
    expect_equal(recm_3132$residuals, recm_3132_lm$residuals)
    expect_equal(recm_3132$effects, recm_3132_lm$effects)
    expect_equal(recm_3132$rank, recm_3132_lm$rank)
    expect_equal(recm_3132$fitted.values, recm_3132_lm$fitted.values)
    expect_equal(recm_3132$assign, recm_3132_lm$assign)
    expect_equal(recm_3132$qr, recm_3132_lm$qr, ignore_attr= TRUE)
    expect_equal(recm_3132$df.residual, recm_3132_lm$df.residual)
    expect_equal(recm_3132$xlevels, recm_3132_lm$xlevels)
    expect_equal(recm_3132$model, recm_3132_lm$model, ignore_attr= TRUE)

    # Case 1
    recm_3132 <- recm(uecm(LRM ~ LRY + IBO + IDE -1, data = denmark, order = c(3,1,3,2)), case = 1)
    recm_3132_lm <- to_lm(recm_3132)

    expect_equal(summary(recm_3132)$coefficients,
                 summary(recm_3132_lm)$coefficients, ignore_attr = TRUE)

    # Case 4
    recm_3132 <- recm(uecm(LRM ~ LRY + IBO + IDE + trend(LRM), data = denmark, order = c(3,1,3,2)), case = 4)
    recm_3132_lm <- to_lm(recm_3132)

    expect_equal(summary(recm_3132)$coefficients,
                 summary(recm_3132_lm)$coefficients, ignore_attr = TRUE)

    # Case 5
    recm_3132 <- recm(uecm(LRM ~ LRY + IBO + IDE + trend(LRM, scale=FALSE), data = denmark, order = c(3,1,3,2)), case = 5)
    recm_3132_lm <- to_lm(recm_3132)

    expect_equal(summary(recm_3132)$coefficients,
                 summary(recm_3132_lm)$coefficients, ignore_attr = TRUE)
})

test_that("Check dynlm models with lagged dependent variable", {
    dynlm_3132 <- dynlm::dynlm(L(LRM, 2) ~ -1 +L(LRM, 3) + LRY + L(LRY, 3) + IBO +
                            L(IBO, 4) + L(IBO, 5) + L(IBO, 6) + IDE + L(IDE, 3) +
                            L(IDE, 4), data=denmark)
    dynlm_3132_lm <- to_lm(dynlm_3132)

    expect_equal(summary(dynlm_3132)$coefficients,
                 summary(dynlm_3132_lm)$coefficients, ignore_attr = TRUE)
})
