test_that("correct results case 2 & 3", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    expected_mult <- data.frame(term=c("(Intercept)", "Prod", "UR", "Wedge", "Union"),
                                estimate=c(2.70108265, 1.06332501, -0.10460915, -0.94285059, 1.48067052),
                                std.error=c(0.2414848597, 0.0499903071, 0.0336700352, 0.2649817179, 0.3106107337),
                                t.statistic=c(11.18530847, 21.27062375, -3.10689148, -3.55817222, 4.76696509))

    expect_equal(multipliers(ardl_model)[,-5], expected_mult)

    ardl_model <- ardl(Union ~ Prod + UR + Wedge | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(5,0,5,1))

    expect_equal(round(multipliers(ardl_model)[,"p.value"],6), c(0.013860, 0.768666, 0.919328, 0.440212))
})

test_that("correct results case 1", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    expected_mult <- data.frame(term=c("Prod", "UR", "Wedge", "Union"),
                                estimate=c(0.4047793, -0.6813798, -10.7233397, -2.6065488),
                                std.error=c(2.1361117, 1.8758181, 23.4065331, 3.3974199),
                                t.statistic=c(0.189493512, -0.363244080, -0.458134473, -0.767214197))

    expect_equal(multipliers(ardl_model)[,-5], expected_mult)
    expect_equal(round(multipliers(ardl_model)[,"p.value"],6), c(0.850205, 0.717418, 0.648146, 0.4453))
})

test_that("type='sr' equals type=0", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_equal(multipliers(ardl_model, type = "sr"), multipliers(ardl_model, type = 0))
})

test_that("equal results with ardl or uecm input", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union -1 | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))
    ardl_model_c <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                         data = PSS2001, start = c(1972, 01),
                         order=c(6,1,5,4,5))
    ardl_model_ct <- ardl(w ~ Prod + UR + Wedge + Union + trend(w) | D7475 + D7579,
                          data = PSS2001, start = c(1972, 01),
                          order=c(6,1,5,4,5))

    expect_equal(multipliers(ardl_model), multipliers(uecm(ardl_model)))
    expect_equal(multipliers(ardl_model, type = "sr"), multipliers(uecm(ardl_model), type = 0))
    expect_equal(multipliers(ardl_model, type = 2), multipliers(uecm(ardl_model), type = 2))

    expect_equal(multipliers(ardl_model_c), multipliers(uecm(ardl_model_c)))
    expect_equal(multipliers(ardl_model_c, type = "sr"), multipliers(uecm(ardl_model_c), type = 0))
    expect_equal(multipliers(ardl_model_c, type = 2), multipliers(uecm(ardl_model_c), type = 2))

    expect_equal(multipliers(ardl_model_ct), multipliers(uecm(ardl_model_ct)))
    expect_equal(multipliers(ardl_model_ct, type = "sr"), multipliers(uecm(ardl_model_ct), type = 0))
    expect_equal(multipliers(ardl_model_ct, type = 2), multipliers(uecm(ardl_model_ct), type = 2))
})

test_that("wrong type causes expected error", {
    ardl_model <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                       data = PSS2001, start = c(1972, 01),
                       order=c(6,1,5,4,5))

    expect_error(multipliers(ardl_model, type = "aa"),
                 "'type' should be one of 'lr', 'sr' or a number between 0 and 100")
    expect_error(multipliers(uecm(ardl_model), type = "aa"),
                 "'type' should be one of 'lr', 'sr' or a number between 0 and 100")
})
