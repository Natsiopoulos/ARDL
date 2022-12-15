test_that("ardl model coefficients", {
    ardl61545 <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                      order = c(6,1,5,4,5), data = PSS2001, start = c(1972, 01))
    ardl_coeffs <- c(0.61914,
                     0.35330,0.08955,-0.19545,0.39035,-0.06366,0.19669,
                     0.31530,-0.07157,
                     0.00340,-0.01186,-0.01249,0.02545,-0.00160,-0.02688,
                     -0.29713,0.03274,-0.04517,-0.09436,0.18780,
                     -0.96873,-1.60663,2.89356,-0.07990,-1.89362,1.99472,
                     0.02931,0.01685)
    names(ardl_coeffs) <- c("(Intercept)",
                            "L(w, 1)", "L(w, 2)", "L(w, 3)", "L(w, 4)", "L(w, 5)", "L(w, 6)",
                            "Prod", "L(Prod, 1)",
                            "UR", "L(UR, 1)", "L(UR, 2)", "L(UR, 3)", "L(UR, 4)", "L(UR, 5)",
                            "Wedge", "L(Wedge, 1)", "L(Wedge, 2)", "L(Wedge, 3)", "L(Wedge, 4)",
                            "Union", "L(Union, 1)", "L(Union, 2)", "L(Union, 3)", "L(Union, 4)", "L(Union, 5)",
                            "D7475", "D7579")

    expect_equal(round(ardl61545$coefficients,5), ardl_coeffs)

    # check if non time-series data work
    ardl61545_df <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                         order = c(6,1,5,4,5), data = data.frame(PSS2001), start = 9)

    expect_equal(ardl61545$coefficients, ardl61545_df$coefficients)
})

test_that("ardl order", {
    ardl61545 <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                      order = c(6,1,5,4,5), data = PSS2001, start = c(1972, 01))
    expect_equal(ardl61545$order, c(6,1,5,4,5), ignore_attr = TRUE)
})

test_that("ardl parsed_formula", {
    ardl61545 <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                      order = c(6,1,5,4,5), data = PSS2001, start = c(1972, 01))
    expect_equal(ardl61545$parsed_formula$y_part$var, "w")
    expect_equal(ardl61545$parsed_formula$x_part$var, c("Prod", "UR", "Wedge", "Union"))
    expect_equal(ardl61545$parsed_formula$z_part$var, c("w", "Prod", "UR", "Wedge", "Union"))
    expect_equal(ardl61545$parsed_formula$fixed_part$var, c("D7475", "D7579"))
    expect_equal(ardl61545$parsed_formula$kz, 5)
    expect_equal(ardl61545$parsed_formula$kx, 4)
    expect_equal(ardl61545$parsed_formula$kw, 1)
    expect_equal(ardl61545$parsed_formula$kfixed, 2)
})

test_that("check equivalence of methods", {
    ardl61545 <- ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                      order = c(6,1,5,4,5), data = PSS2001, start = c(1972, 01))
    uecm61545 <- uecm(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                      order = c(6,1,5,4,5), data = PSS2001, start = c(1972, 01))

    expect_equal(ardl61545, ardl(uecm61545))
})
