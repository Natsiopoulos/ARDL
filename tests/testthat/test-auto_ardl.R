test_that("Combination of max_order, fixed_order, starting_order and selection", {
    ardl_model <- auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                            data = PSS2001, start = c(1972, 01),
                            max_order = 6, fixed_order = c(-1,1,-1,-1,-1), starting_order = 5,
                            selection = "AIC_pss", selection_minmax = "max")

    expect_equal(ardl_model$top_orders[,-6],
                 data.frame(w=c(6,6,6,6,6,5,5,5,5,5),
                            Prod=c(1,1,1,1,1,1,1,1,1,1),
                            UR=c(5,5,5,6,6,5,5,5,6,6),
                            Wedge=c(4,5,4,4,5,4,5,4,4,5),
                            Union=c(5,5,6,5,5,5,5,6,5,5)))
    expect_equal(ardl_model$best_model, ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                                             data = PSS2001, start = c(1972, 01),
                                             order=c(6,1,5,4,5)))

    # search_type = "vertical"
    ardl_model_vert <- auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                            data = PSS2001, start = c(1972, 01),
                            max_order = 6, fixed_order = c(-1,1,-1,-1,-1), starting_order = 5,
                            selection = "AIC_pss", selection_minmax = "max",
                            search_type = "vertical")

    expect_equal(ardl_model_vert$top_orders[,-6],
                 data.frame(w=c(6,6,6,6,5,5,5,5),
                            Prod=c(1,1,1,1,1,1,1,1),
                            UR=c(5,5,5,6,5,5,5,6),
                            Wedge=c(4,5,4,5,4,5,4,5),
                            Union=c(5,5,6,5,5,5,6,5)))
    expect_equal(ardl_model_vert$best_model, ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                                                  data = PSS2001, start = c(1972, 01),
                                                  order=c(6,1,5,4,5)))

    # check if non time-series data work
    ardl_model_df <- auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                               data = data.frame(PSS2001), start = 9,
                               max_order = 6, fixed_order = c(-1,1,-1,-1,-1), starting_order = 5,
                               selection = "AIC_pss", selection_minmax = "max")

    expect_equal(ardl_model_df$best_order, c(6,1,5,4,5))
    expect_equal(ardl_model_df$best_model, ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                                                data = data.frame(PSS2001), start = 9,
                                                order=c(6,1,5,4,5)))
})

test_that("missing selection defaults to AIC", {
    expect_equal(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(-1,1,-1,-1,-1), starting_order = 5),
                 auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(-1,1,-1,-1,-1), starting_order = 5,
                           selection = "AIC"))
})

test_that("starting_order is missing", {
    ardl_model_gridF <- auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                                  data = PSS2001, start = c(1972, 01),
                                  max_order = 6, fixed_order = c(-1,1,-1,-1,-1),
                                  grid = FALSE)
    top_orders_gridF <- data.frame(w=c(4,6,4,5,4,5,4,5,5,4,5,4,4,4,3,3,3,3,3,3),
                                   Prod=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                                   UR=c(5,6,5,5,5,5,6,5,6,4,6,4,5,4,4,4,5,4,5,4),
                                   Wedge=c(4,6,5,4,4,5,4,4,4,4,5,4,4,5,1,1,1,2,2,2),
                                   Union=c(5,6,5,5,6,5,5,6,5,5,5,4,4,4,4,5,4,4,4,3))

    #also checks grid = TRUE & selection_minmax = "max"
    ardl_model_gridT <- auto_ardl(w ~ Prod + UR + Wedge | D7475 + D7579,
                                  data = PSS2001, start = c(1972, 01),
                                  max_order = 6, fixed_order = c(-1,1,-1,-1),
                                  selection = "AIC_pss", selection_minmax = "max",
                                  grid = TRUE)
    top_orders_gridT <- data.frame(w=c(4,5,4,5,5,5,4,4,4,4,6,4,4,4,4,6,6,4,6,5),
                                   Prod=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                                   UR=c(2,2,3,0,1,3,1,6,2,0,2,4,6,2,3,0,1,1,3,4),
                                   Wedge=c(6,6,6,6,6,6,6,2,2,6,6,6,1,1,2,6,6,1,6,6))

    expect_equal(ardl_model_gridF$top_orders[,-6], top_orders_gridF)
    expect_equal(ardl_model_gridT$top_orders[,-5], top_orders_gridT)
})

test_that("fixed_order[1] != (-1)", {
    expect_equal(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(6,1,-1,-1,-1),
                           starting_order = 5)$top_orders[,-6],
                 data.frame(w=c(6,6,6,6,6),
                            Prod=c(1,1,1,1,1),
                            UR=c(5,5,5,6,6),
                            Wedge=c(4,5,4,4,5),
                            Union=c(5,5,6,5,5)))
})

test_that("avoid search for q<0 when search_type = vertical", {
    ardl_model <- auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                            data = PSS2001, start = c(1972, 01),
                            max_order = 6, fixed_order = c(-1,1,-1,-1,-1),
                            starting_order = c(3,5,1,0,1), search_type = "vertical")

    expect_equal(ardl_model$top_orders[,-6],
                 data.frame(w=c(4,4,6,6,5,5,4,4,6,6,5,5,3,3,3,3,3,4,6,4),
                            Prod=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                            UR=c(2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,2,2,3),
                            Wedge=c(2,2,2,2,2,2,2,1,2,1,2,1,2,2,2,3,1,0,0,0),
                            Union=c(2,3,2,3,2,3,1,1,1,1,1,1,2,3,1,1,1,1,1,1)))
})

test_that("missing max_order causes expected error", {
    expect_error(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           fixed_order = c(-1,1,-1,-1,-1), starting_order = 5),
                 "'max_order' is a mandatory argument.")
})

test_that("starting_order[1] < 1 causes expected error", {
    expect_error(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(-1,1,-1,-1,-1),
                           starting_order = c(0,5,5,5,5)),
                 "In 'starting_order', the starting order of p \\(first argument\\) can't be less than 1.")
})

test_that("starting_order > max_order causes expected error", {
    expect_error(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(-1,1,-1,-1,-1),
                           starting_order = c(5,5,5,7,5)),
                 "'starting_order' can't be greater than 'max_order'.")
})

test_that("fixed_order > max_order causes expected error", {
    expect_error(auto_ardl(w ~ Prod + UR + Wedge + Union | D7475 + D7579,
                           data = PSS2001, start = c(1972, 01),
                           max_order = 6, fixed_order = c(-1,1,-1,7,-1),
                           starting_order = 5),
                 "'fixed_order' can't be greater than 'max_order'.")
})
