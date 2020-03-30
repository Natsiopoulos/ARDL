# Run create_asy_bounds() for each 'case' and 'k' (everething else default). It takes time.
# When all combinations are done, run crit_val_bounds <- assemble_crit_val_bounds()
# For the PSS2001 data run crit_val_bounds_pss2001 <- assemble_crit_val_bounds_pss2001()
# Then use the following to store all data as internal data in a single R/sysdata.rda file
# usethis::use_data(crit_val_bounds_pss2001, crit_val_bounds, internal = TRUE, overwrite = TRUE)

create_crit_val_bounds <- function(case, k, alpha, T = 1000, R = 70000) {
    if (missing(alpha)) {
        alpha <- c(seq(0, 0.1, by = 0.001), seq(0.11, 1, by = 0.01))
    }
    len <- length(alpha)
    set.seed(2020)
    fbounds <- bounds_sim(case = case, k = k, alpha = alpha, T = T, R = R)$f_bounds[1:len,]
    if (case %in% c(1, 3, 5)) {
        set.seed(2020)
        tbounds <- t_bounds_sim(case = case, k = k, alpha = alpha, T = T, R = R)[1:len,]
        res <- data.frame(k = rep(k, len), alpha = alpha, fI0 = fbounds$I0, fI1 = fbounds$I1, tI0 = tbounds$I0, tI1 = tbounds$I1)
    } else {
        res <- data.frame(k = rep(k, len), alpha = alpha, fI0 = fbounds$I0, fI1 = fbounds$I1)
    }

    save(res, file = paste0("case", case, "_k", k, "_T", T, "_R", R, ".RData"))
}

assemble_crit_val_bounds <- function() {
    R = 70000
    T = 1000
    crit_val_bounds <- list()
    crit_val_bounds$I0 <- list(
        i = data.frame(k = NA, alpha = NA, fI0 = NA, tI0 = NA),
        ii = data.frame(k = NA, alpha = NA, fI0 = NA),
        iii = data.frame(k = NA, alpha = NA, fI0 = NA, tI0 = NA),
        iv = data.frame(k = NA, alpha = NA, fI0 = NA),
        v = data.frame(k = NA, alpha = NA, fI0 = NA, tI0 = NA)
    )
    crit_val_bounds$I1 <- list(
        i = data.frame(k = NA, alpha = NA, fI1 = NA, tI1 = NA),
        ii = data.frame(k = NA, alpha = NA, fI1 = NA),
        iii = data.frame(k = NA, alpha = NA, fI1 = NA, tI1 = NA),
        iv = data.frame(k = NA, alpha = NA, fI1 = NA),
        v = data.frame(k = NA, alpha = NA, fI1 = NA, tI1 = NA)
    )
    for (case in 1:5) {
        caselatin <- ifelse(case == 1, "i",
                            ifelse(case == 2, "ii",
                                   ifelse(case == 3, "iii",
                                          ifelse(case == 4, "iv", "v"))))
        for (k in 1:10) {
            load(paste0("case", case, "_k", k, "_T", T, "_R", R, ".RData"))
            I0_condition <- which(res$alpha %in% c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.15, 0.2))
            if (case %in% c(1, 3, 5)) {
                crit_val_bounds$I0[[caselatin]] <- rbind(crit_val_bounds$I0[[caselatin]],
                                                         data.frame(k = res$k[I0_condition], alpha = res$alpha[I0_condition],
                                                                    fI0 = res$fI0[I0_condition], tI0 = res$tI0[I0_condition])
                )
                crit_val_bounds$I1[[caselatin]] <- rbind(crit_val_bounds$I1[[caselatin]],
                                                         data.frame(k = res$k, alpha = res$alpha,
                                                                    fI1 = res$fI1, tI1 = res$tI1)
                )
            } else {
                crit_val_bounds$I0[[caselatin]] <- rbind(crit_val_bounds$I0[[caselatin]],
                                                         data.frame(k = res$k[I0_condition], alpha = res$alpha[I0_condition],
                                                                    fI0 = res$fI0[I0_condition])
                )
                crit_val_bounds$I1[[caselatin]] <- rbind(crit_val_bounds$I1[[caselatin]],
                                                         data.frame(k = res$k, alpha = res$alpha, fI1 = res$fI1)
                )
            }
        }
    }
    for (caselatin in c("i", "ii", "iii", "iv", "v")) {
        crit_val_bounds$I0[[caselatin]] <- crit_val_bounds$I0[[caselatin]][complete.cases(crit_val_bounds$I0[[caselatin]]),]
        crit_val_bounds$I1[[caselatin]] <- crit_val_bounds$I1[[caselatin]][complete.cases(crit_val_bounds$I1[[caselatin]]),]
    }
    return(crit_val_bounds)
}

assemble_crit_val_bounds_pss2001 <- function() {
    crit_val_bounds_pss2001 <- list()
    crit_val_bounds_pss2001$f <- list(
        i = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(3, 2.44, 2.17, 2.01, 1.9, 1.81, 1.75, 1.7, 1.66, 1.63, 1.6,
                   4.2, 3.15, 2.72, 2.45, 2.26, 2.14, 2.04, 1.97, 1.91, 1.86, 1.82,
                   5.47, 3.88, 3.22, 2.87, 2.62, 2.44, 2.32, 2.22, 2.15, 2.08, 2.02,
                   7.17, 4.81, 3.88, 3.42, 3.07, 2.82, 2.66, 2.54, 2.45, 2.34, 2.26),
            I1 = c(3, 3.28, 3.19, 3.1, 3.01, 2.93, 2.87, 2.83, 2.79, 2.75, 2.72,
                   4.2, 4.11, 3.83, 3.63, 3.48, 3.34, 3.24, 3.18, 3.11, 3.05, 2.99,
                   5.47, 4.92, 4.5, 4.16, 3.9, 3.71, 3.59, 3.49, 3.4, 3.33, 3.27,
                   7.17, 6.02, 5.3, 4.84, 4.44, 4.21, 4.05, 3.91, 3.79, 3.68, 3.6)),
        ii = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(3.8, 3.02, 2.63, 2.37, 2.2, 2.08, 1.99, 1.92, 1.85, 1.8, 1.76,
                   4.6, 3.62, 3.1, 2.79, 2.56, 2.39, 2.27, 2.17, 2.11, 2.04, 1.98,
                   5.39, 4.18, 3.55, 3.15, 2.88, 2.7, 2.55, 2.43, 2.33, 2.24, 2.18,
                   6.44, 4.94, 4.13, 3.65, 3.29, 3.06, 2.88, 2.73, 2.62, 2.5, 2.41),
            I1 = c(3.8, 3.51, 3.35, 3.2, 3.09, 3, 2.94, 2.89, 2.85, 2.8, 2.77,
                   4.6, 4.16, 3.87, 3.67, 3.49, 3.38, 3.28, 3.21, 3.15, 3.08, 3.04,
                   5.39, 4.79, 4.38, 4.08, 3.87, 3.73, 3.61, 3.51, 3.42, 3.35, 3.28,
                   6.44, 5.58, 5, 4.66, 4.37, 4.15, 3.99, 3.9, 3.77, 3.68, 3.61)),
        iii = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(6.58, 4.04, 3.17, 2.72, 2.45, 2.26, 2.12, 2.03, 1.95, 1.88, 1.83,
                   8.21, 4.94, 3.79, 3.23, 2.86, 2.62, 2.45, 2.32, 2.22, 2.14, 2.06,
                   9.8, 5.77, 4.41, 3.69, 3.25, 2.96, 2.75, 2.6, 2.48, 2.37, 2.28,
                   11.79, 6.84, 5.15, 4.29, 3.74, 3.41, 3.15, 2.96, 2.79, 2.65, 2.54),
            I1 = c(6.58, 4.78, 4.14, 3.77, 3.52, 3.35, 3.23, 3.13, 3.06, 2.99, 2.94,
                   8.21, 5.73, 4.85, 4.35, 4.01, 3.79, 3.61, 3.5, 3.39, 3.3, 3.24,
                   9.8, 6.68, 5.52, 4.89, 4.49, 4.18, 3.99, 3.84, 3.7, 3.6, 3.5,
                   11.79, 7.84, 6.36, 5.61, 5.06, 4.68, 4.43, 4.26, 4.1, 3.97, 3.86)),
        iv = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(5.37, 4.05, 3.38, 2.97, 2.68, 2.49, 2.33, 2.22, 2.13, 2.05, 1.98,
                   6.29, 4.68, 3.88, 3.38, 3.05, 2.81, 2.63, 2.5, 2.38, 2.3, 2.21,
                   7.14, 5.3, 4.37, 3.8, 3.4, 3.11, 2.9, 2.76, 2.62, 2.52, 2.42,
                   8.26, 6.1, 4.99, 4.3, 3.81, 3.5, 3.27, 3.07, 2.93, 2.79, 2.68),
            I1 = c(5.37, 4.49, 4.02, 3.74, 3.53, 3.38, 3.25, 3.17, 3.09, 3.02, 2.97,
                   6.29, 5.15, 4.61, 4.23, 3.97, 3.76, 3.62, 3.5, 3.41, 3.33, 3.25,
                   7.14, 5.83, 5.16, 4.68, 4.36, 4.13, 3.94, 3.81, 3.7, 3.6, 3.52,
                   8.26, 6.73, 5.85, 5.23, 4.92, 4.63, 4.39, 4.23, 4.06, 3.93, 3.84)),
        v = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(9.81, 5.59, 4.19, 3.47, 3.03, 2.75, 2.53, 2.38, 2.26, 2.16, 2.07,
                   11.64, 6.56, 4.87, 4.01, 3.47, 3.12, 2.87, 2.69, 2.55, 2.43, 2.33,
                   13.36, 7.46, 5.49, 4.52, 3.89, 3.47, 3.19, 2.98, 2.82, 2.67, 2.56,
                   15.73, 8.74, 6.34, 5.17, 4.4, 3.93, 3.6, 3.34, 3.15, 2.97, 2.84),
            I1 = c(9.81, 6.26, 5.06, 4.45, 4.06, 3.79, 3.59, 3.45, 3.34, 3.24, 3.16,
                   11.64, 7.3, 5.85, 5.07, 4.57, 4.25, 4, 3.83, 3.68, 3.56, 3.46,
                   13.36, 8.27, 6.59, 5.62, 5.07, 4.67, 4.38, 4.16, 4.02, 3.87, 3.76,
                   15.73, 9.63, 7.52, 6.36, 5.72, 5.23, 4.9, 4.63, 4.43, 4.24, 4.1))
    )

    crit_val_bounds_pss2001$t <- list(
        i = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(rep(-1.62, 11),
                   rep(-1.95, 11),
                   rep(-2.24, 11),
                   rep(-2.58, 11)),
            I1 = c(-1.62, -2.28, -2.68, -3, -3.26, -3.49, -3.7, -3.9, -4.09, -4.26, -4.42,
                   -1.95, -2.6, -3.02, -3.33, -3.6, -3.83, -4.04, -4.23, -4.43, -4.61, -4.76,
                   -2.24, -2.90, -3.31, -3.64, -3.89, -4.12, -4.34, -4.54, -4.72, -4.89, -5.06,
                   -2.58, -3.22, -3.66, -3.97, -4.23, -4.44, -4.67, -4.88, -5.07, -5.25, -5.44)),
        iii = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(rep(-2.57, 11),
                   rep(-2.86, 11),
                   rep(-3.13, 11),
                   c(rep(-3.43, 9), -3.42, -3.43)),
            I1 = c(-2.57, -2.91, -3.21, -3.46, -3.66, -3.86, -4.04, -4.23, -4.4, -4.56, -4.69,
                   -2.86, -3.22, -3.53, -3.78, -3.99, -4.19, -4.38, -4.57, -4.72, -4.88, -5.03,
                   -3.13, -3.5, -3.8, -4.05, -4.26, -4.46, -4.66, -4.85, -5.02, -5.18, -5.34,
                   -3.43, -3.82, -4.1, -4.37, -4.6, -4.79, -4.99, -5.19, -5.37, -5.54, -5.68)),
        v = data.frame(
            alpha = rep(c(0.1, 0.05, 0.025, 0.01), each = 11),
            k = rep(0:10, times = 4),
            I0 = c(rep(-3.13, 11),
                   rep(-3.41, 11),
                   rep(-3.65, 11),
                   rep(-3.96, 11)),
            I1 = c(-3.13, -3.4, -3.63, -3.84, -4.04, -4.21, -4.37, -4.53, -4.68, -4.82, -4.96,
                   -3.41, -3.69, -3.95, -4.16, -4.36, -4.52, -4.69, -4.85, -5.01, -5.15, -5.29,
                   -3.66, -3.96, -4.2, -4.42, -4.62, -4.79, -4.96, -5.14, -5.3, -5.44, -5.59,
                   -3.97, -4.26, -4.53, -4.73, -4.96, -5.13, -5.31, -5.49, -5.65, -5.79, -5.94))
    )
    return(crit_val_bounds_pss2001)
}
