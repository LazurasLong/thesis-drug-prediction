### Change label factor from numeric to character (one time process)
without_antidepressant_bio <- as.data.frame(apply(without_antidepressant_bio, 2, factor, levels = c(0, 1), label = c("no", "yes")))
###
