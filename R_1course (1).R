results1 = c(8, 3, 9, 10, 2, 5, 9)
results2  = c(4, 7, 10, 6, 5, 10, 8, 9)

q = as.data.frame(rbind(results1, results2))

chisq.test(q)
fisher.test(q)
