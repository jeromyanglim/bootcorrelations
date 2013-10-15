
## ------------------------------------------------------------------------
library(bootcorrelations)
set.seed(1234)
x <- bootcorrelations:::generate_correlated_data(n=100, r_set1=.3, r_set2=.5, r_set1_set2=.2)
x$set1
x$set2
head(x$data)


## ------------------------------------------------------------------------
round(cor(x$data), 2)


## ------------------------------------------------------------------------
fit <- boot_mean_cor_diff(x$set1, x$set2, x$data, iterations=1000, ci=.95)
str(fit)


## ------------------------------------------------------------------------
# or use print(fit)
fit <- boot_mean_cor_diff(x$set2, x$set1, x$data, iterations=1000, ci=.95)
fit


