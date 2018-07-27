# ----------
# Issue # 4 Quantify difficulty levels of distributions
# ----------

dist.means.a <- c(4, 4, 4)
dist.sds.a <- c(2.5, 2.5, 11)

dist.means.b <- c(4, 2.5, 2.5)
dist.sds.b <- c(11, 11, 2.5)


a.greater.b <- NULL
a.positive <- NULL
b.positive <- NULL

for (ii in 1:length(dist.means.a)){
  dist.a.temp <- rnorm(10000000, mean = dist.means.a[ii], sd = dist.sds.a[ii])
  dist.b.temp <- rnorm(10000000, mean = dist.means.b[ii], sd = dist.sds.b[ii])
  
  a.greater.b <- c(a.greater.b, mean(dist.a.temp > dist.b.temp))
  a.positive <- c(a.positive, mean(dist.a.temp >= 0))
  b.positive <- c(b.positive, mean(dist.b.temp >= 0))
  
}

rel.li.adb <- a.positive / b.positive

df <- data.frame(a.greater.b, a.positive, b.positive, rel.li.adb)

knitr::kable(round(df, 2))
