# Correlation matrix------------------------------------------------
setwd("E:/21_SeOMap_Bots")

# Clear memory------------------------------------------------------
gc()
rm(list=ls()) # clean memory

# Add library-------------------------------------------------------
library(PerformanceAnalytics)

# Add data----------------------------------------------------------
data <- read.table('All_data.csv', header = TRUE, sep = ",")
head(data)
str(data)

chart.Correlation <- function (R, histogram = TRUE, method = c("pearson", "kendall", 
                                                               "spearman"), ...) 
{
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = "pearson", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "chartreuse4", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "Red", lwd = 2)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel, ...)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, ...)
}

chart.Correlation(data[, c(3:14)], histogram = T, pch = 16, col = "orange2")

rm(list=ls()) # clean memory
# ----------------------------End----------------------------------