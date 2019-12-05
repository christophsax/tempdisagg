library(testthat)
library(tempdisagg)

context("tricky situations")

test_that("disaggregation works for triennial data", {
  hf = c(365056, 391499, 418284, 447350, 484371, 519806, 565522, 609828, 664058, 716983, 802712,
         838831, 903747, 948395, 1045956, 1102447, 1136367, 1118908, 1177859, 1170538, 1237589, 1272784)
  hf = ts(hf, start = 1989, freq = 1)
  lf = c(126751446, 45889022.56, 28093152.02, 101649783.1, 950.3715023, 277421542.5, 183422540.5)
  lf = ts(lf, start = 1989, freq = 1/3)
  expect_is(predict(td(formula = lf~hf, conversion = 'first')), "ts")
})
