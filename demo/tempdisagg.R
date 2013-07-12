require(tempdisagg)

# Suppose we have an annual series and want to create quarterly values that sum
# up to the annual values. Let us explore the annual sales of the pharmaceutical
# and chemical industry in Switzerland, from which we want to create a quarterly
# series.

data(swisspharma)
plot(sales.a)

# The most simple method is \code{denton-cholette} without an indicator. It
# performs a simple interpolation that meets the temporal additivity constraint.
# In R, this can be done the following way:

m1 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")
predict(m1)
plot(predict(m1))
