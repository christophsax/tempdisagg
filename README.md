tempdisagg: Methods for Temporal Disaggregation and Interpolation of Time Series
--------------------------------------------------------------------------------

Temporal disaggregation methods are used to disaggregate or interpolate a low frequency time series to a higher frequency series, where either the sum, the average, the first or the last value of the resulting high frequency series is consistent with the low frequency series. Temporal disaggregation can be performed with or without one or more high frequency indicator series. The R package tempdisagg is a collection of several methods for temporal disaggregation. 

To install or update from from [CRAN][package], run:

    install.packages("tempdisagg")

A good way to start is to run the interactive demo:

    library(tempdisagg)
    demo(tempdisagg)
    
or read the help page of the main function (`?td`). Our article on 
[temporal disaggregation of time series][article] in the R-Journal describes
the package and the theory of temporal disaggregation in more detail.

Please report bugs on [Github][github] or send an [e-mail](mailto:christoph.sax@gmail.com), thank you!

[package]: http://cran.r-project.org/web/packages/tempdisagg
[article]: http://journal.r-project.org/archive/2013-2/sax-steiner.pdf
[github]: https://github.com/christophsax/tempdisagg
