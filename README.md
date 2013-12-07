# sortinghat

The `sortinghat` package is a framework in R to streamline the evaluation of
classifiers (classification models and algorithms) and seeks to determine the
best classifiers on a variety of simulated and benchmark data sets with a
collection of benchmark metrics.

## Installation

You can install the stable version on [CRAN](http://cran.r-project.org/package=sortinghat):

```r
install.packages('sortinghat', dependencies = TRUE)
```

If you prefer to download the latest version, instead type:

```r
library(devtools)
install_github('sortinghat', 'ramey')
```

## Benchmarking

A primary goal of `sortinghat` is to enable rapid benchmarking across a variety of
classification scenarios. To achieve this, we provide a large selection of both
real and simulated data sets collected from the literature and around the
Internet. With `sortinghat`, researchers can quickly replicate findings within the
literature as well as rapidly prototype new classifiers.

*The list of real and simulated data sets will continue to grow. Contributions
are greatly appreciated as pull requests.*

### Data Sets

Benchmark data sets are useful for evaluating and comparing classifiers...

*(Work in Progress: Version 0.2 will include a collection of benchmark data sets)*

### Simulated Data Sets

In addition to benchmark data sets, `sortinghat` provide a large collection of
data-generating models for simulations based on studies in the literature. Thus
far, we have added multivariate simulation models based on the following family
of distributions:

- Multivariate Normal
- Multivariate Student's t
- Multivariate Contaminated Normal
- Multivariate Uniform

Moreover, data can be generated based on the well-known configurations from:

- [Friedman (1989)](http://www.jstor.org/discover/10.2307/2289860)
- [Guo, Hastie, and Tibshirani (2007)](http://biostatistics.oxfordjournals.org/content/8/1/86.long)

The simulated data sets listed above can be generated via the `simdata`
function.

## Error-Rate Estimation

Classifier superiority is often determined by classification error rate (1 -
accuracy). To assess classification efficacy, we utilize the following
error-rate estimators:

- Cross-validation Error Rate
- Bootstrap Error Rate
- .632 Estimator from [Efron (1983)](http://www.jstor.org/discover/10.2307/2288636)
- .632+ Estimator from [Efron and Tibshirani (1997)](http://www.jstor.org/discover/10.2307/2965703)
- Bootstrap Cross-validation from [Fu, Carrol, and Wang (2005)](http://bioinformatics.oxfordjournals.org/content/21/9/1979.abstract)
- Leave-One-Out Bootstrap Error Rate
- Apparent Error Rate

Each of these error rates can be accessed via the `errorest` function, which
acts as a wrapper around the error-rate estimators listed above.


