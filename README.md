# classify

classify is a framework to streamline the evaluation of supervised
classification methods and seeks to determine the best classifiers on a variety
of simulated and benchmark data sets with a collection of benchmark metrics.

Typically, classifier superiority is determined by classification error rate (sometimes called
probability of misclassification or confusion rate). To assess classification
efficacy, we utilize the following error rate estimators:

* random split / Monte-Carlo cross-validation
* cross-validation
* bootstrap
* .632
* .632+
* apparent

Many other estimators are planned. Other metrics, such as
sensitivity/specificity, are also planned.

Many of the estimators that we have implemented are often superior to the more
widely used methods, such as cross-validation, but have not gained in popularity
for a variety of reasons. Two major reasons are that they are not as simple or
straightforward as a simple error-counting approach used in
cross-validation. Additionally, the more superior methods typically require a
large amount of computation.

First, we plan to have a simple interface in order to use even the most
complicated of estimators. Second, we plan to easily interface the error rate
estimators with the 'caret' package. Third, we will provide support for parallel
computations via the 'parallel' package. Many of the planned estimators are
'embarrassingly parallel' and should be easily computed with this package.





