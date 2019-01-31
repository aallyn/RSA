pearson.resids<- function (predicted, observed) {
  pearson<- -1*((predicted - observed)/sqrt(predicted*(1-predicted)))
  pearson
}
