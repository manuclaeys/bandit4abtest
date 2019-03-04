library(partykit)
CtreeFormulaGenerate <- function(dt, visitorReward, armForLearn, explanatoryVariable, learnSize, ctreeControlVal) {
  set.seed(1234)

  dt <- dt[1:learnSize, ]
  visitorReward <- visitorReward[1:learnSize, ]

  #  missing values
  dt <- dt[!is.na(visitorReward[, armForLearn]), ]
  visitorReward <- visitorReward[!is.na(visitorReward[, armForLearn]), ]

  #  extract element of the list
  elt <- c(explanatoryVariable[1])
  if (length(explanatoryVariable) > 1) {
    for (i in 2:length(explanatoryVariable)) {
      elt <- paste(elt, explanatoryVariable[i], sep = " + ")
    }
  }

  #  update corrct bug where we had only one covariate
  dt <- as.data.frame(dt)
  if (length(explanatoryVariable) == 1) {
    colnames(dt) <- c("x")
    elt <- c("x")
  }

  #  add one variation restult to the data
  dt$armForLearn <- visitorReward[, armForLearn]

  Formula <- paste("armForLearn " , " ~ ", elt, sep = "")

  Formula <- as.formula(Formula)

  regTree <- ctree(formula = Formula, dt, control = ctreeControlVal, na.action = na.exclude)

 # pdf("../results/tree.pdf")
  plot(regTree)
 #  dev.off()

  return(regTree)
}
