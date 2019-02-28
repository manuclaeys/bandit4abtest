library(partykit)
ctree_formula_generate <- function(dt, visitorReward, arm_for_learn, explanatory_variable, learn_size,ctree_control_val) {
  set.seed(1234)


  dt <- dt[1:learn_size, ]
  visitorReward <- visitorReward[1:learn_size, ]

  #missing values
  dt <- dt[!is.na(visitorReward[, arm_for_learn]),]
  visitorReward <- visitorReward[!is.na(visitorReward[, arm_for_learn]),]

  #extract element of the list
  elt <- c(explanatory_variable[1])
  if (length(explanatory_variable) > 1) {
    for (i in 2:length(explanatory_variable)) {
      elt <- paste(elt, explanatory_variable[i], sep = " + ")
    }
  }

  #update corrct bug where we had only one covariate
  dt <- as.data.frame(dt)
  if (length(explanatory_variable) == 1) {
    colnames(dt) <- c("x")
    elt <- c("x")
  }

  #add one variation restult to the data
  dt$arm_for_learn <- visitorReward[, arm_for_learn]

  Formula <-  paste("arm_for_learn " , " ~ ", elt, sep = "")

  Formula <- as.formula(Formula)

  reg_tree <- ctree(formula = Formula, dt, control = ctree_control_val, na.action = na.exclude)

 # pdf("../results/tree.pdf")
  plot(reg_tree)
 #  dev.off()

  return(reg_tree)
}
