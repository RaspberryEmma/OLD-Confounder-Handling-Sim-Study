
library(dagitty)
library(dplyr)
library(ggdag)
library(ggplot2)
library(shiny)
library(shinycssloaders)


# dag <- dagitty("dag{y <- z -> x}")
# tidy_dagitty(dag)
# ggdag(dag, layout = "circle")


# dagified <- dagify(x ~ z,
#                    y ~ z,
#                    exposure = "x",
#                    outcome = "y"
# )
# tidy_dagitty(dagified)
# ggdag(dagified, layout = "circle")

"dag{y <- z -> x}" %>% dagitty() %>% tidy_dagitty() %>% ggdag()

png( paste("../plots/example", "_DAG.png", sep = "") )
"dag{y <- z -> x}" %>% dagitty() %>% tidy_dagitty() %>% ggdag()
dev.off()

