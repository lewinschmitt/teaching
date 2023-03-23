## Fitting Classification Trees

# Install and load the ISLR package (to get the practice data)
install.packages("ISLR2")
library(ISLR2)

install.packages("tidyverse")
library(tidyverse)

install.packages("tree")
library(tree)

### Let's look at our data
View(Hitters)

### Let's restrict our dataset
Hitters <- Hitters %>% select(Years, Hits, Salary)
### Now we fit the tree using the tree() function. 
### Note that we use the logged Salary as our outcome!
tree.salary <- tree(log(Salary) ~ ., Hitters)

### Let's have a look 
summary(tree.salary)
plot(tree.salary)
text(tree.salary, pretty = 0)

### pruning the tree, setting k	(cost-complexity parameter) or best (requesting the size (i.e. number of terminal nodes))
tree.salary_pruned <- prune.tree(tree.salary, best = 3) # play around with its value

### Let's have a look again
plot(tree.salary_pruned)
text(tree.salary_pruned, pretty = NULL)

# What are our internal nodes? Terminal nodes?
