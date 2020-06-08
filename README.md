### (Conditional) Permuation Importance for Random Forests.

This R-package computes the Conditional Permutation Importance (CIP; Strobl, 2008) using an alternative implemantation that is both faster and more stable. The (C)PI can be computed for random forest fit using (a) the original impurity reduction method ( `randomForest`-package), and (b) using the Conditional Inference framework (`party`-package). In addition, a plotting method for the resulting `VarImp`-object.



## Installation


The package can be installed using using the `devtools`-package:

```
install.packages("devtools")
devtools::install_github("ddebeer/permimp")
```

The workhorse is the `permimp`-function. 

```
?permimp
```

For documentation about the plotting function:

```
?plot.VarImp
```



