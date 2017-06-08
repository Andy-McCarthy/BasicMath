# BasicMath
BasicMath

The purpose of the BasicMath package is to perform basic mathematical operations that would otherwise have to be performed manually.

If the user wanted to find the mean and sample standard deviation of a data set as well as determine the most likely outcome, he or
she could use the Basic Math package in the following manner:

```{r}
a <- c(4,9,7,NA,3,1,8,NA,12,17,4,4,8,NA)

#visualize what values are in the data set
basictable(a)

#calculate the mean
arithmeticmean(a, na.rm = T)

#calculate the sample standard deviation
stdeviation(a, na.rm = T, sample = T)

#determine the most likely outcome(s)
modes(a, na.rm = T)
```

To install the package and vignette into RStudio, type:
```{r}
install_github("Andy-McCarthy/BasicMath", build_vignettes = T)
```

Usage of the package can be found in the vignette.
