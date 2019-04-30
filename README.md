# Modeling in the tidyverse

Notes and code for a 2019 ODSC East 90min workshop. 

If you want to run the analyses _locally_, please install:

```r
install.packages(
  c('tidymodels', 'tidyverse', 'AmesHousing', 'kknn', 'MASS'),
  repos = "http://cran.rstudio.com"
)

# optionally, you could also install: 
install.packages("rstanarm", repos = "http://cran.r-project.org")
```

We will have **RStudio Server Pro** instances if you would like to avoid installing packages on your laptop (or can't). You just need a web browser. We'll give out instructions at the start of the workshop. 

One note: these notes were generated using R 3.5.3. If you run them on R >= 3.6.0, the results will be slightly different due to different behavior of `sample()` in base R. 

