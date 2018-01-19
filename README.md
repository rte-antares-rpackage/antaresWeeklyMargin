[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin)

# antaresWeeklyMargin


> Calculate Weekly Margin.


You can install the package from Github like this :


```r
# Install package from Github
source("https://install-github.me/rte-antares-rpackage/antaresWeeklyMargin")

library( "antaresWaterValues" )
```


## Read XML files

You can read Hydro XML files :

```r
# Lecture données vallées
vallees <- read_hydro_vallee(path = "inputs/hydro/")
str(vallees)
vallees

# Lecture données usines
usines <- read_hydro_usine(path = "inputs/hydro/")
str(usines)
usines
```

You can read Thermal XML files :
```r
# Lecture données tranche (puissances thermiques)
tranche <- read_thermal_tranche(path = "inputs/thermal/")
str(tranche)
tranche
```





