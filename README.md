[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin)

# antaresWeeklyMargin


> Process data for Weekly Margin simulation.


You can install the package from Github like this :


```r
# Install package from Github
source("https://install-github.me/rte-antares-rpackage/antaresWeeklyMargin")

library( "antaresWeeklyMargin" )
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


## Get Eco2mix data

You can retrieve data from Eco2mix via the [Opendatasoft API](https://rte-opendata.opendatasoft.com/explore/?sort=modified&q=eco2mix), for now only national real time data can be retrieved :

```r
eco2mix <- get_eco2mix(
  from = "2018-01-06", 
  to = "2018-01-12", 
  user = "NNI",         # needeed if no internet connection open
  proxy_pwd = "PASSWORD"
)
```

Specificly you can acces *hydraulique au fil de l'eau éclusée* data like this :


```r
fil_eau <- get_hydraulique_fil_de_l_eau_eclusee(
  user = "NNI", proxy_pwd = "PASSWORD"
)
```
| date       | date_heure       | hydraulique_fil_de_l_eau_eclusee |
|------------|------------------|----------------------------------|
| 13/01/2018 | 13/01/2018 01:00 | 5954                             |
| 13/01/2018 | 13/01/2018 02:00 | 5909                             |
| 13/01/2018 | 13/01/2018 03:00 | 5702                             |
| 13/01/2018 | 13/01/2018 04:00 | 5633                             |
| 13/01/2018 | 13/01/2018 05:00 | 5637                             |






