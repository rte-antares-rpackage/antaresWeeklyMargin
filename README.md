# antaresWeeklyMargin

> Process data for Weekly Margin simulation.

[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresWeeklyMargin)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)



## Installation

You can install the package from Github like this :


```r
# Install package from Github
remotes::install_github("rte-antares-rpackage/antaresWeeklyMargin")

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
  to = "2018-01-12"
)
```

Specificly you can access *hydraulique au fil de l'eau éclusée* data like this :


```r
# Get previous friday
vendredi <- get_previous("vendredi")
# Get previous saturday before friday
samedi <- get_previous("samedi", vendredi)

# Get data
fil_eau <- get_hydraulique_fil_de_l_eau_eclusee(from = samedi, to = vendredi)

```
| date       | date_heure       | hydraulique_fil_de_l_eau_eclusee |
|------------|------------------|----------------------------------|
| 13/01/2018 | 13/01/2018 01:00 | 5954                             |
| 13/01/2018 | 13/01/2018 02:00 | 5909                             |
| 13/01/2018 | 13/01/2018 03:00 | 5702                             |
| 13/01/2018 | 13/01/2018 04:00 | 5633                             |
| 13/01/2018 | 13/01/2018 05:00 | 5637                             |



### Proxy settings

Access to an API can be blocked by the proxy, you can set your credentials to allow retrieveng data :

```r
setupProxy(user = "NNI", proxy_pwd = "MOT_DE_PASSE")
# You need to restart your R session
get_proxy_info() # check if settings are correctly set
```

It only needs to be done once.
Don't forget to restart your R session for change to be effective !


## RTE DATA API

You can retrieve NTC data from [RTE data API](https://data.rte-france.com/) with function `get_ntc`.
This API require to have an account and to subscribe to a specific API (*Abonnez-vous à l'API*) and creating an application.

Before retrieving data, you need a token :

```r
id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
token <- get_token(
  key = list(id_client = id_client, id_secret = id_secret)
)
```

`id_client` and `id_secret` can be found on the page *Mes applications* if you're logged.


With this token you can call `get_ntc` :

```r
ntc <- get_ntc(token = token, type = c("ANNUAL", "MONTHLY"))
```

| type    | sender_country_name | sender_country_eic_code | receiver_country_name | receiver_country_eic_code | start_date | end_date   | updated_date | value |
|---------|---------------------|-------------------------|-----------------------|---------------------------|------------|------------|--------------|-------|
|         |                     |                         |                       |                           |            |            |              |       |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 01/02/2018 | 02/02/2018 | 18/01/2018   | 1200  |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 02/02/2018 | 03/02/2018 | 18/01/2018   | 1200  |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 03/02/2018 | 04/02/2018 | 18/01/2018   | 1200  |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 04/02/2018 | 05/02/2018 | 18/01/2018   | 1200  |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 05/02/2018 | 06/02/2018 | 18/01/2018   | 1200  |
| MONTHLY | Germany             | 10YCB-GERMANY--8        | France                | 10YFR-RTE------C          | 06/02/2018 | 07/02/2018 | 18/01/2018   | 1200  |



