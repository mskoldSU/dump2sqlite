# dump2sqlite

Simple hack R-package to convert a MySQL dump file to an SQLite database. Tested on dumps created by *MySQL dump 10.11* and *MySQL dump 10.19*. Comes with no guarantees.

Install with

``` r
devtools::install_github("https://github.com/mskoldSU/dump2sqlite")
```

### Sample session

To write a database file `my_dump.sqlite`
``` r
library(dump2sqlite)
dump2sqlite("my_dump.sql")
```
