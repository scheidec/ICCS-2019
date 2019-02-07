ICCS
================
Jenny
January, 2019

## Introduction

This file documents the data cleaning for the ICCS 1999-2009-2016
Citizenship Norms Project

Note that for citizenship norm recodes in all three survey waves, the
norms are coded in the descending mean order of the 1999 data:
obey,rights,local,work,envir,vote,history,respect,news,protest,discuss,party

## 1999 data loading and merging

1999 data:
<https://www.icpsr.umich.edu/icpsrweb/civicleads/studies/21661/datadocumentation>  
Downloaded Jan 17, 2019

Load 1999 country files, in chronological order of file names. Bind all
1999 files. Note, total observations of resulting tbl1 (93,882) concur
with xls documentation of expected total n

``` r
# all files
files <- list.files("../data", full.names = TRUE)

# helper function to load files
load_files <- function(file) {
  e <- new.env()
  load(file, envir = e)
  
  stopifnot(length(e) == 1)  # safety first
  
  get(names(e)[1], envir = e)
}

tbl1 <- files %>%
  magrittr::extract(1:28) %>%      # filter to 1999 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, BS3B1, BS3B11, BS3B9, BS3B4, BS3B13,
               BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B12, BS3B3)) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  mutate(`ICCS_year` = 1999) %>%     # add survey year variable
  select(`ICCS_year`, everything())
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl1 %>% 
  select(BS3B1, BS3B11, BS3B9, BS3B4, BS3B13, BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B12, BS3B3) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl1 %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   BS3B1                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         1995
    ## 2 (2) somewhat unimportant  2418
    ## 3 (3) somewhat important   20211
    ## 4 (4) very important       66431
    ## 5 <NA>                      2827
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   BS3B11                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         2975
    ## 2 (2) somewhat unimportant 10785
    ## 3 (3) somewhat important   35630
    ## 4 (4) very important       38503
    ## 5 <NA>                      5989
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   BS3B9                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         2986
    ## 2 (2) somewhat unimportant 11726
    ## 3 (3) somewhat important   42796
    ## 4 (4) very important       31396
    ## 5 <NA>                      4978
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   BS3B4                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         5447
    ## 2 (2) somewhat unimportant 11830
    ## 3 (3) somewhat important   35762
    ## 4 (4) very important       35769
    ## 5 <NA>                      5074
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   BS3B13                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         4064
    ## 2 (2) somewhat unimportant 12865
    ## 3 (3) somewhat important   36910
    ## 4 (4) very important       35383
    ## 5 <NA>                      4660
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   BS3B2                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         4566
    ## 2 (2) somewhat unimportant 13382
    ## 3 (3) somewhat important   37364
    ## 4 (4) very important       35023
    ## 5 <NA>                      3547
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   BS3B6                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         7861
    ## 2 (2) somewhat unimportant 17428
    ## 3 (3) somewhat important   32388
    ## 4 (4) very important       31589
    ## 5 <NA>                      4616
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   BS3B10                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         6109
    ## 2 (2) somewhat unimportant 18138
    ## 3 (3) somewhat important   40898
    ## 4 (4) very important       22579
    ## 5 <NA>                      6158
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   BS3B8                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         7319
    ## 2 (2) somewhat unimportant 18421
    ## 3 (3) somewhat important   42831
    ## 4 (4) very important       20869
    ## 5 <NA>                      4442
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   BS3B5                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         8941
    ## 2 (2) somewhat unimportant 18358
    ## 3 (3) somewhat important   34180
    ## 4 (4) very important       22693
    ## 5 <NA>                      9710
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   BS3B12                       n
    ##   <fct>                    <int>
    ## 1 (1) not importnat        13954
    ## 2 (2) somewhat unimportant 34883
    ## 3 (3) somewhat important   28004
    ## 4 (4) very important        8959
    ## 5 <NA>                      8082
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   BS3B3                        n
    ##   <fct>                    <int>
    ## 1 (1) not important        23205
    ## 2 (2) somewhat unimportant 36163
    ## 3 (3) somewhat important   19853
    ## 4 (4) very important        6741
    ## 5 <NA>                      7920

Cit norm, count and recode 1st indicator as example.

``` r
# recode
tbl1 <- tbl1 %>% 
  mutate(BS3B1_binary = fct_collapse(BS3B1, 
  "not important" = c("(1) not important", "(2) somewhat unimportant"),
  "important"     = c("(3) somewhat important", "(4) very important")))

# confirm correct recode
tbl1 %>%
  count(BS3B1, BS3B1_binary) 
```

    ## # A tibble: 5 x 3
    ##   BS3B1                    BS3B1_binary      n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  1995
    ## 2 (2) somewhat unimportant not important  2418
    ## 3 (3) somewhat important   important     20211
    ## 4 (4) very important       important     66431
    ## 5 <NA>                     <NA>           2827

Repeat for all all cit norm indicators. NOTE: BS3B12 recoded separately
below b/c of typo in string variable.

``` r
tbl1 <-tbl1 %>% 
  mutate_at(vars(BS3B1, BS3B11, BS3B9, BS3B4, BS3B13, BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B3),
            funs(bin = fct_collapse(.,
                                    "not important"= c("(1) not important", "(2) somewhat unimportant"),
                                    "important"    = c("(3) somewhat important", "(4) very important")))
  )
```

BS3B12 error troubleshoot when included in prior chunk. Count table
command below yields console output showing that string text of 1st
category “importnat” spelled incorrectly, i.e. “a” and “n” transposed.
BS3B12 “mutate” command to correctly recode with this typo:

``` r
# troubleshoot
tbl1 %>% count(BS3B12) 
```

    ## # A tibble: 5 x 2
    ##   BS3B12                       n
    ##   <fct>                    <int>
    ## 1 (1) not importnat        13954
    ## 2 (2) somewhat unimportant 34883
    ## 3 (3) somewhat important   28004
    ## 4 (4) very important        8959
    ## 5 <NA>                      8082

``` r
# recode
tbl1 <- tbl1 %>% 
  mutate(BS3B12_bin = fct_collapse(BS3B12, 
                                   "not important" = c("(1) not importnat", "(2) somewhat unimportant"),
                                   "important"     = c("(3) somewhat important", "(4) very important"))
  )
```

Confirm successful mutates for all indicators.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl1 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   BS3B1                    BS3B1_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  1995
    ## 2 (2) somewhat unimportant not important  2418
    ## 3 (3) somewhat important   important     20211
    ## 4 (4) very important       important     66431
    ## 5 <NA>                     <NA>           2827
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   BS3B11                   BS3B11_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  2975
    ## 2 (2) somewhat unimportant not important 10785
    ## 3 (3) somewhat important   important     35630
    ## 4 (4) very important       important     38503
    ## 5 <NA>                     <NA>           5989
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   BS3B9                    BS3B9_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  2986
    ## 2 (2) somewhat unimportant not important 11726
    ## 3 (3) somewhat important   important     42796
    ## 4 (4) very important       important     31396
    ## 5 <NA>                     <NA>           4978
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   BS3B4                    BS3B4_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  5447
    ## 2 (2) somewhat unimportant not important 11830
    ## 3 (3) somewhat important   important     35762
    ## 4 (4) very important       important     35769
    ## 5 <NA>                     <NA>           5074
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   BS3B13                   BS3B13_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  4064
    ## 2 (2) somewhat unimportant not important 12865
    ## 3 (3) somewhat important   important     36910
    ## 4 (4) very important       important     35383
    ## 5 <NA>                     <NA>           4660
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   BS3B2                    BS3B2_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  4566
    ## 2 (2) somewhat unimportant not important 13382
    ## 3 (3) somewhat important   important     37364
    ## 4 (4) very important       important     35023
    ## 5 <NA>                     <NA>           3547
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   BS3B6                    BS3B6_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  7861
    ## 2 (2) somewhat unimportant not important 17428
    ## 3 (3) somewhat important   important     32388
    ## 4 (4) very important       important     31589
    ## 5 <NA>                     <NA>           4616
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   BS3B10                   BS3B10_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  6109
    ## 2 (2) somewhat unimportant not important 18138
    ## 3 (3) somewhat important   important     40898
    ## 4 (4) very important       important     22579
    ## 5 <NA>                     <NA>           6158
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   BS3B8                    BS3B8_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  7319
    ## 2 (2) somewhat unimportant not important 18421
    ## 3 (3) somewhat important   important     42831
    ## 4 (4) very important       important     20869
    ## 5 <NA>                     <NA>           4442
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   BS3B5                    BS3B5_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  8941
    ## 2 (2) somewhat unimportant not important 18358
    ## 3 (3) somewhat important   important     34180
    ## 4 (4) very important       important     22693
    ## 5 <NA>                     <NA>           9710
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   BS3B12                   BS3B12_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not importnat        not important 13954
    ## 2 (2) somewhat unimportant not important 34883
    ## 3 (3) somewhat important   important     28004
    ## 4 (4) very important       important      8959
    ## 5 <NA>                     <NA>           8082
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   BS3B3                    BS3B3_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important 23205
    ## 2 (2) somewhat unimportant not important 36163
    ## 3 (3) somewhat important   important     19853
    ## 4 (4) very important       important      6741
    ## 5 <NA>                     <NA>           7920

Select for LCA vars tibble, including rename all 12 mutated variables
and display first five lines of dataframe. Use “select” for key LCA
variables to create reduced tbl that can be “binded” with other ICCS
years.

``` r
tbl1 <- tbl1 %>%
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         obey    = BS3B1_bin,
         rights  = BS3B11_bin,
         local   = BS3B9_bin,
         work    = BS3B4_bin,
         envir   = BS3B13_bin,
         vote    = BS3B2_bin,
         history = BS3B6_bin,
         respect = BS3B10_bin,
         news    = BS3B8_bin,
         protest = BS3B5_bin,
         discuss = BS3B12_bin,
         party   = BS3B3_bin)

tbl1 %>% head()
```

    ## # A tibble: 6 x 15
    ##   ICCS_year COUNTRY IDSTUD obey  rights local work  envir vote  history
    ##       <dbl> <fct>    <dbl> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      1999 AUS      10302 impo~ not i~ impo~ impo~ impo~ impo~ import~
    ## 2      1999 AUS      10305 impo~ not i~ impo~ impo~ impo~ impo~ not im~
    ## 3      1999 AUS      10311 impo~ <NA>   <NA>  impo~ impo~ not ~ not im~
    ## 4      1999 AUS      10313 impo~ not i~ not ~ not ~ not ~ impo~ not im~
    ## 5      1999 AUS      10317 impo~ impor~ impo~ not ~ impo~ not ~ import~
    ## 6      1999 AUS      10319 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## # ... with 5 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>

## 2009 dataloading and merging

2009 data: <https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36997>
Downloaded Jan 17, 2019

Load 2009 country files, in chronological order of file names. Bind all
2009 files; Note, total observations of resulting tbl2 (140,650) concur
with xls documentation of expected total n.

``` r
tbl2 <- files %>%
  magrittr::extract(29:66) %>%      # filter to 2009 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, IS2P21L, IS2P21I, IS2P21H, IS2P21K, IS2P21J, IS2P21A,
               IS2P21C, IS2P21E, IS2P21D, IS2P21G, IS2P21F, IS2P21B)) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  mutate(`ICCS_year` = 2009) %>%     # survey year variable creation
  select(`ICCS_year`, everything())
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl2 %>% 
  select(IS2P21L, IS2P21I, IS2P21H, IS2P21K, IS2P21J, IS2P21A, IS2P21C, IS2P21E, IS2P21D, IS2P21G, IS2P21F, IS2P21B) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl2 %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   IS2P21L                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       76977
    ## 2 (2) QUITE IMPORTANT      45856
    ## 3 (3) NOT VERY IMPORTANT   10022
    ## 4 (4) NOT IMPORTANT AT ALL  3961
    ## 5 <NA>                      3834
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   IS2P21I                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       53959
    ## 2 (2) QUITE IMPORTANT      59698
    ## 3 (3) NOT VERY IMPORTANT   18844
    ## 4 (4) NOT IMPORTANT AT ALL  3862
    ## 5 <NA>                      4287
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   IS2P21H                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       47674
    ## 2 (2) QUITE IMPORTANT      63169
    ## 3 (3) NOT VERY IMPORTANT   21124
    ## 4 (4) NOT IMPORTANT AT ALL  4368
    ## 5 <NA>                      4315
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   IS2P21K                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       53170
    ## 2 (2) QUITE IMPORTANT      58047
    ## 3 (3) NOT VERY IMPORTANT   20006
    ## 4 (4) NOT IMPORTANT AT ALL  5379
    ## 5 <NA>                      4048
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   IS2P21J                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       61438
    ## 2 (2) QUITE IMPORTANT      54712
    ## 3 (3) NOT VERY IMPORTANT   16255
    ## 4 (4) NOT IMPORTANT AT ALL  4043
    ## 5 <NA>                      4202
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   IS2P21A                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       58412
    ## 2 (2) QUITE IMPORTANT      54399
    ## 3 (3) NOT VERY IMPORTANT   20691
    ## 4 (4) NOT IMPORTANT AT ALL  4019
    ## 5 <NA>                      3129
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   IS2P21C                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       50412
    ## 2 (2) QUITE IMPORTANT      55702
    ## 3 (3) NOT VERY IMPORTANT   24553
    ## 4 (4) NOT IMPORTANT AT ALL  5582
    ## 5 <NA>                      4401
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   IS2P21E                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       40616
    ## 2 (2) QUITE IMPORTANT      65090
    ## 3 (3) NOT VERY IMPORTANT   24294
    ## 4 (4) NOT IMPORTANT AT ALL  6739
    ## 5 <NA>                      3911
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   IS2P21D                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       37359
    ## 2 (2) QUITE IMPORTANT      63832
    ## 3 (3) NOT VERY IMPORTANT   29728
    ## 4 (4) NOT IMPORTANT AT ALL  5920
    ## 5 <NA>                      3811
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   IS2P21G                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       35362
    ## 2 (2) QUITE IMPORTANT      51996
    ## 3 (3) NOT VERY IMPORTANT   37311
    ## 4 (4) NOT IMPORTANT AT ALL 11557
    ## 5 <NA>                      4424
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   IS2P21F                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       15669
    ## 2 (2) QUITE IMPORTANT      43337
    ## 3 (3) NOT VERY IMPORTANT   61418
    ## 4 (4) NOT IMPORTANT AT ALL 15929
    ## 5 <NA>                      4297
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   IS2P21B                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       12868
    ## 2 (2) QUITE IMPORTANT      33456
    ## 3 (3) NOT VERY IMPORTANT   71041
    ## 4 (4) NOT IMPORTANT AT ALL 19402
    ## 5 <NA>                      3883

Recode all cit norm indicators.

``` r
tbl2 <- tbl2 %>% 
  mutate_at(vars(IS2P21L, IS2P21I, IS2P21H, IS2P21K, IS2P21J, IS2P21A, IS2P21C, IS2P21E, IS2P21D, IS2P21G, IS2P21F, IS2P21B),
            funs(bin = fct_collapse(.,
                                    "not important" = c("(3) NOT VERY IMPORTANT", "(4) NOT IMPORTANT AT ALL"),
                                    "important"     = c("(1) VERY IMPORTANT", "(2) QUITE IMPORTANT")))
  )
```

Confirm successful recodes.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl2 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   IS2P21L                  IS2P21L_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     76977
    ## 2 (2) QUITE IMPORTANT      important     45856
    ## 3 (3) NOT VERY IMPORTANT   not important 10022
    ## 4 (4) NOT IMPORTANT AT ALL not important  3961
    ## 5 <NA>                     <NA>           3834
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   IS2P21I                  IS2P21I_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     53959
    ## 2 (2) QUITE IMPORTANT      important     59698
    ## 3 (3) NOT VERY IMPORTANT   not important 18844
    ## 4 (4) NOT IMPORTANT AT ALL not important  3862
    ## 5 <NA>                     <NA>           4287
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   IS2P21H                  IS2P21H_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     47674
    ## 2 (2) QUITE IMPORTANT      important     63169
    ## 3 (3) NOT VERY IMPORTANT   not important 21124
    ## 4 (4) NOT IMPORTANT AT ALL not important  4368
    ## 5 <NA>                     <NA>           4315
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   IS2P21K                  IS2P21K_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     53170
    ## 2 (2) QUITE IMPORTANT      important     58047
    ## 3 (3) NOT VERY IMPORTANT   not important 20006
    ## 4 (4) NOT IMPORTANT AT ALL not important  5379
    ## 5 <NA>                     <NA>           4048
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   IS2P21J                  IS2P21J_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     61438
    ## 2 (2) QUITE IMPORTANT      important     54712
    ## 3 (3) NOT VERY IMPORTANT   not important 16255
    ## 4 (4) NOT IMPORTANT AT ALL not important  4043
    ## 5 <NA>                     <NA>           4202
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   IS2P21A                  IS2P21A_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     58412
    ## 2 (2) QUITE IMPORTANT      important     54399
    ## 3 (3) NOT VERY IMPORTANT   not important 20691
    ## 4 (4) NOT IMPORTANT AT ALL not important  4019
    ## 5 <NA>                     <NA>           3129
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   IS2P21C                  IS2P21C_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     50412
    ## 2 (2) QUITE IMPORTANT      important     55702
    ## 3 (3) NOT VERY IMPORTANT   not important 24553
    ## 4 (4) NOT IMPORTANT AT ALL not important  5582
    ## 5 <NA>                     <NA>           4401
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   IS2P21E                  IS2P21E_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     40616
    ## 2 (2) QUITE IMPORTANT      important     65090
    ## 3 (3) NOT VERY IMPORTANT   not important 24294
    ## 4 (4) NOT IMPORTANT AT ALL not important  6739
    ## 5 <NA>                     <NA>           3911
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   IS2P21D                  IS2P21D_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     37359
    ## 2 (2) QUITE IMPORTANT      important     63832
    ## 3 (3) NOT VERY IMPORTANT   not important 29728
    ## 4 (4) NOT IMPORTANT AT ALL not important  5920
    ## 5 <NA>                     <NA>           3811
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   IS2P21G                  IS2P21G_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     35362
    ## 2 (2) QUITE IMPORTANT      important     51996
    ## 3 (3) NOT VERY IMPORTANT   not important 37311
    ## 4 (4) NOT IMPORTANT AT ALL not important 11557
    ## 5 <NA>                     <NA>           4424
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   IS2P21F                  IS2P21F_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     15669
    ## 2 (2) QUITE IMPORTANT      important     43337
    ## 3 (3) NOT VERY IMPORTANT   not important 61418
    ## 4 (4) NOT IMPORTANT AT ALL not important 15929
    ## 5 <NA>                     <NA>           4297
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   IS2P21B                  IS2P21B_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     12868
    ## 2 (2) QUITE IMPORTANT      important     33456
    ## 3 (3) NOT VERY IMPORTANT   not important 71041
    ## 4 (4) NOT IMPORTANT AT ALL not important 19402
    ## 5 <NA>                     <NA>           3883

Select to rename all 12 mutated variables and display first five lines
of dataframe - listed in order of 1999 descending means (IJCS article).
Use “select” for key LCA variables to create reduced tbl that can be
“binded” with other ICCS years.

``` r
tbl2 <- tbl2 %>%
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         obey    = IS2P21L_bin,
         rights  = IS2P21I_bin,
         local   = IS2P21H_bin,
         work    = IS2P21K_bin,
         envir   = IS2P21J_bin,
         vote    = IS2P21A_bin,
         history = IS2P21C_bin,
         respect = IS2P21E_bin,
         news    = IS2P21D_bin,
         protest = IS2P21G_bin,
         discuss = IS2P21F_bin,
         party   = IS2P21B_bin) 

tbl2 %>% head()
```

    ## # A tibble: 6 x 15
    ##   ICCS_year COUNTRY IDSTUD obey  rights local work  envir vote  history
    ##       <dbl> <fct>    <dbl> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      2009 AUT     1.00e7 not ~ impor~ impo~ not ~ impo~ impo~ not im~
    ## 2      2009 AUT     1.00e7 impo~ impor~ impo~ not ~ impo~ not ~ import~
    ## 3      2009 AUT     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 4      2009 AUT     1.00e7 impo~ impor~ impo~ not ~ impo~ impo~ import~
    ## 5      2009 AUT     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 6      2009 AUT     1.00e7 impo~ impor~ not ~ impo~ impo~ impo~ import~
    ## # ... with 5 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>

## 2016 data loading and merging

2016 data: <https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/37147>
Downloaded Jan 21, 2019

2016 country files, in chronological order of file names. Bind all 2016
country files. Note, total observations of resulting tbl (94,603) concur
with xls documentation of expected total n.

``` r
tbl3 <- files %>%
  magrittr::extract(67:90) %>%      # filter to 2016 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J,
               IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B)) %>% 
  reduce(rbind) %>% 
  as_tibble()%>% 
  mutate(`ICCS_year` = 2016) %>%    # create survey year variable
  select(`ICCS_year`, everything())
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl3 %>% 
  select(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl3 %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   IS3G23L                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       53951
    ## 2 (2) Quite important      30460
    ## 3 (3) Not very important    5857
    ## 4 (4) Not important at all  1711
    ## 5 <NA>                      2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   IS3G23I                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37255
    ## 2 (2) Quite important      40011
    ## 3 (3) Not very important   12492
    ## 4 (4) Not important at all  2228
    ## 5 <NA>                      2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   IS3G23H                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       32569
    ## 2 (2) Quite important      42817
    ## 3 (3) Not very important   14079
    ## 4 (4) Not important at all  2471
    ## 5 <NA>                      2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   IS3G23K                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38082
    ## 2 (2) Quite important      40137
    ## 3 (3) Not very important   11292
    ## 4 (4) Not important at all  2536
    ## 5 <NA>                      2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   IS3G23J                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       42126
    ## 2 (2) Quite important      37204
    ## 3 (3) Not very important   10437
    ## 4 (4) Not important at all  2188
    ## 5 <NA>                      2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   IS3G23A                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38931
    ## 2 (2) Quite important      37262
    ## 3 (3) Not very important   13975
    ## 4 (4) Not important at all  2490
    ## 5 <NA>                      1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   IS3G23C                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37043
    ## 2 (2) Quite important      36919
    ## 3 (3) Not very important   14836
    ## 4 (4) Not important at all  3023
    ## 5 <NA>                      2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   IS3G23E                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       33375
    ## 2 (2) Quite important      43140
    ## 3 (3) Not very important   12581
    ## 4 (4) Not important at all  3017
    ## 5 <NA>                      2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   IS3G23D                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       26980
    ## 2 (2) Quite important      43878
    ## 3 (3) Not very important   18199
    ## 4 (4) Not important at all  3297
    ## 5 <NA>                      2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   IS3G23G                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       22817
    ## 2 (2) Quite important      35020
    ## 3 (3) Not very important   26965
    ## 4 (4) Not important at all  7129
    ## 5 <NA>                      2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   IS3G23F                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       11348
    ## 2 (2) Quite important      30410
    ## 3 (3) Not very important   41390
    ## 4 (4) Not important at all  8739
    ## 5 <NA>                      2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   IS3G23B                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important        9003
    ## 2 (2) Quite important      22157
    ## 3 (3) Not very important   48896
    ## 4 (4) Not important at all 12187
    ## 5 <NA>                      2360

Recode all cit norm indicators.

``` r
tbl3 <- tbl3 %>% 
  mutate_at(vars(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B),
            funs(bin = fct_collapse(.,
                                    "not important" = c("(3) Not very important", "(4) Not important at all"),
                                    "important"     = c("(1) Very important", "(2) Quite important")))
  )
```

Confirm successful mutates.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl3 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   IS3G23L                  IS3G23L_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     53951
    ## 2 (2) Quite important      important     30460
    ## 3 (3) Not very important   not important  5857
    ## 4 (4) Not important at all not important  1711
    ## 5 <NA>                     <NA>           2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   IS3G23I                  IS3G23I_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37255
    ## 2 (2) Quite important      important     40011
    ## 3 (3) Not very important   not important 12492
    ## 4 (4) Not important at all not important  2228
    ## 5 <NA>                     <NA>           2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   IS3G23H                  IS3G23H_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     32569
    ## 2 (2) Quite important      important     42817
    ## 3 (3) Not very important   not important 14079
    ## 4 (4) Not important at all not important  2471
    ## 5 <NA>                     <NA>           2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   IS3G23K                  IS3G23K_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38082
    ## 2 (2) Quite important      important     40137
    ## 3 (3) Not very important   not important 11292
    ## 4 (4) Not important at all not important  2536
    ## 5 <NA>                     <NA>           2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   IS3G23J                  IS3G23J_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     42126
    ## 2 (2) Quite important      important     37204
    ## 3 (3) Not very important   not important 10437
    ## 4 (4) Not important at all not important  2188
    ## 5 <NA>                     <NA>           2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   IS3G23A                  IS3G23A_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38931
    ## 2 (2) Quite important      important     37262
    ## 3 (3) Not very important   not important 13975
    ## 4 (4) Not important at all not important  2490
    ## 5 <NA>                     <NA>           1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   IS3G23C                  IS3G23C_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37043
    ## 2 (2) Quite important      important     36919
    ## 3 (3) Not very important   not important 14836
    ## 4 (4) Not important at all not important  3023
    ## 5 <NA>                     <NA>           2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   IS3G23E                  IS3G23E_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     33375
    ## 2 (2) Quite important      important     43140
    ## 3 (3) Not very important   not important 12581
    ## 4 (4) Not important at all not important  3017
    ## 5 <NA>                     <NA>           2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   IS3G23D                  IS3G23D_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     26980
    ## 2 (2) Quite important      important     43878
    ## 3 (3) Not very important   not important 18199
    ## 4 (4) Not important at all not important  3297
    ## 5 <NA>                     <NA>           2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   IS3G23G                  IS3G23G_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     22817
    ## 2 (2) Quite important      important     35020
    ## 3 (3) Not very important   not important 26965
    ## 4 (4) Not important at all not important  7129
    ## 5 <NA>                     <NA>           2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   IS3G23F                  IS3G23F_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     11348
    ## 2 (2) Quite important      important     30410
    ## 3 (3) Not very important   not important 41390
    ## 4 (4) Not important at all not important  8739
    ## 5 <NA>                     <NA>           2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   IS3G23B                  IS3G23B_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important      9003
    ## 2 (2) Quite important      important     22157
    ## 3 (3) Not very important   not important 48896
    ## 4 (4) Not important at all not important 12187
    ## 5 <NA>                     <NA>           2360

Select to rename all 12 mutated variables and display first five lines
of dataframe - listed in order of 1999 descending means (IJCS article).
Use “select” for key LCA variables to create reduced tbl that can be
“binded” with other ICCS years.

``` r
tbl3 <- tbl3 %>% 
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         obey    = IS3G23L_bin,
         rights  = IS3G23I_bin,
         local   = IS3G23H_bin,
         work    = IS3G23K_bin,
         envir   = IS3G23J_bin,
         vote    = IS3G23A_bin,
         history = IS3G23C_bin,
         respect = IS3G23E_bin,
         news    = IS3G23D_bin,
         protest = IS3G23G_bin,
         discuss = IS3G23F_bin,
         party   = IS3G23B_bin)

tbl3 %>% head()
```

    ## # A tibble: 6 x 15
    ##   ICCS_year COUNTRY IDSTUD obey  rights local work  envir vote  history
    ##       <dbl> <fct>    <dbl> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 2      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 3      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 4      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 5      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 6      2016 BFL     1.00e7 impo~ impor~ impo~ impo~ impo~ not ~ not im~
    ## # ... with 5 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>

## Combining recoded 1999, 2009 and 2016 data frames

Combine the three data frames and create unique id per observation
across binded country files to be able to import Latent Gold class
assignment for each unique observation.

``` r
tbl <- rbind(tbl1, tbl2, tbl3) %>% 
  mutate(id  = row_number(),
         id2 = paste0(COUNTRY, IDSTUD))
```

Check number of observations by survey year of the combined data frame.

``` r
# number of observations by survey year
tbl %>% 
  count(ICCS_year) %>% 
  knitr::kable()
```

| ICCS\_year |      n |
| ---------: | -----: |
|       1999 |  93882 |
|       2009 | 140650 |
|       2016 |  94603 |

## Exporting final combined datafile

Before exporting, convert citizenship norm indicators to integer (0 =
“not important”, 1 =
“important”).

``` r
cit_norm_indicators <- vars(obey, rights, local, work, envir, vote, history, respect, news, protest, discuss, party)

tbl <- tbl %>% 
  mutate_at(cit_norm_indicators,
            funs(case_when(
              . == "not important" ~ 0,
              . == "important"     ~ 1
            ))
  ) %>% 
  mutate_if(is.double, as.integer) %>% 
  mutate(COUNTRY = as.character(COUNTRY))
```

    ## Warning in evalq(as.integer(IDSTUD), <environment>): NAs introduced by
    ## coercion to integer range

``` r
tbl %>% head()
```

    ## # A tibble: 6 x 17
    ##   ICCS_year COUNTRY IDSTUD  obey rights local  work envir  vote history
    ##       <int> <chr>    <int> <int>  <int> <int> <int> <int> <int>   <int>
    ## 1      1999 AUS      10302     1      0     1     1     1     1       1
    ## 2      1999 AUS      10305     1      0     1     1     1     1       0
    ## 3      1999 AUS      10311     1     NA    NA     1     1     0       0
    ## 4      1999 AUS      10313     1      0     0     0     0     1       0
    ## 5      1999 AUS      10317     1      1     1     0     1     0       1
    ## 6      1999 AUS      10319     1      1     1     1     1     1       0
    ## # ... with 7 more variables: respect <int>, news <int>, protest <int>,
    ## #   discuss <int>, party <int>, id <int>, id2 <chr>

We can also attach factor labels to the citizenship norm indicators for
internal use in R, but won’t export the factor labels to the output text
file (we’d have to save as an R object .e.g `.rds`). An example of how
we can do this:

``` r
example <- tbl %>% 
  mutate_at(cit_norm_indicators,
            funs(haven::labelled(., labels = c("not important" = 0, "important" = 1))))

# cit norm indicator vars are now int+lbl type
example %>% 
  glimpse()
```

    ## Observations: 329,135
    ## Variables: 17
    ## $ ICCS_year <int> 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999...
    ## $ COUNTRY   <chr> "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AU...
    ## $ IDSTUD    <int> 10302, 10305, 10311, 10313, 10317, 10319, 10324, 103...
    ## $ obey      <int+lbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ rights    <int+lbl> 0, 0, NA, 0, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ local     <int+lbl> 1, 1, NA, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ work      <int+lbl> 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ envir     <int+lbl> 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, ...
    ## $ vote      <int+lbl> 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, ...
    ## $ history   <int+lbl> 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, ...
    ## $ respect   <int+lbl> 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, ...
    ## $ news      <int+lbl> 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, ...
    ## $ protest   <int+lbl> 0, 1, NA, 0, 1, 1, 0, NA, 1, 0, 1, 1, 1, 1, 1, 1...
    ## $ discuss   <int+lbl> 0, 0, NA, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,...
    ## $ party     <int+lbl> 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ id        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1...
    ## $ id2       <chr> "AUS10302", "AUS10305", "AUS10311", "AUS10313", "AUS...

``` r
# access labels by converting those vars to factors
example %>% 
  mutate_at(cit_norm_indicators, funs(as_factor(.)))
```

    ## # A tibble: 329,135 x 17
    ##    ICCS_year COUNTRY IDSTUD obey  rights local work  envir vote  history
    ##        <int> <chr>    <int> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ##  1      1999 AUS      10302 impo~ not i~ impo~ impo~ impo~ impo~ import~
    ##  2      1999 AUS      10305 impo~ not i~ impo~ impo~ impo~ impo~ not im~
    ##  3      1999 AUS      10311 impo~ <NA>   <NA>  impo~ impo~ not ~ not im~
    ##  4      1999 AUS      10313 impo~ not i~ not ~ not ~ not ~ impo~ not im~
    ##  5      1999 AUS      10317 impo~ impor~ impo~ not ~ impo~ not ~ import~
    ##  6      1999 AUS      10319 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ##  7      1999 AUS      10324 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ##  8      1999 AUS      10325 impo~ <NA>   impo~ impo~ impo~ impo~ import~
    ##  9      1999 AUS      10326 impo~ impor~ impo~ impo~ impo~ not ~ not im~
    ## 10      1999 AUS      10327 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## # ... with 329,125 more rows, and 7 more variables: respect <fct>,
    ## #   news <fct>, protest <fct>, discuss <fct>, party <fct>, id <int>,
    ## #   id2 <chr>

Export recoded data for LCA - RdM to current `ICCS-2019/clean-data`
directory.

``` r
write_delim(tbl, "output/clean_tbl.dat", na = ".")
```

## Tables:

Means of citizenship norm indicators by country and year.

``` r
# count and percent of responses to "obey" grouped by year
means <- tbl %>%
  group_by(COUNTRY, ICCS_year) %>% 
  summarize_at(cit_norm_indicators, funs(mean(., na.rm = TRUE)))

means %>% 
  knitr::kable()
```

| COUNTRY | ICCS\_year |      obey |    rights |     local |      work |     envir |      vote |   history |   respect |      news |   protest |   discuss |     party |
| :------ | ---------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: |
| AUS     |       1999 | 0.9287573 | 0.6870358 | 0.7994714 | 0.8806166 | 0.7470647 | 0.8862080 | 0.5465386 | 0.6673333 | 0.5040053 | 0.5771955 | 0.3389423 | 0.1739425 |
| AUT     |       2009 | 0.8398073 | 0.7592034 | 0.7545290 | 0.6304348 | 0.6721954 | 0.7343750 | 0.7719298 | 0.6792510 | 0.6869723 | 0.6140139 | 0.3621524 | 0.2765700 |
| BFL     |       2009 | 0.8709239 | 0.7703300 | 0.7649660 | 0.7756280 | 0.7907609 | 0.8203999 | 0.4652589 | 0.8242877 | 0.6571816 | 0.4577250 | 0.3131793 | 0.1572348 |
| BFL     |       2016 | 0.9343015 | 0.8141379 | 0.8035221 | 0.8631034 | 0.8645473 | 0.8457575 | 0.5680719 | 0.8785272 | 0.7107438 | 0.4960194 | 0.3777240 | 0.1596292 |
| BFR     |       1999 | 0.9063765 | 0.7764645 | 0.5622407 | 0.5403599 | 0.7199587 | 0.8148893 | 0.4187817 | 0.5545503 | 0.5787863 | 0.5635479 | 0.3865285 | 0.2243724 |
| BGR     |       1999 | 0.8667904 | 0.8225058 | 0.6767757 | 0.8159393 | 0.8146703 | 0.7074202 | 0.7993791 | 0.6759404 | 0.6757797 | 0.7089925 | 0.4818976 | 0.3926483 |
| BGR     |       2009 | 0.8483789 | 0.9029064 | 0.8689413 | 0.8885705 | 0.8966177 | 0.6876972 | 0.8531379 | 0.6747372 | 0.6492846 | 0.7300672 | 0.3455128 | 0.2048920 |
| BGR     |       2016 | 0.8366562 | 0.9015598 | 0.8735314 | 0.8941707 | 0.9038862 | 0.7594632 | 0.8880779 | 0.7230503 | 0.6394065 | 0.7603048 | 0.3681453 | 0.2313278 |
| CHE     |       1999 | 0.9543364 | 0.8769334 | 0.7631579 | 0.6670065 | 0.6986532 | 0.6857996 | 0.6230331 | 0.7306080 | 0.7525569 | 0.6271605 | 0.4236135 | 0.2349914 |
| CHE     |       2009 | 0.8852686 | 0.7328696 | 0.6455211 | 0.6738227 | 0.7133449 | 0.7164539 | 0.7409722 | 0.7966747 | 0.7461140 | 0.5076495 | 0.3642981 | 0.2957064 |
| CHL     |       1999 | 0.9536101 | 0.8259359 | 0.8842719 | 0.8665441 | 0.8908662 | 0.9103722 | 0.8759273 | 0.8568522 | 0.7962360 | 0.6163928 | 0.3450361 | 0.4385491 |
| CHL     |       2009 | 0.8891924 | 0.8838483 | 0.9030244 | 0.8043222 | 0.9025822 | 0.8281827 | 0.8514599 | 0.8489279 | 0.6773500 | 0.6610666 | 0.2981257 | 0.2998243 |
| CHL     |       2016 | 0.8760529 | 0.8390920 | 0.8386774 | 0.7789198 | 0.8618131 | 0.7805317 | 0.8311245 | 0.7665533 | 0.6502699 | 0.6646611 | 0.4201445 | 0.3485485 |
| COL     |       1999 | 0.9737226 | 0.9408348 | 0.9420320 | 0.8372605 | 0.9392670 | 0.8791631 | 0.8427437 | 0.8618322 | 0.6615143 | 0.8071099 | 0.5129084 | 0.5772507 |
| COL     |       2009 | 0.8751908 | 0.9412559 | 0.8983767 | 0.8112245 | 0.9603121 | 0.8706667 | 0.8903479 | 0.8545669 | 0.6444594 | 0.7054118 | 0.2924640 | 0.3199392 |
| COL     |       2016 | 0.8921386 | 0.9250832 | 0.8835096 | 0.8220292 | 0.9513143 | 0.8454097 | 0.9011931 | 0.8354618 | 0.6575797 | 0.7051424 | 0.3204225 | 0.3024885 |
| CYP     |       1999 | 0.9739499 | 0.9372948 | 0.9148450 | 0.6816976 | 0.7931826 | 0.9135153 | 0.9256360 | 0.9088808 | 0.8600392 | 0.8521008 | 0.5591001 | 0.4752177 |
| CYP     |       2009 | 0.8108817 | 0.8374710 | 0.7462834 | 0.7179993 | 0.8050875 | 0.8208809 | 0.8323985 | 0.7935780 | 0.7077326 | 0.7493421 | 0.5858086 | 0.4654212 |
| CZE     |       1999 | 0.9739569 | 0.8229434 | 0.7886964 | 0.7724551 | 0.8161081 | 0.6797863 | 0.7157776 | 0.4272544 | 0.7276036 | 0.6704879 | 0.2812500 | 0.1356668 |
| CZE     |       2009 | 0.9219565 | 0.8167287 | 0.7668348 | 0.9511983 | 0.8348283 | 0.6829533 | 0.6449782 | 0.4582698 | 0.6574417 | 0.5429696 | 0.3476460 | 0.1507297 |
| DEU     |       1999 | 0.9478791 | 0.9020108 | 0.8421201 | 0.6601857 | 0.7166904 | 0.6974015 | 0.5846682 | 0.6880416 | 0.7032168 | 0.6850299 | 0.4328402 | 0.1857394 |
| DNK     |       1999 | 0.9514781 | 0.7860852 | 0.8640553 | 0.5943869 | 0.8232558 | 0.6075949 | 0.4453561 | 0.6311447 | 0.6703801 | 0.5353241 | 0.4418195 | 0.1715353 |
| DNK     |       2009 | 0.9430028 | 0.6881545 | 0.5543884 | 0.7497674 | 0.7629991 | 0.7908210 | 0.6774119 | 0.8826258 | 0.7760296 | 0.4191672 | 0.2552748 | 0.2153099 |
| DNK     |       2016 | 0.9507480 | 0.7132973 | 0.5735269 | 0.8538513 | 0.7291737 | 0.8810720 | 0.7618808 | 0.9210615 | 0.8226510 | 0.4120518 | 0.3116271 | 0.2295413 |
| DNW     |       2016 | 0.9501754 | 0.8787666 | 0.7987288 | 0.6311360 | 0.6713781 | 0.6592022 | 0.7291372 | 0.8335674 | 0.7700908 | 0.5400281 | 0.3494398 | 0.1635220 |
| DOM     |       2009 | 0.9130784 | 0.9065725 | 0.8398374 | 0.9073673 | 0.9252108 | 0.8121994 | 0.9237197 | 0.8738715 | 0.7733192 | 0.6834115 | 0.3217107 | 0.5885624 |
| DOM     |       2016 | 0.9385813 | 0.9194573 | 0.8915420 | 0.9252874 | 0.9514871 | 0.7702590 | 0.9425725 | 0.9162618 | 0.8358671 | 0.7259643 | 0.3893549 | 0.5888889 |
| ENG     |       1999 | 0.9744219 | 0.7206045 | 0.7848620 | 0.9266643 | 0.7634566 | 0.7602983 | 0.4410293 | 0.7089552 | 0.5224383 | 0.5846283 | 0.4202955 | 0.2043366 |
| ENG     |       2009 | 0.9237526 | 0.7638156 | 0.7985258 | 0.9356868 | 0.7884751 | 0.7903564 | 0.6283713 | 0.8008400 | 0.7120364 | 0.5774399 | 0.5063069 | 0.3192011 |
| ESP     |       2009 | 0.9053889 | 0.8541155 | 0.8671587 | 0.7930717 | 0.8802947 | 0.7610159 | 0.6894219 | 0.7867647 | 0.6967188 | 0.7120515 | 0.4192756 | 0.3617087 |
| EST     |       1999 | 0.9319482 | 0.7806552 | 0.8133536 | 0.8272395 | 0.6806877 | 0.7010495 | 0.7104946 | 0.5869969 | 0.6853293 | 0.5882930 | 0.2829728 | 0.1677215 |
| EST     |       2009 | 0.8501292 | 0.8205033 | 0.8635190 | 0.8420664 | 0.7677253 | 0.6685103 | 0.8048871 | 0.7311669 | 0.8290251 | 0.5011078 | 0.2986677 | 0.1566888 |
| EST     |       2016 | 0.8662196 | 0.8328622 | 0.8691655 | 0.7892882 | 0.7802897 | 0.6699577 | 0.7918137 | 0.8085482 | 0.7992237 | 0.5155367 | 0.3526912 | 0.1425044 |
| FIN     |       1999 | 0.9734159 | 0.8299734 | 0.6103501 | 0.9316587 | 0.7419476 | 0.5915649 | 0.6864188 | 0.5932663 | 0.6707544 | 0.3559936 | 0.2367206 | 0.1376949 |
| FIN     |       2009 | 0.9143469 | 0.7342743 | 0.7624885 | 0.9195473 | 0.7983476 | 0.7333333 | 0.6299816 | 0.6524801 | 0.7062424 | 0.4236196 | 0.2511485 | 0.1711574 |
| FIN     |       2016 | 0.9452099 | 0.7930039 | 0.7878691 | 0.9481268 | 0.8213256 | 0.7733376 | 0.7253205 | 0.7473549 | 0.7963555 | 0.4710983 | 0.3532992 | 0.2423951 |
| GRC     |       1999 | 0.9555950 | 0.9181216 | 0.8995172 | 0.8101922 | 0.8879233 | 0.9428741 | 0.8808955 | 0.8212560 | 0.7670901 | 0.8620582 | 0.5933190 | 0.4895961 |
| GRC     |       2009 | 0.8436991 | 0.8476128 | 0.8313174 | 0.6507988 | 0.8686704 | 0.7471600 | 0.8179428 | 0.7176815 | 0.6565558 | 0.7780679 | 0.5677883 | 0.2674494 |
| GTM     |       2009 | 0.9317531 | 0.9363589 | 0.9502734 | 0.8718016 | 0.9664658 | 0.9220444 | 0.9261409 | 0.8995040 | 0.7202194 | 0.6866335 | 0.3546359 | 0.4289075 |
| HKG     |       1999 | 0.9444683 | 0.8014233 | 0.8520499 | 0.9044822 | 0.8251842 | 0.8400262 | 0.6979261 | 0.8071688 | 0.7258212 | 0.6006471 | 0.5395819 | 0.2771790 |
| HKG     |       2009 | 0.9637784 | 0.7845990 | 0.8430398 | 0.9087682 | 0.8839191 | 0.8586648 | 0.7871357 | 0.8931487 | 0.8959886 | 0.6462851 | 0.5688920 | 0.2039076 |
| HKG     |       2016 | 0.9303578 | 0.7728148 | 0.8096339 | 0.8830116 | 0.8521371 | 0.8374327 | 0.7395152 | 0.7960754 | 0.8597843 | 0.6562620 | 0.6138117 | 0.2603846 |
| HRV     |       2016 | 0.9415281 | 0.9071950 | 0.8660063 | 0.9525169 | 0.9269751 | 0.9001035 | 0.8976234 | 0.8642746 | 0.7951245 | 0.7294975 | 0.4784988 | 0.3675591 |
| HUN     |       1999 | 0.9762056 | 0.8925040 | 0.8947704 | 0.8953935 | 0.7654598 | 0.8152967 | 0.8575072 | 0.7377102 | 0.7832857 | 0.6413570 | 0.2091755 | 0.2851625 |
| IDN     |       2009 | 0.9629555 | 0.8574615 | 0.9307272 | 0.9393081 | 0.9359676 | 0.9709911 | 0.9606571 | 0.9474965 | 0.7430598 | 0.8061826 | 0.4824294 | 0.5514795 |
| IRL     |       2009 | 0.9149066 | 0.8344512 | 0.8418477 | 0.9348554 | 0.8532194 | 0.8904027 | 0.7358145 | 0.7928615 | 0.7228476 | 0.6479607 | 0.5289079 | 0.3024954 |
| ITA     |       1999 | 0.9688664 | 0.8677455 | 0.8288845 | 0.8351917 | 0.7930848 | 0.8370650 | 0.6629579 | 0.7607482 | 0.7683057 | 0.7580645 | 0.4909647 | 0.3168678 |
| ITA     |       2009 | 0.9695704 | 0.9062313 | 0.7790105 | 0.8453270 | 0.8896861 | 0.9063995 | 0.9098262 | 0.8574843 | 0.9097699 | 0.7301349 | 0.6036494 | 0.3540670 |
| ITA     |       2016 | 0.9692533 | 0.9157864 | 0.8371614 | 0.8752562 | 0.9024963 | 0.9100467 | 0.9276297 | 0.8278184 | 0.9078717 | 0.7488277 | 0.6400351 | 0.4166423 |
| KOR     |       2009 | 0.9321872 | 0.8376673 | 0.8377965 | 0.9243697 | 0.8792280 | 0.9677358 | 0.7519572 | 0.4255685 | 0.9199771 | 0.9019870 | 0.7585613 | 0.6104318 |
| KOR     |       2016 | 0.9391709 | 0.8357226 | 0.8639007 | 0.9236730 | 0.8980225 | 0.9259977 | 0.8206816 | 0.4418605 | 0.8797984 | 0.8503296 | 0.6396431 | 0.4875969 |
| LIE     |       2009 | 0.8845070 | 0.7138810 | 0.6807910 | 0.6257143 | 0.6524217 | 0.6901408 | 0.7542373 | 0.7806268 | 0.6440678 | 0.4573864 | 0.2991453 | 0.3286119 |
| LTU     |       1999 | 0.9276786 | 0.9030984 | 0.8544957 | 0.7817901 | 0.7951474 | 0.8393598 | 0.8056473 | 0.7598160 | 0.7410955 | 0.7214612 | 0.5653003 | 0.3873285 |
| LTU     |       2009 | 0.9004642 | 0.7648579 | 0.8145078 | 0.7449664 | 0.7713769 | 0.8060356 | 0.8908432 | 0.8222222 | 0.7652062 | 0.7009056 | 0.4276112 | 0.2751227 |
| LTU     |       2016 | 0.9314302 | 0.7544793 | 0.8003908 | 0.7642458 | 0.8087783 | 0.7889321 | 0.8851541 | 0.9075677 | 0.7509066 | 0.6631226 | 0.4679720 | 0.2977716 |
| LUX     |       2009 | 0.8821556 | 0.7499473 | 0.6653302 | 0.6792096 | 0.7476301 | 0.7716618 | 0.7264885 | 0.8067298 | 0.6701593 | 0.6146557 | 0.3764978 | 0.3105407 |
| LVA     |       1999 | 0.9088700 | 0.7932773 | 0.7258065 | 0.8013923 | 0.7608786 | 0.7649919 | 0.7223587 | 0.6655546 | 0.6704684 | 0.6434109 | 0.4361839 | 0.3020426 |
| LVA     |       2009 | 0.8116048 | 0.8422214 | 0.7986009 | 0.7215888 | 0.7937385 | 0.8252107 | 0.7012509 | 0.6595588 | 0.8202494 | 0.6589147 | 0.5486953 | 0.3718704 |
| LVA     |       2016 | 0.8837726 | 0.8236976 | 0.7839821 | 0.7328051 | 0.8401652 | 0.7910872 | 0.8046925 | 0.8168835 | 0.7845374 | 0.5207933 | 0.4704198 | 0.3299460 |
| MEX     |       2009 | 0.8710834 | 0.8632276 | 0.8407513 | 0.8796642 | 0.9030837 | 0.8698566 | 0.8161414 | 0.7844001 | 0.7379235 | 0.6685588 | 0.5155633 | 0.4135220 |
| MEX     |       2016 | 0.8753982 | 0.8852336 | 0.8744151 | 0.8596886 | 0.9166511 | 0.8487941 | 0.8362231 | 0.7963241 | 0.7782552 | 0.7045497 | 0.5881690 | 0.4674501 |
| MLT     |       2009 | 0.9172217 | 0.8250478 | 0.7951173 | 0.8608987 | 0.8273553 | 0.8312796 | 0.7382550 | 0.8277011 | 0.6641185 | 0.6208712 | 0.3741007 | 0.3579952 |
| MLT     |       2016 | 0.9234807 | 0.8486860 | 0.8026024 | 0.8342557 | 0.8338870 | 0.7894160 | 0.7243963 | 0.8370554 | 0.6873278 | 0.6326135 | 0.4276188 | 0.3303005 |
| NLD     |       2009 | 0.7315011 | 0.7231907 | 0.7053524 | 0.7761352 | 0.7186674 | 0.7547368 | 0.4604847 | 0.8048652 | 0.6684238 | 0.3824934 | 0.3909139 | 0.3178947 |
| NLD     |       2016 | 0.8548154 | 0.7204924 | 0.7119506 | 0.8529518 | 0.7043478 | 0.7356446 | 0.5394642 | 0.8547688 | 0.7061800 | 0.3560551 | 0.4002169 | 0.2811143 |
| NOR     |       1999 | 0.9477124 | 0.9029281 | 0.8271208 | 0.7797525 | 0.9056604 | 0.7137059 | 0.4971483 | 0.6473803 | 0.6705220 | 0.6759411 | 0.3618926 | 0.2097235 |
| NOR     |       2009 | 0.8836071 | 0.8673647 | 0.8749563 | 0.8561453 | 0.8966237 | 0.8795014 | 0.7300314 | 0.8598815 | 0.8009709 | 0.6973822 | 0.5019211 | 0.4025748 |
| NOR     |       2016 | 0.9283479 | 0.8106411 | 0.8550629 | 0.8520231 | 0.8560106 | 0.8694226 | 0.7471511 | 0.8714144 | 0.7368940 | 0.6092297 | 0.4791942 | 0.2943309 |
| NZL     |       2009 | 0.8979010 | 0.7352788 | 0.7816332 | 0.9237947 | 0.7808433 | 0.8358325 | 0.7019506 | 0.7878473 | 0.6916883 | 0.5292581 | 0.4427640 | 0.2609261 |
| PER     |       2016 | 0.9235541 | 0.9250100 | 0.8835494 | 0.8211788 | 0.9490815 | 0.9512724 | 0.9401198 | 0.9197138 | 0.7755467 | 0.7080088 | 0.4772546 | 0.5582920 |
| POL     |       1999 | 0.9608669 | 0.8171943 | 0.9069343 | 0.7495350 | 0.7827819 | 0.9138452 | 0.8984185 | 0.8164400 | 0.8605711 | 0.7660550 | 0.5531848 | 0.3867299 |
| POL     |       2009 | 0.8504788 | 0.8506211 | 0.7593798 | 0.6411765 | 0.8170845 | 0.8906733 | 0.9129894 | 0.7638502 | 0.7949985 | 0.6870938 | 0.4342716 | 0.2694926 |
| PRT     |       1999 | 0.9698018 | 0.8832621 | 0.9395952 | 0.8163001 | 0.9213448 | 0.7138728 | 0.7227025 | 0.8279085 | 0.8154331 | 0.7336038 | 0.3985626 | 0.3622246 |
| PRY     |       2009 | 0.9099187 | 0.9140345 | 0.8426047 | 0.8778576 | 0.9595960 | 0.7626360 | 0.9360878 | 0.8495146 | 0.6118485 | 0.6528953 | 0.2667104 | 0.5268088 |
| ROM     |       1999 | 0.9359283 | 0.8858965 | 0.9009585 | 0.8077795 | 0.8786920 | 0.9205298 | 0.8747361 | 0.8478731 | 0.7816609 | 0.6804781 | 0.5632745 | 0.5421779 |
| RUS     |       1999 | 0.9354685 | 0.7842466 | 0.8254581 | 0.8865930 | 0.8015304 | 0.7910519 | 0.7476008 | 0.5004941 | 0.7252274 | 0.8368447 | 0.2684291 | 0.2048314 |
| RUS     |       2009 | 0.8663064 | 0.8267012 | 0.8011310 | 0.8209586 | 0.8772385 | 0.8305640 | 0.8945489 | 0.8487059 | 0.8049412 | 0.6799341 | 0.4535790 | 0.4493299 |
| RUS     |       2016 | 0.9070827 | 0.8081548 | 0.8144159 | 0.8363687 | 0.8868159 | 0.7604296 | 0.9319972 | 0.8816952 | 0.8068338 | 0.6356354 | 0.4916263 | 0.4973779 |
| SVK     |       1999 | 0.9848573 | 0.9313090 | 0.8721068 | 0.9680726 | 0.8593978 | 0.9074993 | 0.7254038 | 0.6987699 | 0.7960681 | 0.7226455 | 0.4955726 | 0.2097310 |
| SVK     |       2009 | 0.9114566 | 0.8438031 | 0.7548321 | 0.6990521 | 0.8443540 | 0.7813450 | 0.6682529 | 0.5335366 | 0.6926995 | 0.5355080 | 0.3421589 | 0.1357481 |
| SVN     |       1999 | 0.9478925 | 0.7758204 | 0.7837370 | 0.6537936 | 0.7309524 | 0.7981439 | 0.7200000 | 0.6871315 | 0.6249156 | 0.5886681 | 0.3781919 | 0.2943562 |
| SVN     |       2009 | 0.9071452 | 0.8073486 | 0.7491738 | 0.7134850 | 0.8242664 | 0.7455621 | 0.6601717 | 0.7246042 | 0.5702044 | 0.5556657 | 0.3439974 | 0.2537019 |
| SVN     |       2016 | 0.9199858 | 0.8463180 | 0.7949358 | 0.8526166 | 0.8557178 | 0.7963945 | 0.6846815 | 0.7608541 | 0.6567164 | 0.5693405 | 0.4024347 | 0.2613475 |
| SWE     |       1999 | 0.9596532 | 0.7816901 | 0.8206107 | 0.7841727 | 0.8069993 | 0.7922428 | 0.6244870 | 0.6427815 | 0.5616580 | 0.6883636 | 0.3889479 | 0.2114187 |
| SWE     |       2009 | 0.8792085 | 0.7608052 | 0.7822104 | 0.8229566 | 0.8079646 | 0.7647059 | 0.4511943 | 0.8114827 | 0.6202830 | 0.5948429 | 0.3406333 | 0.2272995 |
| SWE     |       2016 | 0.9450304 | 0.8214628 | 0.8126598 | 0.8442179 | 0.8383061 | 0.8633207 | 0.4833440 | 0.8774589 | 0.7314195 | 0.6527023 | 0.4766026 | 0.2199488 |
| THA     |       2009 | 0.9486445 | 0.9261103 | 0.9328358 | 0.5644946 | 0.9493017 | 0.9727255 | 0.9295450 | 0.8251147 | 0.9111239 | 0.6480490 | 0.7105666 | 0.6748948 |
| TWN     |       2009 | 0.9634170 | 0.9047341 | 0.8897116 | 0.9451041 | 0.8872005 | 0.7482967 | 0.7900585 | 0.7191274 | 0.8743914 | 0.6419103 | 0.5105263 | 0.1662773 |
| TWN     |       2016 | 0.9802131 | 0.8734145 | 0.8688816 | 0.9436834 | 0.8929206 | 0.7793558 | 0.8180894 | 0.7716316 | 0.8985544 | 0.6809754 | 0.5946701 | 0.2106599 |
| USA     |       1999 | 0.9520826 | 0.8316679 | 0.8808989 | 0.9123134 | 0.8316190 | 0.8303571 | 0.7301168 | 0.7950170 | 0.6398014 | 0.7223127 | 0.5806709 | 0.4737461 |

Count and percentage of missing values for each indicator by country and
year.

``` r
missing <- tbl %>% 
  group_by(COUNTRY, ICCS_year) %>% 
  summarize_at(cit_norm_indicators,
               funs(paste0(sum(is.na(.)), " (", (round(sum(is.na(.)) / length(.) * 100, 2)), "%)")))

missing %>% 
  knitr::kable()
```

| COUNTRY | ICCS\_year | obey         | rights       | local        | work         | envir        | vote        | history      | respect      | news         | protest      | discuss      | party        |
| :------ | ---------: | :----------- | :----------- | :----------- | :----------- | :----------- | :---------- | :----------- | :----------- | :----------- | :----------- | :----------- | :----------- |
| AUS     |       1999 | 257 (7.72%)  | 369 (11.08%) | 304 (9.13%)  | 282 (8.47%)  | 350 (10.51%) | 264 (7.93%) | 312 (9.37%)  | 331 (9.94%)  | 335 (10.06%) | 507 (15.22%) | 419 (12.58%) | 376 (11.29%) |
| AUT     |       2009 | 64 (1.89%)   | 71 (2.1%)    | 73 (2.16%)   | 73 (2.16%)   | 69 (2.04%)   | 57 (1.68%)  | 79 (2.33%)   | 74 (2.19%)   | 69 (2.04%)   | 74 (2.19%)   | 77 (2.27%)   | 73 (2.16%)   |
| BFL     |       2009 | 24 (0.81%)   | 29 (0.98%)   | 28 (0.94%)   | 22 (0.74%)   | 24 (0.81%)   | 17 (0.57%)  | 32 (1.08%)   | 20 (0.67%)   | 16 (0.54%)   | 23 (0.77%)   | 24 (0.81%)   | 17 (0.57%)   |
| BFL     |       2016 | 39 (1.33%)   | 31 (1.06%)   | 35 (1.19%)   | 31 (1.06%)   | 37 (1.26%)   | 20 (0.68%)  | 37 (1.26%)   | 25 (0.85%)   | 27 (0.92%)   | 42 (1.43%)   | 40 (1.36%)   | 18 (0.61%)   |
| BFR     |       1999 | 100 (4.82%)  | 130 (6.26%)  | 148 (7.13%)  | 131 (6.31%)  | 137 (6.6%)   | 88 (4.24%)  | 106 (5.11%)  | 197 (9.49%)  | 115 (5.54%)  | 227 (10.93%) | 146 (7.03%)  | 164 (7.9%)   |
| BGR     |       1999 | 189 (6.55%)  | 298 (10.33%) | 378 (13.11%) | 249 (8.63%)  | 321 (11.13%) | 283 (9.81%) | 307 (10.64%) | 332 (11.51%) | 287 (9.95%)  | 482 (16.71%) | 481 (16.68%) | 490 (16.99%) |
| BGR     |       2009 | 111 (3.41%)  | 126 (3.87%)  | 121 (3.72%)  | 116 (3.56%)  | 123 (3.78%)  | 87 (2.67%)  | 118 (3.62%)  | 118 (3.62%)  | 112 (3.44%)  | 134 (4.11%)  | 137 (4.21%)  | 109 (3.35%)  |
| BGR     |       2016 | 107 (3.61%)  | 81 (2.73%)   | 72 (2.43%)   | 84 (2.83%)   | 84 (2.83%)   | 60 (2.02%)  | 89 (3%)      | 81 (2.73%)   | 68 (2.29%)   | 79 (2.66%)   | 103 (3.47%)  | 74 (2.49%)   |
| CHE     |       1999 | 60 (1.93%)   | 130 (4.19%)  | 140 (4.51%)  | 161 (5.19%)  | 134 (4.32%)  | 90 (2.9%)   | 117 (3.77%)  | 242 (7.8%)   | 73 (2.35%)   | 269 (8.67%)  | 165 (5.32%)  | 189 (6.09%)  |
| CHE     |       2009 | 39 (1.33%)   | 49 (1.68%)   | 55 (1.88%)   | 36 (1.23%)   | 39 (1.33%)   | 25 (0.85%)  | 44 (1.5%)    | 37 (1.27%)   | 29 (0.99%)   | 48 (1.64%)   | 39 (1.33%)   | 36 (1.23%)   |
| CHL     |       1999 | 148 (2.6%)   | 506 (8.9%)   | 201 (3.53%)  | 248 (4.36%)  | 181 (3.18%)  | 154 (2.71%) | 296 (5.2%)   | 281 (4.94%)  | 162 (2.85%)  | 576 (10.13%) | 561 (9.86%)  | 505 (8.88%)  |
| CHL     |       2009 | 66 (1.27%)   | 78 (1.5%)    | 67 (1.29%)   | 102 (1.96%)  | 80 (1.54%)   | 47 (0.91%)  | 89 (1.71%)   | 62 (1.19%)   | 75 (1.44%)   | 73 (1.41%)   | 70 (1.35%)   | 69 (1.33%)   |
| CHL     |       2016 | 95 (1.87%)   | 103 (2.03%)  | 91 (1.79%)   | 119 (2.34%)  | 95 (1.87%)   | 78 (1.54%)  | 101 (1.99%)  | 82 (1.61%)   | 80 (1.57%)   | 95 (1.87%)   | 97 (1.91%)   | 86 (1.69%)   |
| COL     |       1999 | 131 (2.66%)  | 278 (5.64%)  | 182 (3.69%)  | 385 (7.82%)  | 151 (3.07%)  | 242 (4.91%) | 392 (7.96%)  | 265 (5.38%)  | 462 (9.38%)  | 369 (7.49%)  | 549 (11.14%) | 583 (11.84%) |
| COL     |       2009 | 307 (4.95%)  | 280 (4.51%)  | 290 (4.67%)  | 324 (5.22%)  | 308 (4.96%)  | 204 (3.29%) | 340 (5.48%)  | 270 (4.35%)  | 275 (4.43%)  | 328 (5.29%)  | 299 (4.82%)  | 281 (4.53%)  |
| COL     |       2016 | 241 (4.3%)   | 203 (3.62%)  | 218 (3.89%)  | 198 (3.53%)  | 207 (3.69%)  | 130 (2.32%) | 245 (4.37%)  | 206 (3.67%)  | 180 (3.21%)  | 203 (3.62%)  | 213 (3.8%)   | 184 (3.28%)  |
| CYP     |       1999 | 35 (1.13%)   | 60 (1.93%)   | 41 (1.32%)   | 90 (2.9%)    | 55 (1.77%)   | 65 (2.09%)  | 40 (1.29%)   | 77 (2.48%)   | 48 (1.55%)   | 131 (4.22%)  | 128 (4.12%)  | 120 (3.86%)  |
| CYP     |       2009 | 143 (4.48%)  | 173 (5.42%)  | 167 (5.23%)  | 155 (4.85%)  | 167 (5.23%)  | 129 (4.04%) | 163 (5.1%)   | 142 (4.45%)  | 142 (4.45%)  | 154 (4.82%)  | 164 (5.13%)  | 143 (4.48%)  |
| CZE     |       1999 | 36 (1%)      | 94 (2.61%)   | 86 (2.38%)   | 100 (2.77%)  | 56 (1.55%)   | 50 (1.39%)  | 64 (1.77%)   | 136 (3.77%)  | 35 (0.97%)   | 205 (5.68%)  | 119 (3.3%)   | 150 (4.16%)  |
| CZE     |       2009 | 30 (0.65%)   | 63 (1.36%)   | 71 (1.53%)   | 40 (0.86%)   | 59 (1.27%)   | 25 (0.54%)  | 50 (1.08%)   | 41 (0.89%)   | 41 (0.89%)   | 57 (1.23%)   | 42 (0.91%)   | 39 (0.84%)   |
| DEU     |       1999 | 93 (2.51%)   | 169 (4.57%)  | 153 (4.14%)  | 254 (6.86%)  | 195 (5.27%)  | 121 (3.27%) | 204 (5.51%)  | 238 (6.43%)  | 125 (3.38%)  | 360 (9.73%)  | 253 (6.84%)  | 292 (7.89%)  |
| DNK     |       1999 | 96 (2.99%)   | 319 (9.94%)  | 170 (5.3%)   | 215 (6.7%)   | 198 (6.17%)  | 127 (3.96%) | 161 (5.02%)  | 299 (9.32%)  | 156 (4.86%)  | 462 (14.4%)  | 372 (11.6%)  | 264 (8.23%)  |
| DNK     |       2009 | 192 (4.26%)  | 211 (4.68%)  | 224 (4.97%)  | 208 (4.61%)  | 200 (4.44%)  | 172 (3.82%) | 196 (4.35%)  | 197 (4.37%)  | 186 (4.13%)  | 209 (4.64%)  | 195 (4.33%)  | 184 (4.08%)  |
| DNK     |       2016 | 305 (4.88%)  | 328 (5.24%)  | 331 (5.29%)  | 308 (4.92%)  | 324 (5.18%)  | 284 (4.54%) | 299 (4.78%)  | 300 (4.8%)   | 294 (4.7%)   | 313 (5%)     | 311 (4.97%)  | 303 (4.84%)  |
| DNW     |       2016 | 26 (1.79%)   | 24 (1.65%)   | 35 (2.41%)   | 25 (1.72%)   | 36 (2.48%)   | 22 (1.52%)  | 37 (2.55%)   | 27 (1.86%)   | 20 (1.38%)   | 27 (1.86%)   | 23 (1.59%)   | 20 (1.38%)   |
| DOM     |       2009 | 850 (18.52%) | 907 (19.76%) | 899 (19.59%) | 897 (19.55%) | 912 (19.87%) | 638 (13.9%) | 879 (19.15%) | 823 (17.93%) | 826 (18%)    | 966 (21.05%) | 918 (20%)    | 812 (17.69%) |
| DOM     |       2016 | 469 (11.91%) | 473 (12.01%) | 461 (11.71%) | 457 (11.61%) | 474 (12.04%) | 346 (8.79%) | 524 (13.31%) | 438 (11.13%) | 385 (9.78%)  | 463 (11.76%) | 480 (12.19%) | 427 (10.85%) |
| ENG     |       1999 | 189 (6.21%)  | 330 (10.84%) | 361 (11.86%) | 234 (7.69%)  | 312 (10.25%) | 227 (7.46%) | 245 (8.05%)  | 363 (11.93%) | 369 (12.13%) | 662 (21.75%) | 471 (15.48%) | 322 (10.58%) |
| ENG     |       2009 | 70 (2.4%)    | 75 (2.57%)   | 67 (2.3%)    | 55 (1.89%)   | 70 (2.4%)    | 54 (1.85%)  | 61 (2.09%)   | 59 (2.02%)   | 58 (1.99%)   | 88 (3.02%)   | 62 (2.13%)   | 62 (2.13%)   |
| ESP     |       2009 | 43 (1.3%)    | 53 (1.6%)    | 57 (1.72%)   | 47 (1.42%)   | 51 (1.54%)   | 41 (1.24%)  | 57 (1.72%)   | 45 (1.36%)   | 48 (1.45%)   | 48 (1.45%)   | 51 (1.54%)   | 55 (1.66%)   |
| EST     |       1999 | 113 (3.29%)  | 229 (6.67%)  | 139 (4.05%)  | 152 (4.43%)  | 177 (5.15%)  | 99 (2.88%)  | 118 (3.44%)  | 204 (5.94%)  | 94 (2.74%)   | 376 (10.95%) | 568 (16.54%) | 274 (7.98%)  |
| EST     |       2009 | 34 (1.24%)   | 41 (1.49%)   | 32 (1.17%)   | 33 (1.2%)    | 35 (1.28%)   | 31 (1.13%)  | 42 (1.53%)   | 35 (1.28%)   | 35 (1.28%)   | 35 (1.28%)   | 41 (1.49%)   | 37 (1.35%)   |
| EST     |       2016 | 24 (0.84%)   | 27 (0.95%)   | 29 (1.02%)   | 19 (0.67%)   | 26 (0.91%)   | 21 (0.74%)  | 23 (0.81%)   | 26 (0.91%)   | 23 (0.81%)   | 25 (0.88%)   | 33 (1.16%)   | 22 (0.77%)   |
| FIN     |       1999 | 36 (1.29%)   | 153 (5.5%)   | 154 (5.54%)  | 75 (2.7%)    | 112 (4.03%)  | 79 (2.84%)  | 65 (2.34%)   | 198 (7.12%)  | 91 (3.27%)   | 296 (10.64%) | 184 (6.61%)  | 153 (5.5%)   |
| FIN     |       2009 | 38 (1.15%)   | 48 (1.45%)   | 44 (1.33%)   | 38 (1.15%)   | 39 (1.18%)   | 37 (1.12%)  | 45 (1.36%)   | 41 (1.24%)   | 39 (1.18%)   | 47 (1.42%)   | 42 (1.27%)   | 41 (1.24%)   |
| FIN     |       2016 | 52 (1.64%)   | 57 (1.8%)    | 57 (1.8%)    | 50 (1.58%)   | 50 (1.58%)   | 45 (1.42%)  | 53 (1.67%)   | 54 (1.7%)    | 45 (1.42%)   | 59 (1.86%)   | 51 (1.61%)   | 50 (1.58%)   |
| GRC     |       1999 | 82 (2.37%)   | 138 (3.99%)  | 146 (4.22%)  | 183 (5.29%)  | 123 (3.55%)  | 99 (2.86%)  | 110 (3.18%)  | 148 (4.28%)  | 154 (4.45%)  | 263 (7.6%)   | 197 (5.69%)  | 192 (5.55%)  |
| GRC     |       2009 | 82 (2.6%)    | 95 (3.01%)   | 94 (2.98%)   | 86 (2.73%)   | 92 (2.92%)   | 72 (2.28%)  | 110 (3.49%)  | 82 (2.6%)    | 87 (2.76%)   | 89 (2.82%)   | 92 (2.92%)   | 87 (2.76%)   |
| GTM     |       2009 | 163 (4.07%)  | 168 (4.2%)   | 161 (4.02%)  | 211 (5.27%)  | 185 (4.62%)  | 128 (3.2%)  | 211 (5.27%)  | 171 (4.27%)  | 174 (4.35%)  | 179 (4.47%)  | 184 (4.6%)   | 176 (4.4%)   |
| HKG     |       1999 | 333 (6.66%)  | 641 (12.83%) | 509 (10.19%) | 401 (8.02%)  | 518 (10.37%) | 415 (8.3%)  | 561 (11.23%) | 589 (11.79%) | 613 (12.27%) | 670 (13.41%) | 740 (14.81%) | 729 (14.59%) |
| HKG     |       2009 | 86 (2.96%)   | 84 (2.89%)   | 86 (2.96%)   | 85 (2.93%)   | 85 (2.93%)   | 86 (2.96%)  | 88 (3.03%)   | 85 (2.93%)   | 85 (2.93%)   | 89 (3.07%)   | 86 (2.96%)   | 87 (3%)      |
| HKG     |       2016 | 54 (2.04%)   | 56 (2.11%)   | 58 (2.19%)   | 63 (2.37%)   | 56 (2.11%)   | 51 (1.92%)  | 54 (2.04%)   | 54 (2.04%)   | 57 (2.15%)   | 58 (2.19%)   | 61 (2.3%)    | 53 (2%)      |
| HRV     |       2016 | 48 (1.23%)   | 60 (1.54%)   | 60 (1.54%)   | 42 (1.08%)   | 48 (1.23%)   | 32 (0.82%)  | 67 (1.72%)   | 50 (1.28%)   | 40 (1.03%)   | 55 (1.41%)   | 59 (1.51%)   | 49 (1.26%)   |
| HUN     |       1999 | 15 (0.47%)   | 32 (1.01%)   | 31 (0.98%)   | 41 (1.29%)   | 46 (1.45%)   | 16 (0.51%)  | 30 (0.95%)   | 75 (2.37%)   | 20 (0.63%)   | 72 (2.27%)   | 50 (1.58%)   | 60 (1.89%)   |
| IDN     |       2009 | 128 (2.53%)  | 136 (2.68%)  | 131 (2.58%)  | 125 (2.47%)  | 133 (2.62%)  | 104 (2.05%) | 137 (2.7%)   | 135 (2.66%)  | 133 (2.62%)  | 151 (2.98%)  | 145 (2.86%)  | 134 (2.64%)  |
| IRL     |       2009 | 88 (2.62%)   | 75 (2.24%)   | 86 (2.56%)   | 70 (2.09%)   | 78 (2.32%)   | 52 (1.55%)  | 77 (2.3%)    | 77 (2.3%)    | 68 (2.03%)   | 94 (2.8%)    | 86 (2.56%)   | 69 (2.06%)   |
| ITA     |       1999 | 50 (1.31%)   | 224 (5.88%)  | 249 (6.54%)  | 131 (3.44%)  | 106 (2.78%)  | 101 (2.65%) | 123 (3.23%)  | 226 (5.93%)  | 66 (1.73%)   | 212 (5.57%)  | 211 (5.54%)  | 245 (6.43%)  |
| ITA     |       2009 | 14 (0.42%)   | 28 (0.83%)   | 31 (0.92%)   | 17 (0.51%)   | 21 (0.62%)   | 22 (0.65%)  | 28 (0.83%)   | 19 (0.56%)   | 19 (0.56%)   | 31 (0.92%)   | 23 (0.68%)   | 22 (0.65%)   |
| ITA     |       2016 | 35 (1.01%)   | 42 (1.22%)   | 54 (1.57%)   | 35 (1.01%)   | 45 (1.3%)    | 26 (0.75%)  | 37 (1.07%)   | 35 (1.01%)   | 20 (0.58%)   | 38 (1.1%)    | 33 (0.96%)   | 37 (1.07%)   |
| KOR     |       2009 | 19 (0.36%)   | 24 (0.46%)   | 26 (0.49%)   | 18 (0.34%)   | 21 (0.4%)    | 16 (0.3%)   | 17 (0.32%)   | 21 (0.4%)    | 18 (0.34%)   | 20 (0.38%)   | 27 (0.51%)   | 20 (0.38%)   |
| KOR     |       2016 | 20 (0.77%)   | 20 (0.77%)   | 22 (0.85%)   | 20 (0.77%)   | 22 (0.85%)   | 20 (0.77%)  | 19 (0.73%)   | 21 (0.81%)   | 22 (0.85%)   | 22 (0.85%)   | 23 (0.88%)   | 21 (0.81%)   |
| LIE     |       2009 | 2 (0.56%)    | 4 (1.12%)    | 3 (0.84%)    | 7 (1.96%)    | 6 (1.68%)    | 2 (0.56%)   | 3 (0.84%)    | 6 (1.68%)    | 3 (0.84%)    | 5 (1.4%)     | 6 (1.68%)    | 4 (1.12%)    |
| LTU     |       1999 | 134 (3.84%)  | 202 (5.78%)  | 202 (5.78%)  | 254 (7.27%)  | 238 (6.81%)  | 120 (3.43%) | 165 (4.72%)  | 234 (6.7%)   | 153 (4.38%)  | 647 (18.52%) | 347 (9.93%)  | 432 (12.36%) |
| LTU     |       2009 | 24 (0.62%)   | 32 (0.82%)   | 42 (1.08%)   | 28 (0.72%)   | 31 (0.79%)   | 25 (0.64%)  | 36 (0.92%)   | 32 (0.82%)   | 22 (0.56%)   | 37 (0.95%)   | 34 (0.87%)   | 31 (0.79%)   |
| LTU     |       2016 | 58 (1.6%)    | 59 (1.62%)   | 49 (1.35%)   | 51 (1.4%)    | 54 (1.49%)   | 35 (0.96%)  | 61 (1.68%)   | 50 (1.38%)   | 46 (1.27%)   | 57 (1.57%)   | 56 (1.54%)   | 41 (1.13%)   |
| LUX     |       2009 | 83 (1.71%)   | 109 (2.25%)  | 113 (2.33%)  | 95 (1.96%)   | 105 (2.16%)  | 74 (1.53%)  | 99 (2.04%)   | 97 (2%)      | 80 (1.65%)   | 103 (2.12%)  | 95 (1.96%)   | 99 (2.04%)   |
| LVA     |       1999 | 103 (4%)     | 192 (7.47%)  | 154 (5.99%)  | 130 (5.05%)  | 159 (6.18%)  | 104 (4.04%) | 130 (5.05%)  | 174 (6.77%)  | 117 (4.55%)  | 250 (9.72%)  | 245 (9.53%)  | 271 (10.54%) |
| LVA     |       2009 | 38 (1.38%)   | 42 (1.52%)   | 45 (1.63%)   | 42 (1.52%)   | 46 (1.67%)   | 32 (1.16%)  | 43 (1.56%)   | 41 (1.48%)   | 35 (1.27%)   | 52 (1.88%)   | 40 (1.45%)   | 45 (1.63%)   |
| LVA     |       2016 | 75 (2.33%)   | 76 (2.36%)   | 90 (2.79%)   | 69 (2.14%)   | 77 (2.39%)   | 60 (1.86%)  | 70 (2.17%)   | 73 (2.26%)   | 68 (2.11%)   | 98 (3.04%)   | 80 (2.48%)   | 75 (2.33%)   |
| MEX     |       2009 | 161 (2.45%)  | 237 (3.6%)   | 240 (3.65%)  | 144 (2.19%)  | 220 (3.35%)  | 160 (2.43%) | 294 (4.47%)  | 217 (3.3%)   | 200 (3.04%)  | 234 (3.56%)  | 247 (3.76%)  | 216 (3.28%)  |
| MEX     |       2016 | 189 (3.42%)  | 176 (3.18%)  | 183 (3.31%)  | 195 (3.53%)  | 187 (3.38%)  | 136 (2.46%) | 220 (3.98%)  | 194 (3.51%)  | 173 (3.13%)  | 185 (3.35%)  | 201 (3.64%)  | 165 (2.99%)  |
| MLT     |       2009 | 41 (1.91%)   | 51 (2.38%)   | 54 (2.52%)   | 51 (2.38%)   | 52 (2.43%)   | 33 (1.54%)  | 57 (2.66%)   | 42 (1.96%)   | 50 (2.33%)   | 54 (2.52%)   | 58 (2.71%)   | 48 (2.24%)   |
| MLT     |       2016 | 144 (3.83%)  | 149 (3.96%)  | 152 (4.04%)  | 150 (3.99%)  | 152 (4.04%)  | 117 (3.11%) | 161 (4.28%)  | 137 (3.64%)  | 134 (3.56%)  | 152 (4.04%)  | 165 (4.38%)  | 137 (3.64%)  |
| NLD     |       2009 | 72 (3.67%)   | 71 (3.62%)   | 77 (3.92%)   | 70 (3.56%)   | 73 (3.72%)   | 64 (3.26%)  | 66 (3.36%)   | 73 (3.72%)   | 67 (3.41%)   | 79 (4.02%)   | 71 (3.62%)   | 64 (3.26%)   |
| NLD     |       2016 | 50 (1.78%)   | 50 (1.78%)   | 59 (2.1%)    | 51 (1.81%)   | 52 (1.85%)   | 43 (1.53%)  | 50 (1.78%)   | 44 (1.56%)   | 45 (1.6%)    | 54 (1.92%)   | 46 (1.64%)   | 48 (1.71%)   |
| NOR     |       1999 | 108 (3.25%)  | 179 (5.39%)  | 209 (6.29%)  | 170 (5.12%)  | 141 (4.25%)  | 118 (3.55%) | 165 (4.97%)  | 210 (6.32%)  | 122 (3.67%)  | 266 (8.01%)  | 193 (5.81%)  | 174 (5.24%)  |
| NOR     |       2009 | 152 (5.04%)  | 148 (4.91%)  | 150 (4.98%)  | 149 (4.95%)  | 140 (4.65%)  | 125 (4.15%) | 146 (4.85%)  | 144 (4.78%)  | 129 (4.28%)  | 148 (4.91%)  | 150 (4.98%)  | 139 (4.61%)  |
| NOR     |       2016 | 200 (3.19%)  | 219 (3.49%)  | 227 (3.62%)  | 216 (3.44%)  | 215 (3.43%)  | 175 (2.79%) | 216 (3.44%)  | 205 (3.27%)  | 186 (2.97%)  | 247 (3.94%)  | 215 (3.43%)  | 203 (3.24%)  |
| NZL     |       2009 | 120 (3.02%)  | 141 (3.54%)  | 146 (3.67%)  | 121 (3.04%)  | 137 (3.44%)  | 111 (2.79%) | 134 (3.37%)  | 128 (3.22%)  | 129 (3.24%)  | 151 (3.79%)  | 144 (3.62%)  | 135 (3.39%)  |
| PER     |       2016 | 169 (3.27%)  | 152 (2.94%)  | 151 (2.92%)  | 161 (3.12%)  | 158 (3.06%)  | 97 (1.88%)  | 156 (3.02%)  | 134 (2.59%)  | 136 (2.63%)  | 159 (3.08%)  | 154 (2.98%)  | 131 (2.54%)  |
| POL     |       1999 | 54 (1.6%)    | 154 (4.56%)  | 88 (2.61%)   | 150 (4.44%)  | 112 (3.32%)  | 68 (2.01%)  | 88 (2.61%)   | 140 (4.15%)  | 84 (2.49%)   | 324 (9.6%)   | 189 (5.6%)   | 211 (6.25%)  |
| POL     |       2009 | 12 (0.37%)   | 29 (0.89%)   | 24 (0.74%)   | 19 (0.58%)   | 18 (0.55%)   | 11 (0.34%)  | 31 (0.95%)   | 18 (0.55%)   | 10 (0.31%)   | 18 (0.55%)   | 16 (0.49%)   | 17 (0.52%)   |
| PRT     |       1999 | 82 (2.51%)   | 220 (6.75%)  | 99 (3.04%)   | 169 (5.18%)  | 108 (3.31%)  | 147 (4.51%) | 138 (4.23%)  | 158 (4.85%)  | 86 (2.64%)   | 303 (9.29%)  | 200 (6.13%)  | 492 (15.09%) |
| PRY     |       2009 | 324 (9.53%)  | 328 (9.65%)  | 343 (10.09%) | 337 (9.91%)  | 330 (9.71%)  | 273 (8.03%) | 301 (8.86%)  | 309 (9.09%)  | 310 (9.12%)  | 325 (9.56%)  | 347 (10.21%) | 303 (8.91%)  |
| ROM     |       1999 | 90 (3.01%)   | 171 (5.71%)  | 176 (5.88%)  | 345 (11.53%) | 149 (4.98%)  | 124 (4.14%) | 151 (5.05%)  | 219 (7.32%)  | 103 (3.44%)  | 483 (16.14%) | 330 (11.03%) | 385 (12.86%) |
| RUS     |       1999 | 37 (1.74%)   | 85 (3.99%)   | 55 (2.58%)   | 48 (2.25%)   | 38 (1.78%)   | 28 (1.32%)  | 45 (2.11%)   | 105 (4.93%)  | 40 (1.88%)   | 88 (4.13%)   | 162 (7.61%)  | 142 (6.67%)  |
| RUS     |       2009 | 39 (0.91%)   | 48 (1.12%)   | 51 (1.19%)   | 39 (0.91%)   | 51 (1.19%)   | 22 (0.51%)  | 94 (2.19%)   | 45 (1.05%)   | 45 (1.05%)   | 49 (1.14%)   | 62 (1.44%)   | 42 (0.98%)   |
| RUS     |       2016 | 46 (0.63%)   | 54 (0.74%)   | 47 (0.64%)   | 41 (0.56%)   | 53 (0.73%)   | 26 (0.36%)  | 54 (0.74%)   | 45 (0.62%)   | 31 (0.43%)   | 49 (0.67%)   | 64 (0.88%)   | 43 (0.59%)   |
| SVK     |       1999 | 29 (0.84%)   | 71 (2.05%)   | 93 (2.69%)   | 49 (1.41%)   | 42 (1.21%)   | 36 (1.04%)  | 58 (1.67%)   | 130 (3.75%)  | 55 (1.59%)   | 182 (5.26%)  | 75 (2.17%)   | 154 (4.45%)  |
| SVK     |       2009 | 11 (0.37%)   | 25 (0.84%)   | 21 (0.71%)   | 16 (0.54%)   | 21 (0.71%)   | 11 (0.37%)  | 28 (0.94%)   | 18 (0.61%)   | 25 (0.84%)   | 27 (0.91%)   | 24 (0.81%)   | 16 (0.54%)   |
| SVN     |       1999 | 55 (1.79%)   | 173 (5.64%)  | 178 (5.8%)   | 142 (4.63%)  | 128 (4.17%)  | 51 (1.66%)  | 118 (3.85%)  | 185 (6.03%)  | 106 (3.46%)  | 350 (11.41%) | 170 (5.54%)  | 109 (3.55%)  |
| SVN     |       2009 | 33 (1.07%)   | 49 (1.6%)    | 44 (1.43%)   | 37 (1.21%)   | 37 (1.21%)   | 28 (0.91%)  | 42 (1.37%)   | 38 (1.24%)   | 36 (1.17%)   | 43 (1.4%)    | 38 (1.24%)   | 31 (1.01%)   |
| SVN     |       2016 | 32 (1.13%)   | 33 (1.16%)   | 40 (1.41%)   | 35 (1.23%)   | 37 (1.3%)    | 15 (0.53%)  | 50 (1.76%)   | 34 (1.2%)    | 30 (1.05%)   | 39 (1.37%)   | 51 (1.79%)   | 24 (0.84%)   |
| SWE     |       1999 | 74 (2.41%)   | 233 (7.58%)  | 191 (6.22%)  | 154 (5.01%)  | 187 (6.09%)  | 108 (3.51%) | 149 (4.85%)  | 240 (7.81%)  | 178 (5.79%)  | 323 (10.51%) | 250 (8.14%)  | 183 (5.96%)  |
| SWE     |       2009 | 78 (2.25%)   | 86 (2.48%)   | 80 (2.31%)   | 75 (2.17%)   | 74 (2.14%)   | 64 (1.85%)  | 73 (2.11%)   | 85 (2.45%)   | 72 (2.08%)   | 90 (2.6%)    | 85 (2.45%)   | 72 (2.08%)   |
| SWE     |       2016 | 135 (4.14%)  | 133 (4.07%)  | 136 (4.17%)  | 125 (3.83%)  | 147 (4.5%)   | 96 (2.94%)  | 142 (4.35%)  | 163 (4.99%)  | 129 (3.95%)  | 137 (4.2%)   | 144 (4.41%)  | 136 (4.17%)  |
| THA     |       2009 | 25 (0.48%)   | 39 (0.74%)   | 37 (0.7%)    | 30 (0.57%)   | 36 (0.68%)   | 20 (0.38%)  | 54 (1.03%)   | 31 (0.59%)   | 31 (0.59%)   | 35 (0.67%)   | 39 (0.74%)   | 37 (0.7%)    |
| TWN     |       2009 | 28 (0.54%)   | 34 (0.66%)   | 35 (0.68%)   | 30 (0.58%)   | 34 (0.66%)   | 30 (0.58%)  | 37 (0.72%)   | 33 (0.64%)   | 32 (0.62%)   | 37 (0.72%)   | 37 (0.72%)   | 31 (0.6%)    |
| TWN     |       2016 | 11 (0.28%)   | 11 (0.28%)   | 10 (0.25%)   | 11 (0.28%)   | 12 (0.3%)    | 10 (0.25%)  | 17 (0.43%)   | 12 (0.3%)    | 10 (0.25%)   | 16 (0.4%)    | 13 (0.33%)   | 13 (0.33%)   |
| USA     |       1999 | 98 (3.49%)   | 209 (7.44%)  | 141 (5.02%)  | 131 (4.66%)  | 186 (6.62%)  | 123 (4.38%) | 158 (5.62%)  | 162 (5.76%)  | 193 (6.87%)  | 355 (12.63%) | 307 (10.92%) | 259 (9.21%)  |

Write tables to
excel.

``` r
write_xlsx(list(means = means, missing = missing), "output/citizenship-norm-indicator-tables.xlsx")
```

## Figures:

Means for twelve citizenship norm indicators for only the 14 countries
that are included in all three waves of the survey.

``` r
all_wave_countries <- tbl %>%
  count(COUNTRY, ICCS_year) %>% 
  count(COUNTRY) %>%
  filter(nn == 3) %>% 
  pull(COUNTRY)

plot_tbl <- tbl %>% 
  filter(COUNTRY %in% all_wave_countries) %>% 
  group_by(ICCS_year) %>% 
  summarize_at(cit_norm_indicators, funs(mean(., na.rm = TRUE)))

plot_tbl %>% 
  knitr::kable()
```

| ICCS\_year |      obey |    rights |     local |      work |     envir |      vote |   history |   respect |      news |   protest |   discuss |     party |
| ---------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: |
|       1999 | 0.9452247 | 0.8342748 | 0.8227005 | 0.8128673 | 0.8184639 | 0.7851758 | 0.7135590 | 0.7160464 | 0.6967499 | 0.6548606 | 0.4185249 | 0.3091457 |
|       2009 | 0.8934649 | 0.8283053 | 0.8085325 | 0.8148389 | 0.8519541 | 0.8056155 | 0.7781203 | 0.8042819 | 0.7393888 | 0.6256544 | 0.3823448 | 0.2841477 |
|       2016 | 0.9140386 | 0.8223837 | 0.8041600 | 0.8356167 | 0.8508127 | 0.8138835 | 0.8068230 | 0.8360684 | 0.7560593 | 0.6108746 | 0.4350790 | 0.3075041 |

``` r
# line plot with year on x-axis, lines colored by indicator type
plot_tbl %>% 
  gather(Indicator, value, -ICCS_year) %>% 
  ggplot(aes(x = ICCS_year, y = value, group = Indicator, colour = Indicator)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean", title = "Mean Citizenship Norm Indicators By Survey Year")
```

![](README_files/figure-gfm/plots-1.png)<!-- -->

``` r
ggsave("output/mean-citizenship-norm-line-plot-by-year.png")
```

    ## Saving 10 x 5 in image

``` r
# bar plot with indicators on x-axis, lines colored by year
plot_tbl %>% 
  gather(Indicator, value, -ICCS_year) %>% 
  mutate(Year = as.factor(ICCS_year),
         Indicator = factor(Indicator, ordered = TRUE, levels = c("obey", "rights", "local", "work", "envir", "vote", "history",
                                                                  "respect", "news", "protest", "discuss", "party"))) %>% 
  ggplot(aes(x = Indicator, y = value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Indicator", y = "Mean", title = "Mean Citizenship Norm Indicators By Survey Year")
```

![](README_files/figure-gfm/plots-2.png)<!-- -->

``` r
ggsave("output/mean-citizenship-norm-bar-plot-by-indicator.png")
```

    ## Saving 10 x 5 in image
