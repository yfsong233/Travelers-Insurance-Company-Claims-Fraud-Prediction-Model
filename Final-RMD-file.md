Final RMD file
================
Blue Space

You work for Travelers Insurance Company’s fraud detection department as
a modeler. Create a predictive model with concerns about the fraud
detection accuracy AND the key drivers that cause fraudulence.

Goals: (1) identify first-party physical damage fraudulence (2) explain
the indicators of fraudulent claims.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.1
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.4     ✔ forcats 1.0.0
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(vcd)
```

    ## Loading required package: grid

``` r
library(ggmosaic)
```

    ## 
    ## Attaching package: 'ggmosaic'
    ## 
    ## The following objects are masked from 'package:vcd':
    ## 
    ##     mosaic, spine

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(zipcodeR)
library(twosamples)

train <- read_csv("data copy/train_2023.csv") %>% select(-claim_number)
```

    ## Rows: 19000 Columns: 25
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): gender, living_status, claim_date, claim_day_of_week, accident_sit...
    ## dbl (17): claim_number, age_of_driver, marital_status, safty_rating, annual_...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  # data type of past_num_of_claims

train <- train %>% transform(
  age_of_driver=as.integer(age_of_driver),
  gender=as.factor(gender),
  marital_status=as.factor(marital_status),
  safty_rating=as.integer(safty_rating),
  annual_income=as.integer(annual_income),
  high_education_ind=as.factor(high_education_ind),
  address_change_ind=as.factor(address_change_ind),
  living_status=as.factor(living_status),
  zip_code=as.factor(zip_code),
  claim_date=as.factor(claim_date),
  claim_day_of_week=as.factor(claim_day_of_week),
  accident_site=as.factor(accident_site),
  past_num_of_claims=as.factor(past_num_of_claims),
  witness_present_ind=as.factor(witness_present_ind),
  liab_prct=as.integer(liab_prct),
  channel=as.factor(channel),
  policy_report_filed_ind=as.factor(policy_report_filed_ind),
  claim_est_payout=as.double(claim_est_payout),
  age_of_vehicle=as.integer(age_of_vehicle),
  vehicle_category=as.factor(vehicle_category), 
  vehicle_price=as.double(vehicle_price),
  vehicle_color=as.factor(vehicle_color),
  vehicle_weight=as.double(vehicle_weight),
  fraud=as.integer(fraud)
)

train %>% head(10)
```

    ##    age_of_driver gender marital_status safty_rating annual_income
    ## 1             50      F              1           66         39117
    ## 2             47      M              1           78         38498
    ## 3             28      M              0           76         33343
    ## 4             36      M              1           56         35832
    ## 5             60      F              1           79         40948
    ## 6             50      F              1           92         39126
    ## 7             28      M              1           89         33327
    ## 8             55      M              1           84         40079
    ## 9             65      F              1           91         41746
    ## 10            47      M              1           85         38505
    ##    high_education_ind address_change_ind living_status zip_code claim_date
    ## 1                   1                  0           Own    50051   1/2/2016
    ## 2                   1                  0           Own    50012 12/28/2015
    ## 3                   0                  1          Rent    20158  2/26/2016
    ## 4                   1                  0           Own    50054  9/20/2015
    ## 5                   1                  1          Rent    80010  4/14/2015
    ## 6                   1                  1           Own    50047  5/19/2015
    ## 7                   1                  1           Own    85035  1/23/2016
    ## 8                   1                  0           Own    15032   1/9/2015
    ## 9                   1                  1          Rent    15043 11/27/2016
    ## 10                  0                  1           Own    15057 12/24/2015
    ##    claim_day_of_week accident_site past_num_of_claims witness_present_ind
    ## 1           Saturday         Local                  0                   0
    ## 2             Monday   Parking Lot                  0                   0
    ## 3             Friday         Local                  0                   1
    ## 4             Sunday         Local                  0                   1
    ## 5            Tuesday       Highway                  0                   0
    ## 6            Tuesday         Local                  0                   0
    ## 7           Saturday         Local                  0                   1
    ## 8             Friday         Local                  0                   0
    ## 9             Sunday         Local                  0                <NA>
    ## 10          Thursday         Local                  0                   0
    ##    liab_prct channel policy_report_filed_ind claim_est_payout age_of_vehicle
    ## 1         18  Broker                       0         5464.904              3
    ## 2         81  Broker                       0         5448.156              4
    ## 3         94  Broker                       1         1858.972              4
    ## 4         95   Phone                       0         4040.933              5
    ## 5         53   Phone                       1         4876.606              6
    ## 6         41  Broker                       0         4468.934              8
    ## 7         50  Broker                       1         7656.884              5
    ## 8         99   Phone                       0         6359.792              7
    ## 9         17  Online                       0         3795.643              5
    ## 10        95  Broker                       0         3819.521              4
    ##    vehicle_category vehicle_price vehicle_color vehicle_weight fraud
    ## 1             Large     16786.288          blue      34183.436     0
    ## 2             Large     20793.299         black      14528.380     0
    ## 3           Compact      6729.476          blue       7182.833     0
    ## 4           Compact     24914.268          gray      47891.789     0
    ## 5            Medium     17392.420         black       7546.494     0
    ## 6           Compact     15891.701        silver      52379.710     0
    ## 7           Compact     35462.723         black      21971.624     0
    ## 8             Large      9375.375           red      31493.216     0
    ## 9             Large     19468.990           red      18237.078     0
    ## 10            Large     18286.274         black      24021.616     1

``` r
Name <- c("age_of_driver","safty_rating","annual_income","past_num_of_claims","liab_prct","claim_est_payout","age_of_vehicle","vehicle_price","vehicle_weight","claim_number","gender","marital_status","high_education_ind","address_change_ind","living_status","zip_code","claim_date","claim_day_of_week","accident_site","witness_present_ind","channel","policy_report_filed_ind","vehicle_category","vehicle_color","fraud")

Type <- c(rep("Numerical Predictor",9), rep("Categorical Predictor",15),"Response Variable")

Description <- c("Age of driver","Safety rating index of driver","Annual income of driver", "Number of claims the driver reported in past 5 years","Liability percentage of the claim"," Estimated claim payout","Age of first party vehicle","Price of first party vehicle","Weight of first party vehicle","Claim ID (cannot be used in model)","Gender of driver","Marital status of driver","Driver’s high education index","Whether or not the driver changed living address in past 1 year","Driver’s living status, own or rent","Driver’s living address zipcode","Date of first notice of claim","Day of week of first notice of claim","Accident location, highway, parking lot or local","Witness indicator of the claim","The channel of purchasing policy","Policy report filed indicator","Category of first party vehicle","Color of first party vehicle","Fraud indicator (0=no, 1=yes)")



variable_key <- data.frame(Name,Type,Description)
variable_key
```

    ##                       Name                  Type
    ## 1            age_of_driver   Numerical Predictor
    ## 2             safty_rating   Numerical Predictor
    ## 3            annual_income   Numerical Predictor
    ## 4       past_num_of_claims   Numerical Predictor
    ## 5                liab_prct   Numerical Predictor
    ## 6         claim_est_payout   Numerical Predictor
    ## 7           age_of_vehicle   Numerical Predictor
    ## 8            vehicle_price   Numerical Predictor
    ## 9           vehicle_weight   Numerical Predictor
    ## 10            claim_number Categorical Predictor
    ## 11                  gender Categorical Predictor
    ## 12          marital_status Categorical Predictor
    ## 13      high_education_ind Categorical Predictor
    ## 14      address_change_ind Categorical Predictor
    ## 15           living_status Categorical Predictor
    ## 16                zip_code Categorical Predictor
    ## 17              claim_date Categorical Predictor
    ## 18       claim_day_of_week Categorical Predictor
    ## 19           accident_site Categorical Predictor
    ## 20     witness_present_ind Categorical Predictor
    ## 21                 channel Categorical Predictor
    ## 22 policy_report_filed_ind Categorical Predictor
    ## 23        vehicle_category Categorical Predictor
    ## 24           vehicle_color Categorical Predictor
    ## 25                   fraud     Response Variable
    ##                                                        Description
    ## 1                                                    Age of driver
    ## 2                                    Safety rating index of driver
    ## 3                                          Annual income of driver
    ## 4             Number of claims the driver reported in past 5 years
    ## 5                                Liability percentage of the claim
    ## 6                                           Estimated claim payout
    ## 7                                       Age of first party vehicle
    ## 8                                     Price of first party vehicle
    ## 9                                    Weight of first party vehicle
    ## 10                              Claim ID (cannot be used in model)
    ## 11                                                Gender of driver
    ## 12                                        Marital status of driver
    ## 13                                   Driver’s high education index
    ## 14 Whether or not the driver changed living address in past 1 year
    ## 15                             Driver’s living status, own or rent
    ## 16                                 Driver’s living address zipcode
    ## 17                                   Date of first notice of claim
    ## 18                            Day of week of first notice of claim
    ## 19                Accident location, highway, parking lot or local
    ## 20                                  Witness indicator of the claim
    ## 21                                The channel of purchasing policy
    ## 22                                   Policy report filed indicator
    ## 23                                 Category of first party vehicle
    ## 24                                    Color of first party vehicle
    ## 25                                   Fraud indicator (0=no, 1=yes)

``` r
## fill missing values
train <- train[complete.cases(train), ]
# here I remove all missing; an alternative: imputation?
# the following 4 cols contain missing values:
# "marital_status"      "witness_present_ind" "claim_est_payout"    "age_of_vehicle"

## outliers
# age_of_driver, annual_income
```

``` r
if (!is.factor(train$fraud)) {
  train$fraud <- as.factor(train$fraud)
  train$fraud <- factor(train$fraud, levels = c(0, 1), labels = c("No Fraud", "Fraud"))
}

# 1. fraud against age - Done
age_uni <- train %>%
  mutate(age_group = cut(age_of_driver, breaks = seq(0, 100, 10), 
                         include.lowest = TRUE)) %>%
  ggplot(aes(age_group, fill = fraud)) +
  geom_histogram(position = "fill", stat="count") +
  scale_x_discrete(labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")) +
  scale_fill_discrete(name="Fraud") +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

    ## Warning in geom_histogram(position = "fill", stat = "count"): Ignoring unknown
    ## parameters: `binwidth`, `bins`, and `pad`

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

``` r
#H0: p1=...=p10=0 vs H1: pi != pj for at least one pair (i!=j).
## Chi_sq test for multiple proportions
age_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-1.png)<!-- -->

``` r
age_chi <- train %>%
  mutate(age_group = cut(age_of_driver, breaks = seq(0, 100, 10), include.lowest = TRUE)) %>% 
  filter(!is.na(age_group)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(age_group) %>%
  summarize(age_total = n(),
            age_fraud = sum(fraud_numeric == 1))
age_prop_chi <-chisq.test(age_chi$age_fraud,age_chi$age_total)
```

    ## Warning in chisq.test(age_chi$age_fraud, age_chi$age_total): Chi-squared
    ## approximation may be incorrect

``` r
age_prop_chi
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  age_chi$age_fraud and age_chi$age_total
    ## X-squared = 72, df = 64, p-value = 0.2303

``` r
# 2. fraud against gender - Done
gender_uni <- train %>% ggplot(aes(gender, fill = factor(fraud))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Fraud") +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

``` r
gender_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-2.png)<!-- -->

``` r
gender_z <- train %>%
  filter(!is.na(gender)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(gender) %>%
  summarize(gender_total = n(),
            gender_fraud = sum(fraud_numeric == 1))
gender_prop_z <-prop.test(gender_z$gender_fraud,gender_z$gender_total,alternative="greater")
gender_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  gender_z$gender_fraud out of gender_z$gender_total
    ## X-squared = 36.185, df = 1, p-value = 8.974e-10
    ## alternative hypothesis: greater
    ## 95 percent confidence interval:
    ##  0.02305248 1.00000000
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1716135 0.1397424

``` r
# 3. fraud against marital_status - Done
ms_uni <- train %>% 
  filter(!is.na(marital_status)) %>%
  ggplot(aes(x = factor(marital_status), fill = factor(fraud))) + 
  geom_bar(position = "fill")+
  scale_fill_discrete(name="Fraud")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

``` r
ms_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-3.png)<!-- -->

``` r
ms_z <- train %>% 
  filter(!is.na(marital_status)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0),marital_status_text = ifelse(marital_status == 1, "Yes", "No"))%>%
  group_by(marital_status_text) %>%
  summarize(ms_total = n(),
            ms_fraud = sum(fraud_numeric == 1))
ms_prop_z <-prop.test(ms_z$ms_fraud,ms_z$ms_total,alternative="greater")
ms_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  ms_z$ms_fraud out of ms_z$ms_total
    ## X-squared = 105.56, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## 95 percent confidence interval:
    ##  0.04990451 1.00000000
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1980885 0.1378364

``` r
# 4. fraud against safety rating - Done
sr_uni <- train %>% ggplot(aes(safty_rating, factor(fraud))) + 
  geom_boxplot(na.rm=T, fill= c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen")) +
  coord_flip()

sr_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-4.png)<!-- -->

``` r
sr_fraud <- train %>% 
  select(fraud, safty_rating) %>%
  filter(!is.na(safty_rating), fraud == "Fraud")
sr_no_fraud <- train %>% 
  select(fraud, safty_rating) %>%
  filter(!is.na(safty_rating), fraud == "No Fraud")
library(twosamples)
cvm_test(sr_fraud$safty_rating, sr_no_fraud$safty_rating)
```

    ## Test Stat   P-Value 
    ##  28.19663   0.00025

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
ad_test(sr_fraud$safty_rating, sr_no_fraud$safty_rating)
```

    ##    Test Stat      P-Value 
    ## 1.561305e+06 2.500000e-04

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
# 5. fraud against annual income - Done
income_uni <- train %>%
  filter(!is.na(annual_income), annual_income>0, annual_income >= 20000) %>% 
  ggplot(aes(factor(fraud), annual_income)) + 
  geom_violin() +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

income_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-5.png)<!-- -->

``` r
income_uni <- train %>%
  filter(!is.na(annual_income), annual_income >= 20000) %>% 
  ggplot(aes(factor(fraud),annual_income)) + 
  geom_violin()
income_fraud <- train %>% 
  select(fraud, annual_income) %>%
  filter(!is.na(annual_income), fraud == "Fraud")
income_no_fraud <- train %>% 
  select(fraud, annual_income) %>%
  filter(!is.na(annual_income), fraud == "No Fraud")
cvm_test(income_fraud$annual_income, income_no_fraud$annual_income)
```

    ## Test Stat   P-Value 
    ##  86.45077   0.00025

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
ad_test(income_fraud$annual_income, income_no_fraud$annual_income)
```

    ##    Test Stat      P-Value 
    ## 4.328872e+06 2.500000e-04

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
# 6. fraud against high_education_ind - Done
edu_uni <- train %>% 
  filter(!is.na(high_education_ind)) %>%
  ggplot(aes(x = factor(high_education_ind), fill = factor(fraud))) + 
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

edu_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-6.png)<!-- -->

``` r
edu_z <- train %>% 
  filter(!is.na(high_education_ind)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0), edu_text = ifelse(high_education_ind == 1, "Yes", "No"))%>%
  group_by(edu_text) %>%
  summarize(edu_total = n(),
            edu_fraud = sum(fraud_numeric == 1))
edu_prop_z <-prop.test(edu_z$edu_fraud,edu_z$edu_total,alternative="greater")
edu_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  edu_z$edu_fraud out of edu_z$edu_total
    ## X-squared = 234.06, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## 95 percent confidence interval:
    ##  0.07762765 1.00000000
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.2162020 0.1282774

``` r
# 7. fraud against address_change_ind - Done
address_uni <- train %>% 
  filter(!is.na(address_change_ind)) %>%
  ggplot(aes(x = factor(address_change_ind), fill = factor(fraud))) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

address_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-7.png)<!-- -->

``` r
address_z <- train %>% 
  filter(!is.na(address_change_ind)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0), address_text = ifelse(address_change_ind == 1, "Yes", "No")) %>%
  group_by(address_text) %>%
  summarize(address_total = n(), address_fraud = sum(fraud_numeric == 1))
address_prop_z <-prop.test(address_z$address_fraud,address_z$address_total,alternative="less")
address_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  address_z$address_fraud out of address_z$address_total
    ## X-squared = 86.032, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: less
    ## 95 percent confidence interval:
    ##  -1.00000000 -0.04088409
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1264798 0.1760511

``` r
# 8. fraud against living_status - Done
mosaicplot(~ living_status + fraud, 
           data = train, 
           color = c("lightgreen", "lightskyblue"), 
           main = "Relationship between Living Status and Fraud")
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-8.png)<!-- -->

``` r
  # bar graph attempt
live_uni <- train %>% ggplot(aes(living_status, fill = factor(fraud))) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

live_z <- train %>% 
  filter(!is.na(living_status)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(living_status) %>%
  summarize(live_total = n(), live_fraud = sum(fraud_numeric == 1))
live_prop_z <-prop.test(live_z$live_fraud,live_z$live_total,alternative="two.sided")
live_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  live_z$live_fraud out of live_z$live_total
    ## X-squared = 18.141, df = 1, p-value = 2.051e-05
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.03326793 -0.01213650
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1447773 0.1674795

``` r
## 11. fraud against claim_day_of_week - Done
week_uni <- train %>% ggplot(aes(claim_day_of_week, fill=factor(fraud))) +
  geom_bar(position="fill")

week_chi <- train %>%
  filter(!is.na(claim_day_of_week)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(claim_day_of_week) %>%
  summarize(week_total = n(),
            week_fraud = sum(fraud_numeric == 1))
week_prop_chi <-prop.test(week_chi$week_fraud,week_chi$week_total,alternative="two.sided")
week_prop_chi
```

    ## 
    ##  7-sample test for equality of proportions without continuity correction
    ## 
    ## data:  week_chi$week_fraud out of week_chi$week_total
    ## X-squared = 10.519, df = 6, p-value = 0.1044
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6    prop 7 
    ## 0.1414297 0.1439532 0.1606140 0.1667290 0.1567137 0.1537615 0.1611321

``` r
## 12. fraud against accident_site - Done
site_uni <- train %>% ggplot(aes(accident_site, fill=factor(fraud))) +
  geom_bar(position="fill")

site_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-9.png)<!-- -->

``` r
site_chi <- train %>%
  filter(!is.na(accident_site)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(accident_site) %>%
  summarize(site_total = n(),
            site_fraud = sum(fraud_numeric == 1))
site_prop_chi <-prop.test(site_chi$site_fraud,site_chi$site_total, alternative="two.sided")
site_prop_chi
```

    ## 
    ##  3-sample test for equality of proportions without continuity correction
    ## 
    ## data:  site_chi$site_fraud out of site_chi$site_total
    ## X-squared = 4.5327, df = 2, p-value = 0.1037
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3 
    ## 0.1511270 0.1605594 0.1483758

``` r
## 13. fraud against past_num_of_claims - Done
train %>% ggplot(aes(past_num_of_claims, factor(fraud))) +
  geom_count() +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-10.png)<!-- -->

``` r
  # boxplot attempt
past_uni <- train %>% 
  filter(!is.na(as.numeric(past_num_of_claims)) 
         & as.numeric(past_num_of_claims) > 0) %>% 
  ggplot(aes(fraud, log(as.numeric(past_num_of_claims)), fill = fraud)) +
  geom_boxplot() +
   scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

past_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-11.png)<!-- -->

``` r
past_fraud <- train %>% 
  select(fraud, past_num_of_claims) %>%
  filter(!is.na(past_num_of_claims), fraud == "Fraud")
past_no_fraud <- train %>% 
  select(fraud, past_num_of_claims) %>%
  filter(!is.na(past_num_of_claims), fraud == "No Fraud")
cvm_test(past_fraud$past_num_of_claims, past_no_fraud$past_num_of_claims)
```

    ## Test Stat   P-Value 
    ## 165.45517   0.00025

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
ad_test(past_fraud$past_num_of_claims, past_no_fraud$past_num_of_claims)
```

    ##   Test Stat     P-Value 
    ## 9.12275e+06 2.50000e-04

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
## 14. fraud against witness_present_ind - Done
witness_uni <- train %>% 
  filter(!is.na(witness_present_ind)) %>%
  ggplot(aes(x = factor(witness_present_ind), fill = factor(fraud))) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

witness_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-12.png)<!-- -->

``` r
witness_z <- train %>%
  filter(!is.na(witness_present_ind)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0), witness_text = ifelse(witness_present_ind == 1, "Yes", "No")) %>%
  group_by(witness_present_ind) %>%
  summarize(witness_total = n(),
            witness_fraud = sum(fraud_numeric == 1))
witness_prop_z <-prop.test(witness_z$witness_fraud,witness_z$witness_total,alternative="greater")
witness_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  witness_z$witness_fraud out of witness_z$witness_total
    ## X-squared = 105.7, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## 95 percent confidence interval:
    ##  0.05460798 1.00000000
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1700543 0.1061252

``` r
## 15. fraud against liab_prct - Not Done
train %>% ggplot(aes(liab_prct, fill = factor(fraud))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-13.png)<!-- -->

``` r
names(train)
```

    ##  [1] "age_of_driver"           "gender"                 
    ##  [3] "marital_status"          "safty_rating"           
    ##  [5] "annual_income"           "high_education_ind"     
    ##  [7] "address_change_ind"      "living_status"          
    ##  [9] "zip_code"                "claim_date"             
    ## [11] "claim_day_of_week"       "accident_site"          
    ## [13] "past_num_of_claims"      "witness_present_ind"    
    ## [15] "liab_prct"               "channel"                
    ## [17] "policy_report_filed_ind" "claim_est_payout"       
    ## [19] "age_of_vehicle"          "vehicle_category"       
    ## [21] "vehicle_price"           "vehicle_color"          
    ## [23] "vehicle_weight"          "fraud"

``` r
liab_fraud <- train %>% 
  select(fraud, liab_prct) %>%
  filter(!is.na(liab_prct), fraud == "Fraud")
liab_no_fraud <- train %>% 
  select(fraud, liab_prct) %>%
  filter(!is.na(liab_prct), fraud == "No Fraud")
cvm_test(liab_fraud$liab_prct, liab_no_fraud$liab_prct)
```

    ## Test Stat   P-Value 
    ##  5.425545  0.007000

``` r
ad_test(liab_fraud$liab_prct, liab_no_fraud$liab_prct)
```

    ##  Test Stat    P-Value 
    ## 269617.586      0.014

``` r
## 16. fraud against channel - Done
channel_uni <- train %>% 
  filter(!is.na(channel)) %>%
  ggplot(aes(x = factor(channel), fill = factor(fraud))) + 
  geom_bar(position = "fill")

channel_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-14.png)<!-- -->

``` r
channel_chi <- train %>%
  filter(!is.na(channel)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(channel) %>%
  summarize(channel_total = n(),
            channel_fraud = sum(fraud_numeric == 1)) %>%
  select(channel_total, channel_fraud)
as.matrix(channel_chi)
```

    ##      channel_total channel_fraud
    ## [1,]         10082          1553
    ## [2,]          2745           449
    ## [3,]          5996           914

``` r
channel_matrix <- as.matrix(channel_chi)
dim(channel_matrix)
```

    ## [1] 3 2

``` r
channel_prop_chi <- chisq.test(channel_matrix)
channel_prop_chi
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  channel_matrix
    ## X-squared = 1.3908, df = 2, p-value = 0.4989

``` r
## 17. fraud against policy_report_filed_ind
policy_uni <- train %>% 
  filter(!is.na(policy_report_filed_ind)) %>%
  ggplot(aes(x = factor(policy_report_filed_ind), fill = factor(fraud))) + 
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

policy_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-15.png)<!-- -->

``` r
policy_z <- train %>%
  filter(!is.na(policy_report_filed_ind)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0), policy_text = ifelse(policy_report_filed_ind == 1, "Yes", "No")) %>%
  group_by(policy_text) %>%
  summarize(policy_total = n(),
            policy_fraud = sum(fraud_numeric == 1))
policy_prop_z <-prop.test(policy_z$policy_fraud,policy_z$policy_total,alternative="less")
policy_prop_z
```

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  policy_z$policy_fraud out of policy_z$policy_total
    ## X-squared = 9.0919, df = 1, p-value = 0.001284
    ## alternative hypothesis: less
    ## 95 percent confidence interval:
    ##  -1.000000000 -0.007456026
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.1451142 0.1614560

``` r
## 18. fraud against claim_est_payout
payout_uni <- train %>% 
  ggplot(aes(fraud, claim_est_payout, fill = fraud)) + 
  geom_boxplot() +
  scale_fill_discrete(name="Fraud")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

``` r
payout_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-16.png)<!-- -->

``` r
payout_fraud <- train %>% 
  select(fraud, claim_est_payout) %>%
  filter(!is.na(claim_est_payout), fraud == "Fraud")
payout_no_fraud <- train %>% 
  select(fraud, claim_est_payout) %>%
  filter(!is.na(claim_est_payout), fraud == "No Fraud")
cvm_test(payout_fraud$claim_est_payout, payout_no_fraud$claim_est_payout)
```

    ## Test Stat   P-Value 
    ##  38.35846   0.00025

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
ad_test(payout_fraud$claim_est_payout, payout_no_fraud$claim_est_payout)
```

    ##    Test Stat      P-Value 
    ## 2.138105e+06 2.500000e-04

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
## 19. fraud against age_of_vehicle
train %>%
  ggplot(aes(factor(age_of_vehicle), fill = factor(fraud))) +
  geom_bar(position = "dodge") +
  scale_fill_discrete(name="Fraud")+
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))
```

    ## Scale for fill is already present.
    ## Adding another scale for fill, which will replace the existing scale.

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-17.png)<!-- -->

``` r
  # box plot attempt
veh_age_uni <- train %>% 
  ggplot(aes(factor(fraud), age_of_vehicle, fill = fraud)) +
  geom_boxplot(position = "dodge", fill=c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen"))

veh_age_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-18.png)<!-- -->

``` r
veh_age_fraud <- train %>% 
  select(fraud, age_of_vehicle) %>%
  filter(!is.na(age_of_vehicle), fraud == "Fraud")
veh_age_no_fraud <- train %>% 
  select(fraud, age_of_vehicle) %>%
  filter(!is.na(age_of_vehicle), fraud == "No Fraud")
cvm_test(veh_age_fraud$age_of_vehicle, veh_age_no_fraud$age_of_vehicle)
```

    ## Test Stat   P-Value 
    ##  24.55885   0.00025

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
ad_test(veh_age_fraud$age_of_vehicle, veh_age_no_fraud$age_of_vehicle)
```

    ##   Test Stat     P-Value 
    ## 1.21515e+06 2.50000e-04

    ## No bootstrap values were more extreme than the observed value. 
    ##  p-value = 1/(2*bootstraps) is an imprecise placeholder

``` r
## 20. fraud against vehicle_category
category_uni <- train %>% 
  filter(!is.na(vehicle_category)) %>%
  ggplot(aes(x = factor(vehicle_category), fill = factor(fraud))) + 
  geom_bar(position = "fill")

category_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-19.png)<!-- -->

``` r
category_chi <- train %>%
  filter(!is.na(vehicle_category)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(vehicle_category) %>%
  summarize(category_total = n(),
            category_fraud = sum(fraud_numeric == 1)) %>%
  select(category_total, category_fraud)
as.matrix(category_chi)
```

    ##      category_total category_fraud
    ## [1,]           6346            989
    ## [2,]           6263            961
    ## [3,]           6214            966

``` r
category_matrix <- as.matrix(category_chi)
dim(category_matrix)
```

    ## [1] 3 2

``` r
category_prop_chi <- chisq.test(category_matrix)
category_prop_chi
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  category_matrix
    ## X-squared = 0.11704, df = 2, p-value = 0.9432

``` r
## 21. fraud against LOG TRANSFORMED vehicle_price
price_uni <- train %>% 
  ggplot(aes(log(vehicle_price), fill = factor(fraud))) +
  geom_density(alpha = 0.5)

price_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-20.png)<!-- -->

``` r
price_fraud <- train %>% 
  select(fraud, vehicle_price) %>%
  filter(!is.na(vehicle_price), fraud == "Fraud")
price_no_fraud <- train %>% 
  select(fraud, vehicle_price) %>%
  filter(!is.na(vehicle_price), fraud == "No Fraud")
cvm_test(price_fraud$vehicle_price, price_no_fraud$vehicle_price)
```

    ## Test Stat   P-Value 
    ##   1.04266   0.43050

``` r
ad_test(price_fraud$vehicle_price, price_no_fraud$vehicle_price)
```

    ##  Test Stat    P-Value 
    ## 78190.0224     0.3135

``` r
## 22. fraud against vehicle_color
color_uni <- train %>% 
  filter(!is.na(vehicle_color)) %>%
  ggplot(aes(x = factor(vehicle_color), fill = factor(fraud))) + 
  geom_bar(position = "fill")

color_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-21.png)<!-- -->

``` r
color_chi <- train %>%
  filter(!is.na(vehicle_color)) %>%
  mutate(fraud_numeric = ifelse(fraud == "Fraud", 1, 0)) %>%
  group_by(vehicle_color) %>%
  summarize(color_total = n(),
            color_fraud = sum(fraud_numeric == 1)) %>%
  select(color_total, color_fraud)
as.matrix(color_chi)
```

    ##      color_total color_fraud
    ## [1,]        2715         433
    ## [2,]        2656         433
    ## [3,]        2667         414
    ## [4,]        2680         392
    ## [5,]        2706         407
    ## [6,]        2702         412
    ## [7,]        2697         425

``` r
color_matrix <- as.matrix(color_chi)
dim(color_matrix)
```

    ## [1] 7 2

``` r
color_prop_chi <- chisq.test(color_matrix)
color_prop_chi
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  color_matrix
    ## X-squared = 2.9208, df = 6, p-value = 0.8187

``` r
## 23. fraud against vehicle_weight
weight_uni <- train %>% 
  ggplot(aes(factor(fraud), vehicle_weight, fill = factor(fraud))) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_discrete(name="Fraud")

weight_uni
```

![](Final-RMD-file_files/figure-gfm/univariate%20analysis-22.png)<!-- -->

``` r
weight_fraud <- train %>% 
  select(fraud, vehicle_weight) %>%
  filter(!is.na(vehicle_weight), fraud == "Fraud")
weight_no_fraud <- train %>% 
  select(fraud, vehicle_weight) %>%
  filter(!is.na(vehicle_weight), fraud == "No Fraud")
cvm_test(weight_fraud$vehicle_weight, weight_no_fraud$vehicle_weight)
```

    ## Test Stat   P-Value 
    ##  1.572649  0.241000

``` r
ad_test(weight_fraud$vehicle_weight, weight_no_fraud$vehicle_weight)
```

    ##  Test Stat    P-Value 
    ## 76676.7239     0.3305

As result, statistically significant predictor variables: age, gender,
marital_status, annual_income, high_education_ind, address_change_ind,
living_status, witness_present_ind,
liab_prct,policy_report_filed_ind,claim_est_payout,age_of_vehicle,
past_num_of_claims

``` r
#correlated variables: (y,x)
#log_annual_income, marital_status; 
#age_of_driver vs log_annual_income;
#log_annual_income, gender;
#living_status, log_annual_income;
#log_annual_income vs past_num_of_claims;
#age_of_driver vs past_num_claims;
#policy_report_filed_ind, past_num_of_claims; 
#witness_present_ind, policy_report_filed_ind;        
#living_status, policy_report_filed_ind;
#high_education_ind vs living_status;

# 1. 


# 2. marital_status / gender with fraud
mosaicplot(~ gender + marital_status + fraud, 
           data = train, 
           color = 3:5,
           las = 1, 
           shade = TRUE, 
           margin = list(1:2, 3),
           main = "fraud by gender and marital status")
```

![](Final-RMD-file_files/figure-gfm/bivariate:%20visualization-1.png)<!-- -->

``` r
# 3. address_change_id & living_status with fraud
  # make sure `fraud` is a factor
train %>% 
  filter(annual_income > 2000) %>% 
  ggplot() +
  geom_boxplot(aes(past_num_of_claims, log(annual_income), fill = fraud), outlier.alpha = 0.5) +
  geom_point(aes(past_num_of_claims, log(annual_income), color = fraud), alpha = 0.5) +
  facet_grid(~ fraud) +
  labs(title = "Fraud by Annual Income and Past Number of Claims",
       x = "Past Number of Claims",
       y = "Annual Income") +
  theme_minimal() +
  scale_fill_manual(values = c("No Fraud" = "lightskyblue", "Fraud" = "lightgreen")) +
  scale_color_manual(values = c("No Fraud" = "black", "Fraud" = "seagreen"))
```

![](Final-RMD-file_files/figure-gfm/bivariate:%20visualization-2.png)<!-- -->

``` r
# 4. marital_status, address_change_ind, living_status vs fraud
train$fraud <- ifelse(train$fraud == "Fraud", 1, 0)

fraud_summary <- train %>%
  group_by(marital_status, address_change_ind, living_status, fraud) %>%
  summarize(count = n()) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'marital_status', 'address_change_ind',
    ## 'living_status'. You can override using the `.groups` argument.

``` r
fraud_proportion <- fraud_summary %>%
  mutate(proportion = count / sum(count)) %>%
  spread(key = fraud, value = proportion) %>% 
  mutate(Fraud = ifelse(is.na(`1`), `0`, `1`)) %>%
  select(-`0`, -`1`)

fraud_proportion %>% 
  ggplot(aes(marital_status, address_change_ind, fill = Fraud)) + 
  geom_tile() +
  facet_wrap(~ living_status) +
  scale_fill_gradient2(low = "white", mid = "lightskyblue", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  labs(title = "Fraud by Marital Status, Address Change Index, Living status",
       x = "Marital Status", y = "Address Change Ind", fill = "Proportion of Fraud") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"))
```

![](Final-RMD-file_files/figure-gfm/bivariate:%20visualization-3.png)<!-- -->
