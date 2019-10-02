---
title: "Body mass estimation for subadults"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Body mass estimation based on Ruff (2007)
_Reference:_ Ruff, C. (2007). Body size prediction from juvenile skeletal remains. *American Journal of Physical Anthropology*, 133(1), 698-716. 

This is a body mass estimation function based on Ruff (2007)
This function prioritize femoral head breadth for individuals age over 7, but can calculate body mass estimates based on femoral distal metaphyseal breadth for these indivduals as well (if femoral head breadth is not available).

```{r mass estimation function}
mass.estimation <- function(age, dfb, fhb){
               
  if (age >= 0 & age <= 1.4){
    wt = 0.188*dfb + 2.6
    return(c(wt, wt-2*0.65, wt+2*0.65))
  } else  if (age > 1.4 & age <= 2.4) {
    wt = 0.268*dfb + 0.2
    return(c(wt, wt-2*0.81, wt+2*0.81))
  } else  if (age > 2.4 & age <= 3.4) {
    wt = 0.257*dfb + 1.5
    return(c(wt, wt-2*0.91, wt+2*0.91))
  } else  if (age > 3.4 & age <= 4.4) {
    wt = 0.328*dfb - 0.7
    return(c(wt, wt-2*1.08, wt+2*1.08))
  } else  if (age > 4.4 & age <= 5.4) {
    wt = 0.367*dfb - 1.6
    return(c(wt, wt-2*1.08, wt+2*1.08))
  } else  if (age > 5.4 & age <= 6.4) {
    wt = 0.367*dfb - 0.4
    return(c(wt, wt-2*1.32, wt+2*1.32))
  } else  if (age > 6.4 & age <= 7.4) {
    if (is.na(fhb) == FALSE) {
    wt = 0.495*fhb - 8.0
    return(c(wt, wt-2*1.35, wt+2*1.35))}
    else { 
    wt = 0.419*dfb - 1.6
    return(c(wt, wt-2*1.38, wt+2*1.38))}
  } else  if (age > 7.4 & age <= 8.4) {
    if (is.na(fhb) == FALSE) {wt = 0.606*fhb - 6.1
    return(c(wt, wt-2*1.96, wt+2*1.96))}
    else {wt = 0.414*dfb + 0.5
    return(c(wt, wt-2*2.28, wt+2*2.28))}
  } else  if (age > 8.4 & age <= 9.4) {
    if (is.na(fhb) == FALSE) {wt = 1.155*fhb - 8.7
    return(c(wt, wt-2*3.52, wt+2*2.52))}
    else {wt = 0.694*dfb - 12.8
    return(c(wt, wt-2*4.44, wt+2*4.44))}
  } else  if (age > 9.4 & age <= 10.4) {
    if (is.na(fhb) == FALSE) {wt = 1.279*fhb - 12.2
    return(c(wt, wt-2*4.73, wt+2*4.73))}
    else {wt = 0.992*dfb - 29.5
    return(c(wt, wt-2*5.36, wt+2*5.36))}
  } else  if (age > 10.4 & age <= 11.4) {
    if (is.na(fhb) == FALSE) {wt = 1.626*fhb - 23.0
    return(c(wt, wt-2*5.61, wt+2*5.61))}
    else {wt = 0.938*dfb - 23.9
    return(c(wt, wt-2*6.84, wt+2*6.84))}
  } else  if (age > 11.4 & age <= 12.4) {
    if (is.na(fhb) == FALSE) {wt = 1.850*fhb - 31.3
    return(c(wt, wt-2*5.65, wt+2*5.65))}
    else {wt = 1.351*dfb - 49.6
    return(c(wt, wt-2*7.38, wt+2*7.38))}
  } else  if (age > 12.4 & age <= 13.4) {
    wt = 1.830*fhb - 29.4
    return(c(wt, wt-2*7.84, wt+2*7.84))
  } else  if (age > 13.4 & age <= 14.4) {
    wt = 1.438*fhb - 10.3
    return(c(wt, wt-2*7.75, wt+2*7.75))
  }  else  if (age > 14.4 & age <= 15.4) {
    wt = NA
    return(c(wt, wt, wt))
  }  else  if (age > 15.4 & age <= 16.4) {
    wt = 1.009*(0.842*log(fhb) + 0.88)
    return(c(wt, wt-2*6.03, wt+2*6.03))
  } else  if (age > 16.4 & age <= 17.4) {
    wt = 1.750*fhb - 17.2
    return(c(wt, wt-2*7.34, wt+2*7.34))
  } else {}

}

```

## Body mass estimation based on Robbins et al. (2010)
_Reference:_ Robbins, G., Sciulli, P. W., & Blatt, S. H. (2010). Estimating body mass in subadult human skeletons. *American journal of physical anthropology*, 143(1), 146-150.

This is the a subadult body mass estimation function based on Robbins et al. (2010), which is based on femoral mid-shaft cross-sectional property J (polar second moments of area).

```{r mass estimation Robbins et al. 2010}
mass.Robbins <- function(age, J) {
  
  if (age >= 0 & age < 0.5){
    wt = 0.003*J + 3.8
    return(c(wt, wt-2*0.27, wt+2*0.27))
  } else  if (age >= 0.5 & age < 1.5) {
    wt = 0.002*J + 7.1
    return(c(wt, wt-2*0.61, wt+2*0.61))
  } else  if (age >= 1.5 & age < 2.5) {
    wt = 0.002*J + 8.1
    return(c(wt, wt-2*0.68, wt+2*0.68))
  } else  if (age >= 2.5 & age < 3.5) {
    wt = 0.001*J + 10.5
    return(c(wt, wt-2*0.92, wt+2*0.92))
  } else  if (age >= 3.5 & age < 4.5) {
    wt = 0.001*J + 11.4
    return(c(wt, wt-2*1.00, wt+2*1.00))
  } else  if (age >= 4.5 & age < 5.5) {
    wt = 0.001*J + 12.8
    return(c(wt, wt-2*1.06, wt+2*1.06))
  } else  if (age >= 5.5 & age < 6.5) {
    wt = 0.001*J + 14.2
    return(c(wt, wt-2*1.23, wt+2*1.23))
    
  } else  if (age >= 6.5 & age < 7.5) {
    wt = 0.001*J + 15.8
    return(c(wt, wt-2*1.38, wt+2*1.38))
  } else  if (age >= 7.5 & age < 8.5) {
    wt = 0.001*J + 16.0
    return(c(wt, wt-2*1.75, wt+2*1.75))
  } else  if (age >= 8.5 & age < 9.5) {
    wt = 0.001*J + 17.1
    return(c(wt, wt-2*4.11, wt+2*4.11))
  } else  if (age >= 9.5 & age < 10.5) {
    wt = 0.001*J + 16.3
    return(c(wt, wt-2*5.05, wt+2*5.05))
  } else  if (age >= 10.5 & age < 11.5) {
    wt = 0.001*J + 18.4
    return(c(wt, wt-2*6.06, wt+2*6.06))
  } else  if (age >= 11.5 & age < 12.5) {
    wt = 0.001*J + 19.2
    return(c(wt, wt-2*6.48, wt+2*6.48))
  } else  if (age >= 12.5 & age < 13.5) {
    wt = 0.001*J + 21.1
    return(c(wt, wt-2*7.00, wt+2*7.00))
  }  else  if (age >= 13.5 & age < 14.5) {
    wt = 0.001*J + 30.4
    return(c(wt, wt-2*7.29, wt+2*7.29))
  }  else  if (age >= 14.5 & age < 15.5) {
    wt = 0.001*J + 36.6
    return(c(wt, wt-2*6.41, wt+2*6.41))
  } else  if (age >= 15.5 & age < 16.5) {
    wt = 0.000*J + 45.8
    return(c(wt, wt-2*8.13, wt+2*8.13))
  } else  if (age >= 16.5 & age < 17.5) {
    wt = 0.000*J + 46.2
    return(c(wt, wt-2*7.84, wt+2*7.84))
  } else {}
  

}
```

## Body mass estimation using panel regression as reported by Robbins Shug et al. (2013)
*Reference:* Schug, G. R., Gupta, S., Cowgill, L. W., Sciulli, P. W., & Blatt, S. H. (2013). Panel regression formulas for estimating stature and body mass from immature human skeletons: a statistical approach without reference to specific age estimates. *Journal of Archaeological Science*, 40(7), 3076-3086.

This is a function that estimates subadult body mass based on Robbins Shug et al. (2013). It uses a panel regression-derived formula thus eliminating the need to know individual's age. Currently only the equation using J is available here, but I will put another one here as soon as possible.

```{r panel regression using J}
estimate.mass <- function(J) {
  logwt = 2.0683 - 0.3126*log(J) + 0.0477*log(J)^2
  wt = exp(logwt)
  return(wt)
}


```

