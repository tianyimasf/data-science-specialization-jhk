---
title: "quiz1"
author: "Tianyi"
date: "2023-02-17"
output: html_document
---

# code snippets that answers quiz 1 week 1 R Programming JHK Coursera questions

```{r}
data <- read.csv(file = "hw1_data.csv")
ozone_na <- is.na(data$Ozone)
ozone_na_list <- ozone_na[ozone_na == TRUE]
print(length(ozone_na_list))
```
```{r}
ozone_not_na <- data$Ozone[!ozone_na]
print(mean(ozone_not_na))
```
```{r}
valid_data <- data[which(data$Ozone > 31 & data$Temp > 90), ]
print(mean(valid_data$Solar.R))
```
```{r}
ozone_data <- data[data$Month == 5, ]$Ozone
max(ozone_data[!is.na(ozone_data)])
```
```{r}
x <- 1:4
y <- 2:3
x + y
```
