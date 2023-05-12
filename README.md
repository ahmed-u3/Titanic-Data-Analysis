---
title: Titanic Data Analysis
author: 
    - Ahmed Ashraf Mohamed
    - Omar Gamal Abdelaziz
    - Ahmed Yousri Ali
    - Ahmed Dawood Mohamed
    - Alia Medhat
description: 
    A Probability and Statistics II - Project
---

# Tasks

- [x] Summary of the data
- [x] Categories of the data
- [x] Exploring missing data
    - [x] count the missing data.
    - [x] How normal is it that missing values exits in our data?
    - [x] Provide solutions to such a case.
    - [x] Effect of missing data on analysis
- [x] Handling the missing data by either removing them or imputing them.
- [x] Histogram plot the age feature.
- [x] By inspection, determine the distribution that most fits the age and fare features.
- [x] Calculate the population mean, and standard deviation of age feature.
- [x] Take a random sample of size 50 from Age. Using this sample, what is your point estimate of the population mean and standard deviation?
- [x] Since you have access to the population, simulate the sampling distribution for $\bar{Age}$ by taking 50 samples from the population of size 50 and computing 50 sample means. Store these means in a vector called `sample_means50`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] Simulate the sampling distribution for $\bar{Age}$ by taking 100 samples from the population of size 50 and computing 100 sample means. Store these means in a vector called `sample_means100`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] Simulate the sampling distribution for $\bar{Age}$ by taking 1000 samples from the population of size 50 and computing 1000 sample means. Store these means in a vector called `sample_means1000`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] What happens to the sampling distribution when the number of samples increases?
- [x] Since you have access to the population, simulate the sampling distribution for $\bar{Age}$ by taking 1500 samples from the population of size 20 and computing 1500 sample means. Store these means in a vector called `sample_means_s20`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] Simulate the sampling distribution for $\bar{Age}$ by taking 1500 samples from the population of size 100 and computing 1500 sample means. Store these means in a vector called `sample_means_s100`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] Simulate the sampling distribution for $\bar{Age}$ by taking 1500 samples from the population of size 200 and computing 1500 sample means. Store these means in a vector called `sample_means_s200`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, what would you guess the mean age of the population to be?
- [x] What happens to the sampling distribution when the size of each sample increases? check the compatibility of the results with the central limit theorem.
- [x]  Since you have access to the population, simulate the sampling distribution for the sample variances by taking 1500 samples from the population of size 2 and computing 1500 sample variance. Store these sample variances in a vector called `sample_U1500`. Plot the data, then describe the shape of this sampling distribution of variances. Based on this sampling distribution, does it follow normal distribution or not?
- [x] Simulate the sampling distribution for the sample variances by taking 1500 samples from the population of size 50 and computing 1500 sample variance. Store these sample variances in a vector called `sample_U1500`. Plot the data, then describe the shape of this sampling distribution of variances. Based on this sampling distribution
    - [x] What happens to the shape of the sampling distribution using this sampling distribution to estimate the population variance?
- [x] Take a random sample of size 50 from Age. Using this sample to estimate the mean Age of the population using MLE and MME, then determine the bias of those estimators?
- [x] Take a random sample of size 200 from Age. Using this sample to estimate the mean Age of the population with MLE and MME, does the bias increase or decrease as the sample size increases? Can you identify which estimators are the most effective?
- [x] Separate the age column of the titanic dataset into two groups based on gender (male and female) with variable name age_male_(anyone in the team ID) and age_female_(anyone in the team ID), then simulate the sampling distribution for age_male - age_female by taking 15000 samples from the male and female populations of size 50 and computing 15000 sample difference means. Store these means in a vector called `samplediff_means15000`. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, do you believe there is a significant difference in the average ages of men and women in titanic?
- [x] Separate the Survived column of the titanic dataset into two groups based on gender (male and female) with variable name Survived_male and Survived_female, then simulate the sampling distribution for `Survived_male` - `Survived_female` by taking 15000 samples from the male and female populations of size 50 and computing 15000 sample difference between the number of survivals in males and females. Store these differences in a vector called `samplediff_Survived` 15000. Plot the data, then describe the shape of this sampling distribution. Based on this sampling distribution, do you think there is bias in the rescue process between males and females?
- [x] Take a random sample of size 10 from Age. Using this sample, Find 95% confidence interval for the mean of the ages?
- [x] Take a random sample of size 50 from Age. Using this sample, Find 95% confidence interval for the mean of the ages?
- [x] Take a random sample of size 200 from Age. Using this sample, multiply these sample ages by 5 to determine what happens to the E(age) and var (age) after multiplication?
- [x]  Take a random sample of size 200 from Age. Using this sample, add these sample ages to 5 to determine what happens to the E(age) and var (age) after addition?
- [x] kernel distribution and write a short summary of its applications
- [x] KS test and write a short summary of its applications
