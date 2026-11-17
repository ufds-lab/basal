---
title: 'basal: A Modular Bayesian Small Area Estimation Library'
authors:
  - name: Leland Russell
    affiliation: "1"
  - name: Yanchen Wu
    affiliation: "1"
  - name: Grayson W. White
    affiliation: "1"
  - name: Kelly K. McConville
    affiliation: "2"
  - name: George Gaines
    affiliation: "3"

affiliations:
  - index: 1
    name: Department of Mathematics and Statistics, Reed College
  - index: 2
    name: Department of Data Science, Bucknell University
  - index: 3
    name: Rocky Mountain Research Station, USDA Forest Service
date: 0 November 2024
bibliography: paper.bib
---

# Summary

A description of the high-level functionality and purpose of the software for a diverse, non-specialist audience.

A general summary of the paper. Guidelines and advice can be found [here](https://joss.readthedocs.io/en/latest/paper.html#checking-that-your-paper-compiles).

Wystan will do this

`basal` (Bayesian Small Area Estimation Library) offers more accessible Bayesian small area estimation methods to researchers and analysts working with survey data. Small area estimation combines survey observations with auxiliary information to estimate characteristics of areas with limited sample data. Bayesian approaches share information across related areas and use posterior draws to quantify uncertainty in the resulting estimates, a useful feature in applications such as forest inventory. Building on `brms` and `rstanarm`, `basal` provides a simplified interface based on standard R formula syntax for regression and mixed-effects modelling. A common workflow connects model specification, fitting, checking, and estimation. Users can choose standard small area models or specify custom regression models. Additional features include response transformations and two-stage models for zero-inflated data.


# Statement of need

A section that clearly illustrates the research purpose of the software and places it in the context of related work. This should clearly state what problems the software is designed to solve, who the target audience is, and its relation to other work.

Wystan will do this

Researchers and analysts working with survey data need estimates and measures of uncertainty for areas with few observations. In forest inventory, for example, field measurements and remotely sensed information can be combined to estimate local forest attributes. Bayesian models offer a flexible approach for these analyses, but implementing them and obtaining summaries for the areas of interest can require additional programming.

Packages such as `brms` [@burkner_brms_2017] and `rstanarm` [@gabry_rstanarm_2016] already offer accessible interfaces for Bayesian regression. Their use, however, still involves concepts and procedures that may be unfamiliar to those accustomed to frequentist model fitting. Applying these tools to small area estimation involves preparing model inputs, checking model fit, and translating model output into estimates and uncertainty summaries. 

`basal` offers a simplified interface for users who want to use Bayesian small area estimation through familiar regression modelling conventions. It brings standard small area models and custom regression formulas into a shared workflow for model specification, fitting, checking, and estimation. Automatic engine selection and default sampling settings reduce the need for manual configuration, while users can modify model formulas, select engine, and adjust sampling settings.

# State of the field

A description of how this software compares to other commonly-used packages in the research area. If related tools exist, provide a clear “build vs. contribute” justification explaining your unique scholarly contribution and why existing alternatives are insufficient.

Leland will do this

# Software design

An explanation of the trade-offs you weighed, the design/architecture you chose, and why it matters for your research application. This should demonstrate meaningful design thinking beyond a superficial code structure description.

Leland will do this

# Research impact statement

Evidence of realized impact (publications, external use, integrations) or credible near-term significance (benchmarks, reproducible materials, community-readiness signals). The evidence should be compelling and specific, not aspirational.

We will wait to do this

# Example Analysis

Example analysis with basal.

Wystan will do this

I checked the guideline and papers. Seems like we should probably not include this part.

# AI usage disclosure

Transparent disclosure of any use of generative AI in the software creation, documentation, or paper authoring. If no AI tools were used, state this explicitly. If AI tools were used, describe how they were used and how the quality and correctness of AI-generated content was verified.

We used no AI

# Acknowledgements

Whatever grants we have

# References

This is generated automatically

