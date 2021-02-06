---
title: 'ARDL: An R package for the analysis of level relationships'
tags:
- ARDL
- bounds test
- cointegration
- econometrics
- R
date: "6 February 2021"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
authors:
- name: Kleanthis Natsiopoulos
  orcid: 0000-0003-1180-2984
  affiliation: 1
- name: Nickolaos G. Tzeremes
  orcid: 0000-0002-6938-3404
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Department of Economics, University of Thessaly, 28th October, 78, 38333,
    Volos, Greece
  index: 1
---

# Summary

Autoregressive Distributed Lag (ARDL) and Error Correction Models (ECM) are widely used in various economic applications as they are very flexible. Also, these models are used in the context of cointegration analysis as a platform to test and analyze the levels (long-run) relationship between variables. One of the most popular such tests is the bounds test proposed by @Pesaran2001 which allows testing for cointegration while at the same time estimates the level relationship.

# Statement of need

`ARDL` [@ardl-pkg] is an `R` package that aims to help users in the modeling process of ARDL and ECM and it also provides the tools towards the bounds test for cointegration. `ARDL` is implemented in such a way that researchers can use it as a full featured tool for this specific type of analysis and students of all levels can be aware of how each piece of code works through the analytical [manual](https://cran.r-project.org/web/packages/ARDL/ARDL.pdf) and the examples which cover every functionality of the package. A recent example is @Qiu2021 where the software was used to forecast tourist arrivals through an ARDL model, amonth other methods.

# State of the field

`ARDL` distinguishes itself from other related `R` packages like `dLagM` [@dlagm-pkg] and `dynamac` [@dynamac-pkg] in the sense that it is specifically designed to address a particular problem throughout every phase of modeling, testing and interpretation. It includes a rich set of dedicated tools, accompanied by an analytical manual that describes the mathematical process of every function.

On the other hand, `dLagM` and `dynamac` provide additional plotting functionalities and post-estimation diagnostics (serial correlation tests etc). The design of the `ARDL` packages differs from the pre-mentioned packages as it is not an *all-in-one* package. It is designed so that every exported object is of commonly used `R` classes so that it can be easily combined with other packages, each of which are also dedicated to a specific part of the post-estimation part of the analysis.

In addition, the `ARDL` packages natively supports time-series data, thus sub-sample and balanced sample estimations are possible. It also allows to specify any level of significance for the bounds F-test and t-tests, and includes p-values and exact sample critical values for any possible combination. Also, the estimation of long-run, short-run and interim multipliers accompanied by standard errors and p-values as well as the cointegrating equation are some of the available features.

# Breaking down the ARDL package

## Model estimation

The package does not explicitly connects the modeling with the bounds test, as the ARDL and ECM model may well be used independently in other research approaches too. This way each function is dedicated to perform a very specific part of the whole process.

An $ARDL(p, q_1, ..., q_k)$ model can be fully specified by its order. The `ardl` function implements the following formula:

\begin{equation}\label{eq:ardl}
  y_{t} = c_{0} + c_{1}t + \sum_{i=1}^{p}b_{y,i}y_{t-i} + \sum_{j=1}^{k}\sum_{l=0}^{q_{j}}b_{j,l}x_{j,t-l} + \epsilon_{t}
\end{equation}

An Unrestricted ECM (UECM) has a 1:1 relationship with the $ARDL(p, q_1, ..., q_k)$ model. The `uecm` function estimates it using the formula:

\begin{equation}\label{eq:uecm}
  \Delta y_{t} = c_{0} + c_{1}t + \pi_{y}y_{t-1} + \sum_{j=1}^{k}\pi_{j}x_{j,t-1} + \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} + \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} + \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \epsilon_{t}
\end{equation}

The Restricted ECM (RECM) can also be fully described by the order of the underlying ARDL and the case for potential restriction in the deterministic parameters. The `recm` function performs the modeling using:

\begin{equation}\label{eq:recm}
  \Delta y_{t} = c_{0} + c_{1}t + \sum_{i=1}^{p-1}\psi_{y,i}\Delta y_{t-i} + \sum_{j=1}^{k}\sum_{l=1}^{q_{j}-1} \psi_{j,l}\Delta x_{j,t-l} + \sum_{j=1}^{k}\omega_{j}\Delta x_{j,t} + \pi_{y}ECT_{t} + \epsilon_{t}
\end{equation}

> Under Case 1:

\begin{equation}\label{eq:recm1}
  \begin{split}
    &c_{0} =c_{1}=0 \\
    &ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})
  \end{split}
\end{equation}

> Under Case 2:

\begin{equation}\label{eq:recm2}
  \begin{split}
    &c_{0} =c_{1}=0 \\
    &ECT = y_{t-1} - (\mu + \sum_{j=1}^{k}\theta_{j} x_{j,t-1})
  \end{split}
\end{equation}

> Under Case 3:

\begin{equation}\label{eq:recm3}
  \begin{split}
    &c_{1} =0 \\
    &ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})
  \end{split}
\end{equation}

> Under Case 4:

\begin{equation}\label{eq:recm4}
  \begin{split}
    &c_{1} =0 \\
    &ECT = y_{t-1} - (\delta(t-1)+ \sum_{j=1}^{k} \theta_{j} x_{j,t-1})
  \end{split}
\end{equation}

> Under Case 5:

\begin{equation}\label{eq:recm5}
  ECT = y_{t-1} - (\sum_{j=1}^{k} \theta_{j} x_{j,t-1})
\end{equation}

## Model relationships

\autoref{fig:relationships} shows the relationships between the regression model objects. The bounds tests and all the other functions of the package also have this kind of interconnectivity as they inherit all the necessary information.

![Interconnection between ARDL, UECM and RECM. \label{fig:relationships}](relationships.png){ width=70% }

Where $ARDL(p, q_1, ..., q_k)$ represent the ARDL order and Case is the restriction of the deterministic parameters. When an arrow points from one model or information (order or case) to another model, it can be interpreted as the first one can fully describe the second one. When it crosses the case it means that this information is also needed.

## Bounds test

The functions `bounds_f_test` and `bounds_t_test` return a typical `htest` object and they perform a Wald or t test respectively on an UECM. The input of the functions can also be an ARDL model as they are interconnected as described above. The hypothesis tests for the bounds F-test and t-test are based on \autoref{eq:uecm}. For the bounds F-test:

> Cases 1, 3, 5:

\begin{equation}\label{eq:fbounds135}
  \begin{split}
    \mathbf{H_{0}:} & \pi_{y} = \pi_{1} = \dots = \pi_{k} = 0 \\
    \mathbf{H_{1}:} & \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq 0
  \end{split}
\end{equation}

> Cases 2:

\begin{equation}\label{eq:fbounds2}
  \begin{split}
    \mathbf{H_{0}:} & \pi_{y} = \pi_{1} = \dots = \pi_{k} = c_{0} = 0 \\
    \mathbf{H_{1}:} & \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq c_{0} \neq 0
  \end{split}
\end{equation}

> Cases 4:

\begin{equation}\label{eq:fbounds4}
  \begin{split}
    \mathbf{H_{0}:} & \pi_{y} = \pi_{1} = \dots = \pi_{k} = c_{1} = 0 \\
    \mathbf{H_{1}:} & \pi_{y} \neq \pi_{1} \neq \dots \neq \pi_{k} \neq c_{1} \neq 0
  \end{split}
\end{equation}

Finally, the bounds t-test can be represented as:

\begin{equation}\label{eq:tbounds}
  \begin{split}
    \mathbf{H_{0}:} & \pi_{y} = 0 \\
    \mathbf{H_{1}:} & \pi_{y} \neq 0
  \end{split}
\end{equation}

## Making inference after cointegration

After the modeling part and if a cointegrating relationship can be established, the long-run (but also the short-run or interim) multipliers can be computed using the `multipliers` function. Note that it is irrelevant whether they are estimated based on an ARDL (\autoref{eq:ardl}) or an UECM (\autoref{eq:uecm}), in terms of the results. The formulas for the multipliers for the constant and the linear trend are:

> When the input is an ARDL:

\begin{equation}\label{eq:mult-c-ardl}
  \mu = \frac{c_{0}}{1-\sum_{i=1}^{p}b_{y,i}}
\end{equation}

\begin{equation}\label{eq:mult-t-ardl}
  \delta = \frac{c_{1}}{1-\sum_{i=1}^{p}b_{y,i}}
\end{equation}

> When the input in an UECM:

\begin{equation}\label{eq:mult-c-uecm}
  \mu = \frac{c_{0}}{-\pi_{y}}
\end{equation}

\begin{equation}\label{eq:mult-t-uecm}
  \delta = \frac{c_{1}}{-\pi_{y}}
\end{equation}

The relationships for the short-run multipliers are:

> When the input is an ARDL:

\begin{equation}\label{eq:mult-sr-ardl}
  \frac{\partial y_{t}}{\partial x_{j,t}} = \frac{b_{j,0}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

> When the input is an UECM:

\begin{equation}\label{eq:mult-sr-uecm}
  \frac{\partial y_{t}}{\partial x_{j,t}} = \frac{\omega_{j}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

The relationships for the interim multipliers are:

> When the input is an ARDL:

\begin{equation}\label{eq:mult-int-ardl}
  \frac{\partial y_{t+s}}{\partial x_{j,t}} = \frac{\sum_{l=1}^{s}b_{j,l}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k \;\;\;\;\; s \in \{0,\dots,q_{j}\}
\end{equation}

> When the input is an UECM:

\begin{equation}\label{eq:mult-int-uecm}
  \frac{\partial y_{t+s}}{\partial x_{j,t}} = \frac{\pi_{j} + \psi_{j,s}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k \;\;\;\;\; s \in \{1,\dots,q_{j}-1\}
\end{equation}

The relationships for the long-run multipliers are:

> When the input is an ARDL:

\begin{equation}\label{eq:mult-lr-ardl}
  \frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\sum_{l=0}^{q_{j}}b_{j,l}}{1-\sum_{i=1}^{p}b_{y,i}} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

> When the input is an UECM:

\begin{equation}\label{eq:mult-lr-uecm}
  \frac{\partial y_{t+\infty}}{\partial x_{j,t}} = \theta_{j} = \frac{\pi_{j}}{-\pi_{y}} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

Lastly, the cointegrating relationship vector can be constructed using the  function `coint_eq`. The formula is:

> Cases 1, 3, 5:

\begin{equation}\label{eq:cointeq-135}
  CointEq_{t} = \sum_{j=1}^{k}\theta_{j}x_{j,t} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

> Case 2:

\begin{equation}\label{eq:cointeq-2}
  CointEq_{t} = \mu + \sum_{j=1}^{k}\theta_{j}x_{j,t} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

> Case 4:

\begin{equation}\label{eq:cointeq-4}
  CointEq_{t} = \delta + \sum_{j=1}^{k}\theta_{j}x_{j,t} \;\;\;\;\; \forall j=1,\dots,k
\end{equation}

# Conclusion

Using the intuitive API of the `ARDL` package, even the most complex ARDL, UECM or RECM model can be fully specified using the $ARDL(p, q_{1}, ..., q_{k})$ order and the case for the restriction of the constant and linear deterministic parameters. The interconnection between the objects allows for further post-estimation testing. Finally, the detailed estimation formulas are provided in order to enhance the understanding of every step of the analysis giving a reference point for the equations in publications.

# Acknowledgments

We want to acknowledge Nikolaos Chatsios for designing the package's logo and the diagram in \autoref{fig:relationships}.

# References
