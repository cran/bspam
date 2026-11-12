## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE, message=FALSE, warning=FALSE---------------------------
library(tidyverse)
library(bspam)
data("passage2")

## ----desc1--------------------------------------------------------------------
passage2 %>% 
  group_by(id.passage) %>% 
  summarise(n.students = n(),
            numwords.pass = unique(numwords.pass),
            mean.wrc = mean(wrc),
            mean.sec = mean(sec)) %>% 
  ungroup() %>% 
  mutate(rate.wrc = mean.wrc/numwords.pass,
         mean.sec.per10 = mean.sec/numwords.pass*10) %>% 
  knitr::kable(digits = 2)

## ----desc2--------------------------------------------------------------------
passage2 %>% 
  group_by(id.student) %>% 
  summarise(n.passages = n(),
            tot.wrc = sum(wrc),
            tot.sec = sum(sec),
            obs.wcpm = tot.wrc/tot.sec*60) %>% 
  ungroup() %>% 
  knitr::kable(digits = 2)

## ----calibration1, message=FALSE----------------------------------------------
MCEM_run1 <- fit.model(data = passage2,
                       person.id = "id.student",
                       task.id = "id.passage",
                       max.counts = "numwords.pass",
                       obs.counts = "wrc",
                       time = "sec",
                       k.in = 5,
                       reps.in = 50,
                       est = "mcem")

## -----------------------------------------------------------------------------
MCEM_run1

## ----out1---------------------------------------------------------------------
MCEM_run1$task.param %>% knitr::kable(digits = 2)

## ----out2---------------------------------------------------------------------
MCEM_run1$hyper.param %>% knitr::kable(digits = 3)

## ----calibration2, eval=FALSE-------------------------------------------------
# MCEM_run2 <- fit.model(data = passage2,
#                        person.id = "id.student",
#                        task.id = "id.passage",
#                        max.counts = "numwords.pass",
#                        obs.counts = "wrc",
#                        time = "sec",
#                        k.in = 5,
#                        reps.in = 50,
#                        est = "mcem",
#                        se = "analytic")

## ----out3, eval=FALSE---------------------------------------------------------
# MCEM_run2$task.param %>% knitr::kable(digits = 2)

## ----out4, eval=FALSE---------------------------------------------------------
# MCEM_run2$hyper.param %>% knitr::kable(digits = 3)

## ----eval = FALSE-------------------------------------------------------------
# WCPM_all <- scoring(calib.data=MCEM_run,
#                     data = passage2,
#                     person.id = "id.student",
#                     occasion = "occasion",
#                     group = "grade",
#                     task.id = "id.passage",
#                     max.counts = "numwords.pass",
#                     obs.counts = "wrc",
#                     time = "sec",
#                     est = "map",
#                     se = "analytic",
#                     type="orf")
# summary(WCPM_all)

