context("Errors")
test_that("meaninterval", {

  library(estimation)
  GroupA<-runif(100, min=0, max=100)
  GroupB<-runif(100, min=0, max=100)

  RandomData<-data.frame(GroupA, GroupB)

  #Produce error for incorrect data frame type
  bob<-meanintervals(GroupA, GroupB, LandomData)
  bob<-meanintervals(GroupA, GroupB, "RandomData")

  #Correct data frame
  bob<-meanintervals(GroupA, GroupB, RandomData)

  #Produce error for incorrect x and y variable types

  bob<-meanintervals("GroupA", GroupB, RandomData)
  bob<-meanintervals(GroupA, "GroupB", RandomData)

  #Correct mean variable input
  bob<-meanintervals(GroupA, GroupB, RandomData)

  #Produce error for incorrect replicate type
  bob<-meanintervals(GroupA, GroupB, RandomData, replicates="f")

  #Correct replicate variable input
  bob<-meanintervals(GroupA, GroupB, RandomData, replicates=1000)

  #Produce error for incorrect steps type
  bob<-meanintervals(GroupA, GroupB, RandomData, steps="f")

  #Correct steps variable input
  bob<-meanintervals(GroupA, GroupB, RandomData, steps=10000)

})

test_that("geninterval", {

  library(intrvals)
  GroupA<-rnorm(50)
  GroupB<-rnorm(50)

  RandomData<-data.frame(GroupA, GroupB)

  rob<-glm(GroupA ~ GroupB, data=RandomData)

  #Produce error about incorrect model object
  bob<-genintervals("tob", "GroupB", method="lm")

  #Correct model object
  bob<-genintervals(rob, "GroupB", method="lm")

  #Produce error for incorrect method variable type
  bob<-genintervals(rob, "GroupB", method=5)

  #Correct method variable type
  bob<-genintervals(rob, "GroupB", method="lm")

  #Produce error for incorrect replicate input
  bob<-genintervals(rob, "GroupB", method="lm", replicate="f")

  #Correct replicate variable type
  bob<-genintervals(rob, "GroupB", method="lm", replicate=1000)

  #Produce error for incorrect steps input
  bob<-genintervals(rob, "GroupB", method="lm", steps="f")

  #Correct steps variable input
  bob<-genintervals(rob, "GroupB", method="lm", steps=1000)

})

test_that("metainterval", {

  library(estimation)
  library(metafor)

  #Produce random sample data
  GroupAData<-runif(20, min=0, max=100)
  GroupAMean<-round(mean(GroupAData), digits=2)
  GroupASD<-round(sd(GroupAData), digits=2)

  GroupBData<-runif(20, min=0, max=100)
  GroupBMean<-round(mean(GroupBData), digits=2)
  GroupBSD<-round(sd(GroupBData), digits=2)

  GroupCData<-runif(20, min=0, max=100)
  GroupCMean<-round(mean(GroupCData), digits=2)
  GroupCSD<-round(sd(GroupCData), digits=2)

  GroupDData<-runif(20, min=0, max=100)
  GroupDMean<-round(mean(GroupDData), digits=2)
  GroupDSD<-round(sd(GroupDData), digits=2)

  #Combine the data

  StudyName<-c("Study1", "Study2")
  MeanTreatment<-c(GroupAMean, GroupCMean)
  MeanControl<-c(GroupBMean, GroupDMean)
  SDTreatment<-c(GroupASD, GroupCSD)
  SDControl<-c(GroupBSD, GroupDSD)
  NTreatment<-c(20,20)
  NControl<-c(20,20)

  metadf<-data.frame(StudyName, MeanTreatment, MeanControl, SDTreatment, SDControl, NTreatment, NControl)

  #Use metafor to calculate the standardized mean difference

  library(metafor)

  dat<-escalc(measure="SMD", m1i=MeanTreatment, sd1i=SDTreatment, n1i=NTreatment,
              m2i=MeanControl, sd2i=SDControl, n2i=NControl, data=metadf)

  #Pool the data using a particular method. Here "FE" is the fixed-effects model

  res<-rma(yi, vi, data=dat, slab=paste(StudyName, sep=", "), method="FE", digits=2)

  #Calculate the intervals using the metainterval function

  metaf<-metaintervals(res)

  #Produce warning about incorrect model
  metaf<-metaintervals("lob")
  metaf<-metaintervals(2)
  metaf<-metaintervals(RandomData)

  #Correct model input
  metaf<-metaintervals(res)

  #Produce warning about incorrect measure input
  metaf<-metaintervals(res, measure=2)
  metaf<-metaintervals(res, measure=res)

  #Correct measure input
  metaf<-metaintervals(res, measure="norm")
  metaf<-metaintervals(res, measure="log")

  #Produce warning about incorrect steps input
  metaf<-metaintervals(res, steps="norm")

  #Correct steps input
  metaf<-metaintervals(res, steps=1000)

})
