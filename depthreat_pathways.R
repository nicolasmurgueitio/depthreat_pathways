setwd()

# Path Analyses with tasks (Psychopathology) (hypothesized model)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+age_60+sex+ses
      EF~a2*dep+age_60+sex+ses
      cbcl_intn_t~b1*neg.react+b2*EF+age_84+sex+ses
      cbcl_extn_t~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      cbcl_intn_t~~cbcl_extn_t
      
      #Indirect effects
      
      threat.int:=a1*b1
      dep.int:=a2*b2
      threat.ext:=a1*b3
      dep.ext:=a2*b4

      "
set.seed(455)
fit1 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit1, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit1, boot.ci.type = "perc")
parameterEstimates(fit1, boot.ci.type = "bca.simple")



# Path Analyses with tasks (Psychopathology) (reversed model (dep predicts emo and threat predicts cog))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*dep+age_60+sex+ses
      EF~a2*threat+age_60+sex+ses
      cbcl_intn_t~b1*neg.react+b2*EF+age_84+sex+ses
      cbcl_extn_t~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      cbcl_intn_t~~cbcl_extn_t
      
      #Indirect effects
      
      threat.int:=a1*b2
      dep.int:=a2*b1
      threat.ext:=a1*b4
      dep.ext:=a2*b3

      "
set.seed(455)
fit2 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")




summary(fit2, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit2, boot.ci.type = "perc")
parameterEstimates(fit2, boot.ci.type = "bca.simple")



# Path Analyses with tasks (Psychopathology) (restricted model (dep and threat predict both cog and emot)))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+dep+age_60+sex+ses
      EF~threat+a2*dep+age_60+sex+ses
      cbcl_intn_t~b1*neg.react+b2*EF+age_84+sex+ses+c1*threat+c2*dep
      cbcl_extn_t~b3*neg.react+b4*EF+age_84+sex+ses+c3*threat+c4*dep
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      cbcl_intn_t~~cbcl_extn_t
      
      #Indirect effects

      threat.int:=a1*b1
      dep.int:=a2*b2
      threat.ext:=a1*b3
      dep.ext:=a2*b4
      
      #Total effects
      
      threat.int.total:=a1*b1+c1
      dep.int.total:=a2*b2+c2
      threat.ext.total:=a1*b3+c3
       dep.ext.total:=a2*b4+c4
      
    
      "
set.seed(455)
fit3 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit3, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit3, boot.ci.type = "perc")
parameterEstimates(fit3, boot.ci.type = "bca.simple")


# Path Analyses with tasks (Psychopathology) (one path added)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+age_60+sex+ses+dep
      EF~a2*dep+age_60+sex+ses+threat
      cbcl_intn_t~b1*neg.react+b2*EF+age_84+sex+ses
      cbcl_extn_t~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      cbcl_intn_t~~cbcl_extn_t
      
      #Indirect effects
      
      threat.int:=a1*b1
      dep.int:=a2*b2
      threat.ext:=a1*b3
      dep.ext:=a2*b4

      "
set.seed(455)
fit4 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit4, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit4, boot.ci.type = "perc")
parameterEstimates(fit4, boot.ci.type = "bca.simple")



# Comparison Model 1 and Model 3


lavTestLRT(fit1, fit3)



# Comparison Model 1 and Model 4


lavTestLRT(fit1, fit4)



# Path Analyses with parent-report (Psychopathology) (hypothesized model)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+age_60+sex+ses
      CBQS_EFFC60~a2*dep+age_60+sex+ses
      cbcl_intn_t~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      cbcl_extn_t~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      cbcl_intn_t~~cbcl_extn_t
      
       #Indirect effects

      threat.int:=a1*b1
      dep.int:=a2*b2
      threat.ext:=a1*b3
      dep.ext:=a2*b4

      "
set.seed(455)
fit1 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit1, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit1, boot.ci.type = "perc")
parameterEstimates(fit1, boot.ci.type = "bca.simple")



# Path Analyses with parent-report (Psychopathology) (reversed model (dep predicts emo and threat predicts cog))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*dep+age_60+sex+ses
      CBQS_EFFC60~a2*threat+age_60+sex+ses
      cbcl_intn_t~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      cbcl_extn_t~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      cbcl_intn_t~~cbcl_extn_t
      
       #Indirect effects
    
    threat.int:=a1*b2
    dep.int:=a2*b1
    threat.ext:=a1*b4
    dep.ext:=a2*b3

    "
set.seed(455)
fit2 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit2, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit2, boot.ci.type = "perc")
parameterEstimates(fit2, boot.ci.type = "bca.simple")


# Path Analyses with parent-report (Psychopathology) (restricted model (dep and threat predict both cog and emot)))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+dep+age_60+sex+ses
      CBQS_EFFC60~a2*threat+dep+age_60+sex+ses
      cbcl_intn_t~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses+c1*threat+c2*dep
      cbcl_extn_t~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses+c3*threat+c4*dep
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      cbcl_intn_t~~cbcl_extn_t
      
       #Indirect effects
    
    threat.int:=a1*b1
    dep.int:=a2*b2
    threat.ext:=a1*b3
    dep.ext:=a2*b4
    
    #Total effects
    
    threat.int.total:=a1*b1+c1
    dep.int.total:=a2*b2+c2
    threat.ext.total:=a1*b3+c3
    dep.ext.total:=a2*b4+c4

      "
set.seed(455)
fit3 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit3, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit3, boot.ci.type = "perc")
parameterEstimates(fit3, boot.ci.type = "bca.simple")


# Path Analyses with parent-report (Psychopathology) (hypothesized model)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+age_60+sex+ses+dep
      CBQS_EFFC60~a2*dep+age_60+sex+ses+threat
      cbcl_intn_t~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      cbcl_extn_t~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      cbcl_intn_t~~cbcl_extn_t
      
       #Indirect effects

      threat.int:=a1*b1
      dep.int:=a2*b2
      threat.ext:=a1*b3
      dep.ext:=a2*b4

      "
set.seed(455)
fit4 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit4, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit4, boot.ci.type = "perc")
parameterEstimates(fit4, boot.ci.type = "bca.simple")



# Comparison Model 1 and Model 3


lavTestLRT(fit1, fit3)



# Comparison Model 1 and Model 4


lavTestLRT(fit1, fit4)



# Path Analyses with tasks (school) (hypothesized model)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+age_60+sex+ses
      EF~a2*dep+age_60+sex+ses
      WJ3_AP_W~b1*neg.react+b2*EF+age_84+sex+ses
      WJ3_LWI_W~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects

      threat.ap:=a1*b1
      dep.ap:=a2*b2
      threat.lw:=a1*b3
      dep.lw:=a2*b4

      "
set.seed(455)
fit1 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit1, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit1, boot.ci.type = "perc")
parameterEstimates(fit1, boot.ci.type = "bca.simple")



# Path Analyses with tasks (school) (reversed model (dep predicts emo and threat predicts cog))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*dep+age_60+sex+ses
      EF~a2*threat+age_60+sex+ses
      WJ3_AP_W~b1*neg.react+b2*EF+age_84+sex+ses
      WJ3_LWI_W~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects
       
       threat.ap:=a1*b2
    dep.ap:=a2*b1
    threat.lw:=a1*b4
    dep.lw:=a2*b3
    

      "
set.seed(455)
fit2 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit2, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit2, boot.ci.type = "perc")
parameterEstimates(fit2, boot.ci.type = "bca.simple")


# Path Analyses with tasks (school) (restricted model (dep and threat predict both cog and emot)))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+dep+age_60+sex+ses
      EF~threat+a2*dep+age_60+sex+ses
      WJ3_AP_W~b1*neg.react+b2*EF+age_84+sex+ses+c1*threat+c2*dep
      WJ3_LWI_W~b3*neg.react+b4*EF+age_84+sex+ses+c3*threat+c4*dep
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects
    
    threat.ap:=a1*b1
    dep.ap:=a2*b2
    threat.lw:=a1*b3
    dep.lw:=a2*b4
    
    #Total effects
    
    threat.ap.total:=a1*b1+c1
    dep.ap.total:=a2*b2+c2
    threat.lw.total:=a1*b3+c3
    dep.lw.total:=a2*b4+c4

      "
set.seed(455)
fit3 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit3, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit3, boot.ci.type = "perc")
parameterEstimates(fit3, boot.ci.type = "bca.simple")


# Path Analyses with tasks (school) (one path aded)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      neg.react~a1*threat+age_60+sex+ses+dep
      EF~a2*dep+age_60+sex+ses+threat
      WJ3_AP_W~b1*neg.react+b2*EF+age_84+sex+ses
      WJ3_LWI_W~b3*neg.react+b4*EF+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      EF~~neg.react
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects

      threat.ap:=a1*b1
      dep.ap:=a2*b2
      threat.lw:=a1*b3
      dep.lw:=a2*b4

      "
set.seed(455)
fit4 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit4, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit4, boot.ci.type = "perc")
parameterEstimates(fit4, boot.ci.type = "bca.simple")



# Comparison Model 1 and Model 3


lavTestLRT(fit1, fit3)



# Comparison Model 1 and Model 4


lavTestLRT(fit1, fit4)




# Path Analyses with parent-report (school) (hypothesized model)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+age_60+sex+ses
      CBQS_EFFC60~a2*dep+age_60+sex+ses
      WJ3_AP_W~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      WJ3_LWI_W~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects

      threat.ap:=a1*b1
      dep.ap:=a2*b2
      threat.lw:=a1*b3
      dep.lw:=a2*b4

      "
set.seed(455)
fit1 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit1, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit1, boot.ci.type = "perc")
parameterEstimates(fit1, boot.ci.type = "bca.simple")
modindices(fit1, sort = TRUE, maximum.number = 10)
residuals <- residuals(fit1, type="cor")
residual_cov <- residuals$cov
print(residual_cov)


# Path Analyses with parent-report (school) (reversed model (dep predicts emo and threat predicts cog))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*dep+age_60+sex+ses
      CBQS_EFFC60~a2*threat+age_60+sex+ses
      WJ3_AP_W~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      WJ3_LWI_W~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects
    
    threat.ap:=a1*b2
    dep.ap:=a2*b1
    threat.lw:=a1*b4
    dep.lw:=a2*b3

      "
set.seed(455)
fit2 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit2, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit2, boot.ci.type = "perc")
parameterEstimates(fit2, boot.ci.type = "bca.simple")


# Path Analyses with parent-report (school) (restricted model (dep and threat predict both cog and emot)))


mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+dep+age_60+sex+ses
      CBQS_EFFC60~a2*threat+dep+age_60+sex+ses
      WJ3_AP_W~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses+c1*threat+c2*dep
      WJ3_LWI_W~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses+c3*threat+c4*dep
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects
    
    threat.ap:=a1*b1
    dep.ap:=a2*b2
    threat.lw:=a1*b3
    dep.lw:=a2*b4
    
    #Total effects
    
    threat.ap.total:=a1*b1+c1
    dep.ap.total:=a2*b2+c2
    threat.lw.total:=a1*b3+c3
    dep.lw.total:=a2*b4+c4
    

      "
set.seed(455)
fit3 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit3, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit3, boot.ci.type = "perc")
parameterEstimates(fit3, boot.ci.type = "bca.simple")




# Path Analyses with parent-report (school) (one added path)



mod<-"
      #Regressions
      threat~ses
      dep~ses
      EmoReact~a1*threat+age_60+sex+ses+dep
      CBQS_EFFC60~a2*dep+age_60+sex+ses+threat
      WJ3_AP_W~b1*EmoReact+b2*CBQS_EFFC60+age_84+sex+ses
      WJ3_LWI_W~b3*EmoReact+b4*CBQS_EFFC60+age_84+sex+ses
    
      #Covariances
      
      threat~~dep
      CBQS_EFFC60~~EmoReact
      WJ3_AP_W~~WJ3_LWI_W
      
       #Indirect effects

      threat.ap:=a1*b1
      dep.ap:=a2*b2
      threat.lw:=a1*b3
      dep.lw:=a2*b4

      "
set.seed(455)
fit4 <- sem(mod, data=data, meanstructure=TRUE,  missing = "FIML",fixed.x=FALSE, se="bootstrap")



summary(fit4, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fit4, boot.ci.type = "perc")
parameterEstimates(fit4, boot.ci.type = "bca.simple")



# Comparison Model 1 and Model 3


lavTestLRT(fit1, fit3)



# Comparison Model 1 and Model 4


lavTestLRT(fit1, fit4)



