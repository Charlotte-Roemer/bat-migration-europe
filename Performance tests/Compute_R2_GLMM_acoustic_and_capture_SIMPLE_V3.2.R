
library(tidyverse)
library(data.table)
library(beepr)
library(glmmTMB)
library(DHARMa)
library(performance)
library(bestNormalize)

Comparison_success = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/Comparison_success.csv")


Sp = "Nyclei"


# Normalise response
testx = which(names(Comparison_success)==Sp)
Comparison_success$Response_normal = orderNorm(as.numeric(Comparison_success[, ..testx][[1]]))$x.t
Comparison_success$JJulien = yday(Comparison_success$DATE)

# Build different response variables to test them
Comparison_success$TOT_act_cor=NULL
testR = grep(paste0("^", Sp, 
                    "_prop", 
                    "$"), names(Comparison_success))
testR2 = grep(paste0("_cor", "$"), names(Comparison_success))
testR3 = grep(paste0("^", Sp, "_cor", "$"), names(Comparison_success))
Response = names(Comparison_success[, ..testR])
Response2 = names(Comparison_success[, ..testR3])
Comparison_success$TOT_act_cor = apply(Comparison_success[, ..testR2], 1, FUN = sum)

# Build different fix effects to test them
testFE = grep(paste0("^", Sp, "_prop_capture", "$"), names(Comparison_success))
#testFE = grep(paste0("^", Sp, "_capture_PresAbs", "$"), names(Comparison_success))
FixEffect = names(Comparison_success[, ..testFE])

# Different formulaes to test them
# Formula1=as.formula(paste0("Response_normal~",
#                            FixEffect,
#                            "+ (1|INSEE_COM) + (1|DATE)"))
# Formula2=as.formula(paste0("as.integer(", Response, ")", "~",
#                            FixEffect,
#                            "+ (1|INSEE_COM) + (1|DATE)"))
Formula3=as.formula(paste0("Response_normal~",
                           FixEffect, "+ poly(JJulien, 2)",
                           "+ (1|INSEE_COM) + (1|SpYear)"))
Formula4=as.formula(paste0(Response, "~",
                           FixEffect, "+ poly(JJulien, 2)",
                           "+ (1|INSEE_COM) + (1|SpYear)"))
Formula5=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
                           FixEffect, "+ poly(JJulien, 2)",
                           "+ (1|INSEE_COM) + (1|SpYear)"))
Formula6=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
                           FixEffect, "+ poly(JJulien, 3)",
                           "+ (1|INSEE_COM) + (1|SpYear)"))
Formula7=as.formula(paste0("cbind(", Response2, ", TOT_act_cor)", "~",
                           FixEffect, "+ poly(JJulien, 3)",
                           "+ (1|INSEE_COM)"))

# Different models
# Mod_Species_1 = glmmTMB(Formula1,
#                         data = Comparison_success,
#                         family = gaussian())
# Mod_Species_2 = glmmTMB(Formula2, 
#                         data = Comparison_success,
#                         family = nbinom2()) 
# Mod_Species_3 = glmmTMB(Formula3,
#                         data = Comparison_success,
#                         family = gaussian())
# Mod_Species_4 = glmmTMB(Formula4,
#                         data = Comparison_success,
#                         family = nbinom2())
# Mod_Species_5 = glmmTMB(Formula5,
#                         data = Comparison_success,
#                         family = binomial())
Mod_Species_6 = glmmTMB(Formula6,
                        data = Comparison_success,
                        family = binomial())
Mod_Species_7 = glmmTMB(Formula7,
                        data = Comparison_success,
                        family = binomial())

# simulationOutput_1 <- simulateResiduals(fittedModel = Mod_Species_1, plot = F)
# simulationOutput_2 <- simulateResiduals(fittedModel = Mod_Species_2, plot = F)
#simulationOutput_3 <- simulateResiduals(fittedModel = Mod_Species_3, plot = F)
simulationOutput_7 <- simulateResiduals(fittedModel = Mod_Species_7, plot = F)

plot(simulationOutput_7)


