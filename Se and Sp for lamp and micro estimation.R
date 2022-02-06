library(R2OpenBUGS)
library(tidyverse)

path <- file.path(getwd(), "s_hema_bugs")
model <- function() {
  
  ## LAMP PCR and Microscopy
  lm[1:4] ~ dmulti(lmv[1:4], n)
  lmv[1] <- lpi * (se_lmicro * se_lamp) + (1 - lpi) * ((1 - sp_lmicro) * (1 -                                                              sp_lamp))
  lmv[2] <- lpi * (se_lmicro * (1 - se_lamp)) + (1 - lpi) * ((1 - sp_lmicro) * sp_lamp)
  lmv[3] <- lpi * ((1 - se_lmicro) * se_lamp) + (1 - lpi) * (sp_lmicro * (1 - sp_lamp))
  lmv[4] <- lpi * ((1 - se_lmicro) * (1 - se_lamp)) + (1 - lpi) * (sp_lmicro * sp_lamp)
  
  #=== PRIOR ===#

  #Lamp
  se_lamp ~ dbeta(1, 1) ## Non-informative
  sp_lamp ~ dbeta(1, 1) ## Non-informative
  lpi ~ dbeta(a, b) ## Non-informative
  se_lmicro ~ dbeta(1, 1) ## Non-informative
  sp_lmicro ~ dbeta(1, 1) ## Non-informative
}
#pm is of the form : pm=c( (lamp pos; pcr pos), (lamp pos; pcr neg). (lamp neg; pcr pos), (lamp neg; pcr neg) )
data <- list(n = 150,lm = c(65, 8, 13, 64), a = 1, b = 1)

val <- .65
prev <- .05
inits <-function() {
  list(
    se_lmicro = val,
    sp_lmicro = val,
    se_lamp = val,
    sp_lamp = val,
    lpi = prev
  )
}

params <- c( "lpi", "se_lmicro", "sp_lmicro", "se_lamp", "sp_lamp") #, "ppv_lamp", "ppv_pcr", "npv_lamp", "npv_pcr" )
model.file <- paste(path, "/s_hematobium__lamp_micro_analysis.txt", sep = "")
write.model(model, model.file)
out <- bugs(working.directory = path, 
            data =  data, 
            inits =  inits, 
            parameters.to.save = params, 
            model.file =  model.file, 
            n.iter = 30000)

all(out$summary[, "Rhat"] < 1.1)
# prediction
out$summary[,c(1, 2, 5, 3, 7)] %>% round(3)

