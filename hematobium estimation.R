library(R2OpenBUGS)
library(tidyverse)

path <- file.path(getwd(), "s_hema_bugs")
model <- function() {

  #=== LIKELIHOOD   ===#
   ## PCR and Microscopy
  # pm[1:4] ~ dmulti(pmv[1:4], n)
  # pmv[1] <- ppi * (se_pmicro * se_pcr) + (1 - ppi) * ((1 - sp_pmicro) * (1 -                                                              sp_pcr))
  # pmv[2] <- ppi * (se_pmicro * (1 - se_pcr)) + (1 - ppi) * ((1 - sp_pmicro) * sp_pcr)
  # pmv[3] <- ppi * ((1 - se_pmicro) * se_pcr) + (1 - ppi) * (sp_pmicro * (1 - sp_pcr))
  # pmv[4] <- ppi * ((1 - se_pmicro) * (1 - se_pcr)) + (1 - ppi) * (sp_pmicro * sp_pcr)
  # 0.9 <- (se_pcr * ppi) / (se_pcr * ppi) + ((1 - sp_pcr) * (1 - ppi))
  # 0.9 <- (sp_pcr * (1 - ppi)) / (sp_pcr * (1 - ppi)) + ((1 - se_pcr) * ppi)
  
  # ## Realtime PCR and Microscopy
  # rm[1:4] ~ dmulti(rmv[1:4], n)
  # rmv[1] <- rpi * (se_rmicro * se_real) + (1 - rpi) * ((1 - sp_rmicro) * (1 -                                                              sp_real))
  # rmv[2] <- rpi * (se_rmicro * (1 - se_real)) + (1 - rpi) * ((1 - sp_rmicro) * sp_real)
  # rmv[3] <- rpi * ((1 - se_rmicro) * se_real) + (1 - rpi) * (sp_rmicro * (1 - sp_real))
  # rmv[4] <- rpi * ((1 - se_rmicro) * (1 - se_real)) + (1 - rpi) * (sp_rmicro * sp_real)
  # 
  
  ## LAMP PCR and Microscopy
  lm[1:4] ~ dmulti(lmv[1:4], n)
  lmv[1] <- lpi * (se_lmicro * se_lamp) + (1 - lpi) * ((1 - sp_lmicro) * (1 -                                                              sp_lamp))
  lmv[2] <- lpi * (se_lmicro * (1 - se_lamp)) + (1 - lpi) * ((1 - sp_lmicro) * sp_lamp)
  lmv[3] <- lpi * ((1 - se_lmicro) * se_lamp) + (1 - lpi) * (sp_lmicro * (1 - sp_lamp))
  lmv[4] <- lpi * ((1 - se_lmicro) * (1 - se_lamp)) + (1 - lpi) * (sp_lmicro * sp_lamp)
  
  #=== PRIOR ===#
   #PCR
  # se_pcr ~ dbeta(1, 1) ## Non-informative
  # sp_pcr ~ dbeta(1, 1) ## Non-informative
  # ppi ~ dbeta(a, b) ## Non-informative
  # se_pmicro ~ dbeta(1, 1) ## Non-informative
  # sp_pmicro ~ dbeta(1, 1) ## Non-informative
  # 
  # #Real
  # se_real ~ dbeta(1, 1) ## Non-informative
  # sp_real ~ dbeta(1, 1) ## Non-informative
  # rpi ~ dbeta(a, b) ## Non-informative
  # se_rmicro ~ dbeta(1, 1) ## Non-informative
  # sp_rmicro ~ dbeta(1, 1) ## Non-informative
  
  #Lamp
  se_lamp ~ dbeta(1, 1) ## Non-informative
  sp_lamp ~ dbeta(1, 1) ## Non-informative
  lpi ~ dbeta(a, b) ## Non-informative
  se_lmicro ~ dbeta(1, 1) ## Non-informative
  sp_lmicro ~ dbeta(1, 1) ## Non-informative
}
#pm is of the form : pm=c( (lamp pos; pcr pos), (lamp pos; pcr neg). (lamp neg; pcr pos), (lamp neg; pcr neg) )
data <- list(n = 150, 
             # pm = c(60, 13, 7, 70), 
             # rm = c(70, 3, 14, 63), 
             lm = c(65, 8, 13, 64), 
             a = 1, b = 1
             )

val <- .65
prev <- .05
inits <-function() {
    list(
      # se_pmicro = val,
      # sp_pmicro = val,
      # se_pcr = val,
      # sp_pcr = val,
      # ppi = prev,
      # se_rmicro = val,
      # sp_rmicro = val,
      # se_real = val,
      # sp_real = val,
      # rpi = prev,
      se_lmicro = val,
      sp_lmicro = val,
      se_lamp = val,
      sp_lamp = val,
      lpi = prev
    )
  }

params <- c(#"ppi", "se_pmicro", "sp_pmicro", "sp_pcr", "se_pcr" ,
            #"rpi", "se_rmicro", "sp_rmicro", "se_real", "sp_real",
            "lpi", "se_lmicro", "sp_lmicro", "se_lamp", "sp_lamp") #, "ppv_lamp", "ppv_pcr", "npv_lamp", "npv_pcr" )
model.file <- paste(path, "/s_hematobium_analysis.txt", sep = "")
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

