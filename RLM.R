# ==============================================================================
# PROYECTO: RLM (Regresión Lineal Múltiple)
# AUTOR: Luis Bravo Collado (Braco96)
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, corrplot, lmtest, car, install=FALSE)

cargar_datos <- function(f) {
  rutas <- c(f, file.path("RLM", f), file.path("..", f))
  for(r in rutas) if(file.exists(r)) { load(r, .GlobalEnv); return(TRUE) }
  return(FALSE)
}

print("=== PARTE 1: CASO PRÁCTICO (Species) ===")
if(cargar_datos("Species.Rdata") && exists("Species")) {
  vars <- Species[, sapply(Species, is.numeric)]
  print("Correlaciones:"); print(round(cor(vars, use="complete.obs"), 2))
  
  m_step <- step(lm(NumSpecies ~ Area + Elevation + SoilTypes + Latitude + DistBritain, data=Species), trace=0)
  print("Resumen Stepwise:"); print(summary(m_step))
  
  print("Diagnóstico:")
  print(shapiro.test(residuals(m_step)))
  print(bptest(m_step))
} else { warning("No se encontró Species.Rdata") }

print("=== PARTE 2: ANEXO TEÓRICO (Papel) ===")
df_sim <- data.frame(R=c(160,171,175,182,184,181,188,193,200), 
                     M=c(10,15,15,20,20,20,25,25,30), 
                     T=c(150,180,150,180,180,150,180,150,180))
m_sim <- lm(R ~ M + T, data=df_sim)
print(summary(m_sim))
