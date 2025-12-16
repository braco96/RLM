# Proyecto: RLM

## 👤 Autoría
* **Refactorización:** Luis Bravo Collado
* **Base Teórica:** Inma Garcés Andrés (Estadística II)

## 📚 Fundamentación Teórica: Regresión Lineal Múltiple
Modelo estadístico para explicar una variable dependiente a partir de múltiples regresores.

### 1. Especificación del Modelo
El modelo matricial se define como:
$$ Y = X\beta + \epsilon $$

Donde:
* $\epsilon$: Término de error, $\epsilon \sim N_n(0, \sigma^2 I)$.

### 2. Estimación (MCO)
Los estimadores $\hat{\beta}$ minimizan la Suma de Cuadrados del Error ($SSE$):
$$ \hat{\beta} = (X'X)^{-1}X'Y $$

### 3. Validación
* **Coeficiente de Determinación:** $R^2 = 1 - \frac{SSE}{SST}$
* **Test Global F:** $F_0 = \frac{MSR}{MSE}$

## 💻 Sobre el Código
El script `RLM.R` implementa el cálculo matricial y la validación de supuestos mediante diagnóstico de residuos.
