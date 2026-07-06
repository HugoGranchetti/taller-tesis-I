# taller-tesis-I
Repositorio para la entrega final del trabajo de la asignatura "Taller de Tesis I" de la Maestría en Explotación de Datos y Descubrimiento del Conocimiento (2026).

# Representatividad en ensayos clínicos de fase III: aplicación de la minería de datos al análisis de criterios de inclusión y exclusión

Análisis de patrones de exclusión poblacional (edad, sexo, índice de masa
corporal y origen étnico) en ensayos clínicos de fase III registrados en
ClinicalTrials.gov, mediante clustering jerárquico, clasificación
supervisada con Random Forest y análisis de texto libre sobre criterios
de elegibilidad.

Trabajo final — Taller de Tesis I (2026)
Maestría en Explotación de Datos y Descubrimiento del Conocimiento (UBA)
Autor: Hugo Granchetti — Grupo 2

## Contenido del repositorio

- `Hugo Granchetti - Representatividad en ensayos clínicos de fase III.R` — script único con todo el análisis: importación
  y preparación de datos, EDA, clustering, modelado supervisado y análisis de texto libre.
- `session_info.txt` — versión de R y de los paquetes utilizados, generado con `sessionInfo()`.
- `README.md` — este documento.

## Datos de origen

Los datos no se incluyen en este repositorio por su tamaño. Se obtienen directamente desde la API de ClinicalTrials.gov (formato JSON, API v2):

1. Ir a https://clinicaltrials.gov/search?aggFilters=phase:3,studyType:int
2. Filtrar por: Study Type = Interventional, Phase = Phase 3.
3. Exportar los resultados en formato JSON (botón "Download" → JSON).
4. Guardar el archivo descargado como `ctg-studies.json` en el mismo directorio donde se ejecuta el script.

El dataset utilizado en este trabajo contiene 48926 estudios, descargado el 23 de abril de 2026.
Dado que ClinicalTrials.gov se actualiza continuamente, una nueva descarga puede arrojar un número distinto de registros y, por lo tanto, resultados algo diferentes a los reportados en el informe.

## Requisitos

- R ≥ 4.3 (ver versión exacta utilizada en `session_info.txt`)
- Paquetes de R:

```r
install.packages(c(
  "tidyverse", "lubridate", "scales", "jsonlite", "patchwork", "cluster", "factoextra", "caret", "randomForest", "pROC", "nnet"
))
```

## Cómo ejecutar el análisis

1. Clonar este repositorio:
```bash
   git clone https://github.com/HugoGranchetti/taller-tesis-I.git
   cd taller-tesis-I
```
2. Descargar `ctg-studies.json` siguiendo las instrucciones de la sección "Datos de origen" y colocarlo en el mismo directorio.
3. Abrir `Hugo Granchetti - Representatividad en ensayos clínicos de fase III.R` en RStudio (o ejecutarlo desde la terminal con `Rscript analisis_completo_json.R`).
4. Instalar los paquetes requeridos si no están disponibles (ver sección "Requisitos").
5. Ejecutar el script en orden secuencial. El script está organizado en siete secciones numeradas:
   1. Librerías
   2. Importación y preparación de datos
   3. Estadística descriptiva (EDA)
   4. Clustering jerárquico
   5. Aprendizaje supervisado - Random Forest
   6. Análisis en profundidad por área terapéutica
   7. Análisis de texto libre - IMC y etnia

   Cada sección genera tablas y gráficos en la consola/visor de RStudio. No se requieren argumentos ni configuración adicional más allá del archivo de datos.

## Tiempo de ejecución estimado

El cálculo de la distancia de Gower para el clustering (sección 3) es el paso más demandante computacionalmente, y puede tardar varios minutos incluso trabajando sobre la muestra reducida de 5000 registros.
El resto del script se ejecuta en pocos minutos en una computadora estándar.

## Reproducibilidad

Este análisis fue ejecutado originalmente con la versión de R y los paquetes especificados en `session_info.txt`.
Se recomienda utilizar versiones equivalentes para reproducir exactamente los resultados reportados en el informe, dado que algunas funciones (en particular `randomForest()` y la partición `createDataPartition()`) pueden producir resultados levemente distintos entre versiones de paquete, incluso fijando la semilla aleatoria (`set.seed(42)`).

## Contacto

Hugo Granchetti — [hgranchetti@gmail.com]
