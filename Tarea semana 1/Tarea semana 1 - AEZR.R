# DIVERSIDAD GENÉTICA - PCA DE ACCESIONES DE PAPA
# Autor: Alexei Edaurdo Zelada Romero 
# Modelo adaptado del script del docente
# Artículo: Defining a diverse core collection of the Colombian Central Collection of potatoes: a tool to advance research and breeding
# DOI:https://doi.org/10.3389/fpls.2023.1046400

# 1. INSTALACIÓN Y CARGA DE LIBRERÍAS

paquetes <- c(
  "readxl",
  "dplyr",
  "FactoMineR",
  "factoextra",
  "cowplot",
  "knitr"
)

paquetes_faltantes <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if (length(paquetes_faltantes) > 0) {
  
  install.packages(
    paquetes_faltantes,
    dependencies = TRUE
  )
  
}


library(readxl)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(cowplot)
library(knitr)

# 2. IMPORTACIÓN DE DATOS

# Nombre del archivo Excel

archivo <- "Tabla_Semana_1_DG_MBEG_PCA_preparado.xlsx"

# Leer la hoja PCA

datos <- read_excel(
  archivo,
  sheet = "PCA"
)


# 3. REVISIÓN INICIAL DE LA BASE

cat("\n")
cat("============================================================\n")
cat("ESTRUCTURA DE LA BASE ORIGINAL\n")
cat("============================================================\n")

str(datos)


cat("\n")
cat("Dimensiones de la base:\n")

print(dim(datos))


cat("\n")
cat("Primeras filas:\n")

print(head(datos))


# 4. DEFINIR VARIABLES

# Variables cualitativas

vars_quali <- c(
  "Genotype",
  "Type",
  "Ploidy",
  "Species",
  "CCC_Group"
)

# Variables cuantitativas que entrarán al PCA

vars_activas <- c(
  "Average_Tuber_Weight_(g)",
  "Number_of_Tubers_per_plant",
  "Total_Tuber_Yield_(kg_per_plant)"
)

# 5. VERIFICAR COLUMNAS

columnas_necesarias <- c(
  vars_quali,
  vars_activas
)


if (!all(columnas_necesarias %in% names(datos))) {
  
  faltantes <- columnas_necesarias[
    !(columnas_necesarias %in% names(datos))
  ]
  
  stop(
    paste(
      "Faltan las siguientes columnas:",
      paste(faltantes, collapse = ", ")
    )
  )
  
}


# 6. CONVERSIÓN DE VARIABLES CUANTITATIVAS

datos_originales <- datos


datos[vars_activas] <- lapply(
  
  datos[vars_activas],
  
  function(x) {
    
    suppressWarnings(
      as.numeric(x)
    )
    
  }
)


# 7. IDENTIFICACIÓN DE DATOS PROBLEMÁTICOS

datos_problematicos <- datos_originales[
  
  !complete.cases(
    datos[vars_activas]
  ),
  
  c(
    "Genotype",
    vars_activas
  )
  
]


cat("\n")
cat("============================================================\n")
cat("DATOS PROBLEMÁTICOS\n")
cat("============================================================\n")


print(datos_problematicos)


cat("\nNúmero de registros excluidos:",
    nrow(datos_problematicos),
    "\n")


# 8. PREPARACIÓN DE LA BASE PARA PCA

fb <- datos %>%
  
  filter(
    
    if_all(
      
      all_of(vars_activas),
      
      ~ !is.na(.x)
      
    )
    
  ) %>%
  
  select(
    
    all_of(vars_quali),
    all_of(vars_activas)
    
  ) %>%
  
  mutate(
    
    across(
      
      all_of(vars_quali),
      
      as.factor
      
    )
    
  )


# 9. REVISIÓN DE LA BASE FINAL

cat("\n")
cat("============================================================\n")
cat("BASE FINAL PARA PCA\n")
cat("============================================================\n")


str(fb)


cat("\n")
cat("Número de individuos:",
    nrow(fb),
    "\n")


cat("Número de variables:",
    ncol(fb),
    "\n")


cat("\nPrimeras filas:\n")

print(head(fb))


# ============================================================
# 10. DISTRIBUCIÓN DE LOS GRUPOS
# ============================================================


cat("\n")
cat("============================================================\n")
cat("DISTRIBUCIÓN DE CCC_GROUP\n")
cat("============================================================\n")


print(
  table(
    fb$CCC_Group
  )
)


cat("\n")
cat("============================================================\n")
cat("DISTRIBUCIÓN DE PLOIDY\n")
cat("============================================================\n")


print(
  table(
    fb$Ploidy
  )
)


# 11. ANÁLISIS DE COMPONENTES PRINCIPALES - PCA

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS DE COMPONENTES PRINCIPALES\n")
cat("============================================================\n")


mv <- PCA(
  
  X = fb,
  
  # Estandarizar las variables cuantitativas
  scale.unit = TRUE,
  
  # Las primeras cinco columnas son cualitativas
  quali.sup = 1:5,
  
  # No generar gráficos automáticamente
  graph = FALSE
  
)

# 12. RESUMEN DEL PCA

cat("\n")
cat("============================================================\n")
cat("RESUMEN DEL PCA\n")
cat("============================================================\n")


summary(
  
  mv,
  
  nbelements = Inf,
  
  nb.dec = 2
  
)

# 13. INFORMACIÓN DE LAS VARIABLES

pcainfo <- factoextra::get_pca_var(
  mv
)


# 14. CORRELACIÓN DE LAS VARIABLES

cat("\n")
cat("============================================================\n")
cat("CORRELACIÓN DE LAS VARIABLES CON LOS COMPONENTES\n")
cat("============================================================\n")


print(
  pcainfo$cor
)


# 15. CONTRIBUCIÓN DE LAS VARIABLES

cat("\n")
cat("============================================================\n")
cat("CONTRIBUCIÓN DE LAS VARIABLES\n")
cat("============================================================\n")


print(
  pcainfo$contrib
)


# 16. GRÁFICO DE VARIABLES

var <- plot.PCA(
  
  mv,
  
  choix = "var",
  
  cex = 0.7
  
)


print(var)


# 17. GRÁFICO DE INDIVIDUOS

ind <- plot.PCA(
  
  mv,
  
  choix = "ind",
  
  cex = 0.7,
  
  # No colocar los nombres de las 845 accesiones
  label = "none",
  
  # Ocultar variables cualitativas del gráfico
  invisible = "quali",
  
  # Colorear según la tercera variable cualitativa:
  # Ploidy
  habillage = 3
  
)


print(ind)


# 18. COMBINAR GRÁFICOS

pca_grafico <- plot_grid(
  
  var,
  ind,
  
  ncol = 2,
  
  labels = "auto"
  
)


print(pca_grafico)


# 19. CREAR CARPETA DE RESULTADOS

dir.create(
  
  "Resultados",
  
  showWarnings = FALSE
  
)


# 20. GUARDAR GRÁFICO DEL PCA

ggsave(
  
  filename = "Resultados/PCA_variables_individuos.png",
  
  plot = pca_grafico,
  
  width = 12,
  
  height = 6,
  
  dpi = 300
  
)

# 21. EXPORTAR CORRELACIONES

write.csv(
  
  pcainfo$cor,
  
  "Resultados/PCA_correlaciones.csv",
  
  row.names = TRUE
  
)

# 22. EXPORTAR CONTRIBUCIONES

write.csv(
  
  pcainfo$contrib,
  
  "Resultados/PCA_contribucion_variables.csv",
  
  row.names = TRUE
  
)

# 23. EXPORTAR COORDENADAS DE INDIVIDUOS

write.csv(
  
  mv$ind$coord,
  
  "Resultados/PCA_coordenadas_individuos.csv",
  
  row.names = TRUE
  
)

# 24. ANÁLISIS DE CLUSTER - HCPC

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS DE CLUSTER - HCPC\n")
cat("============================================================\n")

set.seed(123)


cls <- HCPC(
  
  mv,
  
  graph = FALSE
  
)

# 25. MAPA DE CLUSTERS

map <- plot.HCPC(
  
  cls,
  
  choice = "map",
  
  nb.clust = -1,
  
  draw.tree = FALSE
  
)


print(map)


# 26. ÁRBOL DE CLUSTER

tree <- plot.HCPC(
  
  cls,
  
  choice = "tree",
  
  nb.clust = -1,
  
  draw.tree = FALSE
  
)


print(tree)


# 27. NÚMERO DE INDIVIDUOS POR CLUSTER

cat("\n")
cat("============================================================\n")
cat("NÚMERO DE INDIVIDUOS POR CLUSTER\n")
cat("============================================================\n")


print(
  table(
    cls$data.clust$clust
  )
)


# 28. EXPORTAR ASIGNACIÓN DE CLUSTERS

write.csv(
  
  cls$data.clust,
  
  "Resultados/Asignacion_clusters_HCPC.csv",
  
  row.names = TRUE
  
)

# 29. MENSAJE FINAL

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS FINALIZADO\n")
cat("============================================================\n")


cat("\n")
cat("El PCA y el análisis HCPC fueron ejecutados correctamente.\n")


cat("\n")
cat("Los resultados fueron guardados en la carpeta:\n")


cat("Resultados/\n")


cat("\n")
