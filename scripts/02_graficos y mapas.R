setwd("C:/Users/54356/Documents/Facultad/Ciencia de Datos/TP_FINAL/1_RAW/NUEVO/input")

# Cargar librerías necesarias
library(tidyverse) # Incluye dplyr, ggplot2, readr, etc.

# Definir la ruta donde se encuentran los archivos limpios
# Si ya estableciste el setwd correctamente, 'input' será el directorio actual
# Si los archivos están en una subcarpeta dentro del setwd, ajusta la ruta
input_path <- "." # Significa el directorio actual

# Cargar cada archivo CSV limpio
delitos_limpios_2019 <- read_csv(file.path(input_path, "limpieza_de_datos2019.csv"))
delitos_limpios_2020 <- read_csv(file.path(input_path, "limpieza_de_datos2020.csv"))
delitos_limpios_2021 <- read_csv(file.path(input_path, "limpieza_de_datos2021.csv"))
delitos_limpios_2022 <- read_csv(file.path(input_path, "limpieza_de_datos2022.csv"))
delitos_limpios_2023 <- read_csv(file.path(input_path, "limpieza_de_datos2023.csv"))

# Opcional: Verificar las primeras filas de una de las tablas para asegurar que se cargó correctamente
head(delitos_limpios_2019)
glimpse(delitos_limpios_2023) # Para ver el tipo de dato de cada columna


#----------------------------------------------------------------------------------------------------

# Asegúrate de cargar la librería tidyverse si no lo has hecho
library(tidyverse)

# --- Función auxiliar para aplicar los cambios a cada tabla (MODIFICADA) ---
aplicar_limpieza_adicional <- function(df) {
  df %>%
    # Paso CRUCIAL: Asegurarse de que 'franja' y 'comuna' sean numéricas
    mutate(
      franja = as.numeric(as.character(franja)), # Convertir franja a numérica
      comuna = as.numeric(as.character(comuna))  # Convertir comuna a numérica
    ) %>%
    
    # Filtra los tipos de delito no deseados
    filter(!tipo %in% c("Amenazas", "Vialidad")) %>%
    
    # Crea la nueva columna "momento_dia"
    mutate(
      momento_dia = case_when(
        franja >= 0 & franja <= 6 ~ "MADRUGADA",
        franja >= 7 & franja <= 12 ~ "MAÑANA",
        franja >= 13 & franja <= 19 ~ "TARDE",
        franja >= 20 & franja <= 23 ~ "NOCHE",
        TRUE ~ NA_character_ # En caso de que franja sea NA o esté fuera de rango
      )
    )
}

# Aplicar la función a cada una de tus tablas (esto re-procesará tus dataframes)
delitos_limpios_2019 <- aplicar_limpieza_adicional(delitos_limpios_2019)
delitos_limpios_2020 <- aplicar_limpieza_adicional(delitos_limpios_2020)
delitos_limpios_2021 <- aplicar_limpieza_adicional(delitos_limpios_2021)
delitos_limpios_2022 <- aplicar_limpieza_adicional(delitos_limpios_2022)
delitos_limpios_2023 <- aplicar_limpieza_adicional(delitos_limpios_2023)

# --- Unir todas las tablas de delitos en una sola (Repetir este paso) ---
# Ahora que 'comuna' debería ser numérica en todas, bind_rows funcionará.
delitos_todos_anios <- bind_rows(
  delitos_limpios_2019,
  delitos_limpios_2020,
  delitos_limpios_2021,
  delitos_limpios_2022,
  delitos_limpios_2023
)

# Opcional: Verifica las dimensiones de la nueva tabla combinada
print(paste("Número total de filas combinadas:", nrow(delitos_todos_anios)))
print(paste("Número total de columnas combinadas:", ncol(delitos_todos_anios)))

# --- Crear la tabla resumen agrupando y contando (Repetir este paso) ---
resumen_delitos_por_momento_y_anio <- delitos_todos_anios %>%
  group_by(anio, tipo, momento_dia) %>%
  summarise(
    conteo_delitos = n(),
    .groups = 'drop' # Es una buena práctica para desagrupar automáticamente
  ) %>%
  arrange(anio, tipo, momento_dia)

# --- Mostrar el resultado ---
print("Tabla Resumen de Delitos por Año, Momento del Día y Tipo:")
print(resumen_delitos_por_momento_y_anio)

#--------------------------------------------------------------------------------
#GRAFICOS DE MAPA SF
# --- 0. Configuración Inicial y Limpieza (Opcional) ---
#rm(list = ls()) # Limpia el entorno de trabajo
#options(scipen = 999) # Evita la notación científica en la consola

# --- 1. Instalar y Cargar Librerías Necesarias ---
# Si no tienes alguna de estas librerías, descomenta la línea 'install.packages()'
# y ejecútala UNA VEZ para instalarla. Luego, vuelve a comentar la línea.
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("viridis")

library(sf)                # Para trabajar con datos espaciales y leer shapefiles
library(ggplot2)           # Para crear gráficos de alta calidad
library(dplyr)             # Para manipulación de datos
library(viridis)           # Para paletas de colores estéticamente agradables

# --- 2. Definir Años y Nombres de Tablas de Delitos ---
# Define los años a procesar.
años_a_procesar <- 2019:2023
# Se asume que las tablas como 'delitos_limpios_2019' ya están cargadas en tu entorno.

# --- 3. Función para Corrección Individual de Coordenadas ---
# Esta función es la que detecta y corrige la escala de cada coordenada.
corregir_coordenada <- function(coord) {
  if (is.na(coord) || coord == 0) {
    return(NA_real_) # Mantiene NA o 0 como NA
  }
  
  log_abs_coord <- log10(abs(coord))
  potencia <- floor(log_abs_coord)
  divisor_potencia <- 10^(max(0, potencia - 1))
  
  corrected_coord <- coord / divisor_potencia
  
  # Asegura que el signo sea correcto (latitud y longitud de CABA son negativas)
  if (corrected_coord > 0) {
    corrected_coord <- -abs(corrected_coord)
  }
  
  return(corrected_coord)
}

# --- 4. Cargar el Shapefile Detallado de CABA (Esto solo se hace una vez) ---
# ¡IMPORTANTE! Reemplaza "C:/Users/..." con la ruta REAL de tu archivo .shp
ruta_shp_caba <- "C:/Users/54356/Documents/Facultad/Ciencia de Datos/TP_FINAL/1_RAW/parcelario_cur3d_20230531.shp"

caba_shapefile <- st_read(ruta_shp_caba, quiet = TRUE)
caba_shapefile_wgs84 <- st_transform(caba_shapefile, 4326)
cat("✔ Shapefile de CABA cargado y transformado.\n")

# --- 5. Definir Colores Personalizados (¡Ahora sin Amenazas ni Vialidad!) ---
# Ajustamos esta lista de colores para que solo incluya los tipos de delito presentes.
colores_delitos_mapa <- c(
  "Homicidios" = "#d95f02", # Naranja quemado
  "Hurto" = "#7570b3",      # Púrpura suave
  "Lesiones" = "#e7298a",   # Rosa oscuro
  "Robo" = "#66a61e"        # Verde lima
)
# Nota: Si en tus datos hay otros tipos de delito además de estos 4,
# deberías agregarlos aquí con sus respectivos colores.

# --- 6. Bucle Principal: Procesar cada Año ---
cat("\nIniciando la generación de mapas por año y tipo de delito...\n")

for (año_actual in años_a_procesar) {
  cat(paste0("\nProcesando año: ", año_actual, "...\n"))
  
  # Construye el nombre de la tabla para el año actual
  nombre_tabla_delitos <- paste0("delitos_limpios_", año_actual)
  
  # Verifica si la tabla existe en el entorno
  if (!exists(nombre_tabla_delitos)) {
    warning(paste0("La tabla '", nombre_tabla_delitos, "' no se encontró en el entorno. Saltando este año."))
    next # Pasa al siguiente año en el bucle
  }
  
  # Obtiene el data frame de delitos para el año actual
  delitos_año_actual <- get(nombre_tabla_delitos)
  
  # Asegura que las columnas 'longitud' y 'latitud' sean numéricas
  delitos_año_actual$longitud <- as.numeric(as.character(delitos_año_actual$longitud))
  delitos_año_actual$latitud <- as.numeric(as.character(delitos_año_actual$latitud))
  
  # Aplicar la Corrección Individual y Limpiar el Dataset para el año actual
  delitos_limpio_corregido_año <- delitos_año_actual %>%
    rowwise() %>%
    mutate(
      latitud_final = corregir_coordenada(latitud),
      longitud_final = corregir_coordenada(longitud)
    ) %>%
    ungroup() %>%
    filter(
      !is.na(latitud_final) & !is.na(longitud_final),
      latitud_final > -35.0 & latitud_final < -34.0,
      longitud_final > -59.0 & longitud_final < -58.0
    )
  
  # Convierte el data frame corregido a un objeto espacial (sf)
  delitos_sf_año <- delitos_limpio_corregido_año %>%
    st_as_sf(coords = c("longitud_final", "latitud_final"), crs = 4326)
  
  # Obtiene los tipos de delito únicos para el año actual
  # Esto automáticamente excluirá "Vialidad" y "Amenazas" si no están en los datos.
  tipos_delito_año <- unique(delitos_sf_año$tipo)
  cat(paste0("  Tipos de delito encontrados para ", año_actual, ": ", paste(tipos_delito_año, collapse = ", "), "\n"))
  
  # Crear una carpeta de salida para el año específico
  output_dir_año <- file.path("Mapas_Delitos_CABA_Por_Año", as.character(año_actual))
  if (!dir.exists(output_dir_año)) {
    dir.create(output_dir_año, recursive = TRUE)
  }
  
  # Bucle Anidado: Generar mapas para cada tipo de delito dentro del año
  for (tipo_actual in tipos_delito_año) {
    # Filtra los delitos para el tipo actual
    delitos_filtrados_por_tipo <- delitos_sf_año %>%
      filter(tipo == tipo_actual)
    
    # Crea el título dinámico para el mapa
    titulo_mapa <- paste0("Distribución de ", tipo_actual, " en CABA (", año_actual, ")")
    
    p <- ggplot() +
      geom_sf(data = caba_shapefile_wgs84,
              fill = "lightgray",
              color = "darkgray",
              linewidth = 0.05) +
      geom_sf(data = delitos_filtrados_por_tipo,
              aes(color = tipo),
              size = 0.8,
              alpha = 0.6) +
      labs(
        title = titulo_mapa,
        subtitle = paste0("Total de ", nrow(delitos_filtrados_por_tipo), " registros para este tipo"),
        caption = paste0("Fuente: Datos de Delitos ", año_actual, " (GCBA) y Mapa Parcelario CUR3D")
      ) +
      # Usa los colores personalizados. Solo los colores definidos se aplicarán.
      scale_color_manual(values = colores_delitos_mapa, name = "Tipo de Delito") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
        plot.caption = element_text(hjust = 0, size = 9, color = "gray50"),
        legend.position = "none",
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent")
      ) +
      coord_sf()
    
    # Guarda el mapa
    nombre_archivo_png <- file.path(output_dir_año, paste0("mapa_delitos_caba_", tolower(gsub(" ", "_", tipo_actual)), "_", año_actual, ".png"))
    ggsave(nombre_archivo_png, plot = p, width = 12, height = 10, dpi = 300)
    
    cat(paste0("    ✔ Mapa para '", tipo_actual, "' generado y guardado en '", nombre_archivo_png, "'\n"))
  }
}

cat("\n¡Proceso de generación de todos los mapas completado!\n")

#-----------------------------------------------------------------------------------
#MOVER CARPETA
# --- 1. Definir las rutas de las carpetas ---

# La carpeta donde se generaron los mapas (por defecto del script anterior)
# Asegúrate de que esta ruta apunte a donde realmente están tus mapas.
# Si el script se ejecutó en tu directorio de trabajo principal, será así:
origen_base <- "Mapas_Delitos_CABA_Por_Año"

# La carpeta de destino donde quieres que estén los mapas.
# Asumo que "output" está al mismo nivel que "input" (donde se generó "Mapas_Delitos_CABA_Por_Año")
# Si "output" es una subcarpeta dentro del directorio de trabajo:
destino_base <- "output/Mapas_Delitos_CABA_Por_Año" # Esto creará la misma estructura de subcarpetas dentro de 'output'

# Si la carpeta 'output' no existe, la creamos
if (!dir.exists("output")) {
  dir.create("output")
  cat("✔ Carpeta 'output' creada.\n")
}

# --- 2. Crear la estructura de carpetas en el destino si no existe ---
# Esto asegurará que, por ejemplo, 'output/Mapas_Delitos_CABA_Por_Año/2019' exista antes de mover.
años_generados <- list.dirs(origen_base, full.names = FALSE, recursive = FALSE)
años_generados <- años_generados[años_generados != ""] # Eliminar posibles entradas vacías

if (length(años_generados) == 0) {
  cat("✖ No se encontraron subcarpetas de años en el directorio de origen:", origen_base, "\n")
  cat("Asegúrate de que 'origen_base' sea la ruta correcta a la carpeta donde están los mapas.\n")
} else {
  cat(paste0("Se encontraron mapas para los años: ", paste(años_generados, collapse = ", "), "\n"))
  for (año in años_generados) {
    dir_origen_año <- file.path(origen_base, año)
    dir_destino_año <- file.path(destino_base, año)
    
    if (!dir.exists(dir_destino_año)) {
      dir.create(dir_destino_año, recursive = TRUE)
      cat(paste0("✔ Carpeta de destino creada: ", dir_destino_año, "\n"))
    }
  }
  
  # --- 3. Mover los archivos PNG ---
  cat("\nIniciando el movimiento de archivos...\n")
  archivos_movidos_total <- 0
  
  for (año in años_generados) {
    dir_origen_año <- file.path(origen_base, año)
    dir_destino_año <- file.path(destino_base, año)
    
    # Lista todos los archivos PNG en la carpeta de origen de ese año
    archivos_png_año <- list.files(dir_origen_año, pattern = "\\.png$", full.names = TRUE)
    
    if (length(archivos_png_año) == 0) {
      cat(paste0("  No se encontraron archivos PNG en ", dir_origen_año, "\n"))
      next
    }
    
    cat(paste0("  Moviendo ", length(archivos_png_año), " archivos PNG del año ", año, "...\n"))
    
    for (archivo_origen in archivos_png_año) {
      nombre_archivo <- basename(archivo_origen) # Obtiene solo el nombre del archivo
      archivo_destino <- file.path(dir_destino_año, nombre_archivo)
      
      # Mueve el archivo. 'overwrite = TRUE' permite reemplazar si ya existe un archivo con el mismo nombre.
      file.rename(from = archivo_origen, to = archivo_destino)
      archivos_movidos_total <- archivos_movidos_total + 1
    }
    cat(paste0("  ✔ Archivos del año ", año, " movidos.\n"))
  }
  
  cat(paste0("\n¡Proceso de movimiento de archivos completado! Total de archivos movidos: ", archivos_movidos_total, ".\n"))
  
  # --- 4. Opcional: Eliminar la carpeta original vacía ---
  # Después de mover todos los archivos, puedes optar por eliminar las carpetas de origen.
  # ¡CUIDADO! Asegúrate de que los archivos se hayan movido correctamente antes de ejecutar esto.
  # Si deseas eliminar las carpetas de origen después de mover los archivos, descomenta las siguientes líneas:
  # for (año in años_generados) {
  #   dir_origen_año <- file.path(origen_base, año)
  #   if (length(list.files(dir_origen_año)) == 0) { # Solo si la carpeta está vacía
  #     unlink(dir_origen_año, recursive = TRUE)
  #     cat(paste0("✔ Carpeta original eliminada: ", dir_origen_año, "\n"))
  #   }
  # }
  # if (length(list.files(origen_base)) == 0) { # Si la carpeta base también queda vacía
  #   unlink(origen_base, recursive = TRUE)
  #   cat(paste0("✔ Carpeta base de origen eliminada: ", origen_base, "\n"))
  # }
}

#---------------------------------------------------------------------------------
#GENERAR TABLA DE DELITOS POR MOMENTOS DEL DIA

# --- 1. Cargar Librerías Necesarias ---
library(dplyr) # Para manipulación de datos (group_by, summarise, bind_rows)

# --- 2. Definir Años y Nombres de Tablas de Delitos ---
años_a_procesar <- 2019:2023

# --- 3. Inicializar listas para almacenar los resultados de cada año ---
listado_delitos_momento_dia <- list()
listado_delitos_tipo <- list()

# --- 4. Bucle para procesar cada año y generar los resúmenes ---
cat("Iniciando la generación de tablas de resumen...\n")

for (año_actual in años_a_procesar) {
  cat(paste0("  Procesando año: ", año_actual, "...\n"))
  
  nombre_tabla_delitos <- paste0("delitos_limpios_", año_actual)
  
  # Verificar si la tabla existe en el entorno
  if (!exists(nombre_tabla_delitos)) {
    warning(paste0("La tabla '", nombre_tabla_delitos, "' no se encontró en el entorno. Saltando este año."))
    next # Pasa al siguiente año
  }
  
  # Obtener el data frame de delitos para el año actual
  delitos_año_actual <- get(nombre_tabla_delitos)
  
  # --- Generar resumen por 'momento_dia' para el año actual ---
  resumen_momento_dia <- delitos_año_actual %>%
    group_by(momento_dia) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    mutate(
      año = año_actual # Añadir la columna de año para combinar después
    ) %>%
    ungroup() # Desagrupar para futuras operaciones
  
  listado_delitos_momento_dia[[as.character(año_actual)]] <- resumen_momento_dia
  
  # --- Generar resumen por 'tipo' de delito para el año actual ---
  resumen_tipo_delito <- delitos_año_actual %>%
    group_by(tipo) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    mutate(
      año = año_actual # Añadir la columna de año
    ) %>%
    ungroup()
  
  listado_delitos_tipo[[as.character(año_actual)]] <- resumen_tipo_delito
}

# --- 5. Combinar los resúmenes de todos los años en una sola tabla ---

# Tabla final de delitos por Año y Momento del Día
tabla_delitos_por_momento_dia <- bind_rows(listado_delitos_momento_dia) %>%
  # Opcional: Reordenar columnas para una mejor lectura
  select(año, momento_dia, conteo_delitos) %>%
  # Opcional: Pivotar la tabla si quieres los momentos del día como columnas
  # pivot_wider(names_from = momento_dia, values_from = conteo_delitos, values_fill = 0)
  arrange(año, momento_dia)

# Tabla final de delitos por Año y Tipo de Delito
tabla_delitos_por_tipo <- bind_rows(listado_delitos_tipo) %>%
  # Opcional: Reordenar columnas
  select(año, tipo, conteo_delitos) %>%
  # Opcional: Pivotar la tabla si quieres los tipos de delito como columnas
  # pivot_wider(names_from = tipo, values_from = conteo_delitos, values_fill = 0)
  arrange(año, tipo)

cat("\n¡Generación de tablas completada!\n")

# --- 6. Mostrar las tablas resultantes ---
cat("\n## Tabla de Delitos por Año y Momento del Día:\n")
print(tabla_delitos_por_momento_dia)

cat("\n\n## Tabla de Delitos por Año y Tipo de Delito:\n")
print(tabla_delitos_por_tipo)

# --- 7. Opcional: Guardar las tablas como CSV ---
# Si quieres guardar estas tablas para usarlas en otro software o simplemente almacenarlas.
# write.csv(tabla_delitos_por_momento_dia, "resumen_delitos_momento_dia.csv", row.names = FALSE)
# write.csv(tabla_delitos_por_tipo, "resumen_delitos_tipo.csv", row.names = FALSE)
# cat("\nTablas guardadas como 'resumen_delitos_momento_dia.csv' y 'resumen_delitos_tipo.csv'.\n")

#----------

# --- Cargar Librería Necesaria (tidyr es parte de tidyverse junto con dplyr) ---
library(tidyr) # Para la función pivot_wider

# --- Asegúrate de que 'tabla_delitos_por_momento_dia' esté en tu entorno ---
# Si no la generaste en la sesión actual, deberías ejecutar el código anterior
# que la crea a partir de tus data frames anuales.

# --- 1. Eliminar observaciones con NA en 'momento_dia' ---
tabla_delitos_por_momento_dia_limpia <- tabla_delitos_por_momento_dia %>%
  filter(!is.na(momento_dia))

# --- 2. Pivotar la tabla para tener momentos del día como filas y años como columnas ---
tabla_final_pivotada_momento_dia <- tabla_delitos_por_momento_dia_limpia %>%
  pivot_wider(
    names_from = año,       # Los nombres de las nuevas columnas vendrán de la columna 'año'
    values_from = conteo_delitos, # Los valores de las nuevas columnas vendrán de 'conteo_delitos'
    values_fill = 0         # Rellenar los valores ausentes (si un momento_dia no tuvo delitos en un año) con 0
  ) %>%
  # Opcional: Reordenar las filas para un orden lógico de los momentos del día
  mutate(
    momento_dia = factor(momento_dia, levels = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"))
  ) %>%
  arrange(momento_dia)


# --- 3. Mostrar la tabla resultante ---
cat("\n## Tabla de Delitos por Momento del Día (Momentos como Filas, Años como Columnas):\n")
print(tabla_final_pivotada_momento_dia)

# --- 4. Opcional: Guardar la tabla pivotada como CSV ---
# write.csv(tabla_final_pivotada_momento_dia, "resumen_delitos_momento_dia_pivotada.csv", row.names = FALSE)
# cat("\nTabla pivotada guardada como 'resumen_delitos_momento_dia_pivotada.csv'.\n")


#.---------------------------------------------------------------------------------------------------------
#GRAFICO PARA TABLA FINAL POVIVOTADA MOMENTO DIA
# --- 1. Cargar Librerías Necesarias ---
library(ggplot2)
library(tidyr) # Para pivot_longer
library(dplyr) # Para manipulación de datos

# --- 2. Preparar los Datos para el Gráfico con Porcentajes ---
# Necesitamos la tabla en formato "largo" (long format) para ggplot2.
# 'tabla_final_pivotada_momento_dia' está en formato "ancho".
# Volvemos a pivotar y luego calculamos los porcentajes.

tabla_grafico_momento_dia_pct <- tabla_final_pivotada_momento_dia %>%
  pivot_longer(
    cols = -momento_dia,
    names_to = "año",
    values_to = "conteo_delitos"
  ) %>%
  mutate(
    año = as.factor(año) # Convertir el año a factor
  ) %>%
  # *** Calcular los porcentajes ***
  group_by(año) %>% # Agrupar por año para calcular el total de cada año
  mutate(
    porcentaje = (conteo_delitos / sum(conteo_delitos)) * 100 # Calcular porcentaje
  ) %>%
  ungroup() %>%
  # Asegurar el orden de los momentos del día para el gráfico
  mutate(
    momento_dia = factor(momento_dia, levels = c("MADRUGADA", "MAÑANA", "TARDE", "NOCHE"))
  ) %>%
  arrange(año, momento_dia) # Ordenar para asegurar el apilamiento correcto

# --- 3. Crear el Gráfico de Barras Apiladas con Porcentajes ---

p_momento_dia_porcentaje <- ggplot(tabla_grafico_momento_dia_pct, aes(x = año, y = porcentaje, fill = momento_dia)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribución Porcentual de Delitos por Momento del Día (2019-2023)",
    x = "Año",
    y = "Porcentaje de Delitos (%)",
    fill = "Momento del Día",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_fill_manual(values = c(
    "MADRUGADA" = "#4e79a7", # Azul oscuro
    "MAÑANA" = "#f28e2b",    # Naranja
    "TARDE" = "#e15759",     # Rojo
    "NOCHE" = "#76b7b2"      # Turquesa
  )) +
  # Asegurar que el eje Y vaya de 0 a 100%
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# Mostrar el gráfico
print(p_momento_dia_porcentaje)

# --- 4. Opcional: Guardar el Gráfico ---
ggsave("grafico_delitos_momento_dia_porcentaje.png", plot = p_momento_dia_porcentaje, width = 10, height = 7, dpi = 300)
cat("\nGráfico guardado como 'grafico_delitos_momento_dia_porcentaje.png'.\n")


#-------------------------------------------------------------------------------
#DELITOS POR TIPO POR AÑO

# --- 1. Cargar Librerías Necesarias ---
library(dplyr) # Para manipulación de datos (group_by, summarise, bind_rows)

# --- 2. Definir Años y Nombres de Tablas de Delitos ---
años_a_procesar <- 2019:2023

# --- 3. Inicializar listas para almacenar los resultados de cada año ---
listado_delitos_momento_dia <- list()
listado_delitos_tipo <- list()

# --- 4. Bucle para procesar cada año y generar los resúmenes ---
cat("Iniciando la generación de tablas de resumen...\n")

for (año_actual in años_a_procesar) {
  cat(paste0("  Procesando año: ", año_actual, "...\n"))
  
  nombre_tabla_delitos <- paste0("delitos_limpios_", año_actual)
  
  # Verificar si la tabla existe en el entorno
  if (!exists(nombre_tabla_delitos)) {
    warning(paste0("La tabla '", nombre_tabla_delitos, "' no se encontró en el entorno. Saltando este año."))
    next # Pasa al siguiente año
  }
  
  # Obtener el data frame de delitos para el año actual
  delitos_año_actual <- get(nombre_tabla_delitos)
  
  # --- Generar resumen por 'momento_dia' para el año actual ---
  resumen_momento_dia <- delitos_año_actual %>%
    group_by(momento_dia) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    mutate(
      año = año_actual # Añadir la columna de año para combinar después
    ) %>%
    ungroup() # Desagrupar para futuras operaciones
  
  listado_delitos_momento_dia[[as.character(año_actual)]] <- resumen_momento_dia
  
  # --- Generar resumen por 'tipo' de delito para el año actual ---
  resumen_tipo_delito <- delitos_año_actual %>%
    group_by(tipo) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    mutate(
      año = año_actual # Añadir la columna de año
    ) %>%
    ungroup()
  
  listado_delitos_tipo[[as.character(año_actual)]] <- resumen_tipo_delito
}

# --- 5. Combinar los resúmenes de todos los años en una sola tabla ---

# Tabla final de delitos por Año y Momento del Día
tabla_delitos_por_momento_dia <- bind_rows(listado_delitos_momento_dia) %>%
  select(año, momento_dia, conteo_delitos) %>%
  arrange(año, momento_dia)

# Tabla final de delitos por Año y Tipo de Delito
tabla_delitos_por_tipo <- bind_rows(listado_delitos_tipo) %>%
  select(año, tipo, conteo_delitos) %>%
  arrange(año, tipo)

cat("\n¡Generación de tablas completada!\n")

# --- 6. Mostrar las tablas resultantes (opcional, si quieres verlas de nuevo) ---
# cat("\n## Tabla de Delitos por Año y Momento del Día:\n")
# print(tabla_delitos_por_momento_dia)

# cat("\n\n## Tabla de Delitos por Año y Tipo de Delito:\n")
# print(tabla_delitos_por_tipo)


#--.-.-.-.-.-.-.-.--.-.-.-.-.-.

# --- 1. Cargar Librería Necesaria (tidyr es parte de tidyverse) ---
library(tidyr) # Para la función pivot_wider
library(dplyr) # Para manipulación de datos (si no está cargada)

# --- 2. Asegúrate de que 'tabla_delitos_por_tipo' esté en tu entorno ---
# Si no la generaste en la sesión actual, deberías ejecutar el código anterior
# que la crea a partir de tus data frames anuales.

# --- 3. Pivotar la tabla para tener tipos de delito como filas y años como columnas ---
tabla_final_pivotada_tipo <- tabla_delitos_por_tipo %>%
  # Opcional: Filtrar NAs en 'tipo' si existieran, aunque no deberían si ya limpiaste los datos fuente
  filter(!is.na(tipo)) %>%
  pivot_wider(
    names_from = año,       # Los nombres de las nuevas columnas vendrán de la columna 'año'
    values_from = conteo_delitos, # Los valores de las nuevas columnas vendrán de 'conteo_delitos'
    values_fill = 0         # Rellenar los valores ausentes (si un tipo no tuvo delitos en un año) con 0
  ) %>%
  # Opcional: Reordenar las filas si quieres un orden específico de los tipos de delito
  # Si quieres un orden particular, descomenta y ajusta esta línea:
  # mutate(
  #   tipo = factor(tipo, levels = c("Homicidios", "Lesiones", "Robo", "Hurto")) # Ejemplo de orden
  # ) %>%
  arrange(tipo) # Ordena alfabéticamente por tipo por defecto si no se usa factor levels

# --- 4. Mostrar la tabla resultante ---
cat("\n## Tabla de Delitos por Tipo (Tipos como Filas, Años como Columnas):\n")
print(tabla_final_pivotada_tipo)

# --- 5. Opcional: Guardar la tabla pivotada como CSV ---
# write.csv(tabla_final_pivotada_tipo, "resumen_delitos_tipo_pivotada.csv", row.names = FALSE)
# cat("\nTabla pivotada guardada como 'resumen_delitos_tipo_pivotada.csv'.\n")

#.-----------------------------------------------------------------------------
#GRAFICO DELITOS POR TIPO POR AÑO

tabla_final_pivotada_tipo[tabla_final_pivotada_tipo$tipo == "Homicidios", "2023"] <- 91
#este codigo es porque cuando filtramos los valores en la limpieza de datos se nos eliminaron muchos datos de homicidios
#el año 2023 presentaba muchas fallas en la base que descargamos, por ende muchos homicidios se borraron
#Esto lo ponemos a mano, contando la cantidad de homicidios que hubo efectivamente segun el archivo excel bajado de la pagina


# --- 1. Cargar Librerías Necesarias ---
library(ggplot2)
library(tidyr) # Para pivot_longer
library(dplyr) # Para manipulación de datos

# --- 2. Preparar los Datos para el Gráfico ---
# 'tabla_final_pivotada_tipo' está en formato "ancho".
# Vamos a transformarla a formato "largo" para ggplot2.

tabla_grafico_tipo_largo <- tabla_final_pivotada_tipo %>%
  pivot_longer(
    cols = -tipo, # Selecciona todas las columnas excepto 'tipo'
    names_to = "año",    # Los nombres de las columnas pivotadas (los años) irán a la nueva columna 'año'
    values_to = "conteo_delitos" # Los valores (los conteos) irán a la nueva columna 'conteo_delitos'
  ) %>%
  mutate(
    año = as.numeric(as.character(año)) # Convertir el año de nuevo a numérico para el eje X
  )

# --- 3. Crear el Gráfico de Líneas ---

p_delitos_por_tipo_lineas <- ggplot(tabla_grafico_tipo_largo, aes(x = año, y = conteo_delitos, color = tipo, group = tipo)) +
  geom_line(linewidth = 1) + # Dibuja las líneas
  geom_point(size = 2) +     # Añade puntos en cada año para mayor claridad
  labs(
    title = "Evolución Anual de Delitos por Tipo en CABA (2019-2023)",
    x = "Año",
    y = "Número de Delitos",
    color = "Tipo de Delito",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_x_continuous(breaks = unique(tabla_grafico_tipo_largo$año)) + # Asegura que se muestren todos los años en el eje X
  scale_y_continuous(labels = scales::comma) + # Formatea el eje Y con comas para miles
  scale_color_manual(values = c(
    "Homicidios" = "#d95f02", # Naranja quemado
    "Hurto" = "#7570b3",      # Púrpura suave
    "Lesiones" = "#e7298a",   # Rosa oscuro
    "Robo" = "#66a61e"        # Verde lima
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# Mostrar el gráfico
print(p_delitos_por_tipo_lineas)

# --- 4. Opcional: Guardar el Gráfico ---
ggsave("grafico_evolucion_delitos_tipo.png", plot = p_delitos_por_tipo_lineas, width = 12, height = 7, dpi = 300)
cat("\nGráfico guardado como 'grafico_evolucion_delitos_tipo.png'.\n")

# --- 1. Cargar Librerías Necesarias ---
library(ggplot2)
library(dplyr) # Para filtrar

# --- 2. Preparar los Datos para el Gráfico de Homicidios ---
# Filtrar solo los datos de Homicidios de la tabla larga ya generada
# (tabla_grafico_tipo_largo del código anterior).
# Si no tienes tabla_grafico_tipo_largo cargada, ejecuta primero el bloque "Preparar los Datos para el Gráfico" del mensaje anterior.

homicidios_para_grafico <- tabla_grafico_tipo_largo %>%
  filter(tipo == "Homicidios")

# --- 3. Crear el Gráfico de Líneas Solo para Homicidios ---

p_homicidios_solo <- ggplot(homicidios_para_grafico, aes(x = año, y = conteo_delitos)) +
  geom_line(color = "#d95f02", linewidth = 1.2) + # Color naranja quemado para Homicidios
  geom_point(color = "#d95f02", size = 3) +       # Puntos más grandes para resaltar
  geom_text(aes(label = conteo_delitos), vjust = -1, hjust = 0.5, size = 4, color = "#d95f02") + # Etiquetas con los valores exactos
  labs(
    title = "Evolución Anual de Homicidios en CABA (2019-2023)",
    subtitle = "Foco en delitos de menor volumen",
    x = "Año",
    y = "Número de Homicidios",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_x_continuous(breaks = unique(homicidios_para_grafico$año)) + # Asegura todos los años
  scale_y_continuous(limits = c(0, max(homicidios_para_grafico$conteo_delitos) * 1.2), # Ajusta el límite Y para dejar espacio para etiquetas
                     labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "none" # No necesitamos leyenda si es un solo tipo
  )

# Mostrar el gráfico
print(p_homicidios_solo)

# --- 4. Opcional: Guardar el Gráfico ---
ggsave("grafico_evolucion_homicidios_solo.png", plot = p_homicidios_solo, width = 8, height = 6, dpi = 300)
cat("\nGráfico guardado como 'grafico_evolucion_homicidios_solo.png'.\n")

#------------------------------------------------------------------------------
#DELITOS CON ARMAS


# --- 0. Configuración Inicial (Opcional, si no lo hiciste antes) ---
# rm(list = ls()) # Limpia el entorno de trabajo
# options(scipen = 999) # Evita la notación científica

# --- 1. Cargar Librerías Necesarias ---
library(dplyr)   # Para manipulación de datos
library(tidyr)   # Para pivot_longer, pivot_wider
library(ggplot2) # Para la creación de gráficos
library(scales)  # Para formatear etiquetas de ejes

# --- 2. Definir Años y Nombres de Tablas de Delitos ---
años_a_procesar <- 2019:2023

# --- 3. Inicializar lista para almacenar los resúmenes de uso de arma ---
listado_delitos_uso_arma <- list()

# --- 4. Bucle para procesar cada año y generar el resumen de uso de arma ---
cat("Iniciando la generación de datos para gráficos de uso de arma...\n")

for (año_actual in años_a_procesar) {
  cat(paste0("  Procesando año: ", año_actual, " para uso_arma...\n"))
  
  nombre_tabla_delitos <- paste0("delitos_limpios_", año_actual)
  
  # Verificar si la tabla existe en el entorno
  if (!exists(nombre_tabla_delitos)) {
    warning(paste0("La tabla '", nombre_tabla_delitos, "' no se encontró en el entorno. Saltando este año."))
    next
  }
  
  # Obtener el data frame de delitos para el año actual
  delitos_año_actual <- get(nombre_tabla_delitos)
  
  # --- Limpieza y preparación de la columna 'uso_arma' ---
  delitos_año_actual_limpio_arma <- delitos_año_actual %>%
    mutate(
      uso_arma = as.character(uso_arma) # Asegurar que sea tipo caracter
    ) %>%
    filter(!is.na(uso_arma) & uso_arma %in% c("SI", "NO")) # Filtrar NAs y asegurar solo SI/NO
  
  # --- Resumen de uso de arma para el año actual ---
  resumen_uso_arma <- delitos_año_actual_limpio_arma %>%
    group_by(uso_arma) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    mutate(
      año = año_actual # Añadir la columna de año
    ) %>%
    ungroup()
  
  listado_delitos_uso_arma[[as.character(año_actual)]] <- resumen_uso_arma
}

# --- 5. Combinar los resúmenes de uso de arma de todos los años ---
tabla_delitos_por_uso_arma <- bind_rows(listado_delitos_uso_arma) %>%
  select(año, uso_arma, conteo_delitos) %>%
  arrange(año, uso_arma)

cat("\n¡Datos de uso de arma procesados y combinados!\n")

# --- 1. Preparar los Datos para el Gráfico de Conteo de Uso de Arma ---
# Filtrar solo los casos donde uso_arma es "SI"
delitos_con_arma_conteo <- tabla_delitos_por_uso_arma %>%
  filter(uso_arma == "SI") %>%
  mutate(año = as.numeric(as.character(año))) # Asegurar año como numérico para el eje X

# --- 2. Crear el Gráfico de Líneas para Conteo de Delitos con Armas ---

cat("\nGenerando Gráfico 1: Evolución Anual de Delitos con Armas (Conteo)...\n")

p_conteo_uso_arma <- ggplot(delitos_con_arma_conteo, aes(x = año, y = conteo_delitos)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) + # Un color azul
  geom_point(color = "#1f77b4", size = 3) +
  geom_text(aes(label = scales::comma(conteo_delitos)), vjust = -1, hjust = 0.5, size = 4, color = "#1f77b4") + # Etiquetas de conteo
  labs(
    title = "Evolución Anual de Delitos con Uso de Arma en CABA (2019-2023)",
    x = "Año",
    y = "Número de Delitos con Arma",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_x_continuous(breaks = unique(delitos_con_arma_conteo$año)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

# Mostrar el gráfico
print(p_conteo_uso_arma)

# --- 3. Opcional: Guardar el Gráfico ---
ggsave("grafico_evolucion_delitos_uso_arma_conteo.png", plot = p_conteo_uso_arma, width = 10, height = 7, dpi = 300)
cat("✔ Gráfico 'grafico_evolucion_delitos_uso_arma_conteo.png' guardado.\n")

# --- 1. Preparar los Datos para el Gráfico de Proporción de Uso de Arma ---
# Calcular el porcentaje para cada uso_arma (SI/NO) por año
delitos_uso_arma_proporcion <- tabla_delitos_por_uso_arma %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(conteo_delitos),
    porcentaje = (conteo_delitos / total_anual) * 100
  ) %>%
  ungroup() %>%
  mutate(
    año = as.factor(año), # Convertir año a factor para el eje X
    uso_arma = factor(uso_arma, levels = c("NO", "SI")) # Ordenar las etiquetas
  ) %>%
  arrange(año, uso_arma)

# --- 2. Crear el Gráfico de Barras Apiladas con Porcentaje de Uso de Arma ---

cat("\nGenerando Gráfico 2: Proporción Anual de Delitos con Armas (Porcentaje)...\n")

p_proporcion_uso_arma <- ggplot(delitos_uso_arma_proporcion, aes(x = año, y = porcentaje, fill = uso_arma)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proporción de Delitos con/sin Uso de Arma por Año (2019-2023)",
    x = "Año",
    y = "Porcentaje de Delitos (%)",
    fill = "Uso de Arma",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_fill_manual(values = c(
    "SI" = "#d62728", # Rojo para "SI"
    "NO" = "#2ca02c"  # Verde para "NO"
  )) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Eje Y en formato porcentaje
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# Mostrar el gráfico
print(p_proporcion_uso_arma)

# --- 3. Opcional: Guardar el Gráfico ---
ggsave("grafico_proporcion_delitos_uso_arma.png", plot = p_proporcion_uso_arma, width = 10, height = 7, dpi = 300)
cat("✔ Gráfico 'grafico_proporcion_delitos_uso_arma.png' guardado.\n")

cat("\n¡Generación de gráficos de uso de arma completada!\n")

#------------------------------------------------------------------------------
#DATOS ADICIONALES PARA GRAFICOS 

# --- 0. Configuración Inicial (Opcional, si no lo hiciste antes) ---
# rm(list = ls()) # Limpia el entorno de trabajo
# options(scipen = 999) # Evita la notación científica

# --- 1. Cargar Librerías Necesarias ---
library(dplyr)   # Para manipulación de datos
library(tidyr)   # Para pivot_longer, pivot_wider (si se necesitan)
library(ggplot2) # Para visualización (aunque aquí solo generamos tablas)
library(lubridate) # Para manejar fechas y extraer el día de la semana

# --- 2. Consolidar todos los data frames anuales en uno solo ---
cat("Consolidando todos los data frames de delitos limpios...\n")

# Lista de nombres de tus data frames anuales
nombres_tablas_delitos <- paste0("delitos_limpios_", 2019:2023)

# Inicializar una lista para almacenar los data frames
lista_delitos_consolidados <- list()

for (nombre_tabla in nombres_tablas_delitos) {
  if (exists(nombre_tabla) && is.data.frame(get(nombre_tabla))) {
    # Asegurarse de que 'fecha' sea tipo Date para lubridate
    df_temp <- get(nombre_tabla) %>%
      mutate(fecha = as.Date(fecha)) # Convertir a formato de fecha
    
    lista_delitos_consolidados[[nombre_tabla]] <- df_temp
  } else {
    warning(paste0("El objeto '", nombre_tabla, "' no es un data frame o no existe. Será omitido."))
  }
}

# Combinar todos los data frames en uno solo
delitos_consolidado_total <- bind_rows(lista_delitos_consolidados)

cat("✔ Data frame consolidado creado: 'delitos_consolidado_total'.\n")

# --- 3. Calcular Información Relevante ---

# 3.1 Promedio Diario de Delitos (General y por Año)
cat("\n--- Calculando Promedio Diario de Delitos ---\n")

# Calcular el número de días únicos por año
dias_por_año <- delitos_consolidado_total %>%
  mutate(año = year(fecha)) %>% # Extraer el año de la fecha
  distinct(año, fecha) %>%     # Obtener fechas únicas por año
  group_by(año) %>%
  summarise(
    total_dias = n()
  ) %>%
  ungroup()

# Conteo de delitos por año
conteo_delitos_por_año <- delitos_consolidado_total %>%
  mutate(año = year(fecha)) %>%
  group_by(año) %>%
  summarise(
    total_delitos = n()
  ) %>%
  ungroup()

# Unir y calcular el promedio diario
promedio_diario_delitos_anual <- left_join(conteo_delitos_por_año, dias_por_año, by = "año") %>%
  mutate(
    promedio_diario = total_delitos / total_dias
  ) %>%
  select(año, total_delitos, total_dias, promedio_diario)

cat("\n### Promedio Diario de Delitos por Año:\n")
print(promedio_diario_delitos_anual)

# Promedio diario total (general)
total_dias_observados <- n_distinct(delitos_consolidado_total$fecha)
total_delitos_observados <- nrow(delitos_consolidado_total)
promedio_diario_general <- total_delitos_observados / total_dias_observados

cat(paste0("\n### Promedio Diario General de Delitos (2019-2023): ", round(promedio_diario_general, 2), " delitos/día\n"))


# 3.2 Delitos por Barrio (Top N y Bottom N)
cat("\n--- Calculando Delitos por Barrio ---\n")

# Filtrar NAs en la columna 'barrio' antes de agrupar
delitos_por_barrio <- delitos_consolidado_total %>%
  filter(!is.na(barrio) & barrio != "") %>% # Eliminar NAs y cadenas vacías
  group_by(barrio) %>%
  summarise(
    conteo_delitos = n()
  ) %>%
  ungroup() %>%
  arrange(desc(conteo_delitos)) # Ordenar de mayor a menor

cat("\n### Top 10 Barrios con Más Delitos (2019-2023):\n")
print(head(delitos_por_barrio, 10))

cat("\n### Top 10 Barrios con Menos Delitos (2019-2023):\n")
print(tail(delitos_por_barrio, 10))


# 3.3 Delitos por Comuna (Top N y Bottom N)
# Asumiendo que tienes una columna 'comuna'
cat("\n--- Calculando Delitos por Comuna ---\n")

if ("comuna" %in% colnames(delitos_consolidado_total)) {
  delitos_por_comuna <- delitos_consolidado_total %>%
    filter(!is.na(comuna) & comuna != "") %>% # Limpiar NAs/vacíos
    group_by(comuna) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    ungroup() %>%
    arrange(desc(conteo_delitos))
  
  cat("\n### Top 5 Comunas con Más Delitos (2019-2023):\n")
  print(head(delitos_por_comuna, 5))
  
  cat("\n### Top 5 Comunas con Menos Delitos (2019-2023):\n")
  print(tail(delitos_por_comuna, 5))
  
} else {
  cat("Advertencia: No se encontró la columna 'comuna' en los datos consolidados. Saltando análisis por Comuna.\n")
}


# 3.4 Delitos por Día de la Semana
cat("\n--- Calculando Delitos por Día de la Semana ---\n")

delitos_por_dia_semana <- delitos_consolidado_total %>%
  mutate(
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, locale = "es_AR") # Nombre completo del día de la semana en español
  ) %>%
  filter(!is.na(dia_semana)) %>% # Limpiar NAs si los hubiera
  group_by(dia_semana) %>%
  summarise(
    conteo_delitos = n()
  ) %>%
  ungroup() %>%
  # Asegurar el orden correcto de los días de la semana
  mutate(dia_semana = factor(dia_semana, levels = c(
    "lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"
  ))) %>%
  arrange(dia_semana)

cat("\n### Conteo de Delitos por Día de la Semana (2019-2023):\n")
print(delitos_por_dia_semana)


# 3.5 Delitos por Sexo de la Víctima (Si la columna 'sexo' está disponible)
cat("\n--- Calculando Delitos por Sexo de la Víctima ---\n")

if ("sexo" %in% colnames(delitos_consolidado_total)) {
  delitos_por_sexo <- delitos_consolidado_total %>%
    mutate(
      sexo = as.character(sexo) # Asegurar que sea caracter
    ) %>%
    filter(!is.na(sexo) & sexo != "") %>% # Limpiar NAs y cadenas vacías
    group_by(sexo) %>%
    summarise(
      conteo_delitos = n()
    ) %>%
    ungroup() %>%
    # Opcional: ordenar si hay un orden preferido (ej. alfabético o por conteo)
    arrange(desc(conteo_delitos))
  
  cat("\n### Conteo de Delitos por Sexo de la Víctima (2019-2023):\n")
  print(delitos_por_sexo)
  
} else {
  cat("Advertencia: No se encontró la columna 'sexo' en los datos consolidados. Saltando análisis por Sexo de la Víctima.\n")
}

cat("\n¡Información relevante generada exitosamente!\n")

###-----------------------------------------------------------------------------
#grafico de barrios con mas delitos

# --- 0. Asegúrate de tener los datos preparados ---
# Si no has ejecutado el script anterior que genera 'delitos_por_barrio',
# deberías ejecutarlo primero para que este objeto esté en tu entorno.
# La tabla 'delitos_por_barrio' debería lucir así:
#    barrio          conteo_delitos
#    <chr>                    <int>
#  1 Palermo                  XXXXX
#  2 Balvanera                YYYYY
#  3 ...

# --- 1. Cargar Librerías Necesarias ---
library(ggplot2) # Para la creación de gráficos
library(dplyr)   # Para manipulación de datos (head, filter)
library(scales)  # Para formatear los números del eje (comas en miles)

# --- 2. Preparar los Datos para el Gráfico ---
# Seleccionar los top 10 barrios de la tabla 'delitos_por_barrio'
# Asegurarse de que 'barrio' sea un factor y reordenarlo por conteo_delitos
# para que las barras aparezcan en orden descendente en el gráfico.

top_10_barrios <- delitos_por_barrio %>%
  head(10) %>% # Selecciona las primeras 10 filas (que ya están ordenadas por conteo_delitos)
  mutate(barrio = factor(barrio, levels = rev(barrio))) # Convierte a factor y revierte el orden
# para que la barra más larga esté arriba.

# --- 3. Crear el Gráfico de Barras ---

cat("\nGenerando Gráfico: Top 10 Barrios con Más Delitos...\n")

p_top_10_barrios <- ggplot(top_10_barrios, aes(x = conteo_delitos, y = barrio)) +
  geom_bar(stat = "identity", fill = "#5B9BD5") + # Barras horizontales, color azul claro
  geom_text(aes(label = scales::comma(conteo_delitos)), # Etiquetas con conteo exacto
            hjust = -0.1, # Ajusta posición de la etiqueta (un poco a la derecha de la barra)
            size = 3.5,
            color = "black") +
  labs(
    title = "Top 10 Barrios con Mayor Cantidad de Delitos Registrados (2019-2023)",
    x = "Número de Delitos",
    y = "Barrio",
    caption = "Fuente: Datos de Delitos CABA"
  ) +
  scale_x_continuous(labels = scales::comma, # Formato de miles en el eje X
                     expand = expansion(mult = c(0, 0.15))) + # Expande el eje X para las etiquetas
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank(), # Elimina líneas de grilla horizontales
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"), # Mantén líneas verticales suaves
    plot.caption = element_text(hjust = 1, size = 9, color = "grey50")
  )

# Mostrar el gráfico
print(p_top_10_barrios)

# --- 4. Opcional: Guardar el Gráfico ---
ggsave("grafico_top_10_barrios.png", plot = p_top_10_barrios, width = 10, height = 7, dpi = 300)
cat("✔ Gráfico 'grafico_top_10_barrios.png' guardado.\n")


