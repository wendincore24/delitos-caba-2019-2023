# 📊 Análisis de Delitos en Ciudad de Buenos Aires (2019–2023)
Este proyecto analiza los delitos registrados en una ciudad durante el periodo 2019–2023. Utiliza datos espaciales, archivos estadísticos y visualizaciones en R para contar una historia visual y comprender patrones geográficos y temporales de los delitos.

---

## 📁 Estructura del repositorio

```
📦 analisis-delitos/
├── 📂 input/         → Archivos de entrada (CSV, Excel, shapefiles)
├── 📂 raw/           → Datos crudos sin procesar
├── 📂 scripts/       → Scripts en R para limpieza, análisis y visualización
├── 📂 output/        → Resultados, gráficos y tablas generadas
└── README.md         → Descripción general del proyecto
```

---

## 🧰 Herramientas utilizadas

- **R & RStudio**
- Paquetes R: `tidyverse`, `sf`, `ggplot2`, `readxl`, `dplyr`
- **ImageJ** (procesamiento de imágenes si aplica)
- **Git y GitHub**
- **Git LFS** para archivos grandes (`.shp`, `.dbf`)

---

## ⚠️ Archivos pesados

Este repositorio usa **Git LFS** para manejar archivos grandes como shapefiles (`.shp`, `.dbf`). Si vas a clonar este repositorio, asegurate de tener Git LFS instalado:  
👉 [https://git-lfs.com](https://git-lfs.com)

---

## 📝 Cómo reproducir el análisis

1. Cloná este repositorio:
   ```bash
   git clone https://github.com/tuusuario/nombre-del-repo.git
   ```

2. Abrí el proyecto en **RStudio**.

3. Instalá los paquetes necesarios:
   ```r
   install.packages(c("tidyverse", "sf", "ggplot2", "readxl"))
   ```

4. Ejecutá los scripts de la carpeta `/scripts` en orden.

---

## 📊 Resultados esperados

- Mapas de calor delictivo
- Tendencias temporales (gráficos de líneas)
- Comparaciones por tipo de delito o zonas
- Tablas resumen

*(Podés agregar capturas de pantalla si lo deseás)*

---

## 👩‍🎓 Proyecto académico

- **Universidad de Buenos Aires** – FCE  
- Curso: Ciencia de Datos para Economía y Negocios
- Año: 2025  
- **Alumnos**:  
  - Ingrid Cortez  
  - Ignacio Abrate

---

## 📄 Licencia

Este proyecto es de uso académico. No redistribuir los datos originales sin autorización si contienen información sensible.
