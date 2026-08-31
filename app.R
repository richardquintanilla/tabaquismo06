# app_obesidad_completa.R
# Dashboard EMP 2025 - Estado Nutricional
# Richard Quintanilla

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(plotly)
library(lubridate)
library(reactable)
library(fst)
library(chilemapas)
library(sf)
library(ggplot2)
library(openxlsx)
library(writexl)
library(janitor)
library(glue)

# =====================================================
# CARGA DE DATOS
# =====================================================

datos_long <- read_fst("rem/listados/data/rem_obesidad_categorias.fst")

if (!"obesidad" %in% names(datos_long)) {
  stop("❌ La columna 'obesidad' no existe. Columnas disponibles: ", 
       paste(names(datos_long), collapse = ", "))
}

mes_maximo <- max(datos_long$mes, na.rm = TRUE)
fecha_corte <- as.Date(paste0(2025, "-", mes_maximo, "-01")) + months(1) - days(1)

# =====================================================
# FUNCIONES DE FORMATEO
# =====================================================

formatear_numero <- function(x) {
  if(length(x) == 0) return("0")
  if(any(is.na(x)) || any(is.null(x))) return("0")
  if(length(x) > 1) {
    resultado <- sapply(x, function(val) {
      if(is.na(val) || is.null(val)) return("0")
      format(val, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
    })
    return(resultado)
  }
  if(is.na(x) || is.null(x)) return("0")
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

meses_orden <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Definición de categorías y sus columnas
categorias_estado_nutricional <- c("Bajo Peso", "Normal", "Sobrepeso", "Obesidad")

colores_categorias <- c(
  "Normal" = "#4CAF50",
  "Bajo Peso" = "#FFC107", 
  "Sobrepeso" = "#FF9800",
  "Obesidad" = "#E53935"
)

columna_por_categoria <- list(
  "Bajo Peso" = "bajo_peso",
  "Normal" = "normal",
  "Sobrepeso" = "sobrepeso",
  "Obesidad" = "obesidad"
)

# =====================================================
# FUNCIÓN DE MAPA
# =====================================================

crear_mapa <- function(df_mapa, 
                       codigo_comuna, nombre_comuna, provincia, 
                       valor, valor_indicador,
                       grupo_etario_label = "Todos",
                       sexo_label = "Ambos",
                       mes_label = "Todos",
                       provincia_label = "Todas",
                       comuna_label = "Todas",
                       titulo_leyenda = "Tasa x 100.000 hab.",
                       label_indicador = "Tasa",
                       es_porcentaje = FALSE) 
{
  cc <- codigo_comuna
  nn <- nombre_comuna
  pp <- provincia
  pv <- valor
  pi <- valor_indicador
  
  df_user <- df_mapa %>% 
    mutate(
      codigo_comuna = .data[[cc]], 
      nombre_comuna = .data[[nn]],
      provincia = .data[[pp]],
      valor = .data[[pv]],
      indicador = .data[[pi]],
      Total_EMP = Total_EMP
    )
  
  mapa_sf <- chilemapas::mapa_comunas %>%
    filter(codigo_region == "06")
  
  mapa_join <- mapa_sf %>% 
    left_join(df_user, by = "codigo_comuna") %>%
    sf::st_as_sf()
  
  mapa_join <- mapa_join %>%
    mutate(
      valor = ifelse(is.na(valor), 0, valor),
      indicador = ifelse(is.na(indicador), 0, indicador),
      activa = ifelse(valor > 0, TRUE, FALSE)
    )
  
  if(!"nombre_comuna" %in% names(mapa_join)) {
    mapa_join$nombre_comuna <- mapa_sf$nombre_comuna
  }
  
  if(!"provincia" %in% names(mapa_join)) {
    mapa_join$provincia <- NA_character_
  }
  
  # Calcular percentiles
  valores_activos <- mapa_join$indicador[mapa_join$activa]
  
  if(length(valores_activos) > 0) {
    p33 <- quantile(valores_activos, 1/3, na.rm = TRUE)
    p66 <- quantile(valores_activos, 2/3, na.rm = TRUE)
    
    mapa_join <- mapa_join %>%
      mutate(
        grupo = case_when(
          !activa ~ "Inactiva",
          indicador <= p33 ~ "Bajo",
          indicador <= p66 ~ "Medio",
          TRUE ~ "Alto"
        ),
        grupo = factor(grupo, levels = c("Bajo", "Medio", "Alto", "Inactiva")),
        fill_var = grupo
      )
  } else {
    mapa_join <- mapa_join %>%
      mutate(
        grupo = "Inactiva",
        grupo = factor(grupo, levels = c("Bajo", "Medio", "Alto", "Inactiva")),
        fill_var = grupo
      )
  }
  
  # Tooltip
  mapa_join <- mapa_join %>%
    mutate(
      text_label = case_when(
        !activa ~ paste0(
          "<b>", nombre_comuna, "</b><br>",
          "Provincia: ", ifelse(is.na(provincia), "Sin dato", provincia), "<br>",
          "Mes: ", mes_label, "<br>",
          "Grupo Etario: ", grupo_etario_label, "<br>",
          "Sexo: ", sexo_label, "<br>",
          "Sin datos para los filtros seleccionados"
        ),
        TRUE ~ paste0(
          "<b>", nombre_comuna, "</b><br>",
          "Provincia: ", ifelse(is.na(provincia), "Sin dato", provincia), "<br>",
          "Mes: ", mes_label, "<br>",
          "Grupo Etario: ", grupo_etario_label, "<br>",
          "Sexo: ", sexo_label, "<br>",
          "Total EMP: ", format(round(Total_EMP, 0), big.mark = ".", decimal.mark = ","), "<br>",
          categoria_seleccionada_global(), ": ", format(round(valor, 0), big.mark = ".", decimal.mark = ","), "<br>",
          "% del total de EMP: ", format(round(indicador, 1), big.mark = ".", decimal.mark = ","), "%"
        )
      )
    )
  
  g <- ggplot(mapa_join) + 
    geom_sf(aes(geometry = geometry, fill = fill_var), linewidth = 0.3) + 
    geom_sf_text(aes(label = nombre_comuna, text = text_label), 
                 size = 2.8, fontface = "bold", color = "black") + 
    labs(fill = titulo_leyenda, 
         x = "Longitud", y = "Latitud") + 
    theme_light(base_size = 10) + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(1, "cm"),
          plot.title = element_blank())
  
  g <- g + scale_fill_manual(
    values = c(
      "Bajo" = "#4CAF50",
      "Medio" = "#FFC107",
      "Alto" = "#E53935",
      "Inactiva" = "#D3D3D3"
    ),
    na.value = "#D3D3D3",
    drop = FALSE
  )
  
  plotly::ggplotly(g, tooltip = "text") %>% 
    layout(
      xaxis = list(autorange = TRUE, scaleanchor = "y", scaleratio = 1),
      yaxis = list(autorange = TRUE),
      margin = list(l = 40, r = 40, t = 20, b = 60),
      hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12))
    )
}

# Variable global para el mapa
categoria_seleccionada_global <- reactiveVal("Obesidad")

# =====================================================
# FUNCIÓN rt_tabla
# =====================================================

rt_tabla <- function(df, fijas = NULL, grupos = NULL, titulos = NULL, filtrar = TRUE, 
                     barras = NULL, color_barra = c("#cd0000", "#ffa500", "#00cd00", 
                                                    "#0000ee", "#551a8b"), destacar_col = NULL, color_destacar = "#e3e3e3", 
                     cols_porcentaje = NULL, destacar_row = NULL, highlight_color = "#f0e68c", 
                     decimales = 0, decimales_col = NULL) 
{
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  if (inherits(df, "SharedData")) {
    df_data <- df$data()
  } else {
    df_data <- df
  }
  
  titulos <- titulos %||% list()
  decimales_col <- decimales_col %||% list()
  
  get_decimales <- function(col) {
    if (!is.null(decimales_col[[col]])) decimales_col[[col]] else decimales
  }
  
  destacar_col <- intersect(destacar_col %||% character(0), names(df_data))
  cols_porcentaje <- intersect(cols_porcentaje %||% character(0), names(df_data))
  barras <- intersect(barras %||% character(0), names(df_data))
  fijas <- intersect(fijas %||% character(0), names(df_data))
  
  css_js <- htmltools::tagList(htmltools::tags$style(htmltools::HTML(sprintf("
    .reactable {
      font-family: sans-serif !important;
      font-size: 13px !important;
    }
    .reactable .rt-th,
    .reactable .rt-th-group {
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      text-align: center !important;
    }
    .reactable .rt-th.col-fija {
      justify-content: flex-start !important;
      text-align: left !important;
      padding-left: 8px;
    }
    .reactable .rt-td-inner:not(:has(.barra-outer)) {
      display: flex;
      align-items: center;
      justify-content: center;
      height: 100%%;
    }
    .reactable .rt-td.col-fija .rt-td-inner {
      justify-content: flex-start !important;
    }
    .reactable .rt-tr:hover .rt-td:not(.col-fija) {
      background-color: %s !important;
    }
    .rt-td.column-hover:not(.col-fija) {
      background-color: %s !important;
    }
    .rt-td.col-fija {
      background-color: #191970 !important;
      color: white !important;
    }
    .rt-tr:hover .rt-td.col-fija {
      background-color: #191970 !important;
    }
    .rt-td, .rt-td .rt-td-inner, .barra-outer, .barra-label {
      transition: font-size 0.14s ease, transform 0.12s ease;
    }
    .rt-td.cell-hover:not(.col-fija) {
      background-color: khaki !important;
      z-index: 999 !important;
      box-shadow: 0 0 0 2px midnightblue !important;
      font-weight: bold !important;
    }
    .rt-td.cell-hover:not(.col-fija) .rt-td-inner,
    .rt-td.cell-hover:not(.col-fija) .barra-label {
      font-size: 16px !important;
      font-weight: bold !important;
    }
    .reactable .rt-thead-group,
    .reactable .rt-th-group {
      background-color: #191970 !important;
      color: white !important;
      font-weight: bold !important;
      text-align: center !important;
      font-family: inherit !important;
    }
    .barra-outer {
      border: 1px solid #d0d0d0 !important;
      border-radius: 4px !important;
    }
  ", highlight_color, highlight_color))), htmltools::tags$script(htmltools::HTML("
    document.addEventListener('DOMContentLoaded', function() {
      const tables = document.querySelectorAll('.reactable');
      tables.forEach(table => {
        const inners = table.querySelectorAll('.rt-td-inner');
        inners.forEach(inner => {
          const cell = inner.closest('.rt-td');
          if (!cell) return;
          const colClass = Array.from(cell.classList)
            .find(cl => cl.startsWith('col-'));
          if (cell.classList.contains('col-fija')) return;
          if (!colClass) return;
          inner.addEventListener('mouseenter', () => {
            table.querySelectorAll('.' + colClass)
              .forEach(td => td.classList.add('column-hover'));
            cell.classList.add('cell-hover');
          });
          inner.addEventListener('mouseleave', () => {
            table.querySelectorAll('.column-hover')
              .forEach(td => td.classList.remove('column-hover'));
            cell.classList.remove('cell-hover');
          });
        });
      });
    });
  ")))
  
  font_family_base <- "sans-serif"
  font_size_base <- "13px"
  
  clean_numeric <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    xch <- gsub("(?<=\\d)\\.(?=\\d{3}(?:\\D|$))", "", trimws(as.character(x)), perl = TRUE)
    xch <- gsub(",", ".", xch, fixed = TRUE)
    suppressWarnings(as.numeric(xch))
  }
  
  columnas <- lapply(names(df_data), function(colname) {
    local({
      col <- colname
      class_col <- paste0("col-", gsub("\\s+", "_", col))
      estilo_base <- list(fontFamily = font_family_base, fontSize = font_size_base, 
                          fontWeight = "normal", textAlign = "center")
      
      if (col %in% fijas) {
        return(reactable::colDef(
          name = titulos[[col]] %||% col, 
          sticky = "left",
          align = "left", 
          class = paste(class_col, "col-fija"), 
          headerStyle = list(background = "#191970", color = "white", fontWeight = "bold", 
                             fontFamily = font_family_base, textAlign = "center"), 
          style = list(background = "#191970", color = "white", fontFamily = font_family_base, 
                       fontSize = font_size_base, fontWeight = "bold", borderRight = "2px solid white")
        ))
      }
      
      if (col %in% barras) {
        valores_limpios <- clean_numeric(df_data[[col]])
        pal <- if (length(color_barra) == 5) color_barra else rep("#ccc", 5)
        es_pct <- col %in% cols_porcentaje
        digs <- get_decimales(col)
        is_dest_col <- col %in% destacar_col
        return(reactable::colDef(
          name = titulos[[col]] %||% col, 
          class = class_col, 
          align = "center", 
          html = TRUE, 
          sortable = TRUE, 
          style = if (is_dest_col) list(background = color_destacar, fontWeight = "normal", 
                                        fontFamily = font_family_base, fontSize = font_size_base) else estilo_base, 
          cell = function(value, index) {
            val_num <- clean_numeric(df_data[[col]][index])
            if (!is.finite(val_num)) {
              displayed <- ""
              prop <- 0
              color_fill <- pal[1]
            } else {
              displayed <- if (es_pct) {
                paste0(formatC(val_num * 100, format = "f", digits = digs, decimal.mark = ","), "%")
              } else {
                formatC(val_num, format = "f", digits = digs, big.mark = ".", decimal.mark = ",")
              }
              min_col <- min(valores_limpios, na.rm = TRUE)
              max_col <- max(valores_limpios, na.rm = TRUE)
              if (es_pct) {
                prop <- min(val_num, 1)
                qs <- seq(0, 1, length.out = 6)
              } else if (max_col - min_col == 0) {
                prop <- 1
                qs <- seq(0, 1, length.out = 6)
              } else {
                prop <- (val_num - min_col)/(max_col - min_col)
                qs <- quantile(valores_limpios, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
              }
              grp <- findInterval(val_num, qs, all.inside = TRUE)
              color_fill <- pal[grp]
            }
            htmltools::HTML(sprintf("
              <div style='display:flex;align-items:center;gap:6px;'>
                <div class='barra-label' style='min-width:45px;text-align:right;font-family:sans-serif;font-size:13px;'>%s</div>
                <div class='barra-outer' style='flex-grow:1;height:14px;background:#f0f0f0;overflow:hidden;'>
                  <div style='height:100%%;width:%s%%;background:%s;'></div>
                </div>
              </div>", displayed, prop * 100, color_fill))
          }
        ))
      }
      
      if (col %in% destacar_col) {
        return(reactable::colDef(
          name = titulos[[col]] %||% col, 
          class = class_col, 
          align = "center", 
          style = list(background = color_destacar, fontWeight = "normal", 
                       fontFamily = font_family_base, fontSize = font_size_base), 
          format = if (col %in% cols_porcentaje) reactable::colFormat(percent = TRUE, 
                                                                      digits = get_decimales(col), locale = "es") else reactable::colFormat(separators = TRUE, 
                                                                                                                                            digits = get_decimales(col), locale = "es")
        ))
      }
      
      if (is.numeric(df_data[[col]])) {
        es_pct <- col %in% cols_porcentaje
        digs <- get_decimales(col)
        return(reactable::colDef(
          name = titulos[[col]] %||% col, 
          class = class_col, 
          align = "center", 
          style = estilo_base, 
          format = if (es_pct) reactable::colFormat(percent = TRUE, digits = digs, locale = "es") 
          else reactable::colFormat(separators = TRUE, digits = digs, locale = "es")
        ))
      }
      
      reactable::colDef(name = titulos[[col]] %||% col, class = class_col, align = "center", style = estilo_base)
    })
  })
  
  names(columnas) <- names(df_data)
  
  fila_style_fun <- function(i) {
    if (!is.null(destacar_row) && df_data[[1]][i] %in% destacar_row) {
      return(list(background = color_destacar, fontWeight = "bold"))
    }
    list()
  }
  
  columnGroups <- NULL
  if (!is.null(grupos)) {
    columnGroups <- lapply(names(grupos), function(g) {
      reactable::colGroup(name = g, columns = grupos[[g]])
    })
  }
  
  tbl <- reactable::reactable(
    df, 
    columns = columnas, 
    columnGroups = columnGroups, 
    rowStyle = fila_style_fun, 
    highlight = TRUE, 
    searchable = filtrar, 
    striped = TRUE, 
    bordered = TRUE, 
    pagination = FALSE, 
    language = reactable::reactableLang(
      searchPlaceholder = "Filtrar", 
      noData = "No se encontraron resultados"
    ), 
    defaultColDef = reactable::colDef(
      align = "center", 
      html = TRUE, 
      headerStyle = list(
        background = "#191970", 
        color = "white", 
        fontWeight = "bold", 
        fontFamily = font_family_base, 
        textAlign = "center"
      ), 
      style = list(
        fontFamily = font_family_base, 
        fontSize = font_size_base
      )
    )
  )
  
  htmltools::browsable(htmltools::tagList(css_js, tbl))
}

# =====================================================
# FUNCIÓN DATOS POR CATEGORÍA
# =====================================================

datos_categoria_comuna <- function(df, categoria) {
  col <- columna_por_categoria[[categoria]]
  df %>%
    group_by(codigo_comuna, nombre_comuna, nombre_provincia) %>%
    summarise(
      Total_EMP = sum(total_emp, na.rm = TRUE),
      Categoria = sum(.data[[col]], na.rm = TRUE),
      `% del total de EMP` = ifelse(Total_EMP > 0, (Categoria / Total_EMP) * 100, 0),
      .groups = "drop"
    )
}

# =====================================================
# UI
# =====================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "EMP2025 - Estado Nutricional",
    titleWidth = 300,
    tags$li(class = "dropdown",
            div(style = "margin-right: 20px; margin-top: 15px; color: white; font-weight: normal;",
                textOutput("fecha_corte_header"))
    )
  ),
  
  dashboardSidebar(
    width = 300,
    tags$style(HTML("
    .skin-blue .main-header { position: fixed; width: 100%; z-index: 1030; top: 0; }
    .main-sidebar {
      position: fixed;
      top: 50px;
      bottom: 0;
      left: 0;
      z-index: 1020;
      overflow-y: auto !important;
      height: calc(100vh - 50px) !important;
    }
    .content-wrapper, .right-side { margin-left: 300px; padding-top: 50px; overflow-x: hidden; }
    @media (max-width: 767px) { .content-wrapper, .right-side { margin-left: 0; } }
    .main-sidebar { background-color: #191970 !important; }
    .sidebar-menu > li > a { color: #ecf0f1 !important; background-color: #191970 !important; }
    .sidebar-menu > li > a:hover { background-color: #2c2c8a !important; }
    .skin-blue .main-header .navbar { background-color: #191970 !important; }
    .skin-blue .main-header .logo { background-color: #191970 !important; }
    .content-wrapper, .right-side { background-color: #f4f4f4; }
    .box, .portlet { border: none !important; box-shadow: none !important; }
    .box.box-primary, .box.box-info { border: none !important; }
    .box-body { border: none !important; }
    .box.box-solid.box-primary > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
    .box.box-solid.box-info > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
    .control-label { font-weight: normal !important; }
    .sidebar .selectize-control, .sidebar .shiny-input-container:not(.shiny-input-container-inline) { width: 100% !important; }
    .sidebar .checkbox, .sidebar .action-button { width: 100%; margin-left: 0; margin-right: 0; }
    .sidebar .action-button { margin-top: 5px; }
    
    /* REDUCIR ESPACIO DE LOS LOGOS */
    .sidebar .logo-container { margin-top: 5px !important; margin-bottom: 5px !important; }
    .sidebar .logo-container img { height: 60px !important; }
    
    .custom-box { border-radius: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24); transition: all 0.3s cubic-bezier(.25,.8,.25,1); margin-bottom: 20px; position: relative; color: white; }
    .custom-box:hover { box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22); }
    .custom-box .inner { padding: 15px; text-align: center; }
    .custom-box .inner h3 { font-size: 38px; font-weight: bold; margin: 0 0 10px 0; white-space: nowrap; padding: 0; }
    .custom-box .inner p { font-size: 14px; margin: 0; font-weight: bold; }
    .custom-box .icon { position: absolute; right: 10px; top: 10px; font-size: 50px; opacity: 0.3; }
    .bg-purple-custom { background-color: #6f42c1 !important; }
    .bg-green-custom { background-color: #28a745 !important; }
    .bg-yellow-custom { background-color: #ffc107 !important; }
    .bg-gray-custom { background-color: #7f7f7f !important; }
    .bg-blue-custom { background-color: #2596be !important; }
    .bg-red-custom { background-color: #ec3d43 !important; }
    .bg-orange-custom { background-color: #FF9800 !important; }
    .box.box-primary > .box-header { background-color: #191970 !important; color: white !important; }
    .box.box-info > .box-header { background-color: #2c2c8a !important; color: white !important; }
    .selectize-input, .selectize-dropdown { background-color: #ecf0f1 !important; color: #191970 !important; }
    
    .reactable { overflow-y: auto !important; max-height: 600px; }
    .reactable .rt-thead {
      position: sticky !important;
      top: 0 !important;
      z-index: 1000 !important;
      background-color: #191970 !important;
    }
    
    .sidebar-menu {margin-top: 0 !important; padding-top: 5px !important;}
    .main-sidebar, .sidebar {padding-top: 0 !important; margin-top: 0 !important; background-color: #191970 !important;}
    .wrapper {background-color: #191970 !important;}
    
    #clear_filters {background-color: #EEE9E9 !important;color: #191970 !important;}
    #clear_filters:hover {background-color: #d3d3d3 !important; color: #191970 !important;}
    .sidebar-menu > li.active > a {border-left-color: #ff0000 !important;}
    .sidebar-menu > li > a:hover {border-left-color: transparent !important; background-color: #EEE9E9 !important; color: #191970 !important;}
    .skin-blue .main-header .sidebar-toggle:hover {background-color: #EEE9E9 !important;}
    
    /* REDUCIR ESPACIO ENTRE FILTROS */
    .form-group { margin-bottom: 8px !important; }
    .shiny-input-container { margin-bottom: 5px !important; }
    hr { margin: 8px 0 !important; }
    h4 { margin-bottom: 8px !important; }
  ")),
    
    div(style = "display: flex; justify-content: center; align-items: center; gap: 10px; padding: 5px 0; margin: 0;",
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_seremi.png", 
                 height = "60px", style = "display: block;"),
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_uaid_blanco.png", 
                 height = "70px", style = "display: block;")
    ),
    
    sidebarMenu(
      menuItem("📊 Resumen General", tabName = "resumen"),
      menuItem("🌎 Mapa Estadístico", tabName = "mapas"),
      menuItem("📋 Tabla Resumen", tabName = "tablas"),
      menuItem("📥 Descarga de datos", tabName = "descarga")
    ),
    
    br(),
    hr(),
    h4("Filtros", style = "padding-left: 15px; color: #ecf0f1; font-weight: normal; margin-bottom: 8px;"),
    
    selectInput("categoria_filter", "📊 Categoría Estado Nutricional:",
                choices = c("Bajo Peso", "Normal", "Sobrepeso", "Obesidad"), 
                selected = "Obesidad",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("provincia_filter", "🏛️ Provincia:",
                choices = c("Todas"), 
                selected = "Todas",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("comuna_filter", "🏘️ Comuna:",
                choices = c("Todas"), 
                selected = "Todas",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("grupo_etario_filter", "🧑‍🤝‍🧑 Grupo Etario (en años):",
                choices = c("Todos", "15-19", "20-24", "25-29", "30-34", 
                            "35-39", "40-44", "45-49", "50-54", "55-59", 
                            "60-64", "65-69", "70-74", "75-79", "80+"), 
                selected = "Todos",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("mes_filter", "📅 Mes:",
                choices = c("Todos", meses_orden), 
                selected = "Todos",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("sexo_filter", "👤 Sexo:",
                choices = c("Todos", "Hombres", "Mujeres"), 
                selected = "Todos",
                multiple = FALSE,
                selectize = TRUE),
    
    actionButton("clear_filters", "Limpiar filtros", icon = icon("eraser"),
                 style = "width: 100%; background-color: #95a5a6; color: #191970; border: none; margin-top: 5px; margin-left: 0; margin-right: 0;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { border: none !important; box-shadow: none !important; border-radius: 0 !important; }
        .box.box-primary > .box-header { background-color: #191970 !important; color: white !important; border-radius: 0 !important; }
        .content-wrapper, .right-side { background-color: #ffffff; }
        .main-header { position: fixed; width: 100%; z-index: 1030; }
        .main-sidebar { position: fixed; height: 100vh; overflow-y: auto; z-index: 1020; }
        .content-wrapper { margin-top: 50px; margin-left: 300px; padding: 15px; }
        .box.box-solid.box-primary { border: 2px solid #191970 !important; border-radius: 5px !important; }
        .box.box-solid.box-primary > .box-header {
          background-color: #191970 !important;
          color: white !important;
          font-size: 16px !important;
          font-weight: bold !important;
        }
      "))
    ),
    tabItems(
      # =====================================================
      # PESTAÑA 1: RESUMEN GENERAL
      # =====================================================
      tabItem(tabName = "resumen",
              fluidRow(
                uiOutput("tarjeta_total_emp"),
                uiOutput("tarjeta_hombres_emp"),
                uiOutput("tarjeta_mujeres_emp")
              ),
              fluidRow(
                uiOutput("tarjeta_total_categoria"),
                uiOutput("tarjeta_hombres_categoria"),
                uiOutput("tarjeta_mujeres_categoria")
              ),
              fluidRow(
                box(title = "N° Exámenes de Medicina Preventiva por Grupo Etario y Sexo (Total EMP vs Estado Nutricional)", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_etario", height = "400px"))
              ),
              fluidRow(
                box(title = "N° Exámenes de Medicina Preventiva por Mes y Sexo (Total EMP vs Estado Nutricional)", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_mensual", height = "400px"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 2: MAPA
      # =====================================================
      tabItem(tabName = "mapas",
              fluidRow(
                box(title = textOutput("titulo_mapa"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("mapa_porcentaje", height = "600px"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 3: TABLAS
      # =====================================================
      tabItem(tabName = "tablas",
              fluidRow(
                box(title = "Tabla de Prevalencia por Provincia y Regional", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("tabla_resumen_provincia"))
              ),
              fluidRow(
                box(title = textOutput("titulo_tabla"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("tabla_porcentaje"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 4: DESCARGA
      # =====================================================
      tabItem(tabName = "descarga",
              fluidRow(
                box(title = "Descarga de datos", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 20px;",
                        icon("database", class = "fa-4x", style = "color: #191970;"),
                        br(),
                        h4("📋 Contenido del archivo Excel", style = "color: #191970; margin-top: 15px;"),
                        p("Este apartado permite descargar un archivo Excel con los siguientes datos:"),
                        tags$ul(style = "text-align: left; display: inline-block; margin: 10px auto;",
                                tags$li(strong("Metadatos"), 
                                        " - Información sobre filtros aplicados y fecha de corte"),
                                tags$li(strong("Resumen por Comuna"), 
                                        " - Datos resumen por comuna"),
                                tags$li(strong("Resumen por Provincia"), 
                                        " - Datos resumen por provincia y regional"),
                                tags$li(strong("Resumen Tarjetas"), 
                                        " - Valores de las tarjetas del dashboard"),
                                tags$li(strong("Detalle por Sexo y Grupo Etario"), 
                                        " - Datos desagregados por sexo y grupo etario"),
                                tags$li(strong("Detalle por Mes y Sexo"), 
                                        " - Datos desagregados por mes y sexo")
                        ),
                        br(),
                        p("Los datos incluyen los filtros actualmente seleccionados:", 
                          style = "font-weight: bold;"),
                        p(textOutput("desc_filtros"), style = "color: #555;"),
                        br(),
                        downloadButton("descargar_excel", 
                                       "Descargar Excel", 
                                       class = "btn-primary",
                                       style = "font-size: 16px; padding: 10px 30px; background-color: #191970; border-color: #191970; color: white;")
                    )
                )
              )
      )
    )
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ACTUALIZAR CATEGORÍA GLOBAL PARA EL MAPA
  observe({
    categoria_seleccionada_global(input$categoria_filter)
  })
  
  # ACTUALIZAR FILTROS
  observe({
    provincias <- sort(unique(datos_long$nombre_provincia))
    updateSelectInput(session, "provincia_filter", 
                      choices = c("Todas", provincias), 
                      selected = input$provincia_filter)
  })
  
  observe({
    if(input$provincia_filter == "Todas") {
      comunas <- sort(unique(datos_long$nombre_comuna))
    } else {
      comunas <- sort(unique(datos_long$nombre_comuna[datos_long$nombre_provincia == input$provincia_filter]))
    }
    updateSelectInput(session, "comuna_filter", 
                      choices = c("Todas", comunas), 
                      selected = input$comuna_filter)
  })
  
  # LIMPIAR FILTROS
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "categoria_filter", selected = "Obesidad")
    updateSelectInput(session, "provincia_filter", selected = "Todas")
    updateSelectInput(session, "comuna_filter", selected = "Todas")
    updateSelectInput(session, "mes_filter", selected = "Todos")
    updateSelectInput(session, "sexo_filter", selected = "Todos")
    updateSelectInput(session, "grupo_etario_filter", selected = "Todos")
  })
  
  # =====================================================
  # DATOS FILTRADOS
  # =====================================================
  
  datos_filtrados <- reactive({
    df <- datos_long %>%
      filter(sexo != "Ambos", grupo_etario != "Total")
    
    if(input$provincia_filter != "Todas") {
      df <- df %>% filter(nombre_provincia == input$provincia_filter)
    }
    if(input$comuna_filter != "Todas") {
      df <- df %>% filter(nombre_comuna == input$comuna_filter)
    }
    if(input$mes_filter != "Todos") {
      df <- df %>% filter(nombre_mes == input$mes_filter)
    }
    if(input$sexo_filter != "Todos") {
      df <- df %>% filter(sexo == input$sexo_filter)
    }
    if(input$grupo_etario_filter != "Todos") {
      df <- df %>% filter(grupo_etario == input$grupo_etario_filter)
    }
    
    df
  })
  
  # Obtener categoría seleccionada
  categoria_seleccionada <- reactive({
    input$categoria_filter
  })
  
  columna_categoria <- reactive({
    columna_por_categoria[[categoria_seleccionada()]]
  })
  
  # =====================================================
  # DATOS RESUMEN POR COMUNA
  # =====================================================
  
  datos_resumen <- reactive({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    
    col <- columna_categoria()
    
    df %>%
      group_by(codigo_comuna, nombre_provincia, nombre_comuna) %>%
      summarise(
        Total_EMP = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Categoria / Total_EMP) * 100, 
                                      0),
        .groups = "drop"
      )
  })
  
  # =====================================================
  # DATOS RESUMEN POR PROVINCIA
  # =====================================================
  
  datos_resumen_provincia <- reactive({
    # Usar datos filtrados SOLO por mes, sexo y grupo_etario
    df <- datos_long %>%
      filter(sexo != "Ambos", grupo_etario != "Total")
    
    # Aplicar SOLO filtros que no rompan la lógica de provincia
    if(input$mes_filter != "Todos") {
      df <- df %>% filter(nombre_mes == input$mes_filter)
    }
    if(input$sexo_filter != "Todos") {
      df <- df %>% filter(sexo == input$sexo_filter)
    }
    if(input$grupo_etario_filter != "Todos") {
      df <- df %>% filter(grupo_etario == input$grupo_etario_filter)
    }
    
    # NO filtrar por provincia ni comuna (para mantener el contexto regional)
    
    if(nrow(df) == 0) {
      return(data.frame())
    }
    
    col <- columna_categoria()
    
    df_provincia <- df %>%
      group_by(nombre_provincia) %>%
      summarise(
        Total_EMP = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Categoria / Total_EMP) * 100, 
                                      0),
        .groups = "drop"
      )
    
    total_regional <- df %>%
      summarise(
        nombre_provincia = "Región de O'Higgins",
        Total_EMP = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Categoria / Total_EMP) * 100, 
                                      0)
      )
    
    bind_rows(df_provincia, total_regional)
  })
  
  # =====================================================
  # DATOS PARA DESCARGA
  # =====================================================
  
  datos_descarga <- reactive({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    
    col <- columna_categoria()
    cat <- categoria_seleccionada()
    
    # Resumen por comuna
    df_comuna <- df %>%
      group_by(codigo_comuna, nombre_provincia, nombre_comuna) %>%
      summarise(
        `Total EMP` = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `Porcentaje del total de EMP` = ifelse(`Total EMP` > 0, (Categoria / `Total EMP`) * 100, 0),
        .groups = "drop"
      ) %>%
      rename(
        `Código comuna` = codigo_comuna,
        Provincia = nombre_provincia,
        Comuna = nombre_comuna
      )
    
    # Renombrar columna Categoria por el nombre de la categoría
    names(df_comuna)[names(df_comuna) == "Categoria"] <- cat
    
    # Resumen por provincia
    df_provincia <- df %>%
      group_by(nombre_provincia) %>%
      summarise(
        `Total EMP` = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `Porcentaje del total de EMP` = ifelse(`Total EMP` > 0, (Categoria / `Total EMP`) * 100, 0),
        .groups = "drop"
      ) %>%
      rename(Provincia = nombre_provincia)
    
    # Renombrar columna Categoria por el nombre de la categoría
    names(df_provincia)[names(df_provincia) == "Categoria"] <- cat
    
    # Regional
    df_regional <- df %>%
      summarise(
        Provincia = "Región de O'Higgins",
        `Total EMP` = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        `Porcentaje del total de EMP` = ifelse(`Total EMP` > 0, (Categoria / `Total EMP`) * 100, 0)
      )
    
    names(df_regional)[names(df_regional) == "Categoria"] <- cat
    
    df_provincia <- bind_rows(df_provincia, df_regional)
    
    # Detalle por sexo y grupo
    df_detalle <- df %>%
      group_by(sexo, grupo_etario) %>%
      summarise(
        `Total EMP` = sum(total_emp, na.rm = TRUE),
        Normal = sum(normal, na.rm = TRUE),
        `Bajo Peso` = sum(bajo_peso, na.rm = TRUE),
        Sobrepeso = sum(sobrepeso, na.rm = TRUE),
        Obesidad = sum(obesidad, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(
        Sexo = sexo,
        `Grupo Etario` = grupo_etario
      )
    
    # Detalle por mes y sexo
    df_detalle_mes <- df %>%
      group_by(nombre_mes, sexo) %>%
      summarise(
        `Total EMP` = sum(total_emp, na.rm = TRUE),
        Normal = sum(normal, na.rm = TRUE),
        `Bajo Peso` = sum(bajo_peso, na.rm = TRUE),
        Sobrepeso = sum(sobrepeso, na.rm = TRUE),
        Obesidad = sum(obesidad, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(nombre_mes = factor(nombre_mes, levels = meses_orden)) %>%
      arrange(nombre_mes) %>%
      mutate(nombre_mes = as.character(nombre_mes)) %>%
      rename(
        Mes = nombre_mes,
        Sexo = sexo
      )
    
    # Resumen tarjetas
    df_tarjetas <- data.frame(
      Indicador = c("Total EMP", "Hombres EMP", "Mujeres EMP", cat, paste("Hombres", cat), paste("Mujeres", cat)),
      Cantidad = c(
        sum(df$total_emp, na.rm = TRUE),
        sum(df$total_emp[df$sexo == "Hombres"], na.rm = TRUE),
        sum(df$total_emp[df$sexo == "Mujeres"], na.rm = TRUE),
        sum(df[[col]], na.rm = TRUE),
        sum(df[[col]][df$sexo == "Hombres"], na.rm = TRUE),
        sum(df[[col]][df$sexo == "Mujeres"], na.rm = TRUE)
      )
    )
    
    # Metadatos
    df_metadatos <- data.frame(
      Campo = c(
        "Fecha de corte",
        "Categoría Estado Nutricional",
        "Provincia",
        "Comuna",
        "Mes",
        "Sexo",
        "Grupo Etario",
        "Fecha de descarga"
      ),
      Valor = c(
        format(fecha_corte, "%d-%m-%Y"),
        cat,
        if(input$provincia_filter == "Todas") "Todas" else input$provincia_filter,
        if(input$comuna_filter == "Todas") "Todas" else input$comuna_filter,
        if(input$mes_filter == "Todos") "Todos" else input$mes_filter,
        if(input$sexo_filter == "Todos") "Todos" else input$sexo_filter,
        if(input$grupo_etario_filter == "Todos") "Todos" else input$grupo_etario_filter,
        format(Sys.Date(), "%d-%m-%Y")
      )
    )
    
    list(
      metadatos = df_metadatos,
      comuna = df_comuna,
      provincia = df_provincia,
      tarjetas = df_tarjetas,
      detalle = df_detalle,
      detalle_mes = df_detalle_mes
    )
  })
  
  # =====================================================
  # TARJETAS
  # =====================================================
  
  # Total EMP - MORADO
  output$tarjeta_total_emp <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    total <- sum(datos$total_emp, na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-purple-custom",
            div(class = "icon", icon("hospital")),
            div(class = "inner", 
                h3(formatear_numero(total)), 
                p("Total EMP", style = "font-weight: bold;"))))
  })
  
  # Hombres EMP - VERDE
  output$tarjeta_hombres_emp <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    total_h <- sum(datos$total_emp[datos$sexo == "Hombres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-green-custom",
            div(class = "icon", icon("mars")),
            div(class = "inner", 
                h3(formatear_numero(total_h)), 
                p("Hombres EMP", style = "font-weight: bold;"))))
  })
  
  # Mujeres EMP - AMARILLO
  output$tarjeta_mujeres_emp <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    total_m <- sum(datos$total_emp[datos$sexo == "Mujeres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-yellow-custom",
            div(class = "icon", icon("venus")),
            div(class = "inner", 
                h3(formatear_numero(total_m)), 
                p("Mujeres EMP", style = "font-weight: bold;"))))
  })
  
  # Total Categoría - GRIS (cambia ícono y nombre según categoría)
  output$tarjeta_total_categoria <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    col <- columna_categoria()
    total <- sum(datos[[col]], na.rm = TRUE)
    cat <- categoria_seleccionada()
    
    # Icono específico por categoría
    icono <- switch(cat,
                    "Normal" = icon("check-circle"),
                    "Bajo Peso" = icon("exclamation-triangle"),
                    "Sobrepeso" = icon("exclamation-circle"),
                    "Obesidad" = icon("times-circle")
    )
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-gray-custom",
            div(class = "icon", icono),
            div(class = "inner", 
                h3(formatear_numero(total)), 
                p(cat, style = "font-weight: bold;"))))
  })
  
  # Hombres Categoría - AZUL
  output$tarjeta_hombres_categoria <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    col <- columna_categoria()
    total_h <- sum(datos[[col]][datos$sexo == "Hombres"], na.rm = TRUE)
    cat <- categoria_seleccionada()
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-blue-custom",
            div(class = "icon", icon("mars")),
            div(class = "inner", 
                h3(formatear_numero(total_h)), 
                p(paste("Hombres", cat), style = "font-weight: bold;"))))
  })
  
  # Mujeres Categoría - ROJO
  output$tarjeta_mujeres_categoria <- renderUI({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    col <- columna_categoria()
    total_m <- sum(datos[[col]][datos$sexo == "Mujeres"], na.rm = TRUE)
    cat <- categoria_seleccionada()
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-red-custom",
            div(class = "icon", icon("venus")),
            div(class = "inner", 
                h3(formatear_numero(total_m)), 
                p(paste("Mujeres", cat), style = "font-weight: bold;"))))
  })
  
  # =====================================================
  # GRÁFICO ETARIO
  # =====================================================
  
  output$grafico_etario <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    col <- columna_categoria()
    cat <- categoria_seleccionada()
    
    # Obtener etiquetas de filtros para el tooltip
    mes_label <- if(input$mes_filter == "Todos") "Todos" else input$mes_filter
    sexo_label <- if(input$sexo_filter == "Todos") "Todos" else input$sexo_filter
    provincia_label <- if(input$provincia_filter == "Todas") "Todas" else input$provincia_filter
    comuna_label <- if(input$comuna_filter == "Todas") "Todas" else input$comuna_filter
    
    datos_grafico <- datos %>%
      group_by(grupo_etario, sexo) %>%
      summarise(
        Total_EMP = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        grupo_etario = factor(grupo_etario, 
                              levels = c("15-19", "20-24", "25-29", "30-34", 
                                         "35-39", "40-44", "45-49", "50-54", 
                                         "55-59", "60-64", "65-69", "70-74", 
                                         "75-79", "80+"))
      ) %>%
      arrange(grupo_etario)
    
    max_global <- max(c(datos_grafico$Total_EMP, datos_grafico$Categoria), na.rm = TRUE)
    if(max_global == 0 || !is.finite(max_global)) {
      max_global <- 1
    }
    
    max_y_redondeado <- ceiling(max_global / 500) * 500
    if(max_y_redondeado == 0) max_y_redondeado <- 500
    max_y_con_offset <- max_y_redondeado * 1.1
    
    tick_vals <- seq(0, max_y_redondeado, by = 500)
    tick_text <- formatear_numero(tick_vals)
    
    etiquetas_originales <- levels(datos_grafico$grupo_etario)
    etiquetas_con_espacio <- paste0("   ", etiquetas_originales)
    
    # Tooltip invisible
    datos_barras_invisibles <- datos_grafico %>%
      group_by(grupo_etario) %>%
      summarise(
        Total_Grupo = sum(Total_EMP, na.rm = TRUE),
        Hombres_Total = sum(Total_EMP[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Total = sum(Total_EMP[sexo == "Mujeres"], na.rm = TRUE),
        Hombres_Categoria = sum(Categoria[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Categoria = sum(Categoria[sexo == "Mujeres"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tooltip_Texto = paste0(
          "<b>Grupo Etario: ", grupo_etario, "</b><br>",
          "Total EMP: ", formatear_numero(Total_Grupo), "<br>",
          "Hombres EMP: ", formatear_numero(Hombres_Total), "<br>",
          "Mujeres EMP: ", formatear_numero(Mujeres_Total), "<br>",
          cat, ": ", formatear_numero(Hombres_Categoria + Mujeres_Categoria), "<br>",
          "Hombres ", cat, ": ", formatear_numero(Hombres_Categoria), "<br>",
          "Mujeres ", cat, ": ", formatear_numero(Mujeres_Categoria), "<br>",
          "Mes: ", mes_label, "<br>",
          "Provincia: ", provincia_label, "<br>",
          "Comuna: ", comuna_label, "<br>",
          "Sexo: ", sexo_label
        )
      )
    
    p <- plot_ly() %>%
      add_trace(
        data = datos_barras_invisibles,
        x = ~grupo_etario,
        y = ~max_global,
        name = "Zona Hover",
        type = "bar",
        opacity = 0,
        hoverinfo = "text",
        text = ~Tooltip_Texto,
        showlegend = FALSE,
        width = 0.8
      ) %>%
      add_trace(
        data = datos_grafico %>% filter(sexo == "Hombres"),
        x = ~grupo_etario,
        y = ~Total_EMP,
        name = "Hombres Total EMP",
        type = "bar",
        marker = list(color = "#28a745")
      ) %>%
      add_trace(
        data = datos_grafico %>% filter(sexo == "Mujeres"),
        x = ~grupo_etario,
        y = ~Total_EMP,
        name = "Mujeres Total EMP",
        type = "bar",
        marker = list(color = "#ffc107")
      ) %>%
      add_trace(
        data = datos_grafico %>% filter(sexo == "Hombres"),
        x = ~grupo_etario,
        y = ~Categoria,
        name = paste("Hombres", cat),
        type = "bar",
        marker = list(color = "#2596be")
      ) %>%
      add_trace(
        data = datos_grafico %>% filter(sexo == "Mujeres"),
        x = ~grupo_etario,
        y = ~Categoria,
        name = paste("Mujeres", cat),
        type = "bar",
        marker = list(color = "#ec3d43")
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Grupo Etario (en años)", tickangle = 0,
                     ticktext = etiquetas_con_espacio, tickvals = etiquetas_originales),
        yaxis = list(title = "N° Exámenes",
                     tickvals = tick_vals, ticktext = tick_text, range = c(0, max_y_con_offset)),
        legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
        margin = list(l = 50, r = 100, t = 30, b = 50),
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12))
      )
    
    p
  })
  
  # =====================================================
  # GRÁFICO MENSUAL
  # =====================================================
  
  output$grafico_mensual <- renderPlotly({
    datos <- datos_filtrados()
    req(nrow(datos) > 0)
    
    col <- columna_categoria()
    cat <- categoria_seleccionada()
    
    # Obtener etiquetas de filtros para el tooltip
    sexo_label <- if(input$sexo_filter == "Todos") "Todos" else input$sexo_filter
    provincia_label <- if(input$provincia_filter == "Todas") "Todas" else input$provincia_filter
    comuna_label <- if(input$comuna_filter == "Todas") "Todas" else input$comuna_filter
    grupo_label <- if(input$grupo_etario_filter == "Todos") "Todos" else input$grupo_etario_filter
    
    datos_mensual <- datos %>%
      group_by(nombre_mes, sexo) %>%
      summarise(
        Total_EMP = sum(total_emp, na.rm = TRUE),
        Categoria = sum(.data[[col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(nombre_mes = factor(nombre_mes, levels = meses_orden)) %>%
      arrange(nombre_mes)
    
    max_y <- max(c(datos_mensual$Total_EMP, datos_mensual$Categoria), na.rm = TRUE)
    if(max_y == 0 || !is.finite(max_y)) max_y <- 1
    
    max_y_redondeado <- ceiling(max_y / 500) * 500
    if(max_y_redondeado == 0) max_y_redondeado <- 500
    max_y_con_offset <- max_y_redondeado * 1.1
    
    tick_vals <- seq(0, max_y_redondeado, by = 500)
    tick_text <- formatear_numero(tick_vals)
    
    # Tooltip invisible
    datos_barras <- datos_mensual %>%
      group_by(nombre_mes) %>%
      summarise(
        Total_Mes = sum(Total_EMP, na.rm = TRUE),
        Hombres_Total = sum(Total_EMP[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Total = sum(Total_EMP[sexo == "Mujeres"], na.rm = TRUE),
        Hombres_Categoria = sum(Categoria[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Categoria = sum(Categoria[sexo == "Mujeres"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tooltip_Texto = paste0(
          "<b>", nombre_mes, "</b><br>",
          "Total EMP: ", formatear_numero(Total_Mes), "<br>",
          "Hombres EMP: ", formatear_numero(Hombres_Total), "<br>",
          "Mujeres EMP: ", formatear_numero(Mujeres_Total), "<br>",
          cat, ": ", formatear_numero(Hombres_Categoria + Mujeres_Categoria), "<br>",
          "Hombres ", cat, ": ", formatear_numero(Hombres_Categoria), "<br>",
          "Mujeres ", cat, ": ", formatear_numero(Mujeres_Categoria), "<br>",
          "Provincia: ", provincia_label, "<br>",
          "Comuna: ", comuna_label, "<br>",
          "Sexo: ", sexo_label, "<br>",
          "Grupo Etario: ", grupo_label
        )
      )
    
    p <- plot_ly() %>%
      add_trace(
        data = datos_barras,
        x = ~nombre_mes,
        y = ~max_y,
        name = "Zona Hover",
        type = "bar",
        opacity = 0,
        hoverinfo = "text",
        text = ~Tooltip_Texto,
        showlegend = FALSE
      ) %>%
      add_trace(
        data = datos_mensual %>% filter(sexo == "Hombres"),
        x = ~nombre_mes,
        y = ~Total_EMP,
        name = "Hombres Total EMP",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#28a745"),
        marker = list(color = "#28a745")
      ) %>%
      add_trace(
        data = datos_mensual %>% filter(sexo == "Mujeres"),
        x = ~nombre_mes,
        y = ~Total_EMP,
        name = "Mujeres Total EMP",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ffc107"),
        marker = list(color = "#ffc107")
      ) %>%
      add_trace(
        data = datos_mensual %>% filter(sexo == "Hombres"),
        x = ~nombre_mes,
        y = ~Categoria,
        name = paste("Hombres", cat),
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2596be"),
        marker = list(color = "#2596be")
      ) %>%
      add_trace(
        data = datos_mensual %>% filter(sexo == "Mujeres"),
        x = ~nombre_mes,
        y = ~Categoria,
        name = paste("Mujeres", cat),
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ec3d43"),
        marker = list(color = "#ec3d43")
      ) %>%
      layout(
        xaxis = list(title = "Mes", categoryorder = "array", categoryarray = meses_orden),
        yaxis = list(title = "N° Exámenes",
                     tickvals = tick_vals, ticktext = tick_text, range = c(0, max_y_con_offset)),
        legend = list(orientation = "v", x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle"),
        margin = list(l = 50, r = 100, t = 30, b = 50),
        hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12))
      )
    
    p
  })
  
  # =====================================================
  # MAPA
  # =====================================================
  
  output$titulo_mapa <- renderText({
    paste("Mapa de Prevalencia de", categoria_seleccionada(), "- % del total de EMP")
  })
  
  output$mapa_porcentaje <- renderPlotly({
    df_mapa <- datos_resumen()
    req(df_mapa)
    req(nrow(df_mapa) > 0)
    
    grupo_label <- if(input$grupo_etario_filter == "Todos") "Todos" else input$grupo_etario_filter
    sexo_label <- if(input$sexo_filter == "Todos") "Ambos sexos" else input$sexo_filter
    mes_label <- if(input$mes_filter == "Todos") "Todos" else input$mes_filter
    provincia_label <- if(input$provincia_filter == "Todas") "Todas" else input$provincia_filter
    comuna_label <- if(input$comuna_filter == "Todas") "Todas" else input$comuna_filter
    
    crear_mapa(
      df_mapa = df_mapa,
      codigo_comuna = "codigo_comuna",
      nombre_comuna = "nombre_comuna",
      provincia = "nombre_provincia",
      valor = "Categoria",
      valor_indicador = "% del total de EMP",
      grupo_etario_label = grupo_label,
      sexo_label = sexo_label,
      mes_label = mes_label,
      provincia_label = provincia_label,
      comuna_label = comuna_label,
      titulo_leyenda = "% del total de EMP",
      label_indicador = "% del total de EMP",
      es_porcentaje = TRUE
    )
  })
  
  # =====================================================
  # TABLA POR COMUNA
  # =====================================================
  
  output$titulo_tabla <- renderText({
    paste("Tabla de Prevalencia de", categoria_seleccionada(), "por Comuna - % del total de EMP")
  })
  
  output$tabla_porcentaje <- renderUI({
    df <- datos_resumen()
    req(df)
    req(nrow(df) > 0)
    
    cat <- categoria_seleccionada()
    
    # Crear tabla sin usar :=
    tabla <- df %>%
      select(
        Provincia = nombre_provincia,
        Comuna = nombre_comuna,
        `Total EMP` = Total_EMP,
        Categoria,
        `% del total de EMP` = `% del total de EMP`
      )
    
    # Renombrar columna Categoria
    names(tabla)[names(tabla) == "Categoria"] <- cat
    
    tabla <- tabla %>% arrange(desc(`% del total de EMP`))
    
    # Crear lista de títulos
    titulos <- list(
      Provincia = "Provincia",
      Comuna = "Comuna",
      `Total EMP` = "Total EMP"
    )
    titulos[[cat]] <- cat
    titulos[["% del total de EMP"]] <- "% del total de EMP"
    
    # Crear lista de decimales
    decimales_col <- list(
      `Total EMP` = 0
    )
    decimales_col[[cat]] <- 0
    decimales_col[["% del total de EMP"]] <- 1
    
    rt_tabla(
      tabla,
      titulos = titulos,
      filtrar = FALSE,
      decimales = 0,
      decimales_col = decimales_col
    )
  })
  
  # =====================================================
  # TABLA POR PROVINCIA
  # =====================================================
  
  output$tabla_resumen_provincia <- renderUI({
    df <- datos_resumen_provincia()
    req(df)
    req(nrow(df) > 0)
    
    cat <- categoria_seleccionada()
    
    # Crear tabla
    tabla <- df %>%
      select(
        Provincia = nombre_provincia,
        `Total EMP` = Total_EMP,
        Categoria,
        `% del total de EMP` = `% del total de EMP`
      )
    
    # Renombrar columna Categoria
    names(tabla)[names(tabla) == "Categoria"] <- cat
    
    tabla <- tabla %>% arrange(desc(`Total EMP`))
    
    # Crear lista de títulos
    titulos <- list(
      Provincia = "Provincia",
      `Total EMP` = "Total EMP"
    )
    titulos[[cat]] <- cat
    titulos[["% del total de EMP"]] <- "% del total de EMP"
    
    # Crear lista de decimales
    decimales_col <- list(
      `Total EMP` = 0
    )
    decimales_col[[cat]] <- 0
    decimales_col[["% del total de EMP"]] <- 1
    
    rt_tabla(
      tabla,
      titulos = titulos,
      filtrar = FALSE,
      decimales = 0,
      decimales_col = decimales_col
    )
  })
  
  # =====================================================
  # DESCARGA
  # =====================================================
  
  output$desc_filtros <- renderText({
    filtros <- c()
    filtros <- c(filtros, paste("Categoría Estado Nutricional:", categoria_seleccionada()))
    if(input$provincia_filter != "Todas") filtros <- c(filtros, paste("Provincia:", input$provincia_filter))
    if(input$comuna_filter != "Todas") filtros <- c(filtros, paste("Comuna:", input$comuna_filter))
    if(input$mes_filter != "Todos") filtros <- c(filtros, paste("Mes:", input$mes_filter))
    if(input$sexo_filter != "Todos") filtros <- c(filtros, paste("Sexo:", input$sexo_filter))
    if(input$grupo_etario_filter != "Todos") filtros <- c(filtros, paste("Grupo Etario:", input$grupo_etario_filter))
    
    if(length(filtros) == 0) {
      return("No hay filtros aplicados (todos los datos)")
    } else {
      return(paste(filtros, collapse = " | "))
    }
  })
  
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(), "%y%m%d"), "_datos_estado_nutricional.xlsx")
    },
    content = function(file) {
      datos <- datos_descarga()
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Metadatos")
      writeData(wb, "Metadatos", datos$metadatos)
      
      addWorksheet(wb, "Resumen por Comuna")
      writeData(wb, "Resumen por Comuna", datos$comuna)
      
      addWorksheet(wb, "Resumen por Provincia")
      writeData(wb, "Resumen por Provincia", datos$provincia)
      
      addWorksheet(wb, "Resumen Tarjetas")
      writeData(wb, "Resumen Tarjetas", datos$tarjetas)
      
      addWorksheet(wb, "Detalle por Sexo y Grupo")
      writeData(wb, "Detalle por Sexo y Grupo", datos$detalle)
      
      addWorksheet(wb, "Detalle por Mes y Sexo")
      writeData(wb, "Detalle por Mes y Sexo", datos$detalle_mes)
      
      saveWorkbook(wb, file)
    }
  )
  
  # =====================================================
  # FECHA DE CORTE
  # =====================================================
  
  output$fecha_corte_header <- renderText({
    paste("📅 Fecha de corte:", format(fecha_corte, "%d-%m-%Y"))
  })
  
}

shinyApp(ui = ui, server = server)
