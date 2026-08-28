# app_rem.R - Dashboard EMP 2025 - Tabaquismo
# Richard Quintanilla

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(plotly)
library(lubridate)
library(reactable)
library(htmltools)
library(fst)
library(writexl)
library(janitor)
library(chilemapas)
library(sf)
library(ggplot2)
library(glue)
library(openxlsx)

# =====================================================
# CARGA DE DATOS (Búsqueda automática en ambas rutas)
# =====================================================

# Definir rutas posibles
ruta_datos_1 <- "rem/listados/data/rem_tabaquismo_con_indicadores.fst"
ruta_datos_2 <- "data/rem_tabaquismo_con_indicadores.fst"

# Usar la primera ruta que exista
if (file.exists(ruta_datos_1)) {
  datos_long <- read_fst(ruta_datos_1, as.data.table = FALSE)
} else if (file.exists(ruta_datos_2)) {
  datos_long <- read_fst(ruta_datos_2, as.data.table = FALSE)
} else {
  stop("No se encontró el archivo de datos en ninguna de las rutas configuradas.")
}

# Calcular fecha de corte
mes_maximo <- max(datos_long$mes, na.rm = TRUE)
fecha_corte <- as.Date(paste0(2025, "-", mes_maximo, "-01")) + months(1) - days(1)

# =====================================================
# FUNCIÓN PARA FORMATEAR NÚMEROS
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

# =====================================================
# FUNCIÓN PARA FORMATEAR PORCENTAJES
# =====================================================

formatear_porcentaje <- function(x) {
  if(is.na(x) || is.null(x) || !is.finite(x)) return("0,0%")
  paste0(format(round(x, 1), decimal.mark = ","), "%")
}

# =====================================================
# ORDEN DE MESES (para usar en todo el dashboard)
# =====================================================

meses_orden <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# =====================================================
# FUNCIÓN rt_tabla (COPIADA DE REMASEP)
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
# FUNCIÓN DE MAPA
# =====================================================

crear_mapa <- function(df_mapa, 
                       codigo_comuna, nombre_comuna, provincia, 
                       valor, valor_indicador,
                       grupo_etario_label = "Todos",
                       sexo_label = "Ambos",
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
          "Sin datos para los filtros seleccionados"
        ),
        TRUE ~ paste0(
          "<b>", nombre_comuna, "</b><br>",
          "Provincia: ", ifelse(is.na(provincia), "Sin dato", provincia), "<br>",
          "Total EMP: ", format(round(Total_EMP, 0), big.mark = ".", decimal.mark = ","), "<br>",
          "Tabaquismo: ", format(round(valor, 0), big.mark = ".", decimal.mark = ","), "<br>",
          "% del total de EMP: ", format(round(indicador, 1), big.mark = ".", decimal.mark = ","), "%", "<br>",
          "Grupo Etario: ", grupo_etario_label, "<br>",
          "Sexo: ", sexo_label
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

# =====================================================
# UI
# =====================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "EMP 2025 - Tabaquismo", 
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
      .main-sidebar { position: fixed; top: 50px; bottom: 0; left: 0; z-index: 1020; overflow-y: auto; }
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
      .box.box-primary > .box-header { background-color: #191970 !important; color: white !important; }
      .box.box-info > .box-header { background-color: #2c2c8a !important; color: white !important; }
      .selectize-input, .selectize-dropdown { background-color: #ecf0f1 !important; color: #191970 !important; }
      
      /* CORRECCIÓN 1: Encabezado fijo en reactable */
      .reactable { overflow-y: auto !important; max-height: 600px; }
      .reactable .rt-thead {
        position: sticky !important;
        top: 0 !important;
        z-index: 1000 !important;
        background-color: #191970 !important;
      }
      
      .sidebar-menu {margin-top: 0 !important; padding-top: 10px !important;}
      .main-sidebar, .sidebar {padding-top: 0 !important; margin-top: 0 !important; background-color: #191970 !important;}
      .wrapper {background-color: #191970 !important;}
      
      #clear_filters {background-color: #EEE9E9 !important;color: #191970 !important;}
      #clear_filters:hover {background-color: #d3d3d3 !important; color: #191970 !important;}
      .sidebar-menu > li.active > a {border-left-color: #ff0000 !important;}
      .sidebar-menu > li > a:hover {border-left-color: transparent !important; background-color: #EEE9E9 !important; color: #191970 !important;}
      .skin-blue .main-header .sidebar-toggle:hover {background-color: #EEE9E9 !important;}
    ")),
    
    div(style = "display: flex; justify-content: center; align-items: center; gap: 15px; padding: 0 0 0 0; margin: 0; margin-top: 10px;",
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_seremi.png", 
                 height = "90px", style = "display: block;"),
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_uaid_blanco.png", 
                 height = "100px", style = "display: block;")
    ),
    
    sidebarMenu(
      menuItem("📊 Resumen General", tabName = "resumen"),
      menuItem("🌎 Mapa Estadístico", tabName = "mapas"),
      menuItem("📋 Tabla Resumen", tabName = "tablas"),
      menuItem("📥 Descarga de datos", tabName = "descarga")
    ),
    
    br(),
    hr(),
    h4("Filtros", style = "padding-left: 15px; color: #ecf0f1; font-weight: normal; margin-bottom: 10px;"),
    
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
                uiOutput("tarjeta_total_tabaquismo"),
                uiOutput("tarjeta_hombres_tabaquismo"),
                uiOutput("tarjeta_mujeres_tabaquismo")
              ),
              fluidRow(
                box(title = "N° Exámenes de Medicina Preventiva por Grupo Etario y Sexo (Total vs Positivos)", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_etario", height = "400px"))
              ),
              fluidRow(
                box(title = "N° Exámenes de Medicina Preventiva por Mes y Sexo (Total vs Positivos)", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_mensual", height = "400px"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 2: MAPA ESTADÍSTICO
      # =====================================================
      tabItem(tabName = "mapas",
              fluidRow(
                box(title = "Mapa de Prevalencia de Tabaquismo - % del total de EMP", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("mapa_porcentaje", height = "600px"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 3: TABLA RESUMEN (PROVINCIA + COMUNA)
      # =====================================================
      tabItem(tabName = "tablas",
              fluidRow(
                box(title = "Tabla de Prevalencia de Tabaquismo por Provincia y Regional - % del total de EMP", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("tabla_resumen_provincia"))
              ),
              fluidRow(
                box(title = "Tabla de Prevalencia de Tabaquismo por Comuna - % del total de EMP", 
                    status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("tabla_porcentaje"))
              )
      ),
      
      # =====================================================
      # PESTAÑA 4: DESCARGA DE DATOS
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
                                        " - Datos desagregados por sexo y grupo etario")
                                
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
# SERVER# =====================================================

server <- function(input, output, session) {
  
  ## 1. ACTUALIZAR FILTROS DEPENDIENTES ----
  
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
  
  ## 2. DATOS FILTRADOS ----
  
  datos_filtrados <- reactive({
    df <- datos_long
    
    # Eliminar "Ambos|Total" 
    df <- df %>% filter(sexo != "Ambos", grupo_etario != "Total")
    
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
  
  ## 3. DATOS RESUMEN (POR COMUNA) ----
  
  datos_resumen <- reactive({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      return(data.frame())
    }
    
    df <- df %>%
      group_by(codigo_comuna, nombre_provincia, nombre_comuna) %>%
      summarise(
        Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
        Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Tabaquismo / Total_EMP) * 100, 
                                      0),
        .groups = "drop"
      )
    
    df
  })
  
  ## 4. DATOS RESUMEN POR PROVINCIA Y REGIONAL ----
  
  datos_resumen_provincia <- reactive({
    df <- datos_filtrados()
    
    if(nrow(df) == 0) {
      return(data.frame())
    }
    
    df_provincia <- df %>%
      group_by(nombre_provincia) %>%
      summarise(
        Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
        Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Tabaquismo / Total_EMP) * 100, 
                                      0),
        .groups = "drop"
      )
    
    total_regional <- df %>%
      summarise(
        nombre_provincia = "Región de O'Higgins",
        Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
        Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
        `% del total de EMP` = ifelse(Total_EMP > 0, 
                                      (Tabaquismo / Total_EMP) * 100, 
                                      0)
      )
    
    bind_rows(df_provincia, total_regional)
  })
  
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "provincia_filter", selected = "Todas")
    updateSelectInput(session, "comuna_filter", selected = "Todas")
    updateSelectInput(session, "mes_filter", selected = "Todos")
    updateSelectInput(session, "sexo_filter", selected = "Todos")
    updateSelectInput(session, "grupo_etario_filter", selected = "Todos")
  })
  
  ## 5. TARJETAS ----
  
  output$tarjeta_total_emp <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total <- sum(datos$total_rem_cantidad, na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-purple-custom",
            div(class = "icon", icon("hospital")),
            div(class = "inner", 
                h3(formatear_numero(total)), 
                p("Total EMP", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_hombres_emp <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total_h <- sum(datos$total_rem_cantidad[datos$sexo == "Hombres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-green-custom",
            div(class = "icon", icon("mars")),
            div(class = "inner", 
                h3(formatear_numero(total_h)), 
                p("Hombres EMP", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_mujeres_emp <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total_m <- sum(datos$total_rem_cantidad[datos$sexo == "Mujeres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-yellow-custom",
            div(class = "icon", icon("venus")),
            div(class = "inner", 
                h3(formatear_numero(total_m)), 
                p("Mujeres EMP", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_total_tabaquismo <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total <- sum(datos$tabaquismo_cantidad, na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-gray-custom",
            div(class = "icon", icon("smoking")),
            div(class = "inner", 
                h3(formatear_numero(total)), 
                p("Tabaquismo", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_hombres_tabaquismo <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total_h <- sum(datos$tabaquismo_cantidad[datos$sexo == "Hombres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-blue-custom",
            div(class = "icon", icon("mars")),
            div(class = "inner", 
                h3(formatear_numero(total_h)), 
                p("Hombres Tabaquismo", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_mujeres_tabaquismo <- renderUI({
    datos <- datos_filtrados()
    req(datos)
    
    total_m <- sum(datos$tabaquismo_cantidad[datos$sexo == "Mujeres"], na.rm = TRUE)
    
    div(class = "col-sm-4",
        div(class = "custom-box bg-red-custom",
            div(class = "icon", icon("venus")),
            div(class = "inner", 
                h3(formatear_numero(total_m)), 
                p("Mujeres Tabaquismo", style = "font-weight: bold;"))))
  })
  
  ## 6. GRÁFICO ETARIO (COMPARATIVO) ----
  
  output$grafico_etario <- renderPlotly({
    datos <- datos_filtrados()
    req(datos)
    
    if(nrow(datos) == 0) {
      return(plotly::plot_ly() %>% layout(title = "No hay datos con los filtros seleccionados"))
    }
    
    datos_grafico <- datos %>%
      group_by(grupo_etario, sexo) %>%
      summarise(
        Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
        Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
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
    
    max_global <- max(c(datos_grafico$Total_EMP, datos_grafico$Tabaquismo), na.rm = TRUE)
    if(max_global == 0 || !is.finite(max_global)) {
      max_global <- 1
    }
    
    max_y_redondeado <- ceiling(max_global / 500) * 500
    if(max_y_redondeado == 0) max_y_redondeado <- 500
    max_y_con_offset <- max_y_redondeado * 1.1
    
    tick_vals <- seq(0, max_y_redondeado, by = 500)
    tick_text <- formatear_numero(tick_vals)
    
    etiquetas_originales <- levels(datos_grafico$grupo_etario)
    etiquetas_con_espacio <- paste0("      ", etiquetas_originales)
    
    datos_barras_invisibles <- datos_grafico %>%
      group_by(grupo_etario) %>%
      summarise(
        Total_Grupo = sum(Total_EMP, na.rm = TRUE),
        Hombres_Total = sum(Total_EMP[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Total = sum(Total_EMP[sexo == "Mujeres"], na.rm = TRUE),
        Hombres_Tabaquismo = sum(Tabaquismo[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Tabaquismo = sum(Tabaquismo[sexo == "Mujeres"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tooltip_Texto = paste0(
          "<b>Grupo Etario: ", grupo_etario, "</b><br>",
          "Total EMP: ", formatear_numero(Total_Grupo), "<br>",
          "Hombres EMP: ", formatear_numero(Hombres_Total), "<br>",
          "Mujeres EMP: ", formatear_numero(Mujeres_Total), "<br>",
          "Tabaquismo: ", formatear_numero(Hombres_Tabaquismo + Mujeres_Tabaquismo), "<br>",
          "Hombres Tabaquismo: ", formatear_numero(Hombres_Tabaquismo), "<br>",
          "Mujeres Tabaquismo: ", formatear_numero(Mujeres_Tabaquismo)
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
        y = ~Tabaquismo,
        name = "Hombres Tabaquismo",
        type = "bar",
        marker = list(color = "#2596be")
      ) %>%
      add_trace(
        data = datos_grafico %>% filter(sexo == "Mujeres"),
        x = ~grupo_etario,
        y = ~Tabaquismo,
        name = "Mujeres Tabaquismo",
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
  
  ## 7. GRÁFICO MENSUAL (COMPARATIVO) ----
  
  output$grafico_mensual <- renderPlotly({
    datos <- datos_filtrados()
    req(datos)
    
    if(nrow(datos) == 0) {
      return(plotly::plot_ly() %>% layout(title = "No hay datos con los filtros seleccionados"))
    }
    
    datos_mensual <- datos %>%
      group_by(nombre_mes, sexo) %>%
      summarise(
        Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
        Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(nombre_mes = factor(nombre_mes, levels = meses_orden)) %>%
      arrange(nombre_mes)
    
    max_y <- max(c(datos_mensual$Total_EMP, datos_mensual$Tabaquismo), na.rm = TRUE)
    if(max_y == 0 || !is.finite(max_y)) max_y <- 1
    
    max_y_redondeado <- ceiling(max_y / 500) * 500
    if(max_y_redondeado == 0) max_y_redondeado <- 500
    max_y_con_offset <- max_y_redondeado * 1.1
    
    tick_vals <- seq(0, max_y_redondeado, by = 500)
    tick_text <- formatear_numero(tick_vals)
    
    datos_barras <- datos_mensual %>%
      group_by(nombre_mes) %>%
      summarise(
        Total_Mes = sum(Total_EMP, na.rm = TRUE),
        Hombres_Total = sum(Total_EMP[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Total = sum(Total_EMP[sexo == "Mujeres"], na.rm = TRUE),
        Hombres_Tabaquismo = sum(Tabaquismo[sexo == "Hombres"], na.rm = TRUE),
        Mujeres_Tabaquismo = sum(Tabaquismo[sexo == "Mujeres"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tooltip_Texto = paste0(
          "<b>", nombre_mes, "</b><br>",
          "Total EMP: ", formatear_numero(Total_Mes), "<br>",
          "Hombres EMP: ", formatear_numero(Hombres_Total), "<br>",
          "Mujeres EMP: ", formatear_numero(Mujeres_Total), "<br>",
          "Tabaquismo: ", formatear_numero(Hombres_Tabaquismo + Mujeres_Tabaquismo), "<br>",
          "Hombres Tabaquismo: ", formatear_numero(Hombres_Tabaquismo), "<br>",
          "Mujeres Tabaquismo: ", formatear_numero(Mujeres_Tabaquismo)
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
        y = ~Tabaquismo,
        name = "Hombres Tabaquismo",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#2596be"),
        marker = list(color = "#2596be")
      ) %>%
      add_trace(
        data = datos_mensual %>% filter(sexo == "Mujeres"),
        x = ~nombre_mes,
        y = ~Tabaquismo,
        name = "Mujeres Tabaquismo",
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
  
  ## 8. MAPA DE PORCENTAJE ----
  
  output$mapa_porcentaje <- renderPlotly({
    df_mapa <- datos_resumen()
    req(df_mapa)
    
    if(nrow(df_mapa) == 0) {
      return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar en el mapa"))
    }
    
    grupo_label <- if(input$grupo_etario_filter == "Todos") "Todos" else input$grupo_etario_filter
    sexo_label <- if(input$sexo_filter == "Todos") "Ambos sexos" else input$sexo_filter
    
    crear_mapa(
      df_mapa = df_mapa,
      codigo_comuna = "codigo_comuna",
      nombre_comuna = "nombre_comuna",
      provincia = "nombre_provincia",
      valor = "Tabaquismo",
      valor_indicador = "% del total de EMP",
      grupo_etario_label = grupo_label,
      sexo_label = sexo_label,
      titulo_leyenda = "% del total de EMP",
      label_indicador = "% del total de EMP",
      es_porcentaje = TRUE
    )
  })
  
  ## 9. TABLA POR COMUNA ----
  
  output$tabla_porcentaje <- renderUI({
    df <- datos_resumen()
    req(df)
    
    if(nrow(df) == 0) {
      return(reactable(
        data.frame(Mensaje = "No hay datos con los filtros seleccionados"),
        columns = list(Mensaje = colDef(name = "", align = "center")),
        defaultColDef = colDef(
          headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
        )
      ))
    }
    
    tabla <- df %>%
      select(
        Provincia = nombre_provincia,
        Comuna = nombre_comuna,
        `Total EMP` = Total_EMP,
        `Tabaquismo` = Tabaquismo,
        `% del total de EMP` = `% del total de EMP`
      ) %>%
      arrange(desc(`% del total de EMP`))
    
    rt_tabla(
      tabla,
      titulos = list(
        Provincia = "Provincia",
        Comuna = "Comuna",
        `Total EMP` = "Total EMP",
        `Tabaquismo` = "Tabaquismo",
        `% del total de EMP` = "% del total de EMP"
      ),
      filtrar = TRUE,
      decimales = 0,
      decimales_col = list(
        `Total EMP` = 0,
        `Tabaquismo` = 0,
        `% del total de EMP` = 1
      )
    )
  })
  
  ## 10. TABLA POR PROVINCIA Y REGIONAL ----
  
  output$tabla_resumen_provincia <- renderUI({
    df <- datos_resumen_provincia()
    req(df)
    
    if(nrow(df) == 0) {
      return(reactable(
        data.frame(Mensaje = "No hay datos con los filtros seleccionados"),
        columns = list(Mensaje = colDef(name = "", align = "center")),
        defaultColDef = colDef(
          headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
        )
      ))
    }
    
    tabla <- df %>%
      select(
        Provincia = nombre_provincia,
        `Total EMP` = Total_EMP,
        `Tabaquismo` = Tabaquismo,
        `% del total de EMP` = `% del total de EMP`
      ) %>%
      arrange(desc(`Total EMP`))
    
    rt_tabla(
      tabla,
      titulos = list(
        Provincia = "Provincia",
        `Total EMP` = "Total EMP",
        `Tabaquismo` = "Tabaquismo",
        `% del total de EMP` = "% del total de EMP"
      ),
      filtrar = FALSE,
      decimales = 0,
      decimales_col = list(
        `Total EMP` = 0,
        `Tabaquismo` = 0,
        `% del total de EMP` = 1
      )
    )
  })
  
  ## 11. DESCARGA ----
  
  output$desc_filtros <- renderText({
    filtros <- c()
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
      paste0(format(Sys.Date(), "%y%m%d"), "_datos_tabaquismo", ".xlsx")
    },
    content = function(file) {
      df_resumen <- datos_resumen()
      df_resumen_provincia <- datos_resumen_provincia()
      df_tarjetas <- datos_filtrados()
      
      df_detalle_etario <- datos_filtrados() %>%
        group_by(sexo, grupo_etario) %>%
        summarise(
          Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
          Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
          `% del total de EMP` = ifelse(Total_EMP > 0, 
                                        (Tabaquismo / Total_EMP) * 100, 
                                        0),
          .groups = "drop"
        )
      
      # CORRECCIÓN: Ordenar por mes cronológicamente
      df_detalle_mes <- datos_filtrados() %>%
        group_by(nombre_mes, sexo) %>%
        summarise(
          Total_EMP = sum(total_rem_cantidad, na.rm = TRUE),
          Tabaquismo = sum(tabaquismo_cantidad, na.rm = TRUE),
          `% del total de EMP` = ifelse(Total_EMP > 0, 
                                        (Tabaquismo / Total_EMP) * 100, 
                                        0),
          .groups = "drop"
        ) %>%
        # Convertir a factor con el orden correcto
        mutate(nombre_mes = factor(nombre_mes, levels = meses_orden)) %>%
        arrange(nombre_mes) %>%
        # Convertir de vuelta a character para que se vea bien en Excel
        mutate(nombre_mes = as.character(nombre_mes))
      
      resumen_tarjetas <- data.frame(
        Variable = c("Total EMP", "Hombres EMP", "Mujeres EMP", "Tabaquismo", "Hombres Tabaquismo", "Mujeres Tabaquismo"),
        Valor = c(
          sum(df_tarjetas$total_rem_cantidad, na.rm = TRUE),
          sum(df_tarjetas$total_rem_cantidad[df_tarjetas$sexo == "Hombres"], na.rm = TRUE),
          sum(df_tarjetas$total_rem_cantidad[df_tarjetas$sexo == "Mujeres"], na.rm = TRUE),
          sum(df_tarjetas$tabaquismo_cantidad, na.rm = TRUE),
          sum(df_tarjetas$tabaquismo_cantidad[df_tarjetas$sexo == "Hombres"], na.rm = TRUE),
          sum(df_tarjetas$tabaquismo_cantidad[df_tarjetas$sexo == "Mujeres"], na.rm = TRUE)
        )
      )
      
      metadatos <- data.frame(
        Campo = c(
          "Fecha de corte",
          "Provincia",
          "Comuna",
          "Mes",
          "Sexo",
          "Grupo Etario",
          "Fecha de descarga"
        ),
        Valor = c(
          format(fecha_corte, "%d-%m-%Y"),
          if(input$provincia_filter == "Todas") "Todas" else input$provincia_filter,
          if(input$comuna_filter == "Todas") "Todas" else input$comuna_filter,
          if(input$mes_filter == "Todos") "Todos" else input$mes_filter,
          if(input$sexo_filter == "Todos") "Todos" else input$sexo_filter,
          if(input$grupo_etario_filter == "Todos") "Todos" else input$grupo_etario_filter,
          format(Sys.Date(), "%d-%m-%Y")
        )
      )
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Metadatos")
      writeData(wb, "Metadatos", metadatos)
      
      addWorksheet(wb, "Resumen por Comuna")
      writeData(wb, "Resumen por Comuna", df_resumen)
      
      addWorksheet(wb, "Resumen por Provincia")
      writeData(wb, "Resumen por Provincia", df_resumen_provincia)
      
      addWorksheet(wb, "Resumen Tarjetas")
      writeData(wb, "Resumen Tarjetas", resumen_tarjetas)
      
      addWorksheet(wb, "Detalle por Sexo y Grupo")
      writeData(wb, "Detalle por Sexo y Grupo", df_detalle_etario)
      
      addWorksheet(wb, "Detalle por Mes y Sexo")
      writeData(wb, "Detalle por Mes y Sexo", df_detalle_mes)
      
      saveWorkbook(wb, file)
    }
  )
  
  ## 12. FECHA DE CORTE ----
  
  output$fecha_corte_header <- renderText({
    paste("📅 Fecha de corte:", format(fecha_corte, "%d-%m-%Y"))
  })
  
}

shinyApp(ui = ui, server = server)
