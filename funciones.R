
# Función para Análisis Exploratorio --------------------------------------

exploratorio <- function(df) {
    tipos <- sapply(df, class)
    valores_min <- sapply(df, function(x) ifelse(is.numeric(x), min(x[complete.cases(x)]), NA))
    valores_max <- sapply(df, function(x) ifelse(is.numeric(x), max(x[complete.cases(x)]), NA))
    coeficientes_asimetria <- sapply(df, function(x) ifelse(is.numeric(x), round(e1071::skewness(x[complete.cases(x)]), 2), NA))
    curtosis <- sapply(df, function(x) ifelse(is.numeric(x), round(kurtosis(x[complete.cases(x)]), 2), NA))
    medias <- sapply(df, function(x) ifelse(is.numeric(x), round(mean(x[complete.cases(x)]), 2), NA))
    medianas <- sapply(df, function(x) ifelse(is.numeric(x), round(median(x[complete.cases(x)]), 2), NA))
    modas <- sapply(df, function(x) ifelse(is.numeric(x), {
        tab <- table(x[complete.cases(x)])
        as.numeric(names(tab)[tab == max(tab)])}, NA))
    rangos <- sapply(df, function(x) ifelse(is.numeric(x), round(max(x[complete.cases(x)]) - min(x[complete.cases(x)]), 2), NA))
    varianzas <- sapply(df, function(x) ifelse(is.numeric(x), round(var(x[complete.cases(x)]), 2), NA))
    desviaciones <- sapply(df, function(x) ifelse(is.numeric(x), round(sd(x[complete.cases(x)]), 2), NA))
    coeficientes_variacion <- sapply(df, function(x) ifelse(is.numeric(x), round(sd(x[complete.cases(x)]) / mean(x[complete.cases(x)]), 2), NA))
    # Calcula valores faltantes en valor absoluto
    valores_missing_abs <- round(sapply(df, function(x) sum(is.na(x))),2)
    
    # Calcula valores faltantes en valor relativo
    total_filas <- nrow(df)
    valores_missing_rel <- round(valores_missing_abs / total_filas * 100,2)
    
    resumen <- data.frame(Variable = names(df),
                          Tipo = tipos,
                          Valor_Min = valores_min,
                          Valor_Max = valores_max,
                          Coeficiente_Asimetria = coeficientes_asimetria,
                          Curtosis = curtosis,
                          Media = medias,
                          Mediana = medianas,
                          Moda = modas,
                          Rango = rangos,
                          Varianza = varianzas,
                          Desviacion_Estandar = desviaciones,
                          Coeficiente_Variacion = coeficientes_variacion,
                          stringsAsFactors = FALSE)
    
    resumen$Tipo <- ifelse(resumen$Tipo == "factor", "Categórica", resumen$Tipo)
    resumen$Tipo <- ifelse(resumen$Tipo == "POSIXct", "Fecha", resumen$Tipo)
    resumen$Tipo <- ifelse(resumen$Tipo == "logical", "Booleana", resumen$Tipo)
    resumen$Tipo <- ifelse(resumen$Tipo == "numeric", "Numérica", resumen$Tipo)
    resumen$Valores_Faltantes_Abs <- valores_missing_abs
    resumen$Valores_Faltantes_Rel <- valores_missing_rel
    
    # Elimina los nombres de las etiquetas de las filas
    rownames(resumen) <- NULL
    
    return(resumen)
}

# Función para graficar múltiples BoxPlot ---------------------------------

library(ggplot2)

boxplots <- function(data, title, subtitle, footnote) {
    # Filtrar solo las variables numéricas
    numeric_vars <- sapply(data, is.numeric)
    data_numeric <- data[, numeric_vars, drop = FALSE]
    
    # Convertir el formato de ancho a largo para ggplot
    data_long <- tidyr::gather(data_numeric, key = "variable", value = "value")
    
    # Crear boxplots
    p <- ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
        geom_boxplot() +
        labs(
            title = title,
            subtitle = subtitle,
            caption = footnote
        ) +
        theme_minimal() +
        theme(
            legend.position = "top",
            legend.justification = "right"
        )
    
    print(p)
}

# Función genérica para graficar la distribución y las transformac --------

distribuciones_var <- function(data, variable) {
    p1 <- ggplot(data = data, aes(x = {{ variable }})) +
        geom_density(fill = "steelblue", alpha = 0.8) +
        geom_rug(alpha = 0.1) +
        scale_x_continuous(labels = scales::comma) +
        labs(title = "Distribución original") +
        theme_bw()
    
    p2 <- ggplot(data = data, aes(x = sqrt({{ variable }}))) +
        geom_density(fill = "steelblue", alpha = 0.8) +
        geom_rug(alpha = 0.1) +
        scale_x_continuous(labels = scales::comma) +
        labs(title = "Transformación raíz cuadrada") +
        theme_bw()
    
    p3 <- ggplot(data = data, aes(x = log({{ variable }}))) +
        geom_density(fill = "steelblue", alpha = 0.8) +
        geom_rug(alpha = 0.1) +
        scale_x_continuous(labels = scales::comma) +
        labs(title = "Transformación logarítmica") +
        theme_bw()
    
    ggarrange(p1, p2, p3, ncol = 1, align = "v")
}





