# =============================================================================
# Taller de Tesis I — 2026
# Hugo Granchetti — Grupo 2
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LIBRERÍAS
# -----------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)
library(jsonlite)
library(patchwork)
library(cluster)
library(factoextra)
library(caret)
library(randomForest)
library(pROC)
library(nnet)


# -----------------------------------------------------------------------------
# 2. IMPORTACIÓN Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------

# Carga del JSON
json_path    <- "ctg-studies.json"
studies_list <- fromJSON(json_path, simplifyVector = FALSE)
cat("Total de estudios cargados:", length(studies_list), "\n\n")

# Función de extracción
`%||%` <- function(a, b) if (!is.null(a)) a else b

extraer_estudio <- function(s) {
  proto         <- s$protocolSection
  id_mod        <- proto$identificationModule
  status_mod    <- proto$statusModule
  design_mod    <- proto$designModule
  elig_mod      <- proto$eligibilityModule
  sponsor_mod   <- proto$sponsorCollaboratorsModule
  cond_mod      <- proto$conditionsModule
  interv_mod    <- proto$armsInterventionsModule
  outcome_mod   <- proto$outcomesModule
  oversight_mod <- proto$oversightModule
  
  tibble(
    nct_number           = id_mod$nctId                                  %||% NA_character_,
    start_date           = status_mod$startDateStruct$date               %||% NA_character_,
    completion_date      = status_mod$completionDateStruct$date          %||% NA_character_,
    phases               = paste(unlist(design_mod$phases), collapse = "|"),
    allocation           = design_mod$designInfo$allocation              %||% NA_character_,
    intervention_model   = design_mod$designInfo$interventionModel       %||% NA_character_,
    masking              = design_mod$designInfo$maskingInfo$masking      %||% NA_character_,
    primary_purpose      = design_mod$designInfo$primaryPurpose          %||% NA_character_,
    enrollment           = design_mod$enrollmentInfo$count               %||% NA_integer_,
    is_fda_drug          = oversight_mod$isFdaRegulatedDrug              %||% NA,
    is_fda_device        = oversight_mod$isFdaRegulatedDevice            %||% NA,
    has_dmc              = oversight_mod$oversightHasDmc                 %||% NA,
    sex                  = elig_mod$sex                                  %||% NA_character_,
    std_ages             = paste(unlist(elig_mod$stdAges), collapse = "|"),
    minimum_age_raw      = elig_mod$minimumAge                          %||% NA_character_,
    maximum_age_raw      = elig_mod$maximumAge                          %||% NA_character_,
    eligibility_criteria = elig_mod$eligibilityCriteria                 %||% NA_character_,
    sponsor_name         = sponsor_mod$leadSponsor$name                 %||% NA_character_,
    sponsor_class        = sponsor_mod$leadSponsor$class                %||% NA_character_,
    has_collaborators    = !is.null(sponsor_mod$collaborators) &&
      length(sponsor_mod$collaborators) > 0,
    conditions           = paste(unlist(cond_mod$conditions), collapse = "|"),
    n_conditions         = length(unlist(cond_mod$conditions)),
    n_interventions      = if (!is.null(interv_mod$interventions))
      length(interv_mod$interventions) else 0L,
    intervention_type    = if (!is.null(interv_mod$interventions) &&
                               length(interv_mod$interventions) > 0)
      interv_mod$interventions[[1]]$type %||% NA_character_
    else NA_character_,
    intervention_name    = if (!is.null(interv_mod$interventions) &&
                               length(interv_mod$interventions) > 0)
      interv_mod$interventions[[1]]$name %||% NA_character_
    else NA_character_,
    n_primary_outcomes   = if (!is.null(outcome_mod$primaryOutcomes))
      length(outcome_mod$primaryOutcomes) else 0L,
    locations_raw        = if (!is.null(proto$contactsLocationsModule$locations)) {
      proto$contactsLocationsModule$locations %>%
        map_chr(~ .x$country %||% "") %>%
        paste(collapse = "|")
    } else { NA_character_ }
  )
}

cat("Extrayendo campos...\n")
df_raw <- map_dfr(studies_list, extraer_estudio)
cat("Extracción completa:", nrow(df_raw), "filas\n\n")

# Limpieza, variables derivadas y text mining
df <- df_raw %>%
  mutate(
    # Fechas
    start_date      = parse_date_time(start_date,      orders = c("Ymd","Ym"), quiet = TRUE),
    completion_date = parse_date_time(completion_date, orders = c("Ymd","Ym"), quiet = TRUE),
    start_year      = year(start_date),
    duration_months = as.numeric(difftime(completion_date, start_date,
                                          units = "days")) / 30.44,
    min_age_num     = as.integer(str_extract(minimum_age_raw, "\\d+")),
    max_age_num     = as.integer(str_extract(maximum_age_raw, "\\d+")),
    
    # Variables objetivo
    excluye_mayores = factor(
      if_else(str_detect(std_ages, "OLDER_ADULT"), "No", "Sí"),
      levels = c("No", "Sí")
    ),
    sex_label = factor(case_when(
      sex == "FEMALE" ~ "Solo mujeres",
      sex == "MALE"   ~ "Solo hombres",
      TRUE            ~ "Ambos sexos"
    ), levels = c("Ambos sexos", "Solo mujeres", "Solo hombres")),
    sex_restringido = factor(
      if_else(sex %in% c("FEMALE","MALE"), "Restringido", "No restringido"),
      levels = c("No restringido", "Restringido")
    ),
    
    # Variables de diseño con etiquetas en español
    phases = factor(recode(phases,
                           "PHASE3"        = "Fase 3",
                           "PHASE2|PHASE3" = "Fase 2 / Fase 3"
    )),
    primary_purpose = factor(recode(primary_purpose,
                                    "TREATMENT"                = "Tratamiento",
                                    "PREVENTION"               = "Prevención",
                                    "DIAGNOSTIC"               = "Diagnóstico",
                                    "SUPPORTIVE_CARE"          = "Cuidado de soporte",
                                    "SCREENING"                = "Tamizaje",
                                    "HEALTH_SERVICES_RESEARCH" = "Inv. servicios de salud",
                                    "BASIC_SCIENCE"            = "Ciencia básica",
                                    "ECT"                      = "Educación/Entrenamiento",
                                    "DEVICE_FEASIBILITY"       = "Factibilidad de dispositivo",
                                    "OTHER"                    = "Otro"
    )),
    allocation = factor(recode(allocation,
                               "RANDOMIZED"     = "Aleatorizado",
                               "NON_RANDOMIZED" = "No aleatorizado",
                               "NA"             = "No aplica"
    )),
    masking = factor(recode(masking,
                            "NONE"      = "Abierto",
                            "SINGLE"    = "Simple ciego",
                            "DOUBLE"    = "Doble ciego",
                            "TRIPLE"    = "Triple ciego",
                            "QUADRUPLE" = "Cuádruple ciego"
    )),
    intervention_model = factor(recode(intervention_model,
                                       "SINGLE_GROUP" = "Grupo único",
                                       "PARALLEL"     = "Paralelo",
                                       "CROSSOVER"    = "Cruzado",
                                       "FACTORIAL"    = "Factorial",
                                       "SEQUENTIAL"   = "Secuencial"
    )),
    intervention_type = factor(recode(intervention_type,
                                      "DRUG"                = "Fármaco",
                                      "BIOLOGICAL"          = "Biológico",
                                      "DEVICE"              = "Dispositivo",
                                      "PROCEDURE"           = "Procedimiento",
                                      "BEHAVIORAL"          = "Conductual",
                                      "RADIATION"           = "Radiación",
                                      "DIETARY_SUPPLEMENT"  = "Suplemento dietario",
                                      "DIAGNOSTIC_TEST"     = "Prueba diagnóstica",
                                      "GENETIC"             = "Genético",
                                      "COMBINATION_PRODUCT" = "Producto combinado",
                                      "OTHER"               = "Otro"
    )),
    sponsor_class = factor(recode(sponsor_class,
                                  "INDUSTRY"  = "Industria",
                                  "NIH"       = "NIH",
                                  "FED"       = "Agencia federal EE.UU.",
                                  "OTHER_GOV" = "Otro gobierno",
                                  "NETWORK"   = "Red de investigación",
                                  "INDIV"     = "Individual",
                                  "OTHER"     = "Otro",
                                  "UNKNOWN"   = "Desconocido"
    )),
    sponsor_class_deriv = factor(case_when(
      str_detect(toupper(sponsor_name), "NIH|NATIONAL INSTITUTES") ~ "NIH",
      str_detect(toupper(sponsor_name),
                 "PFIZER|ROCHE|NOVARTIS|ASTRAZENECA|MERCK|SANOFI|GLAXO|LILLY|BAYER|BOEHRINGER|TAKEDA|AMGEN|NOVO NORDISK|BRISTOL|ABBOTT|JOHNSON") ~ "Industria",
      str_detect(toupper(sponsor_name),
                 "UNIVERSITY|UNIVERSIDAD|UNIVERSIT|HOSPITAL|CLINIC|MEDICAL CENTER|INSTITUTE|CANCER CENTER") ~ "Académico",
      str_detect(toupper(sponsor_name),
                 "DEPARTMENT|MINISTRY|FEDERAL|NATIONAL|AGENCY|GOVERNMENT|GOV") ~ "Gobierno",
      TRUE ~ "Otro"
    )),
    has_collaborators = factor(if_else(has_collaborators, "Sí", "No")),
    has_us_location   = factor(if_else(
      str_detect(locations_raw, "United States") & !is.na(locations_raw), "Sí", "No"
    )),
    is_fda_drug   = factor(case_when(
      is_fda_drug == TRUE  ~ "Sí", is_fda_drug == FALSE ~ "No", TRUE ~ NA_character_)),
    is_fda_device = factor(case_when(
      is_fda_device == TRUE ~ "Sí", is_fda_device == FALSE ~ "No", TRUE ~ NA_character_)),
    has_dmc = factor(case_when(
      has_dmc == TRUE ~ "Sí", has_dmc == FALSE ~ "No", TRUE ~ NA_character_)),
    
    # Área terapéutica
    area_terapeutica = factor(case_when(
      str_detect(tolower(conditions),
                 "cancer|carcinoma|tumor|lymphoma|leukemia|melanoma|sarcoma|myeloma|oncol|neoplasm|glioma|blastoma") ~ "Oncología",
      str_detect(tolower(conditions),
                 "heart|cardiac|coronary|atrial|myocardial|cardiovascular|hypertension|stroke|artery|arterial|angina|infarct") ~ "Cardiología",
      str_detect(tolower(conditions),
                 "diabetes|insulin|glycemi|obesity|overweight|metabolic|cholesterol|lipid|dyslipid") ~ "Metabólica/Endocrina",
      str_detect(tolower(conditions),
                 "depression|depressive|schizophrenia|bipolar|anxiety|psychosis|psychiatric|mental|alzheimer|dementia|parkinson|neurolog|epilepsy|migraine|multiple sclerosis|adhd|attention deficit|fibromyalgia|traumatic brain|amyotrophic") ~ "Neurología/Psiquiatría",
      str_detect(tolower(conditions),
                 "hiv|hepatitis|influenza|covid|infection|infectious|malaria|tuberculosis|pneumonia|sepsis|virus|bacterial|vaccine|immuniz|herpes|zoster") ~ "Infecciosa/Vacunas",
      str_detect(tolower(conditions),
                 "asthma|copd|pulmonary|respiratory|lung|bronch|fibrosis|airway|rhinitis") ~ "Respiratoria",
      str_detect(tolower(conditions),
                 "arthritis|osteo|lupus|crohn|colitis|inflammatory|autoimmune|psoriasis|dermatitis|rheumat|immune|ankylosing|spondylitis") ~ "Inmunología/Reumatología",
      str_detect(tolower(conditions),
                 "kidney|renal|hepatic|liver|cirrhosis|transplant|dialysis|urolog|bladder|prostate|erectile|venous thromboembolism|thrombosis") ~ "Uronefrología/Hepática",
      str_detect(tolower(conditions),
                 "pain|analges|postoperative|anesthes|surgical|perioperative|wound") ~ "Dolor/Anestesia",
      str_detect(tolower(conditions),
                 "fertility|infertility|pregnancy|obstetric|gynecolog|menopause|endometriosis|contraception") ~ "Ginecología/Obstetricia",
      str_detect(tolower(conditions),
                 "macular|glaucoma|retino|cataract|dry eye|conjunctiv|ophthalm|ocular|vision|cornea") ~ "Oftalmología",
      str_detect(tolower(conditions),
                 "anemia|hemophilia|sickle cell|thrombocytopenia|leukopenia|hematol|blood disorder|coagulation") ~ "Hematología",
      str_detect(tolower(conditions),
                 "acne|actinic|keratosis|dermatol|skin|eczema|urticaria|alopecia|rosacea|vitiligo|allerg") ~ "Dermatología/Alergia",
      TRUE ~ "Otra/No especificada"
    )),
    
    # Text mining: BMI
    texto = tolower(eligibility_criteria %||% ""),
    
    bmi_valor_raw = str_extract(texto,
                                "(?:bmi|body mass index)[^\\d]{0,20}(\\d{2,3}(?:\\.\\d)?)") %>%
      str_extract("\\d{2,3}(?:\\.\\d)?") %>% as.numeric(),
    
    bmi_operador = str_extract(texto,
                               "(?:bmi|body mass index)[^\\d]{0,20}(?:of |> |>= |≥ |greater than |more than |exceeding |< |<= |≤ |less than )?\\d{2,3}") %>%
      str_extract(">|>=|≥|greater than|more than|exceeding|<|<=|≤|less than") %>%
      str_trim(),
    
    bmi_rango = str_extract_all(texto,
                                "(?:bmi|body mass index)[^\\d]{0,40}\\d{2,3}[^\\d]{0,10}\\d{2,3}") %>%
      map_lgl(~ length(.x) > 0),
    
    bmi_rango_lower = str_extract(texto,
                                  "(?:bmi|body mass index)[^\\d]{0,20}(\\d{2,3})") %>%
      str_extract("\\d{2,3}") %>% as.numeric(),
    
    bmi_rango_upper = str_extract_all(texto,
                                      "(?:bmi|body mass index)[^\\d]{0,40}(\\d{2,3})") %>%
      map_dbl(~ ifelse(length(.x) >= 2, as.numeric(.x[2]), NA_real_)),
    
    excluye_obesidad = case_when(
      !is.na(bmi_valor_raw) &
        bmi_operador %in% c(">",">=","≥","greater than","more than","exceeding") &
        bmi_valor_raw >= 30           ~ TRUE,
      bmi_rango & !is.na(bmi_rango_upper) & bmi_rango_upper <= 35 ~ TRUE,
      TRUE                            ~ FALSE
    ),
    excluye_bajo_peso = case_when(
      !is.na(bmi_valor_raw) &
        bmi_operador %in% c("<","<=","≤","less than") &
        bmi_valor_raw <= 18.5         ~ TRUE,
      bmi_rango & !is.na(bmi_rango_lower) & bmi_rango_lower >= 18 ~ TRUE,
      TRUE                            ~ FALSE
    ),
    excluye_bmi_ambos = excluye_obesidad & excluye_bajo_peso,
    menciona_bmi      = str_detect(texto, "bmi|body mass index|obesity|obese|overweight"),
    
    bmi_restriccion = factor(case_when(
      excluye_bmi_ambos ~ "Excluye obesidad y bajo peso",
      excluye_obesidad  ~ "Excluye obesidad (IMC alto)",
      excluye_bajo_peso ~ "Excluye bajo peso (IMC bajo)",
      menciona_bmi      ~ "Menciona IMC (no clasificado)",
      TRUE              ~ "No restringe por IMC"
    )),
    
    # Text mining: etnia
    incluye_solo_hispanic = str_detect(texto,
                                       "(?:hispanic|latino|latina|latinx).{0,50}(?:only|must be|restricted to|exclusively|eligible)") |
      str_detect(texto,
                 "(?:only|must be|restricted to|exclusively).{0,50}(?:hispanic|latino|latina|latinx)"),
    excluye_hispanic = str_detect(texto,
                                  "(?:exclud|not eligible|ineligible|excluded).{0,50}(?:hispanic|latino|latina|latinx)") |
      str_detect(texto,
                 "(?:hispanic|latino|latina|latinx).{0,50}(?:exclud|not eligible|ineligible|excluded)"),
    
    incluye_solo_black = str_detect(texto,
                                    "(?:african american|black patients|black subjects|black volunteers).{0,50}(?:only|must be|restricted to|exclusively)") |
      str_detect(texto,
                 "(?:only|must be|restricted to|exclusively).{0,50}(?:african american|black patients|black subjects)"),
    excluye_black = str_detect(texto,
                               "(?:exclud|not eligible|ineligible|excluded).{0,50}(?:african american|black patients|black subjects)") |
      str_detect(texto,
                 "(?:african american|black patients|black subjects).{0,50}(?:exclud|not eligible|ineligible|excluded)"),
    
    incluye_solo_asian = str_detect(texto,
                                    "(?:\\basian\\b).{0,50}(?:only|must be|restricted to|exclusively)") |
      str_detect(texto,
                 "(?:only|must be|restricted to|exclusively).{0,50}(?:\\basian\\b)"),
    excluye_asian = str_detect(texto,
                               "(?:exclud|not eligible|ineligible|excluded).{0,50}(?:\\basian\\b)") |
      str_detect(texto,
                 "(?:\\basian\\b).{0,50}(?:exclud|not eligible|ineligible|excluded)"),
    
    incluye_solo_white = str_detect(texto,
                                    "(?:caucasian|white patients|white subjects|white volunteers).{0,50}(?:only|must be|restricted to|exclusively)") |
      str_detect(texto,
                 "(?:only|must be|restricted to|exclusively).{0,50}(?:caucasian|white patients|white subjects)"),
    excluye_white = str_detect(texto,
                               "(?:exclud|not eligible|ineligible|excluded).{0,50}(?:caucasian|white patients|white subjects)") |
      str_detect(texto,
                 "(?:caucasian|white patients|white subjects).{0,50}(?:exclud|not eligible|ineligible|excluded)"),
    
    incluye_solo_indigenous = str_detect(texto,
                                         "(?:indigenous|aboriginal|native american|first nations|american indian).{0,50}(?:only|must be|restricted to|exclusively)") |
      str_detect(texto,
                 "(?:only|must be|restricted to|exclusively).{0,50}(?:indigenous|aboriginal|native american|first nations|american indian)"),
    excluye_indigenous = str_detect(texto,
                                    "(?:exclud|not eligible|ineligible|excluded).{0,50}(?:indigenous|aboriginal|native american|first nations|american indian)") |
      str_detect(texto,
                 "(?:indigenous|aboriginal|native american|first nations|american indian).{0,50}(?:exclud|not eligible|ineligible|excluded)"),
    
    restriccion_etnica_any = incluye_solo_hispanic | excluye_hispanic |
      incluye_solo_black    | excluye_black    |
      incluye_solo_asian    | excluye_asian    |
      incluye_solo_white    | excluye_white    |
      incluye_solo_indigenous | excluye_indigenous
  )

cat("Dataset completo:", nrow(df), "filas\n\n")

# Dataset de modelado
# Excluye Ginecología/Obstetricia y Oncología (se conservan en df para clustering)
# ─────────────────────────────────────────────────────────────────────────────
AREAS_EXCLUIR <- c("Ginecología/Obstetricia", "Oncología")

df_model <- df %>%
  filter(!area_terapeutica %in% AREAS_EXCLUIR) %>%
  filter(
    !is.na(primary_purpose), !is.na(allocation), !is.na(masking),
    !is.na(intervention_model), !is.na(intervention_type),
    !is.na(start_year), !is.na(is_fda_drug), !is.na(has_dmc)
  ) %>%
  mutate(
    across(c(primary_purpose, intervention_type, sponsor_class_deriv,
             allocation, masking, intervention_model, area_terapeutica),
           ~ fct_lump_min(.x, min = 50, other_level = "Otro")),
    duration_months = if_else(is.na(duration_months) | duration_months <= 0,
                              median(duration_months, na.rm = TRUE), duration_months),
    enrollment  = if_else(is.na(enrollment) | enrollment <= 0,
                          as.integer(median(enrollment, na.rm = TRUE)), enrollment),
    min_age_num = if_else(is.na(min_age_num),
                          as.integer(median(min_age_num, na.rm = TRUE)), min_age_num),
    across(c(duration_months, enrollment, n_conditions,
             n_interventions, n_primary_outcomes, min_age_num),
           ~ pmin(.x, quantile(.x, 0.99)))
  ) %>%
  { df_tmp <- .
  single_level <- df_tmp %>%
    select(where(is.factor)) %>%
    summarise(across(everything(), ~ nlevels(.x) < 2)) %>%
    pivot_longer(everything()) %>% filter(value) %>% pull(name)
  if (length(single_level) > 0) df_tmp <- df_tmp %>% select(-all_of(single_level))
  df_tmp
  }

cat("Dataset de modelado (sin Ginecología/Obstetricia ni Oncología):",
    nrow(df_model), "filas\n\n")


# -----------------------------------------------------------------------------
# 3. ESTADÍSTICA DESCRIPTIVA (EDA)
# -----------------------------------------------------------------------------

cat("=== VARIABLES OBJETIVO ===\n")
df %>% count(excluye_mayores) %>% mutate(pct = percent(n/sum(n), 0.1)) %>% print()
df %>% count(sex_label)       %>% mutate(pct = percent(n/sum(n), 0.1)) %>% print()
df %>% count(bmi_restriccion) %>% mutate(pct = percent(n/sum(n), 0.1)) %>% print()
cat("\n")

cat("=== VARIABLES DE DISEÑO ===\n")
for (v in c("phases","primary_purpose","allocation","masking",
            "intervention_model","intervention_type","sponsor_class_deriv")) {
  cat("\n--", v, "--\n")
  df %>% count(.data[[v]], sort = TRUE) %>%
    mutate(pct = percent(n/sum(n), 0.1)) %>% print()
}

cat("\n=== CRUCE: excluye_mayores × sex_label ===\n")
df %>%
  count(excluye_mayores, sex_label) %>%
  group_by(excluye_mayores) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>%
  ungroup() %>% print()

cat("\n=== CRUCE: excluye_mayores × bmi_restriccion ===\n")
df %>%
  filter(bmi_restriccion != "No restringe por IMC") %>%
  count(bmi_restriccion, excluye_mayores) %>%
  group_by(bmi_restriccion) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>%
  ungroup() %>% print()

cat("\n=== RESUMEN POR ÁREA TERAPÉUTICA ===\n")
df %>%
  group_by(area_terapeutica) %>%
  summarise(
    n               = n(),
    pct_excl_mayor  = percent(mean(excluye_mayores == "Sí"), 0.1),
    pct_sex_restric = percent(mean(sex_restringido == "Restringido"), 0.1),
    pct_bmi         = percent(mean(bmi_restriccion != "No restringe por IMC"), 0.1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_excl_mayor)) %>% print()

# Gráficos EDA

p_excl_mayor <- df %>%
  count(excluye_mayores) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = 2, y = n, fill = excluye_mayores)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") + xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(percent(pct, 0.1), "\n(n=", n, ")")),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4, color = "white") +
  scale_fill_manual(values = c("No" = "#2C7BB6", "Sí" = "#D7191C"), name = NULL) +
  labs(title = "Exclusión de adultos mayores",
       subtitle = paste0("N = ", format(nrow(df), big.mark = ","))) +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C", hjust = 0.5),
        plot.subtitle = element_text(color = "grey50", hjust = 0.5),
        legend.position = "bottom")

p_sex_label <- df %>%
  count(sex_label, sort = TRUE) %>%
  mutate(pct = n/sum(n), sex_label = fct_reorder(sex_label, n)) %>%
  ggplot(aes(x = n, y = sex_label, fill = sex_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", percent(pct, 0.1), ")")),
            hjust = -0.08, size = 3.5, color = "grey30") +
  scale_fill_manual(values = c("Ambos sexos"  = "#2C7BB6",
                               "Solo mujeres" = "#D7191C",
                               "Solo hombres" = "#1A9641")) +
  scale_x_continuous(labels = label_number(), expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Criterio de sexo", x = "N° de ensayos", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())

p_temporal <- df %>%
  filter(!is.na(start_year), start_year >= 1990) %>%
  count(start_year) %>%
  ggplot(aes(x = start_year, y = n)) +
  geom_area(fill = "#2C7BB6", alpha = 0.15) +
  geom_line(color = "#2C7BB6", linewidth = 1.8) +
  geom_point(color = "#2C7BB6", size = 2) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(labels = label_number()) +
  coord_cartesian(xlim = c(1990, NA)) +
  labs(title = "Ensayos iniciados por año", x = "Año de inicio", y = "N° de ensayos") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.minor = element_blank())

p_area <- df %>%
  count(area_terapeutica, sort = TRUE) %>%
  mutate(pct = n/sum(n), area_terapeutica = fct_reorder(area_terapeutica, n)) %>%
  ggplot(aes(x = n, y = area_terapeutica)) +
  geom_col(fill = "#2C7BB6", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(n, " (", percent(pct, 0.1), ")")),
            hjust = -0.08, size = 3, color = "grey30") +
  scale_x_continuous(labels = label_number(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Distribución por área terapéutica", x = "N° de ensayos", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())

print(p_excl_mayor)
print(p_sex_label)
print(p_temporal)
print(p_area)

# Exclusión de adultos mayores por área
p_excl_area <- df %>%
  group_by(area_terapeutica) %>%
  summarise(pct = mean(excluye_mayores == "Sí"), n = n(), .groups = "drop") %>%
  mutate(
    area_terapeutica = fct_reorder(area_terapeutica, pct),
    justif = if_else(area_terapeutica %in% AREAS_EXCLUIR,
                     "Justificación clínica obvia", "Sin justificación obvia")
  ) %>%
  ggplot(aes(x = pct, y = area_terapeutica, fill = justif)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = percent(pct, 0.1)), hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = c("Justificación clínica obvia" = "#AAAAAA",
                               "Sin justificación obvia"     = "#D7191C"), name = NULL) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Exclusión de adultos mayores por área terapéutica",
       subtitle = "Gris: áreas excluidas del modelado por restricción clínicamente justificada",
       x = "% que excluye adultos mayores", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())
print(p_excl_area)

# Restricción por sexo por área
p_sexo_area <- df %>%
  filter(!area_terapeutica %in% AREAS_EXCLUIR) %>%
  mutate(sex_cat = case_when(
    sex == "FEMALE" ~ "Solo mujeres", sex == "MALE" ~ "Solo hombres", TRUE ~ "Ambos sexos")) %>%
  filter(sex_cat != "Ambos sexos") %>%
  count(area_terapeutica, sex_cat) %>%
  group_by(area_terapeutica) %>% mutate(pct = n/sum(n)) %>% ungroup() %>%
  mutate(area_terapeutica = fct_reorder(area_terapeutica, pct, .fun = sum)) %>%
  ggplot(aes(x = pct, y = area_terapeutica, fill = sex_cat)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("Solo mujeres" = "#D7191C", "Solo hombres" = "#2C7BB6"),
                    name = NULL) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Restricción por sexo — áreas sin justificación clínica obvia",
       subtitle = "Excluye Ginecología/Obstetricia y Oncología",
       x = "% de ensayos con restricción por sexo", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())
print(p_sexo_area)

# Coexclusión edad por sexo
p_coexcl_sexo <- df %>%
  filter(!area_terapeutica %in% AREAS_EXCLUIR) %>%
  mutate(sex_flag = if_else(sex %in% c("FEMALE","MALE"), "Sí", "No")) %>%
  count(excluye_mayores, sex_flag) %>%
  group_by(sex_flag) %>% mutate(pct = n/sum(n)) %>% ungroup() %>%
  ggplot(aes(x = sex_flag, y = pct, fill = excluye_mayores)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = percent(pct, 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("No" = "#2C7BB6", "Sí" = "#D7191C"),
                    name = "Excluye adultos mayores") +
  scale_y_continuous(labels = label_percent()) +
  labs(title    = "Coexclusión: adultos mayores × restricción por sexo",
       subtitle = "Excluye Ginecología/Obstetricia y Oncología",
       x = "Restricción por sexo", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        legend.position = "bottom",
        panel.grid.major.x = element_blank())
print(p_coexcl_sexo)

# Evolución temporal de exclusión de adultos mayores por área terapéutica
AREAS_TEMPORAL <- c("Dermatología/Alergia", "Infecciosa/Vacunas",
                    "Neurología/Psiquiatría", "Hematología", "Dolor/Anestesia")

p_temporal_area <- df %>%
  filter(
    area_terapeutica %in% AREAS_TEMPORAL,
    !is.na(start_year),
    start_year >= 2000,
    start_year <= 2023
  ) %>%
  group_by(area_terapeutica, start_year) %>%
  summarise(
    pct_excluye = mean(excluye_mayores == "Sí"),
    n           = n(),
    .groups     = "drop"
  ) %>%
  filter(n >= 5) %>%
  ggplot(aes(x = start_year, y = pct_excluye,
             color = area_terapeutica, group = area_terapeutica)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(size = n), alpha = 0.6) +
  # geom_smooth(se = FALSE, method = "loess", linewidth = 0.6,
  #             linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(2000, 2023, 3)) +
  scale_color_brewer(palette = "Set1", name = NULL) +
  scale_size_continuous(name = "N ensayos", range = c(1, 4)) +
  labs(
    title    = "Evolución temporal de la exclusión de adultos mayores",
    # subtitle = "Áreas seleccionadas — 2000 a 2023 | línea discontinua = tendencia suavizada (loess) | mín. 5 ensayos/año",
    x        = "Año de inicio",
    y        = "% que excluye adultos mayores"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1A3A5C"),
    plot.subtitle = element_text(color = "grey50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 3)))

print(p_temporal_area)


# -----------------------------------------------------------------------------
# 4. CLUSTERING JERÁRQUICO
# -----------------------------------------------------------------------------

vars_cluster <- c(
  "phases","primary_purpose","allocation","masking","intervention_model",
  "intervention_type","sponsor_class_deriv","area_terapeutica","sex_restringido",
  "n_conditions","n_interventions","n_primary_outcomes","has_collaborators",
  "start_year","duration_months","has_us_location","enrollment","is_fda_drug",
  "is_fda_device","has_dmc","min_age_num"
)

df_cluster <- df %>%
  select(nct_number, all_of(vars_cluster), excluye_mayores, sex_restringido, sex) %>%
  filter(!is.na(primary_purpose), !is.na(allocation), !is.na(masking),
         !is.na(intervention_model), !is.na(intervention_type), !is.na(start_year)) %>%
  mutate(
    duration_months = if_else(is.na(duration_months)|duration_months<=0,
                              median(duration_months, na.rm=TRUE), duration_months),
    enrollment  = if_else(is.na(enrollment)|enrollment<=0,
                          as.integer(median(enrollment, na.rm=TRUE)), enrollment),
    min_age_num = if_else(is.na(min_age_num),
                          as.integer(median(min_age_num, na.rm=TRUE)), min_age_num),
    across(c(duration_months,enrollment,n_conditions,n_interventions,
             n_primary_outcomes,min_age_num), ~ pmin(.x, quantile(.x, 0.99)))
  )

set.seed(42)
N_MUESTRA  <- 5000
df_sample  <- df_cluster %>%
  group_by(excluye_mayores) %>%
  slice_sample(prop = N_MUESTRA / nrow(df_cluster)) %>%
  ungroup()

cat("Muestra para clustering:", nrow(df_sample), "filas\n")
cat("Calculando distancia de Gower...\n")
gower_dist <- daisy(df_sample %>% select(all_of(vars_cluster)), metric = "gower")
cat("Listo.\n\n")

# Número óptimo de clusters (según silhouette)
sil_scores <- map_dbl(2:8, function(k) {
  hc  <- hclust(gower_dist, method = "ward.D2")
  cls <- cutree(hc, k = k)
  mean(silhouette(cls, gower_dist)[, "sil_width"])
})
sil_df   <- tibble(k = 2:8, silhouette = sil_scores)
K_OPTIMO <- sil_df$k[which.max(sil_df$silhouette)]
# K_OPTIMO <- 3

hc_final  <- hclust(gower_dist, method = "ward.D2")
df_sample <- df_sample %>% mutate(cluster = factor(cutree(hc_final, k = K_OPTIMO)))

p_sil <- sil_df %>%
  ggplot(aes(x = k, y = silhouette)) +
  geom_line(color = "#2C7BB6", linewidth = 1.5) +
  geom_point(color = "#2C7BB6", size = 3) +
  geom_point(data = filter(sil_df, silhouette == max(silhouette)),
             color = "#D7191C", size = 5, shape = 18) +
  scale_x_continuous(breaks = 2:8) +
  labs(title    = "Selección del número óptimo de clusters",
       subtitle = "Coeficiente de silhouette promedio — Ward D2 + Gower",
       x = "k", y = "Silhouette promedio") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"))
print(p_sil)

p_dendro <- fviz_dend(hc_final, k = K_OPTIMO, show_labels = FALSE, rect = TRUE,
                      k_colors = c("#2C7BB6","#D7191C","#1A9641","#FDAE61","#ABD9E9","#984EA3")[1:K_OPTIMO],
                      main = paste0("Dendrograma — Ward D2 + Gower (k=", K_OPTIMO, ")"),
                      sub  = paste0("n=", nrow(df_sample)), ggtheme = theme_minimal())
print(p_dendro)

labels_cluster <- df_sample %>%
  count(cluster) %>%
  mutate(label = paste0("Cluster ", cluster, "\n(n=", n, ")")) %>%
  select(cluster, label) %>% deframe()

p_excl_cluster <- df_sample %>%
  group_by(cluster) %>%
  summarise(pct = mean(excluye_mayores == "Sí"), n = n(), .groups = "drop") %>%
  mutate(label_x = paste0("Cluster ", cluster, "\n(n=", n, ")")) %>%
  ggplot(aes(x = label_x, y = pct, fill = cluster)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = percent(pct, 0.1)), vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(title = "% excluye adultos mayores por cluster", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.x = element_blank())

p_sexo_cluster <- df_sample %>%
  mutate(sex_cat = case_when(
    sex == "FEMALE" ~ "Solo mujeres", sex == "MALE" ~ "Solo hombres", TRUE ~ "Ambos sexos")) %>%
  count(cluster, sex_cat) %>%
  group_by(cluster) %>% mutate(pct = n/sum(n)) %>% ungroup() %>%
  ggplot(aes(x = factor(cluster, labels = labels_cluster), y = pct, fill = sex_cat)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = if_else(pct > 0.02, percent(pct, 0.1), "")),
            position = position_stack(vjust = 0.5),
            size = 3, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Ambos sexos"  = "#2C7BB6",
                               "Solo mujeres" = "#D7191C",
                               "Solo hombres" = "#1A9641"), name = NULL) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Criterio de sexo por cluster", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        legend.position = "bottom", panel.grid.major.x = element_blank())

print(p_excl_cluster / p_sexo_cluster +
        plot_annotation(title = "Patrones de exclusión por cluster",
                        theme = theme(plot.title = element_text(face = "bold", size = 13, color = "#1A3A5C"))))

p_area_cluster <- df_sample %>%
  count(cluster, area_terapeutica) %>%
  group_by(cluster) %>% mutate(pct = n/sum(n)) %>% ungroup() %>%
  mutate(cluster_label = factor(cluster, labels = labels_cluster)) %>%
  ggplot(aes(x = cluster_label, y = pct, fill = area_terapeutica)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c(
    "Oncología"                = "#4DAF4A",
    "Cardiología"              = "#66C2A5",
    "Metabólica/Endocrina"     = "#FFB347",
    "Neurología/Psiquiatría"   = "#B3B3B3",
    "Infecciosa/Vacunas"       = "#FF7F00",
    "Respiratoria"             = "#A6CEE3",
    "Inmunología/Reumatología" = "#78C679",
    "Uronefrología/Hepática"   = "#6A3D9A",
    "Dolor/Anestesia"          = "#8DA0CB",
    "Ginecología/Obstetricia"  = "#F46D43",
    "Oftalmología"             = "#984EA3",
    "Hematología"              = "#377EB8",
    "Dermatología/Alergia"     = "#FFFFB3",
    "Otra/No especificada"     = "#FCCDE5"
  ), name = "Área terapéutica") +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Distribución de área terapéutica por cluster", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        legend.position = "bottom", legend.text = element_text(size = 8),
        panel.grid.major.x = element_blank()) +
  guides(fill = guide_legend(nrow = 3))
print(p_area_cluster)

# Heatmap de perfil modal por cluster
vars_heatmap <- c("primary_purpose","allocation","masking",
                  "intervention_model","intervention_type",
                  "sponsor_class_deriv","has_us_location","has_dmc")

# Traducción de nombres de variables para el heatmap
VARS_HEATMAP_ES <- c(
  "primary_purpose"   = "Propósito primario",
  "allocation"        = "Aleatorización",
  "masking"           = "Enmascaramiento",
  "intervention_model"= "Modelo de intervención",
  "intervention_type" = "Tipo de intervención",
  "sponsor_class_deriv"= "Tipo de sponsor",
  "has_us_location"   = "Sede en EE.UU.",
  "has_dmc"           = "Tiene DSMB"
)

heatmap_data <- vars_heatmap %>%
  map_dfr(function(v) {
    df_sample %>%
      count(cluster, valor = as.character(.data[[v]])) %>%
      mutate(pct = n / sum(n), .by = cluster) %>%
      filter(pct == max(pct), .by = cluster) %>%
      slice_head(n = 1, by = cluster) %>%
      mutate(variable = VARS_HEATMAP_ES[v])
  }) %>%
  dplyr::select(cluster, variable, valor_modal = valor, pct_modal = pct)

p_heatmap <- heatmap_data %>%
  mutate(
    cluster  = paste0("Cluster ", cluster),
    variable = fct_rev(factor(variable, levels = rev(VARS_HEATMAP_ES)))
  ) %>%
  ggplot(aes(x = cluster, y = variable, fill = pct_modal)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(valor_modal, "\n(", percent(pct_modal, 0.1), ")")),
            size = 2.8, lineheight = 0.9, color = "grey10") +
  scale_fill_gradient(low = "#EBF4FB", high = "#1A3A5C",
                      labels = label_percent(), name = "% modal") +
  labs(
    title    = "Heatmap de perfil modal por cluster",
    subtitle = "Categoría más frecuente de cada variable de diseño en cada cluster",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1A3A5C"),
    plot.subtitle = element_text(color = "grey50"),
    axis.text.x   = element_text(face = "bold"),
    panel.grid    = element_blank(),
    legend.position = "right"
  )

print(p_heatmap)


# -----------------------------------------------------------------------------
# 5. APRENDIZAJE SUPERVISADO — RANDOM FOREST
# (sin Ginecología/Obstetricia ni Oncología)
# Variables objetivo: excluye_mayores, sex_label, excluye_obesidad
# -----------------------------------------------------------------------------

# Variables a excluir de los predictores en todos los modelos
VARS_EXCLUIR_PRED <- c(
  "excluye_mayores", "sex_label", "sex", "sex_restringido", "nct_number",
  "bmi_restriccion", "excluye_obesidad", "excluye_bajo_peso", "excluye_bmi_ambos",
  "menciona_bmi", "restriccion_etnica_any", "texto", "rango_max_edad",
  "bmi_valor_raw", "bmi_operador", "bmi_rango", "bmi_rango_lower", "bmi_rango_upper",
  "eligibility_criteria", "conditions", "sponsor_name", "sponsor_class",
  "minimum_age_raw", "maximum_age_raw", "max_age_num", "start_date", "completion_date",
  "std_ages", "locations_raw"
)
# Agregar variables de etnia
VARS_EXCLUIR_PRED <- c(VARS_EXCLUIR_PRED,
                       grep("^incluye_solo|^excluye_[a-z]", names(df_model), value = TRUE))

# Añadir columnas lógicas del text mining explícitamente
VARS_EXCLUIR_PRED <- c(VARS_EXCLUIR_PRED,
                       "bmi_rango", "excluye_bajo_peso", "excluye_bmi_ambos", "menciona_bmi",
                       "restriccion_etnica_any", "excluye_obesidad")
VARS_EXCLUIR_PRED <- unique(VARS_EXCLUIR_PRED)

# Función auxiliar
limpiar_df <- function(data, target) {
  cols_logicas <- names(data)[sapply(data, is.logical)]
  cols_excluir <- unique(c(cols_logicas,
                           VARS_EXCLUIR_PRED[VARS_EXCLUIR_PRED != target]))
  cols_excluir <- cols_excluir[cols_excluir %in% names(data)]
  data <- data %>% select(-all_of(cols_excluir))
  na.omit(data)
}

# Traducción de nombres de variables para gráficos
VARS_ES <- c(
  # Diseño
  "phases"                          = "Fase",
  "primary_purpose"                 = "Propósito primario",
  "allocation"                      = "Aleatorización",
  "masking"                         = "Enmascaramiento",
  "intervention_model"              = "Modelo de intervención",
  "intervention_type"               = "Tipo de intervención",
  # Sponsor
  "sponsor_class_deriv"             = "Tipo de sponsor",
  # Área y condición
  "area_terapeutica"                = "Área terapéutica",
  "n_conditions"                    = "N° de condiciones",
  "n_interventions"                 = "N° de intervenciones",
  "n_primary_outcomes"              = "N° de outcomes primarios",
  # Temporales
  "start_year"                      = "Año de inicio",
  "duration_months"                 = "Duración (meses)",
  # Geográficas y operativas
  "has_us_location"                 = "Sede en EE.UU.",
  "has_collaborators"               = "Tiene colaboradores",
  "enrollment"                      = "Enrollment previsto",
  # Edad
  "min_age_num"                     = "Edad mínima de inclusión",
  # Regulación
  "is_fda_drug"                     = "Regulado FDA (fármaco)",
  "is_fda_device"                   = "Regulado FDA (dispositivo)",
  "has_dmc"                         = "Tiene DSMB",
  # Otras variables usadas como predictores
  "excluye_mayores"                 = "Excluye adultos mayores",
  "sex_restringido"                 = "Restricción por sexo"
)

# Función para traducir nombres en gráficos
traducir_vars <- function(vars) {
  ifelse(vars %in% names(VARS_ES), VARS_ES[vars], vars)
}

# Variable objetivo: excluye_mayores
cat("=== RANDOM FOREST: excluye_mayores ===\n")

set.seed(42)
idx_bin    <- createDataPartition(df_model$excluye_mayores, p = 0.80, list = FALSE)
train_bin  <- df_model[ idx_bin, ]
test_bin   <- df_model[-idx_bin, ]
folds_bin  <- createFolds(train_bin$excluye_mayores, k = 5, list = TRUE)

auc_rf_bin <- map_dbl(folds_bin, function(fold_idx) {
  tr  <- train_bin[setdiff(seq_len(nrow(train_bin)), fold_idx), ]
  val <- train_bin[fold_idx, ]
  
  # Eliminar predictores con un solo nivel
  single_lvl <- tr %>%
    select(where(is.factor), -excluye_mayores) %>%
    summarise(across(everything(), ~ nlevels(droplevels(.x)) < 2)) %>%
    pivot_longer(everything()) %>% filter(value) %>% pull(name)
  if (length(single_lvl) > 0) {
    tr  <- tr  %>% select(-all_of(single_lvl))
    val <- val %>% select(-all_of(single_lvl)) }
  
  for (col in names(tr)) if (is.factor(tr[[col]]) && col != "excluye_mayores") {
    val[[col]] <- factor(val[[col]], levels = levels(droplevels(tr[[col]])))
    tr[[col]]  <- droplevels(tr[[col]]) }
  
  # Eliminar columnas lógicas antes del na.omit
  cols_log <- names(tr)[sapply(tr, is.logical)]
  if (length(cols_log) > 0) {
    tr  <- tr  %>% select(-all_of(cols_log))
    val <- val %>% select(-all_of(cols_log)) }
  tr  <- na.omit(tr)
  val <- na.omit(val)
  
  if (nlevels(droplevels(tr$excluye_mayores))  < 2) return(NA_real_)
  if (nlevels(droplevels(val$excluye_mayores)) < 2) return(NA_real_)
  
  vars_p <- names(tr)[!names(tr) %in% c(VARS_EXCLUIR_PRED, "excluye_mayores")]
  m <- randomForest(reformulate(vars_p, "excluye_mayores"), data = tr,
                    ntree = 500, mtry = floor(sqrt(length(vars_p))),
                    sampsize = c(No = 2000, "Sí" = 2000), importance = FALSE)
  as.numeric(auc(roc(val$excluye_mayores, predict(m, val, type = "prob")[,"Sí"],
                     levels = c("No","Sí"), direction = "<", quiet = TRUE)))
})
# Descartar folds fallidos antes de promediar
auc_rf_bin <- auc_rf_bin[!is.na(auc_rf_bin)]
cat("AUC CV:", round(mean(auc_rf_bin), 4), "± SD:", round(sd(auc_rf_bin), 4), "\n")

train_bin <- limpiar_df(train_bin, "excluye_mayores")
test_bin  <- limpiar_df(test_bin,  "excluye_mayores")
for (col in names(train_bin)) if (is.factor(train_bin[[col]])) {
  train_bin[[col]] <- droplevels(train_bin[[col]])
  test_bin[[col]]  <- factor(test_bin[[col]], levels = levels(train_bin[[col]])) }
single_lvl_bin <- train_bin %>%
  select(where(is.factor), -excluye_mayores) %>%
  summarise(across(everything(), ~ nlevels(.x) < 2)) %>%
  pivot_longer(everything()) %>% filter(value) %>% pull(name)
if (length(single_lvl_bin) > 0) {
  train_bin <- train_bin %>% select(-all_of(single_lvl_bin))
  test_bin  <- test_bin  %>% select(-all_of(single_lvl_bin)) }
vars_bin <- names(train_bin)[!names(train_bin) %in% c(VARS_EXCLUIR_PRED, "excluye_mayores")]

set.seed(42)
rf_bin      <- randomForest(reformulate(vars_bin, "excluye_mayores"), data = train_bin,
                            ntree = 500, mtry = floor(sqrt(length(vars_bin))),
                            sampsize = c(No = 2000, "Sí" = 2000), importance = TRUE)
prob_rf_bin <- predict(rf_bin, test_bin, type = "prob")[,"Sí"]
roc_bin     <- roc(test_bin$excluye_mayores, prob_rf_bin,
                   levels = c("No","Sí"), direction = "<", quiet = TRUE)
cat("AUC test:", round(auc(roc_bin), 4), "\n\n")

p_roc_bin <- tibble(fpr = 1 - roc_bin$specificities, tpr = roc_bin$sensitivities) %>%
  ggplot(aes(x = fpr, y = tpr)) +
  geom_line(color = "#D7191C", linewidth = 1.3) +
  geom_abline(linetype = "dashed", color = "grey60") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title    = "Curva ROC — Random Forest (excluye_mayores)",
       subtitle = paste0("AUC test = ", round(auc(roc_bin), 3),
                         " | AUC CV = ", round(mean(auc_rf_bin), 3),
                         " ± ", round(sd(auc_rf_bin), 3)),
       x = "1 - Especificidad", y = "Sensibilidad") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"))
print(p_roc_bin)

imp_bin <- importance(rf_bin, type = 1) %>% as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(importancia = MeanDecreaseAccuracy) %>%
  arrange(desc(importancia)) %>%
  mutate(
    variable_es = traducir_vars(variable),
    variable_es = fct_reorder(variable_es, importancia)
  )

p_imp_bin <- imp_bin %>%
  ggplot(aes(x = importancia, y = variable_es)) +
  geom_col(fill = "#D7191C", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = round(importancia, 2)), hjust = -0.15, size = 3.2, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Importancia de variables — RF (excluye_mayores)",
       subtitle = "Mean Decrease Accuracy",
       x = "Mean Decrease Accuracy", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())
print(p_imp_bin)


# Variable objetivo: sex_label
cat("=== RANDOM FOREST MULTICLASE: sex_label ===\n")

df_sex    <- df_model %>% filter(!is.na(sex_label)) %>% limpiar_df("sex_label")
set.seed(42)
idx_sex   <- createDataPartition(df_sex$sex_label, p = 0.80, list = FALSE)
train_sex <- df_sex[ idx_sex, ]
test_sex  <- df_sex[-idx_sex, ]

for (col in names(train_sex)) if (is.factor(train_sex[[col]])) {
  train_sex[[col]] <- droplevels(train_sex[[col]])
  test_sex[[col]]  <- factor(test_sex[[col]], levels = levels(train_sex[[col]])) }

single_lvl_sex <- train_sex %>%
  select(where(is.factor), -sex_label) %>%
  summarise(across(everything(), ~ nlevels(.x) < 2)) %>%
  pivot_longer(everything()) %>% filter(value) %>% pull(name)
if (length(single_lvl_sex) > 0) {
  train_sex <- train_sex %>% select(-all_of(single_lvl_sex))
  test_sex  <- test_sex  %>% select(-all_of(single_lvl_sex)) }

vars_sex <- names(train_sex)[!names(train_sex) %in% c(VARS_EXCLUIR_PRED, "sex_label")]

set.seed(42)
rf_sex   <- randomForest(reformulate(vars_sex, "sex_label"), data = train_sex,
                         ntree = 500, mtry = floor(sqrt(length(vars_sex))), importance = TRUE)
pred_sex <- predict(rf_sex, test_sex)
cat("Accuracy test:", round(mean(pred_sex == test_sex$sex_label), 4), "\n")

prob_sex    <- predict(rf_sex, test_sex, type = "prob")
roc_mujeres <- roc(as.integer(test_sex$sex_label == "Solo mujeres"),
                   prob_sex[,"Solo mujeres"], quiet = TRUE)
roc_hombres <- roc(as.integer(test_sex$sex_label == "Solo hombres"),
                   prob_sex[,"Solo hombres"], quiet = TRUE)
cat("AUC Solo mujeres:", round(auc(roc_mujeres), 4),
    "| AUC Solo hombres:", round(auc(roc_hombres), 4), "\n\n")

p_roc_sex <- bind_rows(
  tibble(fpr = 1-roc_mujeres$specificities, tpr = roc_mujeres$sensitivities,
         clase = paste0("Solo mujeres (AUC=", round(auc(roc_mujeres),3), ")")),
  tibble(fpr = 1-roc_hombres$specificities, tpr = roc_hombres$sensitivities,
         clase = paste0("Solo hombres (AUC=", round(auc(roc_hombres),3), ")"))
) %>%
  ggplot(aes(x = fpr, y = tpr, color = clase)) +
  geom_line(linewidth = 1.3) +
  geom_abline(linetype = "dashed", color = "grey60") +
  scale_color_manual(
    values = setNames(c("#D7191C","#2C7BB6"),
                      c(paste0("Solo mujeres (AUC=",round(auc(roc_mujeres),3),")"),
                        paste0("Solo hombres (AUC=",round(auc(roc_hombres),3),")"))),
    name = NULL) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title    = "Curvas ROC — RF multiclase (sex_label)",
       subtitle = "Clases minoritarias vs. resto (uno-vs-resto)",
       x = "1 - Especificidad", y = "Sensibilidad") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        legend.position = "bottom")
print(p_roc_sex)

p_imp_sex <- importance(rf_sex, type = 1) %>% as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(importancia = MeanDecreaseAccuracy) %>%
  arrange(desc(importancia)) %>%
  mutate(
    variable_es = traducir_vars(variable),
    variable_es = fct_reorder(variable_es, importancia)
  ) %>%
  ggplot(aes(x = importancia, y = variable_es)) +
  geom_col(fill = "#2C7BB6", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = round(importancia, 2)), hjust = -0.15, size = 3.2, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Importancia de variables — RF multiclase (sex_label)",
       subtitle = "Mean Decrease Accuracy",
       x = "Mean Decrease Accuracy", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())
print(p_imp_sex)


# Variable objetivo: excluye_obesidad
cat("=== RANDOM FOREST: excluye_obesidad (IMC) ===\n")

df_bmi    <- df_model %>%
  filter(!is.na(excluye_obesidad)) %>%
  mutate(excluye_obesidad = factor(if_else(excluye_obesidad, "Sí", "No"),
                                   levels = c("No","Sí"))) %>%
  limpiar_df("excluye_obesidad")
set.seed(42)
idx_bmi   <- createDataPartition(df_bmi$excluye_obesidad, p = 0.80, list = FALSE)
train_bmi <- df_bmi[ idx_bmi, ]
test_bmi  <- df_bmi[-idx_bmi, ]

for (col in names(train_bmi)) if (is.factor(train_bmi[[col]])) {
  train_bmi[[col]] <- droplevels(train_bmi[[col]])
  test_bmi[[col]]  <- factor(test_bmi[[col]], levels = levels(train_bmi[[col]])) }

single_lvl_bmi <- train_bmi %>%
  select(where(is.factor), -excluye_obesidad) %>%
  summarise(across(everything(), ~ nlevels(.x) < 2)) %>%
  pivot_longer(everything()) %>% filter(value) %>% pull(name)
if (length(single_lvl_bmi) > 0) {
  train_bmi <- train_bmi %>% select(-all_of(single_lvl_bmi))
  test_bmi  <- test_bmi  %>% select(-all_of(single_lvl_bmi)) }

vars_bmi <- names(train_bmi)[!names(train_bmi) %in% c(VARS_EXCLUIR_PRED, "excluye_obesidad")]
ss       <- min(sum(train_bmi$excluye_obesidad == "Sí"), 2000)

set.seed(42)
rf_bmi   <- randomForest(reformulate(vars_bmi, "excluye_obesidad"), data = train_bmi,
                         ntree = 500, mtry = floor(sqrt(length(vars_bmi))),
                         sampsize = c(No = ss, "Sí" = ss), importance = TRUE)
prob_bmi <- predict(rf_bmi, test_bmi, type = "prob")[,"Sí"]
roc_bmi  <- roc(test_bmi$excluye_obesidad, prob_bmi,
                levels = c("No","Sí"), direction = "<", quiet = TRUE)
cat("AUC test:", round(auc(roc_bmi), 4), "\n\n")

p_imp_bmi <- importance(rf_bmi, type = 1) %>% as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(importancia = MeanDecreaseAccuracy) %>%
  arrange(desc(importancia)) %>%
  mutate(
    variable_es = traducir_vars(variable),
    variable_es = fct_reorder(variable_es, importancia)
  ) %>%
  ggplot(aes(x = importancia, y = variable_es)) +
  geom_col(fill = "#1A9641", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = round(importancia, 2)), hjust = -0.15, size = 3.2, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Importancia de variables — RF (excluye_obesidad)",
       subtitle = "Mean Decrease Accuracy",
       x = "Mean Decrease Accuracy", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())
print(p_imp_bmi)


# -----------------------------------------------------------------------------
# 6. ANÁLISIS EN PROFUNDIDAD POR ÁREA TERAPÉUTICA
# Neurología/Psiquiatría (adultos mayores)
# Hematología (restricción por sexo)
# Metabólica/Endócrina (IMC)
# -----------------------------------------------------------------------------

# Neurología/Psiquiatría — exclusión de adultos mayores
cat("=== Neurología/Psiquiatría × excluye_mayores ===\n")
df_neuro <- df %>% filter(area_terapeutica == "Neurología/Psiquiatría")
cat("N:", nrow(df_neuro), "\n")

df_neuro %>% count(excluye_mayores) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% print()

# Top intervenciones por frecuencia y por porcentaje de exclusión
cat("\nTop 20 intervenciones más frecuentes y su % de exclusión de adultos mayores:\n")
df_neuro %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n         = n(),
    pct_excl  = mean(excluye_mayores == "Sí"),
    .groups   = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  mutate(pct_excl = percent(pct_excl, 0.1)) %>%
  print()

cat("\nIntervenciones con mayor % de exclusión de adultos mayores (mín. 10 ensayos):\n")
df_neuro %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n        = n(),
    pct_excl = mean(excluye_mayores == "Sí"),
    .groups  = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(desc(pct_excl)) %>%
  slice_head(n = 20) %>%
  mutate(pct_excl = percent(pct_excl, 0.1)) %>%
  print()

# Gráfico: top 15 intervenciones por porcentaje de exclusión
p_neuro <- df_neuro %>%
  mutate(intervention_name = str_to_title(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(pct = mean(excluye_mayores == "Sí"), n = n(), .groups = "drop") %>%
  filter(n >= 10) %>%
  slice_max(pct, n = 15) %>%
  mutate(intervention_name = fct_reorder(intervention_name, pct)) %>%
  ggplot(aes(x = pct, y = intervention_name)) +
  geom_col(fill = "#D7191C", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(percent(pct, 0.1), " (n=", n, ")")),
            hjust = -0.08, size = 3.2, color = "grey30") +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Neurología/Psiquiatría: exclusión de adultos mayores por intervención primaria",
    subtitle = "Top 15 intervenciones | mínimo 10 ensayos | área de alta prevalencia en adultos mayores",
    x = "% que excluye adultos mayores", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        panel.grid.major.y = element_blank())
print(p_neuro)

# Neurología/Psiquiatría — restricción por sexo
cat("\nNeurología/Psiquiatría × restricción por sexo:\n")
cat("Distribución:\n")
df_neuro %>% count(sex_label, sort = TRUE) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% print()

cat("\nIntervenciones primarias en ensayos restringidos por sexo:\n")
df_neuro %>%
  filter(sex %in% c("FEMALE","MALE")) %>%
  mutate(
    sex_label_bin    = if_else(sex == "FEMALE", "Solo mujeres", "Solo hombres"),
    intervention_name = str_to_title(str_trim(tolower(intervention_name)))
  ) %>%
  count(sex_label_bin, intervention_name, sort = TRUE) %>%
  group_by(sex_label_bin) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  print(n = 30)

# Clasificación manual: justificación obvia vs. no obvia
interv_justif_obvia <- c(
  "estradiol", "estrogen", "17-beta-estradiol", "raloxifene",
  "brexanolone", "testosterone gel", "levitra", "vardenafil",
  "sumatriptan succinate/naproxen sodium",
  "care management for postpartum depression"
)

cat("\nIntervenciones SIN justificación obvia de restricción por sexo:\n")
df_neuro %>%
  filter(sex %in% c("FEMALE","MALE")) %>%
  mutate(
    sex_label_bin     = if_else(sex == "FEMALE", "Solo mujeres", "Solo hombres"),
    intervention_name = str_trim(tolower(intervention_name)),
    justif_obvia      = intervention_name %in% interv_justif_obvia
  ) %>%
  filter(!justif_obvia) %>%
  count(sex_label_bin, intervention_name, sort = TRUE) %>%
  group_by(sex_label_bin) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  print(n = 20)

# Gráfico: intervenciones sin justificación obvia de restricción por sexo
p_neuro_sex <- df_neuro %>%
  filter(sex %in% c("FEMALE","MALE")) %>%
  mutate(
    sex_label_bin     = if_else(sex == "FEMALE", "Solo mujeres", "Solo hombres"),
    intervention_name = str_to_title(str_trim(tolower(intervention_name))),
    justif_obvia      = tolower(intervention_name) %in% interv_justif_obvia
  ) %>%
  filter(!justif_obvia) %>%
  count(sex_label_bin, intervention_name) %>%
  group_by(sex_label_bin) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(intervention_name = fct_reorder(intervention_name, n)) %>%
  ggplot(aes(x = n, y = intervention_name, fill = sex_label_bin)) +
  geom_col(width = 0.7, alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2, color = "grey30") +
  scale_fill_manual(values = c("Solo mujeres" = "#D7191C",
                               "Solo hombres" = "#2C7BB6")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~ sex_label_bin, scales = "free") +
  labs(
    title    = "Neurología/Psiquiatría: intervenciones sin justificación obvia de restricción por sexo",
    subtitle = "Top 10 por clase | excluye intervenciones hormonales y condiciones perinatales",
    x = "N° de ensayos", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        strip.text    = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())
print(p_neuro_sex)


# Hematología — restricción por sexo
cat("\n=== Hematología × restricción por sexo ===\n")
df_hema <- df %>%
  filter(area_terapeutica == "Hematología") %>%
  mutate(sex_bin = factor(if_else(sex %in% c("FEMALE","MALE"), "Sí","No"),
                          levels = c("No","Sí")))
cat("N:", nrow(df_hema), "\n")

df_hema %>% count(sex_label, sort = TRUE) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% print()

cat("\nTop 20 intervenciones más frecuentes y su % de restricción por sexo:\n")
df_hema %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n           = n(),
    pct_restric = mean(sex_bin == "Sí"),
    .groups     = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  mutate(pct_restric = percent(pct_restric, 0.1)) %>%
  print()

cat("\nIntervenciones con mayor % de restricción por sexo (mín. 5 ensayos):\n")
df_hema %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n           = n(),
    pct_restric = mean(sex_bin == "Sí"),
    .groups     = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(pct_restric)) %>%
  slice_head(n = 20) %>%
  mutate(pct_restric = percent(pct_restric, 0.1)) %>%
  print()

p_hema <- df_hema %>%
  mutate(intervention_name = str_to_title(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(pct = mean(sex_bin == "Sí"), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>%
  slice_max(pct, n = 15) %>%
  mutate(intervention_name = fct_reorder(intervention_name, pct)) %>%
  ggplot(aes(x = pct, y = intervention_name)) +
  geom_col(fill = "#2C7BB6", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(percent(pct, 0.1), " (n=", n, ")")),
            hjust = -0.08, size = 3.2, color = "grey30") +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Hematología: restricción por sexo por intervención primaria",
    subtitle = "Top 15 intervenciones | mínimo 5 ensayos | restricción por sexo no anticipada",
    x = "% con restricción por sexo", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        panel.grid.major.y = element_blank())
print(p_hema)


# Metabólica/Endócrina — restricción por IMC
cat("\n=== Metabólica/Endocrina × excluye_obesidad ===\n")
df_meta <- df %>% filter(area_terapeutica == "Metabólica/Endocrina")
cat("N:", nrow(df_meta), "\n")

df_meta %>% count(bmi_restriccion, sort = TRUE) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% print()

cat("\nTop 20 intervenciones más frecuentes y su % de restricción por IMC:\n")
df_meta %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n       = n(),
    pct_bmi = mean(excluye_obesidad),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  mutate(pct_bmi = percent(pct_bmi, 0.1)) %>%
  print()

cat("\nIntervenciones con mayor % de exclusión por obesidad (mín. 5 ensayos):\n")
df_meta %>%
  mutate(intervention_name = tolower(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(
    n       = n(),
    pct_bmi = mean(excluye_obesidad),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(pct_bmi)) %>%
  slice_head(n = 20) %>%
  mutate(pct_bmi = percent(pct_bmi, 0.1)) %>%
  print()

p_meta <- df_meta %>%
  mutate(intervention_name = str_to_title(str_trim(intervention_name))) %>%
  group_by(intervention_name) %>%
  summarise(pct = mean(excluye_obesidad), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>%
  slice_max(pct, n = 15) %>%
  mutate(intervention_name = fct_reorder(intervention_name, pct)) %>%
  ggplot(aes(x = pct, y = intervention_name)) +
  geom_col(fill = "#1A9641", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(percent(pct, 0.1), " (n=", n, ")")),
            hjust = -0.08, size = 3.2, color = "grey30") +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Metabólica/Endocrina: exclusión por obesidad (IMC) por intervención primaria",
    subtitle = "Top 15 intervenciones | mínimo 5 ensayos | área con mayor restricción por IMC",
    x = "% que excluye por obesidad", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        plot.subtitle = element_text(color = "grey50"),
        panel.grid.major.y = element_blank())
print(p_meta)


# -----------------------------------------------------------------------------
# 7. ANÁLISIS DE TEXTO LIBRE — IMC Y ETNIA
# -----------------------------------------------------------------------------

cat("=== ANÁLISIS DE TEXTO LIBRE: IMC Y ETNIA ===\n")
cat("Cobertura eligibility_criteria:",
    percent(mean(!is.na(df$eligibility_criteria) & df$eligibility_criteria != ""),
            accuracy = 0.01), "\n\n")

df_texto <- df %>% filter(!is.na(eligibility_criteria))

cat("--- Restricción por IMC ---\n")
df_texto %>% count(bmi_restriccion, sort = TRUE) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% print()

cat("\n--- IMC × excluye_mayores ---\n")
df_texto %>%
  filter(bmi_restriccion != "No restringe por IMC") %>%
  count(bmi_restriccion, excluye_mayores) %>%
  group_by(bmi_restriccion) %>%
  mutate(pct = percent(n/sum(n), 0.1)) %>% ungroup() %>% print()

cat("\n--- Restricciones étnicas (por grupo y dirección) ---\n")
tibble(
  grupo     = rep(c("Hispanic","Black","Asian","White","Indígena"), each = 2),
  direccion = rep(c("Incluye solo","Excluye"), 5),
  n = c(sum(df_texto$incluye_solo_hispanic),   sum(df_texto$excluye_hispanic),
        sum(df_texto$incluye_solo_black),       sum(df_texto$excluye_black),
        sum(df_texto$incluye_solo_asian),       sum(df_texto$excluye_asian),
        sum(df_texto$incluye_solo_white),       sum(df_texto$excluye_white),
        sum(df_texto$incluye_solo_indigenous),  sum(df_texto$excluye_indigenous))
) %>% mutate(pct = percent(n/nrow(df_texto), 0.01)) %>% print()

cat("\n--- Acumulación de criterios restrictivos ---\n")
df_texto %>%
  mutate(n_excl = as.integer(excluye_mayores == "Sí") +
           as.integer(bmi_restriccion != "No restringe por IMC") +
           as.integer(restriccion_etnica_any)) %>%
  count(n_excl) %>% mutate(pct = percent(n/sum(n), 0.1)) %>% print()

# Gráficos
p_bmi_distrib <- df_texto %>%
  count(bmi_restriccion) %>%
  mutate(pct = n/sum(n), bmi_restriccion = fct_reorder(bmi_restriccion, n)) %>%
  ggplot(aes(x = n, y = bmi_restriccion)) +
  geom_col(fill = "#2C7BB6", width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(n, " (", percent(pct, 0.1), ")")),
            hjust = -0.08, size = 3.2, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(title    = "Restricción por IMC en criterios de elegibilidad",
       subtitle = "Texto completo | N = 48.926 ensayos",
       x = "N° de ensayos", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.y = element_blank())
print(p_bmi_distrib)

p_bmi_area <- df_texto %>%
  filter(!is.na(area_terapeutica), bmi_restriccion != "No restringe por IMC") %>%
  left_join(df_texto %>% count(area_terapeutica, name = "n_total_area"),
            by = "area_terapeutica") %>%
  filter(n_total_area >= 100) %>%
  count(area_terapeutica, bmi_restriccion, n_total_area) %>%
  mutate(pct = n/n_total_area,
         area_terapeutica = fct_reorder(area_terapeutica, pct, .fun = sum)) %>%
  ggplot(aes(x = pct, y = area_terapeutica, fill = bmi_restriccion)) +
  geom_col(width = 0.7, alpha = 0.85) +
  scale_fill_manual(values = c(
    "Excluye obesidad (IMC alto)"    = "#D7191C",
    "Excluye bajo peso (IMC bajo)"   = "#2C7BB6",
    "Excluye obesidad y bajo peso"   = "#7B0000",
    "Menciona IMC (no clasificado)"  = "#FDAE61"), name = NULL) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Tipo de restricción por IMC por área terapéutica",
       subtitle = "% del total de ensayos por área (mínimo 100 ensayos)",
       x = "% de ensayos", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        legend.position = "bottom", panel.grid.major.y = element_blank())
print(p_bmi_area)

etnia_df <- tibble(
  grupo     = rep(c("Hispanic","Black","Asian","White","Indígena"), each = 2),
  direccion = rep(c("Incluye solo","Excluye"), 5),
  n = c(sum(df_texto$incluye_solo_hispanic),  sum(df_texto$excluye_hispanic),
        sum(df_texto$incluye_solo_black),      sum(df_texto$excluye_black),
        sum(df_texto$incluye_solo_asian),      sum(df_texto$excluye_asian),
        sum(df_texto$incluye_solo_white),      sum(df_texto$excluye_white),
        sum(df_texto$incluye_solo_indigenous), sum(df_texto$excluye_indigenous))
) %>% mutate(pct = n/nrow(df_texto), grupo = fct_reorder(grupo, n, .fun = sum))

p_etnia <- etnia_df %>%
  filter(n > 0) %>%
  ggplot(aes(x = pct, y = grupo, fill = direccion)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(n, " (", percent(pct, 0.01), ")")),
            position = position_dodge(width = 0.6),
            hjust = -0.08, size = 3, color = "grey30") +
  scale_fill_manual(values = c("Incluye solo" = "#2C7BB6", "Excluye" = "#D7191C"),
                    name = NULL) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.25))) +
  labs(title    = "Restricciones por grupo étnico en criterios de elegibilidad",
       subtitle = "Por dirección: incluye solo ese grupo vs. excluye ese grupo",
       x = "% de ensayos", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        legend.position = "bottom", panel.grid.major.y = element_blank())
print(p_etnia)

p_coexcl <- df_texto %>%
  mutate(n_excl = as.integer(excluye_mayores == "Sí") +
           as.integer(bmi_restriccion != "No restringe por IMC") +
           as.integer(restriccion_etnica_any)) %>%
  count(n_excl) %>% mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = factor(n_excl), y = pct, fill = factor(n_excl))) +
  geom_col(width = 0.6, show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = paste0(percent(pct, 0.1), "\n(n=", n, ")")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("0" = "#2C7BB6","1" = "#FDAE61",
                               "2" = "#D7191C","3" = "#7B0000")) +
  scale_x_discrete(labels = c("0" = "Ninguno",
                              "1" = "1 criterio\nrestrictivo",
                              "2" = "2 criterios\nrestrictivos",
                              "3" = "3 criterios\nrestrictivos")) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(title    = "Acumulación de criterios restrictivos por ensayo",
       subtitle = "Edad + IMC + etnia/raza",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", color = "#1A3A5C"),
        panel.grid.major.x = element_blank())
print(p_coexcl)

cat("\n=== Análisis completado ===\n")
