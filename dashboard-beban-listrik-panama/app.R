# =============================================================================
#  DASHBOARD PERAMALAN BEBAN LISTRIK PANAMA
#  Time Series Foundation Model vs Metode Statistik, Machine Learning, Deep Learning
#  Tim: Tototro (TimeGPT), Akmal (Chronos-2), Jihan (TimesFM 2.5)
# -----------------------------------------------------------------------------
#  Cara menjalankan:
#   1. Taruh file ini (app.R) satu folder dengan folder "hasil_csv"
#      (atau file "hasil_csv.zip") hasil notebook.
#   2. Buka app.R di RStudio, lalu klik tombol "Run App".
# =============================================================================

# ---- Paket ------------------------------------------------------------------
paket <- c("shiny", "bslib", "plotly", "DT", "dplyr", "tidyr", "readr")
baru  <- paket[!paket %in% rownames(installed.packages())]
if (length(baru)) install.packages(baru)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ---- Lokasi data --------------------------------------------------------------
cari_folder_data <- function() {
  kandidat <- c("hasil_csv", file.path(getwd(), "hasil_csv"))
  for (k in kandidat) if (dir.exists(k) && file.exists(file.path(k, "12_ramalan_semua_model.csv"))) return(k)
  if (file.exists("hasil_csv.zip")) {
    unzip("hasil_csv.zip", exdir = "hasil_csv")
    if (file.exists(file.path("hasil_csv", "12_ramalan_semua_model.csv"))) return("hasil_csv")
  }
  stop("Folder 'hasil_csv' (atau file 'hasil_csv.zip') tidak ditemukan di folder yang sama dengan app.R.")
}
DIR_DATA <- cari_folder_data()

baca <- function(f, wajib = TRUE) {
  p <- file.path(DIR_DATA, f)
  if (!file.exists(p)) {
    if (wajib) stop("File tidak ditemukan: ", p)
    return(NULL)
  }
  suppressWarnings(read_csv(p, show_col_types = FALSE, name_repair = "minimal",
                            locale = locale(encoding = "UTF-8")))
}
ke_lgl   <- function(x) toupper(trimws(as.character(x))) %in% c("TRUE", "YA", "1", "T")
ke_waktu <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  x <- as.character(x)
  x <- ifelse(nchar(x) == 10, paste(x, "00:00:00"), substr(x, 1, 19))
  as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}

# ---- Baca semua hasil -------------------------------------------------------------
ringkasan_data <- baca("01_ringkasan_data.csv")
deskripsi      <- baca("02_deskripsi_variabel.csv")
stat_desk      <- baca("03_statistik_deskriptif.csv")
bulanan        <- baca("04_beban_bulanan.csv") %>% setNames(c("Bulan", "Beban")) %>%
                    mutate(Bulan = as.Date(substr(as.character(Bulan), 1, 10)))
profil         <- baca("05_profil_jam_hari.csv")
korelasi       <- baca("06_korelasi_eksogen.csv") %>% setNames(c("Variabel", "Korelasi"))
ekstrem        <- baca("07_nilai_ekstrem.csv")
periode        <- baca("08_periode_uji.csv")
meta           <- baca("09_daftar_model.csv") %>%
                    mutate(Eksogen = ke_lgl(Eksogen), Fondasi = ke_lgl(Fondasi))
waktu_komp     <- baca("10_waktu_komputasi.csv") %>% setNames(c("Model", "Waktu"))
gagal          <- baca("11_model_gagal.csv", wajib = FALSE)
ramalan        <- baca("12_ramalan_semua_model.csv") %>% mutate(ds = ke_waktu(ds))
akurasi_cv     <- baca("19_akurasi_cv_per_origin.csv", wajib = FALSE)
akurasi1       <- baca("13_akurasi_origin1.csv")
dm_tabel       <- baca("18_uji_diebold_mariano.csv", wajib = FALSE)
kesimpulan     <- baca("21_kesimpulan.csv", wajib = FALSE)

if (is.null(akurasi_cv)) akurasi_cv <- akurasi1
akurasi_cv <- akurasi_cv %>% mutate(Eksogen = ke_lgl(Eksogen), Fondasi = ke_lgl(Fondasi))
akurasi1   <- akurasi1   %>% mutate(Eksogen = ke_lgl(Eksogen), Fondasi = ke_lgl(Fondasi))

# Hanya model yang benar-benar punya ramalan
meta    <- meta %>% filter(Model %in% unique(ramalan$model))
ORIGINS <- sort(unique(ramalan$origin))
N_ORIG  <- length(ORIGINS)

label_origin <- setNames(ORIGINS, sapply(ORIGINS, function(o) {
  p <- periode[periode$Origin == o, ]
  if (nrow(p) == 0) return(paste("Origin", o))
  aw <- ke_waktu(p[["Awal uji"]]); ak <- ke_waktu(p[["Akhir uji"]])
  paste0("Minggu ", o, ": ", format(aw, "%d %b"), " - ", format(ak, "%d %b %Y"),
         if (o == 1) " (utama)" else "")
}))

# ---- Warna ----------------------------------------------------------------------
WARNA_KEL <- c("Patokan" = "#8C8C8C", "Statistik" = "#3B82C4", "Machine learning" = "#E69F00",
               "Deep learning" = "#2BA84A", "Model fondasi" = "#C2418E", "Ensemble" = "#D55E00")
URUT_KEL  <- names(WARNA_KEL)[names(WARNA_KEL) %in% meta$Kelompok]
meta      <- meta %>% mutate(Kelompok = factor(Kelompok, levels = URUT_KEL)) %>% arrange(Kelompok, Model)

PALET <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
           "#17BECF", "#BCBD22", "#7F7F7F", "#393B79", "#AD494A", "#637939", "#7B4173",
           "#3182BD", "#E6550D", "#31A354", "#756BB1")
WARNA_MODEL <- setNames(rep(PALET, length.out = nrow(meta)), meta$Model)
WARNA_AKTUAL <- "#111111"

pilihan_model <- lapply(split(meta$Model, meta$Kelompok), as.list)
pilihan_model <- pilihan_model[lengths(pilihan_model) > 0]

# ---- Ringkasan angka -------------------------------------------------------------
urut1      <- akurasi1 %>% arrange(MASE)
best1      <- urut1[1, ]
ringkas_cv <- akurasi_cv %>% group_by(Model, Kelompok, Eksogen, Fondasi) %>%
                summarise(MASE_rata = mean(MASE), MASE_sd = sd(MASE), MAPE_rata = mean(MAPE),
                          MAE_rata = mean(MAE), RMSE_rata = mean(RMSE), n = n(), .groups = "drop") %>%
                arrange(MASE_rata)
best_cv    <- ringkas_cv[1, ]
best_fond  <- urut1 %>% filter(Fondasi) %>% slice(1)
best_klas  <- urut1 %>% filter(!Fondasi, Kelompok != "Ensemble") %>% slice(1)
mase_sn    <- urut1$MASE[urut1$Model == "Seasonal Naive"]
n_obs      <- ringkasan_data$Nilai[ringkasan_data$Keterangan == "Jumlah observasi"]
fk  <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
pct <- function(x, d = 2) paste0(fk(x, d), "%")

# ---- Fungsi bantu ------------------------------------------------------------------
dm_test <- function(e1, e2, h = 1) {
  d <- e1^2 - e2^2; T <- length(d); dbar <- mean(d)
  gamma <- sapply(0:(h - 1), function(k) sum((d[(k + 1):T] - dbar) * (d[1:(T - k)] - dbar)) / T)
  v <- (gamma[1] + 2 * sum(gamma[-1])) / T
  s <- dbar / sqrt(v) * sqrt((T + 1 - 2 * h + h * (h - 1) / T) / T)
  c(stat = s, p = 2 * pt(-abs(s), df = T - 1))
}

gaya_plot <- function(p, ...) {
  p %>% layout(font = list(family = "Inter, Segoe UI, Roboto, sans-serif", size = 12),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               hoverlabel = list(font = list(size = 12)),
               legend = list(orientation = "h", y = -0.18), margin = list(t = 30), ...) %>%
    config(displaylogo = FALSE, locale = "id",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

GAYA_DT <- local({
  ch <- tryCatch(eval(formals(DT::datatable)$style), error = function(e) "default")
  if ("bootstrap5" %in% ch) "bootstrap5" else "default"
})

tabel_dt <- function(df, judul = "data", page = 15, ...) {
  datatable(df, rownames = FALSE, extensions = "Buttons", style = GAYA_DT, class = "compact stripe hover nowrap", fillContainer = FALSE,
            options = list(dom = "<'d-flex justify-content-between align-items-center mb-2'Bf>rt<'d-flex justify-content-between mt-2'ip>", pageLength = page, scrollX = TRUE,
                           buttons = list(list(extend = "copy", text = "Salin"),
                                          list(extend = "csv", filename = judul),
                                          list(extend = "excel", filename = judul)),
                           language = list(search = "Cari:", info = "_START_-_END_ dari _TOTAL_ baris",
                                           paginate = list(previous = "Sebelumnya", `next` = "Berikutnya"))),
            ...)
}

info <- function(...) div(class = "kotak-info", icon("circle-info"), span(...))

# =============================================================================
#  TAMPILAN (UI)
# =============================================================================
tema <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1F4E79", secondary = "#C2418E",
                 base_font = font_collection("Inter", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
                 "navbar-bg" = "#1F4E79")

css <- "
.bslib-value-box .value-box-title { font-size: .85rem; opacity: .9; }
.bslib-value-box .value-box-value { font-size: 1.45rem; font-weight: 700; line-height: 1.2; }
.bslib-value-box p { font-size: .8rem; margin-bottom: 0; opacity: .9; }
.card { border-radius: 14px; box-shadow: 0 2px 10px rgba(0,0,0,.06); border: 1px solid rgba(0,0,0,.06); }
.card-header { font-weight: 600; background: transparent; }
.kotak-info { background: #EEF4FB; border-left: 4px solid #1F4E79; padding: 10px 14px; border-radius: 8px;
              font-size: .9rem; margin-bottom: 12px; display: flex; gap: 10px; align-items: flex-start; }
.kotak-info .fa { margin-top: 3px; color: #1F4E79; }
.kesimpulan li { margin-bottom: 8px; }
.hero { background: linear-gradient(120deg, #1F4E79 0%, #2E6FA8 55%, #C2418E 130%); color: white;
        padding: 22px 26px; border-radius: 16px; margin-bottom: 16px; }
.hero h2 { font-weight: 700; margin-bottom: 4px; font-size: 1.6rem; }
.hero p { margin: 0; opacity: .92; }
.lencana { display: inline-block; padding: 3px 10px; border-radius: 20px; font-size: .78rem; font-weight: 600;
           margin: 2px 4px 2px 0; color: white; }
.navbar-brand { font-weight: 700; }
.navbar .nav-link { color: rgba(255,255,255,.85) !important; border-radius: 8px; padding: 6px 10px !important; }
.navbar .nav-link:hover { background: rgba(255,255,255,.12); color: #fff !important; }
.navbar .nav-link.active { background: rgba(255,255,255,.22) !important; color: #fff !important; font-weight: 600; }
.dt-buttons .btn, button.dt-button { margin-right: 6px !important; border-radius: 8px !important;
  border: 1px solid #1F4E79 !important; background: white !important; color: #1F4E79 !important;
  padding: 3px 12px !important; font-size: .85rem !important; }
button.dt-button:hover { background: #1F4E79 !important; color: white !important; }
.dataTables_wrapper .dataTables_filter input { border: 1px solid #ccc; border-radius: 8px; padding: 3px 8px; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current,
.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover { background: #1F4E79 !important; color: white !important;
  border-radius: 8px; border: none !important; }
table.dataTable thead th { border-bottom: 2px solid #1F4E79 !important; }
"

ui <- page_navbar(
  title = tags$span(icon("bolt"), " Peramalan Beban Listrik Panama"),
  theme = tema, fillable = FALSE, bg = "#1F4E79",
  header = tags$head(tags$style(HTML(css))),

  # ---------------------------------------------------------------- RINGKASAN
  nav_panel(
    title = "Ringkasan", icon = icon("gauge-high"),
    div(class = "hero",
        h2("Model Fondasi vs Metode Mapan"),
        p(paste0("Peramalan beban listrik per jam Panama untuk 168 jam (7 hari) ke depan. ",
                 nrow(meta), " model dibandingkan pada ", N_ORIG, " minggu uji. ",
                 "Tim: Tototro (TimeGPT), Akmal (Chronos-2), Jihan (TimesFM 2.5)."))),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box(title = "Model terbaik (minggu utama)", value = best1$Model,
                showcase = icon("trophy"), theme = "primary",
                p(paste0("MASE ", fk(best1$MASE), " | MAPE ", pct(best1$MAPE)))),
      value_box(title = paste0("Terbaik rata-rata ", N_ORIG, " minggu"), value = best_cv$Model,
                showcase = icon("ranking-star"), theme = "success",
                p(paste0("MASE rata-rata ", fk(best_cv$MASE_rata), " | MAPE ", pct(best_cv$MAPE_rata)))),
      value_box(title = "Model fondasi terbaik", value = best_fond$Model,
                showcase = icon("brain"), theme = "secondary",
                p(paste0("MASE ", fk(best_fond$MASE), " vs ", best_klas$Model, " ", fk(best_klas$MASE)))),
      value_box(title = "Patokan Seasonal Naive", value = paste("MASE", fk(mase_sn)),
                showcase = icon("flag-checkered"), theme = "light",
                p(paste0(sum(urut1$MASE < mase_sn), " dari ", nrow(urut1) - 1, " model mengalahkannya")))
    ),
    br(),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Peringkat model menurut MASE"),
           layout_columns(col_widths = c(6, 6),
             radioButtons("rk_dasar", NULL, inline = TRUE,
                          choices = c("Minggu utama" = "utama", "Rata-rata semua minggu" = "cv")),
             radioButtons("rk_eks", NULL, inline = TRUE,
                          choices = c("Semua" = "semua", "Tanpa eksogen" = "tidak", "Dengan eksogen" = "ya"))),
           plotlyOutput("plot_ringkas_rank", height = "470px"),
           info("Garis putus-putus = MASE Seasonal Naive. Batang di kiri garis berarti model mengalahkan patokan paling sederhana.")),
      card(full_screen = TRUE,
           card_header(tagList(icon("list-check"), " Kesimpulan otomatis")),
           uiOutput("ui_kesimpulan"),
           hr(),
           card_header("Model terbaik di setiap kelompok"),
           plotlyOutput("plot_ringkas_kelompok", height = "330px"))
    ),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box(title = "Observasi", value = fk(as.numeric(n_obs), 0), showcase = icon("database"),
                p("jam data beban listrik")),
      value_box(title = "Model dibandingkan", value = nrow(meta), showcase = icon("layer-group"),
                p(paste(sum(meta$Fondasi), "model fondasi"))),
      value_box(title = "Horizon ramalan", value = "168 jam", showcase = icon("clock"), p("7 hari ke depan")),
      value_box(title = "Model gagal", value = if (is.null(gagal)) 0 else nrow(gagal),
                showcase = icon("triangle-exclamation"),
                p(if (is.null(gagal) || nrow(gagal) == 0) "semua model berhasil" else paste(gagal$Model, collapse = ", ")))
    )
  ),

  # ---------------------------------------------------------------- EKSPLORASI
  nav_panel(
    title = "Eksplorasi Data", icon = icon("magnifying-glass-chart"),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE, card_header("Rata-rata beban listrik bulanan"),
           plotlyOutput("plot_tren", height = "380px"),
           info("Beban naik perlahan 2015-2019, lalu turun tajam sejak Maret 2020 akibat pembatasan aktivitas pandemi (area merah).")),
      card(full_screen = TRUE, card_header("Korelasi variabel eksogen dengan beban"),
           plotlyOutput("plot_korelasi", height = "380px"),
           info("Suhu berkorelasi positif kuat dengan beban (pendingin ruangan). Hari libur menurunkan beban."))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Peta panas: beban rata-rata per jam dan hari"),
           plotlyOutput("plot_heatmap", height = "400px")),
      card(full_screen = TRUE, card_header("Profil harian per hari"),
           checkboxGroupInput("pilih_hari", NULL, inline = TRUE,
                              choices = names(profil)[-1], selected = names(profil)[-1]),
           plotlyOutput("plot_profil", height = "340px"))
    ),
    info("Data memiliki musiman ganda: pola harian (24 jam) dengan puncak siang dan puncak kedua sekitar pukul 19.00, serta pola mingguan (168 jam) dengan beban terendah hari Minggu."),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Statistik deskriptif beban (MWh)"), tableOutput("tbl_stat")),
      card(card_header("Ringkasan data"), tableOutput("tbl_ringkas_data")),
      card(card_header("10 nilai beban terendah (kemungkinan pemadaman)"), tableOutput("tbl_ekstrem"))
    ),
    card(card_header("Deskripsi variabel"), card_body(fillable = FALSE, DTOutput("tbl_deskripsi")))
  ),

  # ---------------------------------------------------------------- RAMALAN
  nav_panel(
    title = "Ramalan", icon = icon("chart-line"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 320, title = "Pengaturan",
        selectInput("rm_origin", "Minggu uji", choices = label_origin, selected = 1),
        selectizeInput("rm_model", "Model", choices = pilihan_model, multiple = TRUE,
                       selected = unique(c(best1$Model, best_fond$Model, "MSTL", "Seasonal Naive")),
                       options = list(plugins = list("remove_button"), placeholder = "Pilih model...")),
        div(class = "d-flex flex-wrap gap-1",
            actionButton("btn_top3", "3 terbaik", class = "btn-sm btn-outline-primary"),
            actionButton("btn_fondasi", "Model fondasi", class = "btn-sm btn-outline-primary"),
            actionButton("btn_semua", "Semua", class = "btn-sm btn-outline-primary"),
            actionButton("btn_hapus", "Kosongkan", class = "btn-sm btn-outline-secondary")),
        hr(),
        radioButtons("rm_pi", "Selang prediksi", inline = TRUE,
                     choices = c("Tidak" = "0", "80%" = "80", "95%" = "95"), selected = "0"),
        checkboxInput("rm_histori", "Tampilkan histori 1 minggu sebelumnya", TRUE),
        info("Geser/zoom grafik dengan mouse. Klik dua kali untuk kembali. Klik nama di legenda untuk menyembunyikan model.")
      ),
      card(full_screen = TRUE, card_header("Ramalan vs aktual"),
           plotlyOutput("plot_ramalan", height = "470px")),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, card_header("Galat ramalan (aktual - ramalan)"),
             plotlyOutput("plot_galat", height = "330px"),
             info("Nilai positif = model meramal terlalu rendah; negatif = terlalu tinggi.")),
        card(full_screen = TRUE, card_header("Aktual vs ramalan"),
             plotlyOutput("plot_scatter", height = "360px"))
      ),
      card(card_header("Akurasi model terpilih pada minggu ini"), card_body(fillable = FALSE, DTOutput("tbl_rm_akurasi")))
    )
  ),

  # ---------------------------------------------------------------- AKURASI
  nav_panel(
    title = "Akurasi & Peringkat", icon = icon("ranking-star"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 300, title = "Filter",
        selectInput("ak_metrik", "Metrik", choices = c("MASE", "MAPE", "MAE", "RMSE"), selected = "MASE"),
        selectInput("ak_origin", "Minggu uji",
                    choices = c("Rata-rata semua minggu" = "cv", label_origin), selected = "1"),
        radioButtons("ak_eks", "Variabel eksogen",
                     choices = c("Semua" = "semua", "Tanpa eksogen" = "tidak", "Dengan eksogen" = "ya")),
        checkboxGroupInput("ak_kel", "Kelompok", choices = URUT_KEL, selected = URUT_KEL),
        info("Model dengan dan tanpa eksogen sebaiknya dibandingkan terpisah: model eksogen memakai suhu aktual (prakiraan sempurna).")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header(textOutput("judul_ak_bar")), plotlyOutput("plot_ak_bar", height = "480px")),
        card(full_screen = TRUE, card_header("Peringkat di setiap metrik (1 = terbaik)"),
             plotlyOutput("plot_ak_multi", height = "480px"))
      ),
      card(card_header("Tabel akurasi lengkap"), card_body(fillable = FALSE, DTOutput("tbl_akurasi"))),
      card(full_screen = TRUE, card_header("Peta panas galat absolut menurut jam (MWh)"),
           plotlyOutput("plot_galat_jam", height = "520px"),
           info("Warna lebih gelap = kesalahan lebih besar. Perhatikan jam-jam puncak siang dan sore."))
    )
  ),

  # ---------------------------------------------------------------- SELANG PREDIKSI
  nav_panel(
    title = "Selang Prediksi", icon = icon("arrows-left-right-to-line"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 300, title = "Pengaturan",
        selectInput("pi_origin", "Minggu uji", choices = label_origin, selected = 1),
        selectInput("pi_model", "Model untuk grafik selang",
                    choices = NULL),
        info("Cakupan ideal mendekati nilai nominal (80% / 95%). Jauh di bawah = selang terlalu sempit (terlalu percaya diri); 100% bisa berarti terlalu lebar.")
      ),
      card(full_screen = TRUE, card_header("Ramalan dengan selang prediksi"), plotlyOutput("plot_pi", height = "430px")),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("Cakupan selang vs nilai nominal"), plotlyOutput("plot_cakupan", height = "430px")),
        card(full_screen = TRUE, card_header("Winkler score (makin kecil makin baik)"), plotlyOutput("plot_winkler", height = "430px"))
      ),
      card(card_header("Tabel kualitas selang prediksi"), card_body(fillable = FALSE, DTOutput("tbl_pi")))
    )
  ),

  # ---------------------------------------------------------------- UJI DM
  nav_panel(
    title = "Uji Signifikansi", icon = icon("scale-balanced"),
    info("Uji Diebold-Mariano memeriksa apakah beda akurasi dua model nyata secara statistik (kerugian kuadrat, koreksi Harvey-Leybourne-Newbold). Statistik negatif = model pertama lebih akurat."),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Uji dua model (interaktif)"),
           selectInput("dm_a", "Model A", choices = pilihan_model, selected = best1$Model),
           selectInput("dm_b", "Model B", choices = pilihan_model, selected = "MSTL"),
           selectInput("dm_origin", "Minggu uji", choices = label_origin, selected = 1),
           uiOutput("ui_dm_hasil")),
      card(full_screen = TRUE, card_header(textOutput("judul_dm")),
           selectInput("dm_ref_origin", NULL, choices = label_origin, selected = 1, width = "320px"),
           plotlyOutput("plot_dm", height = "420px"))
    ),
    card(card_header("Model terbaik vs semua model (dengan koreksi Holm)"), card_body(fillable = FALSE, DTOutput("tbl_dm")))
  ),

  # ---------------------------------------------------------------- CROSS-VALIDATION
  nav_panel(
    title = "Cross-Validation", icon = icon("repeat"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 280, title = "Pengaturan",
        selectInput("cv_metrik", "Metrik", choices = c("MASE", "MAPE", "MAE", "RMSE")),
        radioButtons("cv_eks", "Variabel eksogen",
                     choices = c("Semua" = "semua", "Tanpa eksogen" = "tidak", "Dengan eksogen" = "ya")),
        info(paste0("Setiap model diuji pada ", N_ORIG, " minggu berbeda. Model yang baik: rata-rata kecil DAN sebaran sempit (stabil)."))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("Sebaran metrik di semua minggu uji"), plotlyOutput("plot_cv_box", height = "480px")),
        card(full_screen = TRUE, card_header("Akurasi vs kestabilan"), plotlyOutput("plot_cv_stabil", height = "480px"),
             info("Kiri-bawah = akurat dan stabil (ideal). Arahkan kursor ke titik untuk melihat nama model."))
      ),
      card(full_screen = TRUE, card_header("Perubahan metrik antarminggu"), plotlyOutput("plot_cv_garis", height = "420px")),
      card(card_header("Ringkasan cross-validation"), card_body(fillable = FALSE, DTOutput("tbl_cv")))
    )
  ),

  # ---------------------------------------------------------------- EFISIENSI
  nav_panel(
    title = "Efisiensi", icon = icon("stopwatch"),
    info("Waktu komputasi = waktu latih + ramal pada minggu utama. Model fondasi tidak perlu dilatih, jadi umumnya cepat; TimeGPT diukur termasuk waktu panggilan API."),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE, card_header("Akurasi vs waktu komputasi"), plotlyOutput("plot_efisiensi", height = "470px"),
           info("Sumbu waktu berskala log. Kiri-bawah = cepat dan akurat. Arahkan kursor ke titik untuk melihat nama model.")),
      card(full_screen = TRUE, card_header("Waktu komputasi per model"), plotlyOutput("plot_waktu", height = "470px"))
    )
  ),

  # ---------------------------------------------------------------- METODOLOGI
  nav_panel(
    title = "Metodologi", icon = icon("book-open"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Model yang dibandingkan"), card_body(fillable = FALSE, DTOutput("tbl_model"))),
      card(card_header("Metrik evaluasi"),
           tags$table(class = "table table-sm",
             tags$thead(tags$tr(tags$th("Metrik"), tags$th("Arti"), tags$th("Baik bila"))),
             tags$tbody(
               tags$tr(tags$td(strong("MAE")), tags$td("Rata-rata besar kesalahan (MWh)"), tags$td("kecil")),
               tags$tr(tags$td(strong("RMSE")), tags$td("Seperti MAE, kesalahan besar dihukum lebih berat"), tags$td("kecil")),
               tags$tr(tags$td(strong("MAPE")), tags$td("Kesalahan dalam persen"), tags$td("kecil (< 10% sangat baik)")),
               tags$tr(tags$td(strong("MASE")), tags$td("MAE dibagi galat Seasonal Naive di data latih (metrik utama)"), tags$td("kecil")),
               tags$tr(tags$td(strong("Cakupan")), tags$td("% aktual di dalam selang 80% / 95%"), tags$td("dekat nominal")),
               tags$tr(tags$td(strong("Winkler")), tags$td("Lebar selang + penalti bila aktual di luar selang"), tags$td("kecil")),
               tags$tr(tags$td(strong("Diebold-Mariano")), tags$td("Uji beda akurasi dua model"), tags$td("p < 0,05 = berbeda nyata"))
             )))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Skema pengujian"), tableOutput("tbl_periode"),
           tags$ul(
             tags$li("Horizon 168 jam; data konteks 1 tahun sebelum periode uji."),
             tags$li("Eksogen: suhu Tocumen, Santiago, David; hari libur; masa sekolah."),
             tags$li("Minggu 1 = evaluasi utama; minggu lain = rolling-origin cross-validation."))),
      card(card_header("Catatan penting"),
           tags$ul(
             tags$li(strong("Eksogen optimistis: "), "suhu periode uji memakai nilai aktual (setara prakiraan cuaca sempurna)."),
             tags$li(strong("Kebocoran data: "), "dataset Panama tersedia publik, mungkin pernah dipakai melatih sebagian model fondasi."),
             tags$li(strong("TimesFM: "), "hanya menghasilkan kuantil 10%-90%, sehingga hanya selang 80% yang tersedia."),
             tags$li(strong("Tuning: "), "hiperparameter LightGBM dan N-HiTS tidak dituning khusus.")))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Tototro · Akmal · Jihan"))
)

# =============================================================================
#  SERVER
# =============================================================================
server <- function(input, output, session) {

  filter_eks <- function(d, pilih) {
    if (pilih == "tidak") d %>% filter(!Eksogen) else if (pilih == "ya") d %>% filter(Eksogen) else d
  }

  # ---------------------------------------------------------- RINGKASAN
  output$plot_ringkas_rank <- renderPlotly({
    d <- if (input$rk_dasar == "utama") akurasi1 %>% select(Model, Kelompok, Eksogen, MASE, MAPE)
         else ringkas_cv %>% transmute(Model, Kelompok, Eksogen, MASE = MASE_rata, MAPE = MAPE_rata)
    d <- filter_eks(d, input$rk_eks) %>% arrange(desc(MASE)) %>%
           mutate(Model = factor(Model, levels = Model), Kelompok = factor(Kelompok, levels = URUT_KEL))
    ref <- if (input$rk_dasar == "utama") mase_sn else ringkas_cv$MASE_rata[ringkas_cv$Model == "Seasonal Naive"]
    plot_ly(d, x = ~MASE, y = ~Model, color = ~Kelompok, colors = WARNA_KEL, type = "bar", orientation = "h",
            text = ~fk(MASE), textposition = "outside",
            hovertemplate = ~paste0("<b>", Model, "</b><br>MASE: ", fk(MASE, 3), "<br>MAPE: ", pct(MAPE), "<extra></extra>")) %>%
      layout(shapes = list(list(type = "line", x0 = ref, x1 = ref, y0 = -0.5, y1 = nrow(d) - 0.5,
                                line = list(dash = "dash", color = "#333"))),
             xaxis = list(title = "MASE (makin kecil makin baik)", range = c(0, max(d$MASE) * 1.15)), yaxis = list(title = "")) %>%
      gaya_plot()
  })

  output$ui_kesimpulan <- renderUI({
    if (is.null(kesimpulan) || nrow(kesimpulan) == 0) return(p("Tidak ada kesimpulan."))
    tags$ol(class = "kesimpulan", lapply(kesimpulan$Kesimpulan, tags$li))
  })

  output$plot_ringkas_kelompok <- renderPlotly({
    d <- akurasi1 %>% group_by(Kelompok) %>% slice_min(MASE, n = 1, with_ties = FALSE) %>% ungroup() %>%
           mutate(Kelompok = factor(Kelompok, levels = URUT_KEL)) %>% arrange(MASE)
    d <- d %>% mutate(label = paste0("<b>", Kelompok, "</b><br>", Model))
    plot_ly(d, y = ~label, x = ~MASE, type = "bar", orientation = "h",
            marker = list(color = unname(WARNA_KEL[as.character(d$Kelompok)])),
            text = ~fk(MASE), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = ~paste0("<b>", Model, "</b><br>MASE ", fk(MASE, 3), "<extra></extra>")) %>%
      layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(d$label), tickfont = list(size = 10)),
             xaxis = list(title = "MASE", range = c(0, max(d$MASE) * 1.25)), showlegend = FALSE) %>%
      gaya_plot()
  })

  # ---------------------------------------------------------- EKSPLORASI
  output$plot_tren <- renderPlotly({
    plot_ly(bulanan, x = ~Bulan, y = ~Beban, type = "scatter", mode = "lines+markers",
            line = list(color = "#1F4E79", width = 2.5), marker = list(size = 5),
            hovertemplate = "%{x|%b %Y}<br>%{y:.0f} MWh<extra></extra>") %>%
      layout(shapes = list(list(type = "rect", x0 = as.Date("2020-03-01"), x1 = max(bulanan$Bulan) + 15,
                                y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(214,39,40,.12)", line = list(width = 0))),
             annotations = list(list(x = as.Date("2020-03-01"), y = 1, yref = "paper", text = "Pandemi COVID-19",
                                     showarrow = FALSE, xanchor = "right", font = list(color = "#D62728"))),
             xaxis = list(title = "", rangeslider = list(visible = TRUE)), yaxis = list(title = "Beban (MWh)")) %>%
      gaya_plot()
  })

  output$plot_korelasi <- renderPlotly({
    d <- korelasi %>% arrange(Korelasi) %>% mutate(Variabel = factor(Variabel, levels = Variabel))
    plot_ly(d, x = ~Korelasi, y = ~Variabel, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$Korelasi > 0, "#1F4E79", "#D62728")),
            text = ~fk(Korelasi), textposition = "outside",
            hovertemplate = "%{y}: r = %{x:.3f}<extra></extra>") %>%
      layout(xaxis = list(title = "Korelasi (r)", range = c(min(0, min(d$Korelasi)) - .15, 1)), yaxis = list(title = "")) %>%
      gaya_plot()
  })

  output$plot_heatmap <- renderPlotly({
    m <- as.matrix(profil[, -1])
    plot_ly(x = names(profil)[-1], y = profil$Jam, z = m, type = "heatmap", colorscale = "YlOrRd", reversescale = TRUE,
            hovertemplate = "%{x}, jam %{y}:00<br>%{z:.0f} MWh<extra></extra>",
            colorbar = list(title = "MWh")) %>%
      layout(yaxis = list(title = "Jam", autorange = "reversed", dtick = 2), xaxis = list(title = "")) %>%
      gaya_plot()
  })

  output$plot_profil <- renderPlotly({
    req(input$pilih_hari)
    d <- profil %>% pivot_longer(-Jam, names_to = "Hari", values_to = "Beban") %>%
           filter(Hari %in% input$pilih_hari) %>% mutate(Hari = factor(Hari, levels = names(profil)[-1]))
    plot_ly(d, x = ~Jam, y = ~Beban, color = ~Hari, type = "scatter", mode = "lines",
            colors = c("#1F77B4", "#2CA02C", "#17BECF", "#9467BD", "#8C564B", "#FF7F0E", "#D62728"),
            hovertemplate = ~paste0(Hari, ", jam ", Jam, ":00<br>", fk(Beban, 0), " MWh<extra></extra>")) %>%
      layout(xaxis = list(title = "Jam", dtick = 3), yaxis = list(title = "Beban (MWh)")) %>%
      gaya_plot()
  })

  output$tbl_stat <- renderTable({
    s <- stat_desk
    data.frame(Statistik = c("Jumlah", "Rata-rata", "Simpangan baku", "Minimum", "Q1", "Median", "Q3", "Maksimum"),
               Nilai = c(fk(as.numeric(s[1, 2]), 0), fk(as.numeric(unlist(s[1, -(1:2)])), 1)))
  }, striped = TRUE, width = "100%")
  output$tbl_ringkas_data <- renderTable(ringkasan_data %>% mutate(Nilai = as.character(Nilai)), striped = TRUE, width = "100%")
  output$tbl_ekstrem <- renderTable({
    e <- ekstrem; names(e) <- c("Waktu", "Beban (MWh)")
    e %>% mutate(Waktu = format(ke_waktu(Waktu), "%d %b %Y %H:%M"), `Beban (MWh)` = fk(`Beban (MWh)`, 1))
  }, striped = TRUE, width = "100%")
  output$tbl_deskripsi <- renderDT(tabel_dt(deskripsi, "deskripsi_variabel", page = 20))

  # ---------------------------------------------------------- RAMALAN
  observeEvent(input$btn_top3, {
    o <- as.integer(input$rm_origin)
    top <- akurasi_cv %>% filter(origin == o) %>% arrange(MASE) %>% slice(1:3) %>% pull(Model)
    updateSelectizeInput(session, "rm_model", selected = top)
  })
  observeEvent(input$btn_fondasi, updateSelectizeInput(session, "rm_model", selected = meta$Model[meta$Fondasi]))
  observeEvent(input$btn_semua,   updateSelectizeInput(session, "rm_model", selected = meta$Model))
  observeEvent(input$btn_hapus,   updateSelectizeInput(session, "rm_model", selected = character(0)))

  data_rm <- reactive({
    o <- as.integer(input$rm_origin)
    ramalan %>% filter(origin == o, model %in% input$rm_model)
  })
  aktual_rm <- reactive({
    o <- as.integer(input$rm_origin)
    ramalan %>% filter(origin == o) %>% distinct(ds, aktual) %>% arrange(ds)
  })

  output$plot_ramalan <- renderPlotly({
    o  <- as.integer(input$rm_origin)
    ak <- aktual_rm()
    p  <- plot_ly()
    if (isTRUE(input$rm_histori) && (o + 1) %in% ORIGINS) {
      hs <- ramalan %>% filter(origin == o + 1) %>% distinct(ds, aktual)
      p <- p %>% add_lines(data = hs, x = ~ds, y = ~aktual, name = "Histori", line = list(color = "#9E9E9E", width = 1.5),
                           hovertemplate = "%{x|%a %d %b %H:%M}<br>Histori: %{y:.0f} MWh<extra></extra>")
    }
    d <- data_rm()
    if (input$rm_pi != "0" && nrow(d)) {
      lo <- paste0("lo", input$rm_pi); hi <- paste0("hi", input$rm_pi)
      for (m in unique(d$model)) {
        dm <- d %>% filter(model == m)
        if (all(is.na(dm[[lo]]))) next
        p <- p %>% add_ribbons(data = dm, x = ~ds, ymin = dm[[lo]], ymax = dm[[hi]], name = paste0(m, " (selang ", input$rm_pi, "%)"),
                               fillcolor = adjustcolor(WARNA_MODEL[m], .18), line = list(width = 0),
                               legendgroup = m, showlegend = FALSE, hoverinfo = "skip")
      }
    }
    p <- p %>% add_lines(data = ak, x = ~ds, y = ~aktual, name = "Aktual", line = list(color = WARNA_AKTUAL, width = 3),
                         hovertemplate = "%{x|%a %d %b %H:%M}<br><b>Aktual: %{y:.0f} MWh</b><extra></extra>")
    for (m in unique(d$model)) {
      dm <- d %>% filter(model == m)
      p <- p %>% add_lines(data = dm, x = ~ds, y = ~mean, name = m, legendgroup = m,
                           line = list(color = WARNA_MODEL[m], width = 1.8),
                           hovertemplate = paste0("%{x|%a %d %b %H:%M}<br>", m, ": %{y:.0f} MWh<extra></extra>"))
    }
    p %>% layout(hovermode = "x unified", xaxis = list(title = "", rangeslider = list(visible = TRUE, thickness = .07)),
                 yaxis = list(title = "Beban (MWh)")) %>% gaya_plot()
  })

  output$plot_galat <- renderPlotly({
    d <- data_rm(); validate(need(nrow(d) > 0, "Pilih minimal satu model."))
    p <- plot_ly()
    for (m in unique(d$model)) {
      dm <- d %>% filter(model == m)
      p <- p %>% add_lines(data = dm, x = ~ds, y = ~(aktual - mean), name = m, line = list(color = WARNA_MODEL[m], width = 1.5),
                           hovertemplate = paste0("%{x|%a %d %b %H:%M}<br>", m, ": %{y:.0f} MWh<extra></extra>"))
    }
    p %>% layout(hovermode = "x unified", yaxis = list(title = "Galat (MWh)", zeroline = TRUE, zerolinewidth = 2),
                 xaxis = list(title = "")) %>% gaya_plot()
  })

  output$plot_scatter <- renderPlotly({
    d <- data_rm(); validate(need(nrow(d) > 0, "Pilih minimal satu model."))
    rng <- range(c(d$aktual, d$mean), na.rm = TRUE)
    p <- plot_ly()
    for (m in unique(d$model)) {
      dm <- d %>% filter(model == m)
      p <- p %>% add_markers(data = dm, x = ~aktual, y = ~mean, name = m, marker = list(color = WARNA_MODEL[m], size = 6, opacity = .6),
                             hovertemplate = paste0(m, "<br>Aktual %{x:.0f}<br>Ramalan %{y:.0f}<extra></extra>"))
    }
    p %>% add_lines(x = rng, y = rng, name = "Ramalan sempurna", line = list(color = "#333", dash = "dash"), inherit = FALSE) %>%
      layout(xaxis = list(title = "Aktual (MWh)"), yaxis = list(title = "Ramalan (MWh)")) %>% gaya_plot()
  })

  output$tbl_rm_akurasi <- renderDT({
    o <- as.integer(input$rm_origin)
    d <- akurasi_cv %>% filter(origin == o, Model %in% input$rm_model) %>% arrange(MASE) %>%
           transmute(Model, Kelompok, Eksogen = ifelse(Eksogen, "Ya", "Tidak"), MAE, RMSE, `MAPE (%)` = MAPE, MASE,
                     `Cakupan 80%` = Cakupan80, `Cakupan 95%` = Cakupan95)
    tabel_dt(d, paste0("akurasi_minggu_", o), page = 15) %>%
      formatRound(c("MAE", "RMSE", "MAPE (%)", "MASE", "Cakupan 80%", "Cakupan 95%"), 2) %>%
      formatStyle("MASE", background = styleColorBar(c(0, max(d$MASE, 1)), "#C9DDF2"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # ---------------------------------------------------------- AKURASI
  data_ak <- reactive({
    d <- if (input$ak_origin == "cv") {
      akurasi_cv %>% group_by(Model, Kelompok, Eksogen, Fondasi) %>%
        summarise(across(c(MAE, RMSE, MAPE, MASE, Cakupan80, Cakupan95, Winkler), mean), .groups = "drop")
    } else akurasi_cv %>% filter(origin == as.integer(input$ak_origin))
    filter_eks(d, input$ak_eks) %>% filter(Kelompok %in% input$ak_kel)
  })

  output$judul_ak_bar <- renderText(paste0(input$ak_metrik, " per model (makin kecil makin baik)"))

  output$plot_ak_bar <- renderPlotly({
    d <- data_ak(); validate(need(nrow(d) > 0, "Tidak ada model sesuai filter."))
    met <- input$ak_metrik
    d <- d %>% mutate(nilai = .data[[met]]) %>% arrange(desc(nilai)) %>%
           mutate(Model = factor(Model, levels = Model), Kelompok = factor(Kelompok, levels = URUT_KEL))
    plot_ly(d, x = ~nilai, y = ~Model, color = ~Kelompok, colors = WARNA_KEL, type = "bar", orientation = "h",
            text = ~fk(nilai), textposition = "outside",
            hovertemplate = ~paste0("<b>", Model, "</b><br>", met, ": ", fk(nilai, 3), "<extra></extra>")) %>%
      layout(xaxis = list(title = met, range = c(0, max(d$nilai) * 1.15)), yaxis = list(title = "")) %>% gaya_plot()
  })

  output$plot_ak_multi <- renderPlotly({
    d <- data_ak(); validate(need(nrow(d) > 1, "Butuh minimal dua model."))
    dd <- d %>% mutate(across(c(MAE, RMSE, MAPE, MASE), ~ rank(.x, ties.method = "min"))) %>%
            pivot_longer(c(MAE, RMSE, MAPE, MASE), names_to = "Metrik", values_to = "Skor") %>%
            mutate(Model = factor(Model, levels = d$Model[order(d$MASE, decreasing = TRUE)]))
    plot_ly(dd, x = ~Metrik, y = ~Model, z = ~Skor, type = "heatmap",
            colorscale = list(c(0, "#2BA84A"), c(.5, "#F7E08A"), c(1, "#D62728")),
            text = ~Skor, texttemplate = "%{text}",
            hovertemplate = "%{y}<br>%{x}: peringkat %{z}<extra></extra>", colorbar = list(title = "Peringkat")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "")) %>% gaya_plot()
  })

  output$tbl_akurasi <- renderDT({
    d <- data_ak() %>% arrange(MASE) %>% mutate(Peringkat = row_number()) %>%
           transmute(Peringkat, Model, Kelompok, Eksogen = ifelse(Eksogen, "Ya", "Tidak"),
                     `Model fondasi` = ifelse(Fondasi, "Ya", "Tidak"), MAE, RMSE, `MAPE (%)` = MAPE, MASE,
                     `Cakupan 80%` = Cakupan80, `Cakupan 95%` = Cakupan95, Winkler)
    tabel_dt(d, "akurasi_model", page = 20) %>%
      formatRound(c("MAE", "RMSE", "MAPE (%)", "MASE", "Cakupan 80%", "Cakupan 95%", "Winkler"), 2) %>%
      formatStyle("MASE", background = styleColorBar(c(0, max(d$MASE, 1)), "#C9DDF2"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
      formatStyle("Peringkat", fontWeight = "bold",
                  backgroundColor = styleEqual(1:3, c("#FFD700", "#D9D9D9", "#E3B47C")))
  })

  output$plot_galat_jam <- renderPlotly({
    o <- if (input$ak_origin == "cv") ORIGINS else as.integer(input$ak_origin)
    mdl <- data_ak()$Model
    validate(need(length(mdl) > 0, "Tidak ada model sesuai filter."))
    d <- ramalan %>% filter(origin %in% o, model %in% mdl) %>%
           mutate(jam = as.integer(format(ds, "%H")), galat = abs(aktual - mean)) %>%
           group_by(model, jam) %>% summarise(MAE = mean(galat), .groups = "drop")
    urutan <- d %>% group_by(model) %>% summarise(m = mean(MAE)) %>% arrange(desc(m)) %>% pull(model)
    d <- d %>% mutate(model = factor(model, levels = urutan))
    plot_ly(d, x = ~jam, y = ~model, z = ~MAE, type = "heatmap", colorscale = "Reds",
            hovertemplate = "%{y}<br>Jam %{x}:00<br>MAE %{z:.1f} MWh<extra></extra>", colorbar = list(title = "MWh")) %>%
      layout(xaxis = list(title = "Jam", dtick = 2), yaxis = list(title = "")) %>% gaya_plot()
  })

  # ---------------------------------------------------------- SELANG PREDIKSI
  model_pi <- meta$Model[sapply(meta$Model, function(m) any(!is.na(ramalan$lo80[ramalan$model == m])))]
  updateSelectInput(session, "pi_model", choices = model_pi,
                    selected = if (best1$Model %in% model_pi) best1$Model else model_pi[1])

  data_pi <- reactive({
    o <- as.integer(input$pi_origin)
    ramalan %>% filter(origin == o, model %in% model_pi) %>% group_by(model) %>%
      summarise(c80 = mean(aktual >= lo80 & aktual <= hi80) * 100,
                c95 = if (all(is.na(lo95))) NA_real_ else mean(aktual >= lo95 & aktual <= hi95) * 100,
                w80 = mean((hi80 - lo80) + (2 / .2) * pmax(lo80 - aktual, 0) + (2 / .2) * pmax(aktual - hi80, 0)),
                w95 = if (all(is.na(lo95))) NA_real_ else
                        mean((hi95 - lo95) + (2 / .05) * pmax(lo95 - aktual, 0) + (2 / .05) * pmax(aktual - hi95, 0)),
                lebar80 = mean(hi80 - lo80), .groups = "drop")
  })

  output$plot_pi <- renderPlotly({
    req(input$pi_model)
    o <- as.integer(input$pi_origin); m <- input$pi_model
    d <- ramalan %>% filter(origin == o, model == m)
    p <- plot_ly(d, x = ~ds)
    if (any(!is.na(d$lo95)))
      p <- p %>% add_ribbons(ymin = ~lo95, ymax = ~hi95, name = "Selang 95%", fillcolor = adjustcolor(WARNA_MODEL[m], .15), line = list(width = 0))
    p %>% add_ribbons(ymin = ~lo80, ymax = ~hi80, name = "Selang 80%", fillcolor = adjustcolor(WARNA_MODEL[m], .32), line = list(width = 0)) %>%
      add_lines(y = ~mean, name = m, line = list(color = WARNA_MODEL[m], width = 2)) %>%
      add_markers(y = ~aktual, name = "Aktual",
                  marker = list(size = 5, color = ifelse(d$aktual >= d$lo80 & d$aktual <= d$hi80, "#111", "#D62728"))) %>%
      layout(hovermode = "x unified", xaxis = list(title = ""), yaxis = list(title = "Beban (MWh)"),
             annotations = list(list(x = 0, y = 1.08, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
                                     text = "Titik merah = aktual di luar selang 80%", font = list(color = "#D62728", size = 11)))) %>%
      gaya_plot()
  })

  output$plot_cakupan <- renderPlotly({
    d <- data_pi() %>% arrange(c80) %>% mutate(model = factor(model, levels = model))
    plot_ly(d, y = ~model) %>%
      add_markers(x = ~c80, name = "Cakupan 80%", marker = list(size = 11, color = "#3B82C4"),
                  hovertemplate = "%{y}<br>Cakupan 80%: %{x:.1f}%<extra></extra>") %>%
      add_markers(x = ~c95, name = "Cakupan 95%", marker = list(size = 11, color = "#C2418E", symbol = "diamond"),
                  hovertemplate = "%{y}<br>Cakupan 95%: %{x:.1f}%<extra></extra>") %>%
      layout(shapes = list(list(type = "line", x0 = 80, x1 = 80, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#3B82C4", dash = "dash")),
                           list(type = "line", x0 = 95, x1 = 95, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#C2418E", dash = "dash"))),
             xaxis = list(title = "Cakupan (%)", range = c(0, 105)), yaxis = list(title = "")) %>% gaya_plot()
  })

  output$plot_winkler <- renderPlotly({
    d <- data_pi() %>% mutate(skor = ifelse(is.na(w95), w80, w95), selang = ifelse(is.na(w95), "80%", "95%")) %>%
           arrange(desc(skor)) %>% mutate(model = factor(model, levels = model))
    plot_ly(d, x = ~skor, y = ~model, type = "bar", orientation = "h", color = ~selang,
            colors = c("80%" = "#9DB9D9", "95%" = "#1F4E79"), text = ~fk(skor, 0), textposition = "outside",
            hovertemplate = ~paste0(model, "<br>Winkler (selang ", selang, "): ", fk(skor, 1), "<extra></extra>")) %>%
      layout(xaxis = list(title = "Winkler score", range = c(0, max(d$skor) * 1.15)), yaxis = list(title = ""),
             legend = list(title = list(text = "Selang"))) %>% gaya_plot()
  })

  output$tbl_pi <- renderDT({
    d <- data_pi() %>% arrange(w95, w80) %>%
           transmute(Model = model, `Cakupan 80% (%)` = c80, `Cakupan 95% (%)` = c95,
                     `Lebar rata-rata selang 80% (MWh)` = lebar80, `Winkler 80%` = w80, `Winkler 95%` = w95)
    tabel_dt(d, "selang_prediksi") %>% formatRound(2:6, 2) %>%
      formatStyle("Cakupan 80% (%)", color = styleInterval(c(70, 90), c("#D62728", "#2BA84A", "#E69F00"))) %>%
      formatStyle("Cakupan 95% (%)", color = styleInterval(c(88, 99), c("#D62728", "#2BA84A", "#E69F00")))
  })

  # ---------------------------------------------------------- UJI DM
  output$ui_dm_hasil <- renderUI({
    req(input$dm_a, input$dm_b)
    if (input$dm_a == input$dm_b) return(div(class = "alert alert-warning", "Pilih dua model yang berbeda."))
    o <- as.integer(input$dm_origin)
    a <- ramalan %>% filter(origin == o, model == input$dm_a) %>% arrange(ds)
    b <- ramalan %>% filter(origin == o, model == input$dm_b) %>% arrange(ds)
    r <- dm_test(a$aktual - a$mean, b$aktual - b$mean)
    lebih <- if (r["stat"] < 0) input$dm_a else input$dm_b
    nyata <- r["p"] < 0.05
    tagList(
      div(class = paste("alert", if (nyata) "alert-success" else "alert-secondary"),
          h5(if (nyata) "Berbeda nyata" else "Tidak berbeda nyata"),
          p(HTML(paste0("Statistik DM = <b>", fk(r["stat"], 3), "</b><br>p-value = <b>",
                        format.pval(r["p"], digits = 3, eps = 1e-4), "</b>"))),
          p(if (nyata) HTML(paste0("<b>", lebih, "</b> secara statistik lebih akurat."))
            else "Perbedaan akurasi kedua model belum cukup bukti (bisa kebetulan).")),
      p(class = "small text-muted", paste0("RMSE ", input$dm_a, ": ", fk(sqrt(mean((a$aktual - a$mean)^2)), 1),
                                           " | ", input$dm_b, ": ", fk(sqrt(mean((b$aktual - b$mean)^2)), 1)))
    )
  })

  data_dm_ref <- reactive({
    o <- as.integer(input$dm_ref_origin)
    ak <- akurasi_cv %>% filter(origin == o) %>% arrange(MASE)
    best <- ak$Model[1]
    eb <- ramalan %>% filter(origin == o, model == best) %>% arrange(ds)
    hasil <- lapply(setdiff(ak$Model, best), function(m) {
      em <- ramalan %>% filter(origin == o, model == m) %>% arrange(ds)
      r <- dm_test(eb$aktual - eb$mean, em$aktual - em$mean)
      data.frame(Pembanding = m, Statistik = unname(r["stat"]), p = unname(r["p"]))
    }) %>% bind_rows()
    hasil$p_holm <- p.adjust(hasil$p, "holm")
    hasil$Kesimpulan <- ifelse(hasil$p_holm < 0.05, "Berbeda nyata", "Tidak berbeda nyata")
    list(best = best, tabel = hasil %>% arrange(Statistik))
  })

  output$judul_dm <- renderText(paste0("Statistik DM: ", data_dm_ref()$best, " vs model lain"))

  output$plot_dm <- renderPlotly({
    d <- data_dm_ref()$tabel %>% mutate(Pembanding = factor(Pembanding, levels = rev(Pembanding)))
    plot_ly(d, x = ~Statistik, y = ~Pembanding, type = "bar", orientation = "h", color = ~Kesimpulan,
            colors = c("Berbeda nyata" = "#2BA84A", "Tidak berbeda nyata" = "#BDBDBD"),
            hovertemplate = ~paste0(Pembanding, "<br>DM = ", fk(Statistik, 3), "<br>p (Holm) = ",
                                    format.pval(p_holm, digits = 3, eps = 1e-4), "<extra></extra>")) %>%
      layout(xaxis = list(title = "Statistik DM (negatif = model terbaik lebih akurat)"), yaxis = list(title = "")) %>%
      gaya_plot()
  })

  output$tbl_dm <- renderDT({
    r <- data_dm_ref()
    d <- r$tabel %>% transmute(`Model terbaik` = r$best, Pembanding, `Statistik DM` = Statistik,
                               `p-value` = format.pval(p, digits = 3, eps = 1e-4),
                               `p-value (Holm)` = format.pval(p_holm, digits = 3, eps = 1e-4), Kesimpulan)
    tabel_dt(d, "uji_diebold_mariano") %>% formatRound("Statistik DM", 3) %>%
      formatStyle("Kesimpulan", color = styleEqual(c("Berbeda nyata", "Tidak berbeda nyata"), c("#2BA84A", "#757575")),
                  fontWeight = "bold")
  })

  # ---------------------------------------------------------- CROSS-VALIDATION
  data_cv <- reactive(filter_eks(akurasi_cv, input$cv_eks))

  output$plot_cv_box <- renderPlotly({
    met <- input$cv_metrik
    d <- data_cv() %>% mutate(nilai = .data[[met]])
    urutan <- d %>% group_by(Model) %>% summarise(m = mean(nilai)) %>% arrange(desc(m)) %>% pull(Model)
    d <- d %>% mutate(Model = factor(Model, levels = urutan), Kelompok = factor(Kelompok, levels = URUT_KEL))
    plot_ly(d, x = ~nilai, y = ~Model, color = ~Kelompok, colors = WARNA_KEL, type = "box", boxpoints = "all",
            jitter = .3, pointpos = 0, hoverinfo = "x+y") %>%
      layout(xaxis = list(title = met), yaxis = list(title = "")) %>% gaya_plot()
  })

  output$plot_cv_stabil <- renderPlotly({
    met <- input$cv_metrik
    d <- data_cv() %>% group_by(Model, Kelompok) %>%
           summarise(rata = mean(.data[[met]]), sd = sd(.data[[met]]), .groups = "drop") %>%
           mutate(Kelompok = factor(Kelompok, levels = URUT_KEL))
    plot_ly(d, x = ~rata, y = ~sd, color = ~Kelompok, colors = WARNA_KEL, type = "scatter", mode = "markers",
            marker = list(size = 13, line = list(color = "white", width = 1)),
            hovertemplate = ~paste0("<b>", Model, "</b><br>Rata-rata ", met, ": ", fk(rata, 3),
                                    "<br>Simpangan baku: ", fk(sd, 3), "<extra></extra>")) %>%
      layout(xaxis = list(title = paste(met, "rata-rata")), yaxis = list(title = "Simpangan baku (ketidakstabilan)")) %>%
      gaya_plot()
  })

  output$plot_cv_garis <- renderPlotly({
    met <- input$cv_metrik
    d <- data_cv() %>% mutate(nilai = .data[[met]], Minggu = factor(paste("Minggu", origin)))
    p <- plot_ly()
    for (m in unique(d$Model)) {
      dm <- d %>% filter(Model == m) %>% arrange(origin)
      p <- p %>% add_trace(data = dm, x = ~Minggu, y = ~nilai, type = "scatter", mode = "lines+markers", name = m,
                           line = list(color = WARNA_MODEL[m]), marker = list(color = WARNA_MODEL[m]),
                           hovertemplate = paste0(m, "<br>%{x}: %{y:.3f}<extra></extra>"))
    }
    p %>% layout(xaxis = list(title = ""), yaxis = list(title = met)) %>% gaya_plot()
  })

  output$tbl_cv <- renderDT({
    met <- input$cv_metrik
    d <- data_cv() %>% group_by(Model, Kelompok, Eksogen) %>%
           summarise(`Jumlah minggu` = n(), Rata = mean(.data[[met]]), SD = sd(.data[[met]]),
                     Min = min(.data[[met]]), Maks = max(.data[[met]]), .groups = "drop") %>%
           arrange(Rata) %>% mutate(Peringkat = row_number(), Eksogen = ifelse(Eksogen, "Ya", "Tidak")) %>%
           relocate(Peringkat)
    names(d)[names(d) %in% c("Rata", "SD", "Min", "Maks")] <- paste(met, c("rata-rata", "SD", "min", "maks"))
    tabel_dt(d, "ringkasan_cross_validation", page = 20) %>% formatRound(6:9, 3) %>%
      formatStyle("Peringkat", fontWeight = "bold", backgroundColor = styleEqual(1:3, c("#FFD700", "#D9D9D9", "#E3B47C")))
  })

  # ---------------------------------------------------------- EFISIENSI
  data_eff <- akurasi1 %>% select(Model, Kelompok, MASE, MAPE) %>% inner_join(waktu_komp, by = "Model") %>%
                filter(!is.na(Waktu)) %>% mutate(Waktu = pmax(Waktu, 0.01), Kelompok = factor(Kelompok, levels = URUT_KEL))

  output$plot_efisiensi <- renderPlotly({
    plot_ly(data_eff, x = ~Waktu, y = ~MASE, color = ~Kelompok, colors = WARNA_KEL, type = "scatter", mode = "markers",
            marker = list(size = 14, line = list(color = "white", width = 1)),
            hovertemplate = ~paste0("<b>", Model, "</b><br>Waktu: ", fk(Waktu, 2), " detik<br>MASE: ", fk(MASE, 3),
                                    "<br>MAPE: ", pct(MAPE), "<extra></extra>")) %>%
      layout(xaxis = list(title = "Waktu komputasi (detik, skala log)", type = "log"),
             yaxis = list(title = "MASE (makin kecil makin baik)")) %>% gaya_plot()
  })

  output$plot_waktu <- renderPlotly({
    d <- data_eff %>% arrange(Waktu) %>% mutate(Model = factor(Model, levels = Model))
    plot_ly(d, x = ~Waktu, y = ~Model, color = ~Kelompok, colors = WARNA_KEL, type = "bar", orientation = "h",
            text = ~paste0(fk(Waktu, 1), " s"), textposition = "outside",
            hovertemplate = "%{y}: %{x:.2f} detik<extra></extra>") %>%
      layout(xaxis = list(title = "Detik (skala log)", type = "log"), yaxis = list(title = "")) %>% gaya_plot()
  })

  # ---------------------------------------------------------- METODOLOGI
  output$tbl_model <- renderDT({
    ket <- c("Seasonal Naive" = "Nilai jam yang sama minggu lalu (patokan)",
             "MSTL" = "Dekomposisi tren + musiman harian & mingguan, ETS",
             "TBATS" = "Pemulusan eksponensial musiman ganda (trigonometri)",
             "DHR + eksogen" = "Regresi Fourier + suhu/kalender, galat ARIMA",
             "LightGBM + eksogen" = "Gradient boosting dengan lag 168/336/504 jam",
             "N-HiTS + eksogen" = "Deep learning hierarkis, dilatih khusus",
             "TimeGPT" = "Model fondasi Nixtla via API (zero-shot)",
             "TimeGPT + eksogen" = "TimeGPT dengan kovariat",
             "Chronos-2" = "Model fondasi Amazon (zero-shot)",
             "Chronos-2 + eksogen" = "Chronos-2 dengan kovariat",
             "TimesFM 2.5" = "Model fondasi Google (zero-shot)",
             "TimesFM 2.5 + eksogen" = "TimesFM dengan regresi kovariat in-context")
    d <- meta %>% transmute(Model, Kelompok = as.character(Kelompok),
                            Keterangan = ifelse(Model %in% names(ket), ket[Model], "Rata-rata MSTL + model fondasi terbaik"),
                            Eksogen = ifelse(Eksogen, "Ya", "Tidak"), `Model fondasi` = ifelse(Fondasi, "Ya", "Tidak"))
    tabel_dt(d, "daftar_model", page = 20)
  })

  output$tbl_periode <- renderTable({
    periode %>% mutate(across(c(`Awal uji`, `Akhir uji`, `Awal konteks`), ~ format(ke_waktu(.x), "%d %b %Y %H:%M")))
  }, striped = TRUE, width = "100%")
}

shinyApp(ui, server)
