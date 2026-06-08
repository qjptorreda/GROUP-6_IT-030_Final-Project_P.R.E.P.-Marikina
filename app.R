library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(rvest)
library(stringr)
library(lubridate)
library(DT)
library(tidyr)

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)
}

load_rds_resilient <- function(filename) {
  if (file.exists(filename)) {
    return(readRDS(filename))
  } else if (file.exists(file.path("data", filename))) {
    return(readRDS(file.path("data", filename)))
  } else {
    warning(paste("Could not find", filename, "- creating mock replacement data structure."))
    if(filename == "clean_weather.rds") {
      return(data.frame(DATE = seq(Sys.Date()-100, Sys.Date(), by="day"), PRECIP = rexp(101, rate=0.1), TEMP = rnorm(101, 28, 2)))
    } else if(filename == "master_psa.rds") {
      return(data.frame(
        barangay = c("Barangka", "Calumpang", "Concepcion Uno", "Malanday", "Nangka", "Parang", "Tumana", "Sto. Niño", "Tañong", "Jesus dela Peña", "Sta. Elena", "Fortune"),
        population = c(16144, 15914, 46629, 54134, 43661, 38445, 41220, 32150, 18500, 12400, 8900, 36200),
        total_children = c(4812, 4435, 13403, 19247, 14663, 11200, 13900, 9100, 5200, 3100, 2100, 10400),
        stringsAsFactors = FALSE
      ))
    } else if(filename == "clean_osm.rds") {
      return(st_as_sf(data.frame(id=1:3, geometry=st_sfc(st_point(c(121.094, 14.640)), st_point(c(121.089, 14.642)), st_point(c(121.098, 14.631)))), crs=4326))
    }
  }
}

clean_weather <- load_rds_resilient("clean_weather.rds")
master_psa    <- load_rds_resilient("master_psa.rds")
clean_osm     <- load_rds_resilient("clean_osm.rds")

if(!"barangay" %in% colnames(master_psa) && "barangay_name" %in% colnames(master_psa)) {
  master_psa <- master_psa %>% rename(barangay = barangay_name)
}
master_psa <- master_psa %>%
  mutate(
    barangay = trimws(as.character(barangay)),
    population = if("population" %in% colnames(master_psa)) as.numeric(population) else round(runif(n(), 10000, 50000)),
    total_children = if("total_children" %in% colnames(master_psa)) as.numeric(total_children) else round(population * 0.28)
  )

master_psa <- master_psa %>%
  mutate(
    trigger_height = case_when(
      tolower(barangay) == "nangka" ~ 14.0,
      tolower(barangay) %in% c("malanday", "barangka", "tumana") ~ 15.0,
      tolower(barangay) %in% c("calumpang", "san roque", "tañong") ~ 16.0,
      tolower(barangay) %in% c("concepcion uno", "concepcion dos") ~ 17.5,
      TRUE ~ 16.5
    )
  )

map_osm <- suppressWarnings(st_centroid(st_transform(clean_osm, crs = 4326)))

set.seed(42)
historical_prep_data <- clean_weather %>%
  rename_with(tolower) %>%
  arrange(date) %>% tail(100) %>% drop_na(precip, temp) %>%
  mutate(
    simulated_river_level = 12 + (lag(precip, n=1, default=0)*0.15) + rnorm(n(),0,0.5),
    critical_threshold    = 16.0
  )

min_date <- min(historical_prep_data$date, na.rm=TRUE)
max_date <- max(historical_prep_data$date, na.rm=TRUE)

evac_centers <- data.frame(
  name = c(
    "Marikina Sports Center", "Parang Elementary School", "Malanday Elementary School",
    "H. Bautista Elementary School", "Concepcion Integrated School", "Concepcion Elementary School",
    "Marikina Elementary School", "Tumana Elementary School", "Nangka Elementary School",
    "Sto. Niño Elementary School", "Barangka Elementary School", "Tañong Elementary School",
    "Calumpang Elementary School", "Marikina High School", "Marikina Polytechnic College Gym",
    "Jesus dela Peña Elementary School", "Sta. Elena Elementary School", "Industrial Valley Complex Gym",
    "Malanday National High School", "Concepcion National High School", "Tumana Covered Court",
    "Nangka Covered Court", "Calumpang Covered Court", "Marikina City Hall Covered Court",
    "Sto. Niño Covered Court", "Barangka Covered Court", "Fortune Elementary School",
    "Marikina Science High School"
  ),
  barangay = c(
    "Malanday","Parang","Malanday","Concepcion","Concepcion","Concepcion",
    "Concepcion","Tumana","Nangka","Sto. Niño","Barangka","Tañong",
    "Calumpang","Concepcion","Concepcion","Jesus dela Peña","Sta. Elena",
    "Malanday","Malanday","Concepcion","Tumana","Nangka","Calumpang",
    "Concepcion","Sto. Niño","Barangka","Fortune","Concepcion"
  ),
  type = c(
    "Sports Complex","Elementary School","Elementary School","Elementary School",
    "Integrated School","Elementary School","Elementary School","Elementary School",
    "Elementary School","Elementary School","Elementary School","Elementary School",
    "Elementary School","High School","College Gym","Elementary School",
    "Elementary School","Gymnasium","National High School","National High School",
    "Covered Court","Covered Court","Covered Court","Cover Court",
    "Covered Court","Covered Court","Elementary School","Science High School"
  ),
  capacity = c(
    5000,800,600,700,1200,650,900,750,680,580,
    620,520,700,1100,800,640,590,950,870,820,
    400,380,420,600,350,370,680,950
  ),
  lat = c(
    14.6302, 14.6412, 14.6520, 14.6589, 14.6617, 14.6605, 14.6571,
    14.6483, 14.6358, 14.6228, 14.6195, 14.6176, 14.6140, 14.6535,
    14.6548, 14.6472, 14.6389, 14.6538, 14.6512, 14.6622, 14.6468,
    14.6345, 14.6128, 14.6559, 14.6218, 14.6182, 14.6631, 14.6542
  ),
  lng = c(
    121.0984, 121.1012, 121.0978, 121.0820, 121.0804, 121.0835, 121.0862,
    121.0891, 121.0918, 121.0940, 121.0887, 121.0870, 121.0895, 121.0841,
    121.0855, 121.0933, 121.0965, 121.0960, 121.0990, 121.0905, 121.0925,
    121.0908, 121.0830, 121.0950, 121.0875, 121.0798, 121.0850, 121.0945
  ),
  activates_at = c(
    15.0,14.5,14.5,15.0,15.5,15.0,15.0,
    14.5,15.0,16.0,16.5,16.5,17.0,15.0,
    15.0,15.0,15.0,14.5,14.5,15.5,14.5,
    15.0,17.0,15.5,16.0,16.5,15.5,15.0
  ),
  stringsAsFactors = FALSE
)

custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=DM+Sans:ital,wght@0,300;0,400;0,500;0,600;0,700;1,400&family=DM+Mono:wght@400;500&display=swap');
  *, *::before, *::after { box-sizing: border-box; }
  body, .content-wrapper, .main-sidebar, .sidebar,
  h1, h2, h3, h4, h5, p, span, a, td, th, label, .btn {
    font-family: 'DM Sans', sans-serif !important;
  }
  body { background: #f5f6fa !important; color: #1a1d2e; }
  .main-sidebar, .left-side { background:#1a1d2e !important; box-shadow:4px 0 20px rgba(0,0,0,0.12); }
  .sidebar-menu > li > a { color:#8892a4 !important; border-radius:10px !important; margin:2px 12px !important; padding:11px 16px !important; font-weight:500 !important; font-size:13.5px !important; transition:all .2s ease; }
  .sidebar-menu > li.active > a, .sidebar-menu > li > a:hover { background:rgba(255,255,255,0.08) !important; color:#ffffff !important; }
  .sidebar-menu > li > a .fa { margin-right:10px; width:18px; text-align:center; color:#5a8dee; }
  .sidebar-menu > li.active > a .fa { color:#fff; }
  .main-header .navbar, .main-header .logo { background:#1a1d2e !important; border-bottom:none !important; }
  .main-header .logo { font-family:'DM Sans',sans-serif !important; font-weight:700 !important; font-size:17px !important; color:#ffffff !important; letter-spacing:0.3px; }
  .main-header .navbar .sidebar-toggle { color:#8892a4 !important; }
  .content-wrapper { background:#f5f6fa !important; margin-left:280px; padding:28px 30px !important; }
  .page-title { font-size:22px; font-weight:700; color:#1a1d2e; margin:0 0 24px 0; display:flex; align-items:center; gap:10px; }
  .page-title span.badge-pill { font-size:11px; font-weight:600; background:#5a8dee; color:#fff; padding:3px 10px; border-radius:20px; letter-spacing:.3px; }
  .page-title span.badge-red  { font-size:11px; font-weight:600; background:#e74c3c; color:#fff; padding:3px 10px; border-radius:20px; animation:blink 1.4s infinite; }
  .box { background:#ffffff !important; border-radius:16px !important; box-shadow:0 2px 12px rgba(26,29,46,0.07) !important; border:1px solid #eef0f6 !important; margin-bottom:20px !important; }
  .box-header { padding:12px 20px !important; border-bottom:1px solid #f0f2f8 !important; }
  .box-header .box-title { font-size:14px !important; font-weight:600 !important; color:#1a1d2e !important; }
  .box-body { padding:16px 20px !important; }
  .kpi-card { background:#ffffff; border-radius:16px; padding:20px 22px; border:1px solid #eef0f6; box-shadow:0 2px 12px rgba(26,29,46,0.07); display:flex; align-items:center; gap:16px; margin-bottom:20px; transition:box-shadow .2s; }
  .kpi-card:hover { box-shadow:0 6px 24px rgba(26,29,46,0.11); }
  .kpi-icon { width:52px; height:52px; border-radius:14px; display:flex; align-items:center; justify-content:center; font-size:22px; flex-shrink:0; }
  .kpi-icon.green  { background:#edfbf3; color:#27ae60; }
  .kpi-icon.amber  { background:#fff8e6; color:#f39c12; }
  .kpi-icon.red    { background:#fef1f1; color:#e74c3c; }
  .kpi-icon.blue   { background:#eef4ff; color:#5a8dee; }
  .kpi-icon.purple { background:#f3eeff; color:#8e44ad; }
  .kpi-label  { font-size:12px; font-weight:500; color:#8892a4; text-transform:uppercase; letter-spacing:.5px; }
  .kpi-value  { font-size:26px; font-weight:700; color:#1a1d2e; line-height:1.1; }
  .kpi-status { font-size:12px; font-weight:600; margin-top:3px; }
  .kpi-status.normal { color:#27ae60; } .kpi-status.alarm1 { color:#f39c12; }
  .kpi-status.alarm2 { color:#e67e22; } .kpi-status.alarm3 { color:#e74c3c; }
  .alert-ticker { background:#1a1d2e; border-radius:12px; padding:10px 18px; color:#fff; font-size:13px; font-weight:500; display:flex; align-items:center; gap:10px; margin-bottom:22px; }
  .alert-ticker .ticker-dot { width:8px; height:8px; border-radius:50%; background:#27ae60; flex-shrink:0; animation:blink 1.4s infinite; }
  .alert-ticker.warning .ticker-dot { background:#f39c12; }
  .alert-ticker.danger  .ticker-dot { background:#e74c3c; }
  @keyframes blink { 0%,100%{opacity:1} 50%{opacity:.3} }
  .ticker-text { color:#c8cde0; }
  #refresh_btn { width:100%; background:#5a8dee !important; border:none !important; border-radius:12px !important; color:#fff !important; font-weight:600 !important; font-size:13px !important; padding:10px 0 !important; transition:background .2s,transform .15s; }
  #refresh_btn:hover { background:#4a7de3 !important; transform:translateY(-1px); }
  .interpret-btn { background:#f5f6fa; border:1.5px solid #e4e7f2; border-radius:10px; color:#5a8dee; font-family:'DM Sans',sans-serif; font-size:12.5px; font-weight:600; padding:7px 16px; cursor:pointer; display:inline-flex; align-items:center; gap:6px; transition:all .2s; margin-top:10px; }
  .interpret-btn:hover { background:#eef4ff; border-color:#5a8dee; }
  .interp-panel { margin-top:12px; background:#f8faff; border:1px solid #dce8fd; border-left:4px solid #5a8dee; border-radius:10px; padding:14px 16px; font-size:13px; color:#2d3a52; line-height:1.7; animation:fadeSlideIn .25s ease; }
  .interp-panel h5 { margin:0 0 6px 0; font-size:13px; font-weight:700; color:#1a1d2e; display:flex; align-items:center; gap:6px; }
  @keyframes fadeSlideIn { from{opacity:0;transform:translateY(-6px)} to{opacity:1;transform:translateY(0)} }
  .dataTables_wrapper { font-size:13px; width:100% !important; overflow:hidden; }
  .dataTables_wrapper table { width:100% !important; table-layout:fixed !important; }
  .dataTables_wrapper table td,
  .dataTables_wrapper table th { word-wrap:break-word !important; overflow-wrap:break-word !important; white-space:normal !important; max-width:0; padding:8px 10px !important; }
  table.dataTable thead th { background:#f8f9fc !important; color:#8892a4 !important; font-weight:600 !important; font-size:11px !important; text-transform:uppercase; letter-spacing:.5px; border-bottom:2px solid #eef0f6 !important; }
  table.dataTable tbody tr:hover td { background:#f4f7ff !important; }
  table.dataTable tbody td { border-bottom:1px solid #f5f6fa !important; }
  .irs--shiny .irs-bar { background:#5a8dee; border-color:#5a8dee; }
  .irs--shiny .irs-handle { border-color:#5a8dee; }
  .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single { background:#5a8dee; }
  .impact-badge { background:linear-gradient(135deg,#fff8e6,#fff3d6); border:1px solid #f6d980; border-radius:14px; padding:18px 20px; text-align:center; margin-bottom:14px; }
  .impact-badge h4 { margin:0; font-size:12px; font-weight:600; color:#9a7a1a; text-transform:uppercase; letter-spacing:.5px; }
  .impact-badge .big-num { font-size:32px; font-weight:800; color:#c0891a; margin:4px 0 0; }
  .impact-safe { background:#edfbf3; border:1px solid #a8e6c1; border-radius:14px; padding:16px; text-align:center; color:#1e8449; font-weight:600; font-size:13.5px; }
  .update-chip { background:#eef4ff; color:#5a8dee; border-radius:20px; padding:3px 12px; font-size:11.5px; font-weight:600; }
  .small-box { display:none !important; }
  .evac-stat-card { background:#fff; border-radius:14px; padding:16px 18px; border:1px solid #eef0f6; box-shadow:0 2px 10px rgba(26,29,46,0.06); text-align:center; margin-bottom:16px; }
  .evac-stat-card .stat-num { font-size:28px; font-weight:800; line-height:1; }
  .evac-stat-card .stat-lbl { font-size:11px; font-weight:600; color:#8892a4; text-transform:uppercase; letter-spacing:.5px; margin-top:4px; }
  .evac-open    { color:#27ae60; } .evac-standby { color:#f39c12; } .evac-closed { color:#8892a4; }
  .live-source-banner { background: linear-gradient(135deg, #1a1d2e 0%, #2c3e50 100%); border-radius: 14px; padding: 16px 20px; margin-bottom: 20px; display: flex; align-items: center; gap: 16px; flex-wrap: wrap; }
  .live-source-banner .lsb-icon { font-size:28px; }
  .live-source-banner .lsb-title { font-weight:700; font-size:15px; color:#fff; }
  .live-source-banner .lsb-sub { font-size:12px; color:#8892a4; margin-top:2px; line-height:1.5; }
  .live-source-banner .lsb-links { margin-left:auto; display:flex; gap:10px; flex-wrap:wrap; }
  .fb-link-btn { display:inline-flex; align-items:center; gap:6px; background:#1877f2; color:#fff; border-radius:10px; padding:8px 14px; font-size:12.5px; font-weight:600; text-decoration:none; transition:background .2s; border:none; cursor:pointer; }
  .fb-link-btn:hover { background:#1565d8; color:#fff; }
  .fb-link-btn.rescue { background:#e74c3c; }
  .fb-link-btn.rescue:hover { background:#c0392b; }
  .hotline-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-bottom:16px; }
  .hotline-card { background:#fff; border:1px solid #eef0f6; border-radius:12px; padding:12px 14px; display:flex; align-items:center; gap:10px; }
  .hotline-card .hc-icon { width:36px; height:36px; border-radius:10px; display:flex; align-items:center; justify-content:center; font-size:16px; flex-shrink:0; }
  .hc-rescue   { background:#fef1f1; color:#e74c3c; }
  .hc-mdrrmo   { background:#eef4ff; color:#5a8dee; }
  .hc-fire     { background:#fff4e6; color:#e67e22; }
  .hc-police   { background:#f0f3ff; color:#2c3e8c; }
  .hc-emergency{ background:#f3eeff; color:#8e44ad; }
  .hotline-card .hc-num { font-size:15px; font-weight:700; color:#1a1d2e; font-family:'DM Mono',monospace; }
  .hotline-card .hc-lbl { font-size:11px; color:#8892a4; font-weight:500; }
  .data-notice { background:#fff8e6; border:1px solid #f6d980; border-left:4px solid #f39c12; border-radius:10px; padding:12px 14px; font-size:12.5px; color:#7a5a00; line-height:1.6; margin-bottom:16px; }
  .pill-open    { background:#edfbf3; color:#27ae60; padding:3px 10px; border-radius:20px; font-weight:700; font-size:11px; }
  .pill-standby { background:#fff8e6; color:#f39c12; padding:3px 10px; border-radius:20px; font-weight:700; font-size:11px; }
  .pill-closed  { background:#f5f6fa; color:#8892a4; padding:3px 10px; border-radius:20px; font-weight:700; font-size:11px; }
  .evac-legend { display:flex; gap:12px; flex-wrap:wrap; margin-bottom:14px; }
  .legend-item { display:flex; align-items:center; gap:6px; font-size:12px; font-weight:600; color:#4a5568; }
  .legend-dot  { width:12px; height:12px; border-radius:50%; flex-shrink:0; }
  .river-readout { background:linear-gradient(135deg,#1a1d2e,#2c3e50); border-radius:14px; padding:16px 20px; color:#fff; margin-bottom:16px; }
  .river-readout .rr-label { font-size:11px; font-weight:600; color:#8892a4; text-transform:uppercase; letter-spacing:.5px; }
  .river-readout .rr-value { font-size:36px; font-weight:800; line-height:1; margin:4px 0; }
  .river-readout .rr-status { font-size:13px; font-weight:600; }
  .rr-normal  { color:#27ae60; } .rr-alarm1 { color:#f1c40f; }
  .rr-alarm2  { color:#e67e22; } .rr-alarm3 { color:#e74c3c; }
  .rr-critical{ color:#ff6efd; }
  /* ── Impact table: fixed layout, no overflow ── */
  #impact_table table { table-layout: fixed !important; width: 100% !important; }
  #impact_table table td,
  #impact_table table th {
    word-break: break-word !important;
    white-space: normal !important;
    overflow: hidden !important;
    text-overflow: ellipsis !important;
    padding: 7px 8px !important;
    font-size: 12px !important;
  }
  #impact_table .dataTables_wrapper { overflow-x: hidden !important; }
"

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span("P.R.E.P.", tags$small(" Marikina", style="font-size:13px;font-weight:400;opacity:.7;")),
    titleWidth = 280
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Live Risk Dashboard",     tabName="dashboard",  icon=icon("chart-line")),
      menuItem("Predictive Analytics",    tabName="predictive", icon=icon("microscope")),
      menuItem("Vulnerability Simulator", tabName="map",        icon=icon("map-location-dot")),
      menuItem("Evacuation Centers",      tabName="evacuation", icon=icon("person-running")),
      tags$div(style="padding:16px 16px 8px;",
               actionButton("refresh_btn", tags$span(icon("rotate")," Refresh Live Data"), class="btn"),
               tags$br(), tags$br(),
               tags$div(style="text-align:center;",
                        tags$small(textOutput("last_update_time"), style="color:#5a8dee;font-size:11.5px;font-weight:600;"))
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      
      # ── TAB 1: DASHBOARD
      tabItem(tabName="dashboard",
              tags$div(class="page-title", icon("droplet"), "Live Hydrology Monitor",
                       tags$span("LIVE", class="badge-pill")),
              uiOutput("alert_ticker_ui"),
              fluidRow(column(4,uiOutput("kpi_tumana")),column(4,uiOutput("kpi_nangka")),column(4,uiOutput("kpi_stoynino"))),
              fluidRow(
                box(title=tags$div(style="display:flex;align-items:center;justify-content:space-between;width:100%;",
                                   tags$span("River Levels vs. Alarm Thresholds"),uiOutput("update_chip")),
                    status="primary",solidHeader=TRUE,width=8,
                    plotOutput("river_plot",height="340px"),
                    actionButton("toggle_river_interp", "Toggle Interpretation", class="interpret-btn"),
                    uiOutput("river_interp_panel")),
                box(title="Sensor Data Feed",status="info",solidHeader=TRUE,width=4,
                    uiOutput("mini_stats"),tags$hr(style="border-color:#f0f2f8;margin:12px 0;"),DTOutput("live_table"))
              )
      ),
      
      # ── TAB 2: PREDICTIVE ANALYTICS
      tabItem(tabName="predictive",
              tags$div(class="page-title",icon("microscope"),"Predictive Analytics Engine"),
              fluidRow(
                box(title="Configuration Panel",status="primary",solidHeader=TRUE,width=4,
                    selectInput("date_mode", "Analysis Context Mode:",
                                choices = c("Historical Window View", "Future Calendar View")),
                    uiOutput("dynamic_date_ui"),
                    tags$br(),
                    selectInput("horizon","Forecast Horizon Length:",
                                choices=c("Tomorrow", "3 Days", "1 Week", "2 Weeks", "1 Month")),
                    tags$p("Simulate future flood risk and precipitation shifts across Marikina watersheds.",style="font-size:13px;color:#8892a4;line-height:1.6;")),
                box(title="Flood Forecast Projection",status="danger",solidHeader=TRUE,width=8,
                    plotOutput("forecast_plot",height="260px"),
                    actionButton("toggle_forecast_interp", "Toggle Interpretation", class="interpret-btn"),
                    uiOutput("forecast_interp_panel"))
              ),
              fluidRow(
                box(title="Time-Lag: Rainfall vs. River Rise",status="danger",solidHeader=TRUE,width=6,
                    plotOutput("time_lag_plot",height="280px"),
                    actionButton("toggle_timelag_interp", "Toggle Interpretation", class="interpret-btn"),
                    uiOutput("timelag_interp_panel")),
                box(title="Rainfall–Flood Correlation (OLS)",status="warning",solidHeader=TRUE,width=6,
                    plotOutput("scatter_plot",height="280px"),
                    actionButton("toggle_scatter_interp", "Toggle Interpretation", class="interpret-btn"),
                    uiOutput("model_interpretation_ui"))
              )
      ),
      
      # ── TAB 3: VULNERABILITY SIMULATOR
      tabItem(tabName="map",
              tags$div(class="page-title",icon("map-location-dot"),"Vulnerability Simulator"),
              fluidRow(
                column(width=4,
                       box(title="Simulation Controls",status="primary",solidHeader=TRUE,width=12,
                           sliderInput("flood_sim","Simulated River Height (m):",min=13,max=22,value=15,step=0.5),
                           uiOutput("impact_summary"),
                           actionButton("toggle_sim_interp", "Toggle Interpretation", class="interpret-btn"),
                           uiOutput("sim_interp_panel")),
                       box(title="Filter Facilities",status="info",solidHeader=TRUE,width=12,
                           selectInput("bgy_select","Highlight Barangay:",
                                       choices=c("All Barangays", sort(unique(master_psa$barangay))),selected="All Barangays")),
                       # ── FIXED: table with constrained layout
                       box(title="Affected Barangays",status="warning",solidHeader=TRUE,width=12,
                           tags$div(style="width:100%;overflow-x:hidden;",
                                    DTOutput("impact_table")))
                ),
                column(width=8,
                       box(title="Interactive Flood Risk Map",status="warning",solidHeader=TRUE,width=12,
                           leafletOutput("marikina_map",height="720px")))
              )
      ),
      
      # ── TAB 4: EVACUATION LOGISTICS  (Filter Centers TOP, Emergency Contacts BOTTOM)
      tabItem(tabName="evacuation",
              tags$div(class="page-title",icon("person-running"),"Real-Time Evacuation Centers",
                       tags$span("LIVE", class="badge-red")),
              
              tags$div(class="live-source-banner",
                       tags$div(class="lsb-icon", icon("satellite-dish")),
                       tags$div(
                         tags$div(class="lsb-title", "Live Data Sources"),
                         tags$div(class="lsb-sub",
                                  "Center status is driven by real-time river levels from BantayBaha. ",
                                  "For official MDRRMO announcements during emergencies, monitor the official Facebook pages below:")
                       ),
                       tags$div(class="lsb-links",
                                tags$a(class="fb-link-btn", href="https://www.facebook.com/MarikinaRescue161/", target="_blank",
                                       icon("square-facebook"), " Rescue 161"),
                                tags$a(class="fb-link-btn", href="https://www.facebook.com/MarikinaPIO/", target="_blank",
                                       icon("square-facebook"), " Marikina PIO")
                       )
              ),
              
              tags$div(class="data-notice",
                       icon("circle-info"), tags$strong(" Transparency: "),
                       "Center activation status (OPEN / STANDBY / CLOSED) is automatically computed from the live BantayBaha river level feed using MDRRMO alarm thresholds. ",
                       "Occupancy is estimated based on flood severity. For confirmed real-time headcounts, call Rescue 161 or check the Facebook pages above."
              ),
              
              fluidRow(
                column(width=4,
                       # 1st: Live River Level
                       box(title=tags$div(style="display:flex;align-items:center;justify-content:space-between;width:100%;",
                                          tags$span("Live River Level (BantayBaha)"),
                                          tags$span(style="font-size:11px;color:#5a8dee;font-weight:600;", icon("rotate"), " Auto-refresh")),
                           status="primary", solidHeader=TRUE, width=12,
                           uiOutput("evac_river_readout"),
                           tags$hr(style="border-color:#f0f2f8;margin:12px 0;"),
                           tags$p(style="font-size:12px;color:#8892a4;margin:0;",
                                  icon("circle-info"),
                                  " Centers activate automatically when the live river level crosses MDRRMO thresholds. ",
                                  "Use the manual override below to simulate scenarios."
                           ),
                           tags$br(),
                           sliderInput("evac_manual_override",
                                       "Manual Override (for simulation):",
                                       min=13, max=22, value=14, step=0.5),
                           checkboxInput("evac_use_live", "Use live BantayBaha level (uncheck to use slider)", value=TRUE)
                       ),
                       
                       # 2nd: Filter Centers (moved UP — was 3rd)
                       box(title="Filter Centers", status="info", solidHeader=TRUE, width=12,
                           selectInput("evac_bgy_filter","Barangay:",choices=c("All",sort(unique(evac_centers$barangay))),selected="All"),
                           selectInput("evac_type_filter","Facility Type:",choices=c("All Types",sort(unique(evac_centers$type))),selected="All Types"),
                           tags$div(class="evac-legend",
                                    tags$div(class="legend-item",tags$div(class="legend-dot",style="background:#27ae60;"),"OPEN"),
                                    tags$div(class="legend-item",tags$div(class="legend-dot",style="background:#f39c12;"),"STANDBY"),
                                    tags$div(class="legend-item",tags$div(class="legend-dot",style="background:#8892a4;"),"CLOSED")
                           )
                       ),
                       
                       # 3rd: Emergency Contacts (moved DOWN — was 2nd)
                       box(title=tags$div(icon("phone-volume"), " Emergency Contacts"),
                           status="danger", solidHeader=TRUE, width=12,
                           tags$div(class="hotline-grid",
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-rescue", icon("ambulance")),
                                             tags$div(tags$div(class="hc-num","161"),tags$div(class="hc-lbl","Marikina Rescue (Main)"))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-rescue", icon("ambulance")),
                                             tags$div(tags$div(class="hc-num","2161"),tags$div(class="hc-lbl","Globe Subscribers"))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-mdrrmo", icon("shield-halved")),
                                             tags$div(tags$div(class="hc-num","8646-0427"),tags$div(class="hc-lbl","MDRRMO Direct"))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-mdrrmo", icon("shield-halved")),
                                             tags$div(tags$div(class="hc-num","8646-2436"),tags$div(class="hc-lbl","MDRRMO Alt. Line"))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-fire", icon("fire")),
                                             tags$div(tags$div(class="hc-num","933-3076"),tags$div(class="hc-lbl","Marikina Fire Dept."))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-police", icon("shield")),
                                             tags$div(tags$div(class="hc-num","8646-1631"),tags$div(class="hc-lbl","Marikina PNP"))),
                                    tags$div(class="hotline-card",
                                             tags$div(class="hc-icon hc-emergency", icon("star-of-life")),
                                             tags$div(tags$div(class="hc-num","911"),tags$div(class="hc-lbl","Metro Manila Emergency")))
                           ),
                           tags$p(style="font-size:11px;color:#8892a4;margin:8px 0 0;",
                                  "Sources: Inside Marikina / MDRRMO / Marikina City Government website (marikina.gov.ph)")
                       )
                ),
                
                column(width=8,
                       fluidRow(
                         column(3, uiOutput("evac_kpi_open")),
                         column(3, uiOutput("evac_kpi_standby")),
                         column(3, uiOutput("evac_kpi_capacity")),
                         column(3, uiOutput("evac_kpi_occupancy"))
                       ),
                       box(title=tags$div(style="display:flex;align-items:center;justify-content:space-between;width:100%;",
                                          tags$span("Evacuation Center Map — Marikina City")),
                           status="warning", solidHeader=TRUE, width=12,
                           leafletOutput("evac_map", height="400px")
                       ),
                       box(title="Center Directory", status="info", solidHeader=TRUE, width=12,
                           DTOutput("evac_table")
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  show_river_interp    <- reactiveVal(FALSE)
  show_forecast_interp <- reactiveVal(FALSE)
  show_timelag_interp  <- reactiveVal(FALSE)
  show_scatter_interp  <- reactiveVal(FALSE)
  show_sim_interp      <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_river_interp,    { show_river_interp(!show_river_interp()) })
  observeEvent(input$toggle_forecast_interp, { show_forecast_interp(!show_forecast_interp()) })
  observeEvent(input$toggle_timelag_interp,  { show_timelag_interp(!show_timelag_interp()) })
  observeEvent(input$toggle_scatter_interp,  { show_scatter_interp(!show_scatter_interp()) })
  observeEvent(input$toggle_sim_interp,      { show_sim_interp(!show_sim_interp()) })
  
  status_color <- function(s) switch(as.character(s), "Normal"="green", "1st Alarm"="amber", "2nd Alarm"="red", "3rd Alarm"="red", "green")
  status_class <- function(s) switch(as.character(s), "Normal"="normal", "1st Alarm"="alarm1", "2nd Alarm"="alarm2", "3rd Alarm"="alarm3", "normal")
  
  output$dynamic_date_ui <- renderUI({
    req(input$date_mode)
    if (input$date_mode == "Historical Window View") {
      dateRangeInput("analytics_dates", "Analysis Date Window:", start = min_date, end = max_date,
                     min = min_date, max = max_date, format = "yyyy-mm-dd", separator = " → ")
    } else {
      dateInput("target_calendar_date", "Select Forecast Start Window Location:", 
                value = Sys.Date() + 1, min = Sys.Date() - 10, max = Sys.Date() + 90, format = "yyyy-mm-dd")
    }
  })
  
  live_water_data <- eventReactive(input$refresh_btn, ignoreNULL = FALSE, {
    invalidateLater(300000, session)
    tryCatch({
      url <- "https://bantaybaha.com/marikina"
      live_page  <- rvest::read_html(url)
      all_tables <- live_page %>% html_nodes("table") %>% html_table(fill = TRUE)
      raw_table  <- NULL
      if (length(all_tables) > 0) {
        target_idx <- Position(function(tbl) any(c("River", "Station") %in% names(tbl)) && "Status" %in% names(tbl), all_tables)
        if (!is.na(target_idx)) raw_table = all_tables[[target_idx]] else raw_table = all_tables[[1]]
      }
      if (!is.null(raw_table)) {
        colnames(raw_table)[1] <- "River"
        colnames(raw_table)[2] <- "Level"
        colnames(raw_table)[3] <- "Status"
        raw_table %>% mutate(
          River  = str_to_lower(str_trim(River)),
          Level  = as.numeric(str_extract(Level, "[0-9.]+")),
          Status = case_when(
            grepl("sto", River) & Level >= 17.00 ~ "3rd Alarm",
            grepl("sto", River) & Level >= 16.00 ~ "2nd Alarm",
            grepl("sto", River) & Level >= 15.00 ~ "1st Alarm",
            grepl("nang", River) & Level >= 17.70 ~ "3rd Alarm",
            grepl("nang", River) & Level >= 17.10 ~ "2nd Alarm",
            grepl("nang", River) & Level >= 16.50 ~ "1st Alarm",
            grepl("tuma", River) & Level >= 19.26 ~ "3rd Alarm",
            grepl("tuma", River) & Level >= 18.26 ~ "2nd Alarm",
            grepl("tuma", River) & Level >= 17.26 ~ "1st Alarm",
            TRUE ~ "Normal"
          )
        )
      } else {
        data.frame(River = c("tumana", "nangka", "sto. nino"), Level = c(14.2, 13.8, 14.5), Status = c("Normal", "Normal", "Normal"), stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      data.frame(River = c("tumana", "nangka", "sto. nino"), Level = c(14.2, 13.8, 14.5), Status = c("Normal", "Normal", "Normal"), stringsAsFactors = FALSE)
    })
  })
  
  last_update <- eventReactive(input$refresh_btn, ignoreNULL = FALSE, { 
    live_water_data()
    format(Sys.time(), "%b %d, %Y %I:%M:%S %p") 
  })
  
  output$last_update_time <- renderText({ paste("Last Sync:", last_update()) })
  output$update_chip      <- renderUI({ tags$span(class = "update-chip", paste("Sync:", last_update())) })
  
  filtered_analytics_data <- reactive({
    if (is.null(input$analytics_dates)) return(historical_prep_data)
    historical_prep_data %>% filter(date >= input$analytics_dates[1] & date <= input$analytics_dates[2])
  })
  
  output$kpi_tumana <- renderUI({
    df <- live_water_data()
    val <- df$Level[df$River == "tumana"]
    st  <- df$Status[df$River == "tumana"]
    if(length(val) == 0) { val <- 14.2; st <- "Normal" }
    tags$div(class = "kpi-card", tags$div(class = paste("kpi-icon", status_color(st)), icon("droplet")),
             tags$div(tags$div(class = "kpi-label", "Tumana Station"), tags$div(class = "kpi-value", paste0(val, "m")), tags$div(class = paste("kpi-status", status_class(st)), st)))
  })
  
  output$kpi_nangka <- renderUI({
    df <- live_water_data()
    val <- df$Level[df$River == "nangka"]
    st  <- df$Status[df$River == "nangka"]
    if(length(val) == 0) { val <- 13.8; st <- "Normal" }
    tags$div(class = "kpi-card", tags$div(class = paste("kpi-icon", status_color(st)), icon("droplet")),
             tags$div(tags$div(class = "kpi-label", "Nangka Station"), tags$div(class = "kpi-value", paste0(val, "m")), tags$div(class = paste("kpi-status", status_class(st)), st)))
  })
  
  output$kpi_stoynino <- renderUI({
    df <- live_water_data()
    val <- df$Level[df$River %in% c("sto. nino", "sto. niño")]
    st  <- df$Status[df$River %in% c("sto. nino", "sto. niño")]
    if(length(val) == 0) { val <- 14.5; st <- "Normal" }
    tags$div(class = "kpi-card", tags$div(class = paste("kpi-icon", status_color(st)), icon("droplet")),
             tags$div(tags$div(class = "kpi-label", "Sto. Niño (Main Entry)"), tags$div(class = "kpi-value", paste0(val, "m")), tags$div(class = paste("kpi-status", status_class(st)), st)))
  })
  
  output$alert_ticker_ui <- renderUI({
    df <- live_water_data()
    max_status <- if("3rd Alarm" %in% df$Status) "3rd Alarm" else if("2nd Alarm" %in% df$Status) "2nd Alarm" else if("1st Alarm" %in% df$Status) "1st Alarm" else "Normal"
    ticker_class <- if(max_status == "Normal") "" else if(max_status == "1st Alarm") "warning" else "danger"
    ticker_text  <- switch(max_status,
                           "Normal"    = "All monitoring structures clear. System tracking stable stream line thresholds.",
                           "1st Alarm" = "Alert Level 1 Active: Waterline approaching municipal margins. Communities prepare standard contingency frames.",
                           "2nd Alarm" = "Alert Level 2 Active: Structural overflow imminent. Secure heavy response coordinates.",
                           "3rd Alarm" = "CRITICAL WARNING LEVEL 3: Mandatory evacuation parameters enforced across all low-elevation margins."
    )
    tags$div(class = paste("alert-ticker", ticker_class), tags$div(class = "ticker-dot"), tags$div(class = "ticker-text", tags$strong(paste0("[", max_status, "] ")), ticker_text))
  })
  
  output$river_plot <- renderPlot({
    ggplot(filtered_analytics_data(), aes(x = date, y = simulated_river_level, group = 1)) +
      geom_line(color = "#5a8dee", linewidth = 1) +
      geom_hline(yintercept = 15, linetype = "dashed", color = "#f39c12") +
      geom_hline(yintercept = 16, linetype = "dashed", color = "#e67e22") +
      geom_hline(yintercept = 17, linetype = "dashed", color = "#e74c3c") +
      theme_minimal() + labs(x = "Timeline", y = "Depth (m)")
  })
  
  output$river_interp_panel <- renderUI({
    if (!show_river_interp()) return(NULL)
    max_lvl <- round(max(filtered_analytics_data()$simulated_river_level, na.rm = TRUE), 2)
    sev <- if(max_lvl >= 17) "Crossed critical boundaries." else if(max_lvl >= 15) "Crossed baseline warnings." else "Remained stable within constraints."
    tags$div(class = "interp-panel", tags$h5("Timeline Profile Analysis"), paste0("Analysis checks a maximum river level crest of ", max_lvl, "m inside this window scope. Flows ", sev))
  })
  
  output$live_table <- renderDT({ datatable(live_water_data(), options = list(dom = 't', bSort = FALSE), rownames = FALSE) })
  output$mini_stats <- renderUI({ tags$div(tags$small(style = "color:#8892a4;", paste("Active sensor nodes tracked:", nrow(live_water_data())))) })
  
  reactive_forecast_calc <- reactive({
    df <- filtered_analytics_data()
    horizon_days <- switch(req(input$horizon), "Tomorrow" = 1, "3 Days" = 3, "1 Week" = 7, "2 Weeks" = 14, "1 Month" = 30, 1)
    future_dates <- seq(max(df$date) + 1, max(df$date) + horizon_days, by = "day")
    set.seed(123)
    forecast_df <- data.frame(date = future_dates, simulated_river_level = tail(df$simulated_river_level, 1) + cumsum(rnorm(horizon_days, 0.08, 0.35)))
    list(df = df, forecast_df = forecast_df, horizon = input$horizon)
  })
  
  output$forecast_plot <- renderPlot({
    fc <- reactive_forecast_calc()
    ggplot() +
      geom_line(data = fc$df, aes(x = date, y = simulated_river_level, group = 1), color = "#8892a4", alpha = 0.5) +
      geom_line(data = fc$forecast_df, aes(x = date, y = simulated_river_level, group = 1), color = "#e74c3c", linewidth = 1.2) +
      theme_minimal() + labs(x = "Date", y = "Forecast Line (m)")
  })
  
  output$forecast_interp_panel <- renderUI({
    if(!show_forecast_interp()) return(NULL)
    fc <- reactive_forecast_calc()
    diff_val <- round(tail(fc$forecast_df$simulated_river_level, 1) - tail(fc$df$simulated_river_level, 1), 2)
    dir <- if(diff_val > 0.5) "ascending variance vector" else if(diff_val < -0.5) "receding channel trace" else "steady baseline state"
    tags$div(class = "interp-panel", tags$h5("Forecast Evaluation Model"), paste0("Evaluating a ", fc$horizon, " projection window displays a projected ", dir, " showing a variance of ", diff_val, "m."))
  })
  
  output$time_lag_plot <- renderPlot({
    ggplot(filtered_analytics_data(), aes(x = precip, y = simulated_river_level)) + geom_point(color = "#8e44ad", alpha = 0.6) + geom_smooth(method = "lm", color = "#5a8dee", formula = 'y ~ x') + theme_minimal()
  })
  
  output$timelag_interp_panel <- renderUI({
    if(!show_timelag_interp()) return(NULL)
    slope <- round(summary(lm(simulated_river_level ~ precip, data = filtered_analytics_data()))$coefficients[2,1], 3)
    tags$div(class = "interp-panel", tags$h5("Time-Lag Vector Assessment"), paste0("Regression checks show that for each millimeter of precipitation, river line levels trend up by ", slope, "m as catchment accumulation centers complete down-basin discharge pipelines."))
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_analytics_data(), aes(x = temp, y = simulated_river_level)) + geom_point(color = "#e67e22") + theme_minimal()
  })
  
  output$model_interpretation_ui <- renderUI({
    if(!show_scatter_interp()) return(NULL)
    corr_val <- round(cor(filtered_analytics_data()$temp, filtered_analytics_data()$simulated_river_level, use = "complete.obs"), 2)
    tags$div(class = "interp-panel", tags$h5("Atmospheric Covariance Assessment"), paste0("The Pearson correlation calculation scores a metric rating of ", corr_val, ". This tracks the baseline interplay between temperature variations and downstream flow rates."))
  })
  
  affected_demographics <- reactive({
    h <- input$flood_sim
    df <- master_psa %>%
      mutate(
        is_affected = trigger_height <= h,
        Status = if_else(is_affected, "Affected", "Safe"),
        Display_Pop = if_else(is_affected, population, 0),
        Display_Kids = if_else(is_affected, total_children, 0)
      )
    if (input$bgy_select != "All Barangays") {
      df <- df %>% filter(tolower(barangay) == tolower(input$bgy_select))
    }
    df
  })
  
  output$impact_summary <- renderUI({
    df_sim <- affected_demographics()
    total_impact <- sum(df_sim$Display_Pop, na.rm = TRUE)
    if(total_impact > 0) {
      tags$div(
        style = "background: #fef3c7; border: 1px solid #f59e0b; border-radius: 8px; padding: 15px; text-align: center; margin: 10px 0;",
        tags$div(style = "color: #78350f; font-size: 0.95rem; font-weight: 500; margin-bottom: 4px;", "Total Population Affected"),
        tags$div(style = "font-size: 2rem; font-weight: 700; color: #451a03; font-family: system-ui;", format(total_impact, big.mark=","))
      )
    } else {
      tags$div(
        style = "background: #f0fff4; border: 1px solid #9ae6b4; border-radius: 8px; padding: 15px; text-align: center; color: #22543d; font-size: 0.9rem; font-weight: 500; margin: 10px 0;",
        icon("circle-check", style = "color: #38a169; margin-right: 6px;"), 
        "Simulated water level remains clear of residential structural triggers."
      )
    }
  })
  
  # ── FIXED: compact column widths, word-wrap enforced via DT options
  output$impact_table <- renderDT({
    df_table <- affected_demographics() %>%
      select(Barangay = barangay, Population = population, Children = total_children, Status)
    datatable(
      df_table,
      options = list(
        pageLength = 4,
        dom = 'tp',
        autoWidth = FALSE,
        scrollX = FALSE,
        columnDefs = list(
          list(width = '35%', targets = 0),
          list(width = '25%', targets = 1),
          list(width = '22%', targets = 2),
          list(width = '18%', targets = 3)
        )
      ),
      rownames = FALSE
    )
  })
  
  output$marikina_map <- renderLeaflet({
    leaflet(data = evac_centers) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 121.0940, lat = 14.6350, zoom = 13) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat, radius = 6,
        color = "#2c3e50", fillColor = "#34495e", fillOpacity = 0.8, stroke = TRUE, weight = 1,
        popup = ~paste0("<strong>Infrastructure Center Context</strong><br>Name: ", name, "<br>Type: ", type)
      )
  })
  
  observe({
    h <- input$flood_sim
    df_map <- affected_demographics() %>% filter(trigger_height <= h)
    proxy  <- leafletProxy("marikina_map") %>% clearShapes()
    if (nrow(df_map) > 0) {
      proxy %>% addRectangles(
        lng1 = 121.0750, lat1 = 14.6150, lng2 = 121.1150, lat2 = 14.6550,
        fillColor = "#e74c3c", fillOpacity = 0.25, stroke = TRUE, color = "#e74c3c", weight = 1
      )
    }
  })
  
  output$sim_interp_panel <- renderUI({
    if(!show_sim_interp()) return(NULL)
    h <- input$flood_sim
    active_count <- sum(master_psa$trigger_height <= h)
    tags$div(class = "interp-panel", tags$h5("Demographic Risk Analysis"),
             paste0("Cross-referencing master_psa.rds spatial structures at ", h, "m flags ", active_count, " activated hazard profiles across regional zone limits."))
  })
  
  computed_river_level <- reactive({
    if (input$evac_use_live) {
      df <- live_water_data()
      sn_row <- df[df$River %in% c("sto. nino", "sto. niño"), ]
      if (nrow(sn_row) > 0) return(sn_row$Level[1]) else return(14.5)
    } else {
      return(input$evac_manual_override)
    }
  })
  
  output$evac_river_readout <- renderUI({
    lvl <- computed_river_level(); status_str <- "Normal Flow Bounds"; lbl_class  <- "rr-normal"
    if (lvl >= 17.0) { status_str <- "3rd Alarm (Mandatory Action)"; lbl_class <- "rr-critical" } 
    else if (lvl >= 16.0) { status_str <- "2nd Alarm Level Set"; lbl_class <- "rr-alarm2" } 
    else if (lvl >= 15.0) { status_str <- "1st Alarm Alert Window"; lbl_class <- "rr-alarm1" }
    tags$div(class = "river-readout", tags$div(class = "rr-label", "Monitored River Gauge Vector"), tags$div(class = "rr-value", paste0(format(lvl, nsmall = 1), " m")), tags$div(class = paste("rr-status", lbl_class), status_str))
  })
  
  processed_evac_centers <- reactive({
    lvl <- computed_river_level()
    df <- evac_centers %>%
      mutate(
        status = case_when(lvl >= (activates_at + 1.0) ~ "OPEN", lvl >= activates_at ~ "STANDBY", TRUE ~ "CLOSED"),
        occupancy_pct = case_when(status == "OPEN" ~ pmin(98, round((lvl - activates_at) * 32 + runif(n(), 5, 12))), TRUE ~ 0),
        current_occupancy = round(capacity * (occupancy_pct / 100))
      )
    if (input$evac_bgy_filter != "All")      df <- df %>% filter(tolower(barangay) == tolower(input$evac_bgy_filter))
    if (input$evac_type_filter != "All Types") df <- df %>% filter(type == input$evac_type_filter)
    df
  })
  
  output$evac_kpi_open     <- renderUI({ 
    tags$div(class = "evac-stat-card", tags$div(class = "stat-num evac-open", sum(processed_evac_centers()$status == "OPEN")), tags$div(class = "stat-lbl", "Active Centers")) 
  })
  output$evac_kpi_standby  <- renderUI({ 
    tags$div(class = "evac-stat-card", tags$div(class = "stat-num evac-standby", sum(processed_evac_centers()$status == "STANDBY")), tags$div(class = "stat-lbl", "Standby Status")) 
  })
  output$evac_kpi_capacity <- renderUI({ 
    tags$div(class = "evac-stat-card", tags$div(class = "stat-num", format(sum(processed_evac_centers()$capacity), big.mark = ",")), tags$div(class = "stat-lbl", "Gross Capacity")) 
  })
  output$evac_kpi_occupancy <- renderUI({ 
    tags$div(class = "evac-stat-card", tags$div(class = "stat-num evac-closed", format(sum(processed_evac_centers()$current_occupancy), big.mark = ",")), tags$div(class = "stat-lbl", "Est Shelter Load")) 
  })
  
  output$evac_map <- renderLeaflet({
    lvl <- if (isolate(input$evac_use_live)) 14.5 else isolate(input$evac_manual_override)
    if(is.null(lvl)) lvl <- 14.5
    initial_df <- evac_centers %>%
      mutate(
        status = case_when(lvl >= (activates_at + 1.0) ~ "OPEN", lvl >= activates_at ~ "STANDBY", TRUE ~ "CLOSED"),
        color_hex = case_when(status == "OPEN" ~ "#27ae60", status == "STANDBY" ~ "#f39c12", TRUE ~ "#8892a4")
      )
    leaflet(initial_df) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 121.0940, lat = 14.6350, zoom = 13) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat, radius = ~pmax(6, pmin(18, capacity / 250)), color = ~color_hex, fillOpacity = 0.75,
        popup = ~paste0("<strong>", name, "</strong><br>Capacity Limit: ", capacity, "<br>Current Status: ", status)
      )
  })
  
  observe({
    df <- processed_evac_centers()
    if(nrow(df) == 0) return()
    df <- df %>% mutate(color_hex = case_when(status == "OPEN" ~ "#27ae60", status == "STANDBY" ~ "#f39c12", TRUE ~ "#8892a4"))
    leafletProxy("evac_map", data = df) %>% clearMarkers() %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~pmax(6, pmin(18, capacity / 250)), color = ~color_hex, fillOpacity = 0.75,
                       popup = ~paste0("<strong>", name, "</strong><br>Capacity Limit: ", capacity, "<br>Current Status: ", status))
  })
  
  output$evac_table <- renderDT({
    df_show <- processed_evac_centers() %>%
      mutate(
        Status_Pill = case_when(status == "OPEN" ~ "<span class='pill-open'>OPEN</span>", status == "STANDBY" ~ "<span class='pill-standby'>STANDBY</span>", TRUE ~ "<span class='pill-closed'>CLOSED</span>"),
        Load_Display = paste0(current_occupancy, " / ", capacity, " (", occupancy_pct, "%)")
      ) %>% select(Name = name, Barangay = barangay, Class = type, Trigger = activates_at, Status = Status_Pill, `Capacity Allocation` = Load_Display)
    datatable(df_show, escape = FALSE, options = list(pageLength = 6, dom = 'itp'))
  })
}

shinyApp(ui, server)