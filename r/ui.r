
ui <- tagList(
  includeCSS('www/style.css'),
  includeScript('www/script.js'),
  navbarPage(
    title='Bed-Net Optimisation App',
    
    
    # WELCOME ####
    
    tabPanel(
      'Welcome',
      img(src='syngenta-logo.png', alt="Syngenta logo", height='60px'),
      includeHTML('www/welcome.html')
    ),
    
    
    # SIMULATION ####
    
    tabPanel(
      'Simulation',
      
      fluidRow(
        column(
          width=2,
          class='center-content',
          actionButton('restore_defaults', 'Restore Defaults', class='btn-primary'),
          actionButton('run_sim', 'Run Simulation', class='btn-success sim-button'),
          textInput('sim_name', 'Simulation name', value='sim_name')
        )
      ),
      
      navlistPanel(
        id='settings_panel',
        widths=c(2, 10),
        
        tabPanel(
          'Basic',
          div(
            class='sim-settings',
            with_info(
              radioButtons('os', 'Resistance-management strategy', c('Mixture', 'Rotation', 'Mosaic'))
            ),
            with_info(
              numericInput(
                'xb',
                'Public health budget per household for purchasing bed-nets in USD',
                value=2,
                step = 0.1
              )
            ),
            with_info(
              radioButtons(
                'iA',
                'First Insecticide',
                c('Pyrethroid', 'PBO', 'Chlorfenapyr', 'Pyriproxyfen', 'New AI 1', 'New AI 2'),
                selected='Pyrethroid'
              )
            ),
            with_info(
              radioButtons(
                'iB',
                'Second Insecticide',
                c('Pyrethroid', 'PBO', 'Chlorfenapyr', 'Pyriproxyfen', 'New AI 1', 'New AI 2'),
                selected='New AI 1'
              )
            )
          )
        ),
        
        tabPanel(
          'Advanced',
          fluidRow(
            class='sim-settings',
            column(
              width=6,
              # class='side-label',
              a(
                `data-toggle`="collapse", href="#collapse101", style="text-decoration: none", 
                h2('Mosquito ecology and genetics')
              ),
              div(
                id="collapse101", class="collapse in",
                with_info(numericInput('og', PARAMETER_MAP[['og']], value=12, step=1)),
                with_info(numericInput('er', PARAMETER_MAP[['er']], value=1, step=0.1)),
                with_info(numericInput('ek', PARAMETER_MAP[['ek']], value=10^9, step=10^8)),
                with_info(numericInput('ou', PARAMETER_MAP[['ou']], value=10^-6, step=10^-6)),
                with_info(numericInput('el', PARAMETER_MAP[['el']], value=0.5, step=0.05)),
                with_info(numericInput('mrx1', PARAMETER_MAP[['mrx1']], value=2, step=1))
              ),
              a(
                `data-toggle`="collapse", href="#collapse102", style="text-decoration: none", 
                h2('Optimisation range and precision')
              ),
              div(
                id="collapse102", class="collapse",
                with_info(numericInput('oy', PARAMETER_MAP[['oy']], value=12, step=0.25)),
                with_info(numericInput('ol', PARAMETER_MAP[['ol']], value=4, step=0.2)),
                with_info(numericInput('oi_b', PARAMETER_MAP[['oi_b']], value=0.25, step=0.05)),
                with_info(numericInput('oi_c', PARAMETER_MAP[['oi_c']], value=0.2, step=0.05))
              )
            ),
            column(
              width=6,
              # class='side-label',
              a(
                `data-toggle`="collapse", href="#collapse103", style="text-decoration: none",
                h2('Bed-net economics and usage')
              ),
              div(
                id="collapse103", class="collapse",
                with_info(numericInput('xn', PARAMETER_MAP[['xn']], value=1.8, step=0.1)),
                with_info(numericInput('xs1', PARAMETER_MAP[['xs1']], value=0, step=0.1)),
                with_info(numericInput('xs2', PARAMETER_MAP[['xs2']], value=0, step=0.1)),
                with_info(numericInput('pm', PARAMETER_MAP[['pm']], value=0.88, step=0.01)),
                with_info(numericInput('p0', PARAMETER_MAP[['p0']], value=0.995711963, step=10^-9)),
                with_info(numericInput('p3', PARAMETER_MAP[['p3']], value=0.464204692, step=10^-9))
              )
            )
          )
        ),
        
        tabPanel(
          'Expert',
          fluidRow(
            class='sim-settings',
            column(
              width=6,
              actionButton('plot_test1', 'Test new insecticide', class='btn-primary bottom-space'),
              with_info(numericInput('mrx6', PARAMETER_MAP[['mrx6']], value=0)),
              with_info(textInput('iX6', PARAMETER_MAP[['iX6']], value="New AI 1")),
              with_info(textInput('iX6_abb', PARAMETER_MAP[['iX6_abb']], value="NW1")),
              with_info(numericInput('cd6', PARAMETER_MAP[['cd6']], value=0.02067709, step=10^-8)),
              with_info(numericInput('md6', PARAMETER_MAP[['md6']], value=-0.0312781, step=10^-7)),
              with_info(numericInput('mh6', PARAMETER_MAP[['mh6']], value=2.512306, step=10^-6)),
              with_info(numericInput('xu6', PARAMETER_MAP[['xu6']], value=2.50, step=0.1))
            ),
            column(
              width=6,
              actionButton('plot_test2', 'Test new insecticide', class='btn-primary bottom-space'),
              with_info(numericInput('mrx7', PARAMETER_MAP[['mrx7']], value=0)),
              with_info(textInput('iX7', PARAMETER_MAP[['iX7']], value="New AI 2")),
              with_info(textInput('iX7_abb', PARAMETER_MAP[['iX7_abb']], value="NW2")),
              with_info(numericInput('cd7', PARAMETER_MAP[['cd7']], value=0.02067709, step=10^-8)),
              with_info(numericInput('md7', PARAMETER_MAP[['md7']], value=-0.0312781, step=10^-7)),
              with_info(numericInput('mh7', PARAMETER_MAP[['mh7']], value=2.512306, step=10^-6)),
              with_info(numericInput('xu7', PARAMETER_MAP[['xu7']], value=2.50, step=0.1))
            )
          )
        )
      )
    ),
    
    
    # RESULTS ####
    
    tabPanel(
      'Results',
      
      navlistPanel(
        widths=c(2, 10),
        
        tabPanel(
          'Summary',
          tags$details(OUTPUT_INFO[['table_summary']]),
          lapply(1:N_SIM_ROWS, function(x){
            div(
              a(
                `data-toggle`="collapse", href=paste0("#collapse",50+x), style="text-decoration: none", 
                h2(textOutput(paste0('table_simname', x)))
              ),
              div(
                id=paste0("collapse",50+x), class="collapse in",
                DT::dataTableOutput(paste0('summary', x))
              )
            )
          })
        ),
        
        make_results_tab(title='Baseline (BL)', prefix='base', 1),
        make_results_tab(title='Maximum Vector Control (MC)', prefix='mvc', 2),
        make_results_tab(title='Cheapest Bed-net that satisfies WHO Requirements (CW)', prefix='cheapest', 3),
        make_results_tab(title='Maximum Vector Control that satisfies WHO Requirements (MW)', prefix='who', 4),
        make_results_tab(title='Maximum Vector Control with Universal Coverage (UC)', prefix='universal', 5),
        
        tabPanel(
          'Saved Simulations',
          actionButton('show_export_modal', 'Export simulations', class='btn-primary bottom-space'),
          actionButton('show_import_modal', 'Import simulations', class='btn-primary bottom-space'),
          tagList(lapply(1:N_SIM_ROWS, function(x){
            div(
              selectInput(
                paste0('sim_selected', x),
                'Select a simulation',
                choices=NA
              ),
              htmlOutput(paste0('sim_details', x)),
              actionButton(paste0('del_modal', x), 'Delete this simulation', class='btn-warning'),
              hr()
            )
          }))
        )
      )
    )
  )
)
