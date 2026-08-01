# Stardew Valley Recipe Finder
library(shiny)
library(dplyr)
library(DT)
library(bslib)
library(ggplot2)
library(stringr)
library(purrr)

# Load data
recipes <- readRDS("data/recipes.rds")
ingredients_data <- readRDS("data/recipe_ingredients.rds")
all_ingredients <- sort(unique(ingredients_data$ingredient))
ingredient_imgs <- readRDS("data/ingredient_images.rds")

# Load Ingredient Category Tables for Explorer
ingredient_tables <- readRDS("data/all_ingredient_tables.rds")
# Combine separate category tables into one dataframe for the search bar
ingredient_names <- purrr::map_dfr(ingredient_tables, ~ select(.x, category, everything())) %>%
  select(category, name = dplyr::matches("name", ignore.case = TRUE)) %>%
  filter(!is.na(name)) %>%
  distinct()
ingredient_categories <- sort(unique(ingredient_names$category))

# Create clickable ingredient display for recipes
recipe_ingredient_display <- ingredients_data %>%
  left_join(ingredient_imgs, by = "ingredient") %>%
  mutate(
    img_html = case_when(
      !is.na(image_path) ~ paste0('<img src="images/ingredients/', basename(image_path), '" height="25" style="vertical-align:middle; margin-right:5px;">'),
      TRUE ~ ""
    ),
    # Make ingredient name clickable by constructing a custome HTML link
    # Links to Ingredient Explorer
    single_ing_html = sprintf(
      '%s<a href="#" onclick="Shiny.setInputValue(\'switch_to_explorer\', \'%s\', {priority: \'event\'}); return false;" style="color:#0066cc; text-decoration:underline; cursor:pointer;">%s</a> (%s)',
      img_html, ingredient, ingredient, quantity
    )
  ) %>%
  group_by(recipe_id) %>%
  summarize(
    ingredients_html = paste(single_ing_html, collapse = "<br>"),
    .groups = "drop"
  )

# Extracts the list of buffs (Mining, Farming, etc.) from the recipes to populate the filter dropdown.
all_buffs <- recipes %>%
  pull(buff) %>%
  .[. != "N/A"] %>%
  str_split("\n") %>%
  unlist() %>%
  # Regex to clean buff strings: converts "+2 Fishing (+5m)" to just "Fishing"
  str_extract("^[A-Za-z ]+") %>%
  str_trim() %>%
  unique() %>%
  sort()

# A hardcoded list of items that satisfy the "Any Fish" requirement in recipes
fish_list <- c("Salmon", "Tuna", "Bass", "Catfish", "Pike", "Sunfish", "Sardine", 
               "Anchovy", "Carp", "Herring", "Eel", "Octopus", "Squid", "Sea Cucumber",
               "Super Cucumber", "Ghostfish", "Stonefish", "Ice Pip", "Lava Eel",
               "Scorpion Carp", "Flounder", "Midnight Carp", "Sturgeon", "Tiger Trout",
               "Bullhead", "Tilapia", "Chub", "Dorado", "Albacore", "Shad", "Lingcod",
               "Halibut", "Hardwood", "Legend", "Angler", "Crimsonfish", "Glacierfish",
               "Mutant Carp", "Blobfish", "Slimejack", "Midnight Squid", "Spook Fish",
               "Stingray", "Lionfish", "Blue Discus", "Pufferfish", "Perch", "Rainbow Trout",
               "Red Mullet", "Red Snapper", "Sandfish", "Void Salmon", "Woodskip")

# Helper Functions
# Takes the user's ingredients and filters the recipe list
# Returns only recipes where ALL required ingredients are present
find_makeable_recipes <- function(selected_ing, buff_filter = NULL, min_energy = 0, min_health = 0, show_all = FALSE) {
  if (show_all) {
    recipes_can_make <- recipes %>%
      select(recipe_id, image_path, name, description, energy, health, buff, buff_duration, sell_price)
  } else {
    # Check if ANY fish in the selected list matches the game's "Any Fish" category logic
    has_fish <- any(selected_ing %in% fish_list)
    
    recipes_can_make <- ingredients_data %>%
      group_by(recipe_id) %>%
      summarize(
        # Ingredient is available if it's in the user's list OR if the recipe calls for "Any Fish" and the user has at least one fish
        all_available = all(ingredient %in% selected_ing | (ingredient == "Any Fish" & has_fish)),
        .groups = "drop"
      ) %>%
      filter(all_available) %>%
      left_join(recipes, by = "recipe_id") %>%
      select(recipe_id, image_path, name, description, energy, health, buff, buff_duration, sell_price)
  }
  
  if (nrow(recipes_can_make) > 0) {
    recipes_can_make <- recipes_can_make %>%
      filter(energy >= min_energy, health >= min_health)
  }
  
  if (!is.null(buff_filter) && length(buff_filter) > 0) {
    # Filter rows where the buff column contains ANY of the selected buffs
    recipes_can_make <- recipes_can_make %>%
      filter(str_detect(buff, paste(buff_filter, collapse = "|")))
  }
  
  return(recipes_can_make)
}

# Calculates the difference between required ingredients and user ingredients
# Returns recipes missing exactly 'max_missing' (default 2) items
find_almost_recipes <- function(selected_ing, max_missing = 2, buff_filter = NULL, min_energy = 0, min_health = 0) {
  if (length(selected_ing) == 0) {
    return(tibble())
  }
  
  has_fish <- any(selected_ing %in% fish_list)
  
  recipes_almost <- ingredients_data %>%
    group_by(recipe_id) %>%
    summarize(
      num_required = n(),
      num_have = sum(ingredient %in% selected_ing | (ingredient == "Any Fish" & has_fish)),
      num_missing = num_required - num_have,
      # Store missing ingredients as a nested list to be processed later for display
      missing_ingredients = list(ingredient[!(ingredient %in% selected_ing) & !(ingredient == "Any Fish" & has_fish)]),
      .groups = "drop"
    ) %>%
    filter(num_missing > 0 & num_have > 0 & num_missing <= max_missing) %>%
    left_join(recipes, by = "recipe_id") %>%
    mutate(
      missing_list = sapply(missing_ingredients, function(x) paste(x, collapse = ", "))
    ) %>%
    select(recipe_id, image_path, name, missing_list, num_missing, energy, health, buff, buff_duration, sell_price) %>%
    arrange(num_missing, name)
  
  if (nrow(recipes_almost) > 0) {
    recipes_almost <- recipes_almost %>%
      filter(energy >= min_energy, health >= min_health)
  }
  
  if (!is.null(buff_filter) && length(buff_filter) > 0) {
    recipes_almost <- recipes_almost %>%
      filter(str_detect(buff, paste(buff_filter, collapse = "|")))
  }
  
  return(recipes_almost)
}

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    success = "#18BC9C"
  ),
  
  # CUSTOM CSS that defines the gradient header, centers recipe images, and formats the counts
  tags$head(
    tags$style(HTML("
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        text-align: center;
      }
      .ingredient-count {
        font-size: 18px;
        font-weight: bold;
        color: #2C3E50;
        margin-top: 10px;
      }
      .recipe-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
      }
    "))
  ),
  
  # Sidebar for filters/inputs; Main Panel for tabs/results
  div(class = "main-title",
      h1("🍳 Stardew Valley Recipe Finder"),
      p("Find recipes based on the ingredients you have!")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Select Your Ingredients"),
      
      selectInput("ingredient_select",
                  "Search ingredients:",
                  choices = all_ingredients,
                  multiple = TRUE,
                  selected = NULL),
      
      hr(),
      
      h5("Quick Add:"),
      actionButton("add_staples", 
                   "🥚 Add Staples", 
                   class = "btn-primary btn-sm",
                   style = "margin-bottom: 5px; width: 100%;"),
      actionButton("add_dairy", 
                   "🥛 Add Dairy", 
                   class = "btn-info btn-sm",
                   style = "margin-bottom: 5px; width: 100%;"),
      actionButton("add_veggies", 
                   "🥬 Add Veggies", 
                   class = "btn-success btn-sm",
                   style = "margin-bottom: 5px; width: 100%;"),
      actionButton("add_all", 
                   "📖 Show All Recipes", 
                   class = "btn-secondary btn-sm",
                   style = "width: 100%;"),
      
      hr(),
      
      sliderInput("energy_filter",
                  "Min Energy:",
                  min = 0,
                  max = 270,
                  value = 0,
                  step = 10),
      
      sliderInput("health_filter",
                  "Min Health:",
                  min = 0,
                  max = 120,
                  value = 0,
                  step = 10),
      
      selectInput("buff_select",
                  "Filter by Buff:",
                  choices = c("All" = "", all_buffs),
                  selected = NULL,
                  multiple = TRUE),
      
      hr(),
      
      actionButton("clear_all", "Clear All", class = "btn-warning", style = "width: 100%;"),
      
      hr(),
      
      div(class = "ingredient-count",
          textOutput("selected_count")
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Defines the 5 distinct views (Can Make, Almost, All, Stats, Explorer)
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("✅ Can Make Now",
                 br(),
                 h4("Recipes you can make right now:"),
                 DTOutput("can_make_table")
        ),
        
        tabPanel("🔜 Almost There",
                 br(),
                 h4("You're close! Missing just 1-2 ingredients:"),
                 DTOutput("almost_table")
        ),
        
        tabPanel("📖 All Recipes",
                 br(),
                 h4("Browse all available recipes:"),
                 DTOutput("all_recipes_table")
        ),
        
        tabPanel("📊 Statistics",
                 br(),
                 h4("Recipe Statistics"),
                 plotOutput("ingredient_freq_plot", height = "400px"),
                 hr(),
                 h4("Your Ingredient Coverage"),
                 plotOutput("coverage_plot", height = "300px")
        ),
        
        tabPanel("🧺 Ingredient Explorer",
                 br(),
                 fluidRow(
                   column(5,
                          selectizeInput("ingredient_search",
                                         "Search Ingredient Names:",
                                         choices = sort(unique(ingredient_names$name)),
                                         multiple = TRUE,
                                         options = list(placeholder = "Select one or more ingredients..."))
                   ),
                   column(5,
                          selectInput("ingredient_category",
                                      "Filter by Category:",
                                      choices = c("All", ingredient_categories),
                                      selected = "All")
                   ),
                   column(2,
                          br(),
                          actionButton("clear_explorer", 
                                       "Clear Search", 
                                       class = "btn-warning btn-sm",
                                       style = "width: 100%; margin-top: 5px;")
                   )
                 ),
                 hr(),
                 uiOutput("ingredient_tables_ui")
        )
      )
    )
  ),
  div(
    style = "text-align:center; margin-top:30px; padding:10px; 
           font-size:11px; color:#999;",
    HTML("Data and images sourced from the 
        <a href='https://stardewvalleywiki.com/Cooking' target='_blank'>
        Stardew Valley Wiki – Cooking page</a>.")
  )
)

server <- function(input, output, session) {
  
  show_all_recipes <- reactiveVal(FALSE)
  
  # Quick add buttons
  # They take the current list of selected ingredients, add specific items (like Staples or Dairy), and update the input
  observeEvent(input$add_staples, {
    staples <- c("Egg", "Milk", "Wheat Flour", "Sugar")
    current <- input$ingredient_select
    updated <- unique(c(current, staples))
    updateSelectInput(session, "ingredient_select", selected = updated)
    show_all_recipes(FALSE)
  })
  
  observeEvent(input$add_dairy, {
    dairy <- c("Milk", "Cheese", "Goat Milk", "Goat Cheese")
    current <- input$ingredient_select
    updated <- unique(c(current, dairy))
    updateSelectInput(session, "ingredient_select", selected = updated)
    show_all_recipes(FALSE)
  })
  
  observeEvent(input$add_veggies, {
    veggies <- c("Tomato", "Potato", "Cauliflower", "Kale", "Parsnip")
    current <- input$ingredient_select
    updated <- unique(c(current, veggies))
    updateSelectInput(session, "ingredient_select", selected = updated)
    show_all_recipes(FALSE)
  })
  
  observeEvent(input$add_all, {
    show_all_recipes(TRUE)
    updateSelectInput(session, "ingredient_select", selected = character(0))
  })
  
  observeEvent(input$clear_all, {
    updateSelectInput(session, "ingredient_select", selected = character(0))
    updateSliderInput(session, "energy_filter", value = 0)
    updateSliderInput(session, "health_filter", value = 0)
    updateSelectInput(session, "buff_select", selected = character(0))
    show_all_recipes(FALSE)
  })
  
  observeEvent(input$clear_explorer, {
    updateSelectizeInput(session, "ingredient_search", selected = character(0))
    updateSelectInput(session, "ingredient_category", selected = "All")
  })
  
  # Handle ingredient click - switch to Explorer tab
  observeEvent(input$switch_to_explorer, {
    # Captures the JavaScript event sent from the HTML link in the DT table
    ingredient_clicked <- input$switch_to_explorer
    updateTabsetPanel(session, "main_tabs", selected = "🧺 Ingredient Explorer")
    updateSelectizeInput(session, "ingredient_search", selected = ingredient_clicked)
  })
  
  # Warns the user if they try to filter energy/health without selecting any ingredients first
  observe({
    has_filters <- input$energy_filter > 0 || input$health_filter > 0 || length(input$buff_select) > 0
    
    if (has_filters && length(input$ingredient_select) == 0 && !show_all_recipes()) {
      showNotification(
        "💡 To use filters, please first select ingredients or click 'Show All Recipes'",
        type = "warning",
        duration = 4
      )
    }
  })
  
  observeEvent(input$ingredient_select, {
    if (length(input$ingredient_select) > 0) {
      show_all_recipes(FALSE)
    }
  })
  
  output$selected_count <- renderText({
    if (show_all_recipes()) {
      "🌟 Showing all recipes"
    } else {
      count <- length(input$ingredient_select)
      if (count == 0) {
        "No ingredients selected"
      } else {
        paste("🎒", count, "ingredients selected")
      }
    }
  })
  
  # Table: Can Make Now
  # Calls find_makeable_recipes() and formats the result as a DataTable
  output$can_make_table <- renderDT({
    if (!show_all_recipes() && length(input$ingredient_select) == 0) {
      return(datatable(
        data.frame(Message = "👈 Select ingredients or click 'Show All Recipes' to browse with filters!"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    makeable <- find_makeable_recipes(input$ingredient_select, input$buff_select, 
                                      input$energy_filter, input$health_filter, 
                                      show_all = show_all_recipes())
    
    if (nrow(makeable) == 0) {
      return(datatable(
        data.frame(Message = "No recipes match your filters! Try adjusting them. 🔍"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    makeable <- makeable %>%
      left_join(recipe_ingredient_display, by = "recipe_id") %>%
      mutate(
        Image = sprintf('<img src="images/recipes/%s" height="40" class="recipe-image">', basename(image_path)),
        buff = str_replace_all(buff, "\n", "<br>")
      ) %>%
      select(Image, name, ingredients_html, energy, health, buff, buff_duration, sell_price)
    
    datatable(
      makeable,
      options = list(
        pageLength = 10, 
        dom = 'ftp',
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = 0))
      ),
      rownames = FALSE,
      # Allows the <img> and <a> tags we built to render as HTML instead of text
      escape = FALSE, 
      colnames = c("", "Recipe", "Ingredients", "Energy", "Health", "Buff", "Duration", "Sell Price")
    )
  })
  
  # Table: Almost There
  # Calls find_almost_recipes() and dynamically generates images for the missing items
  output$almost_table <- renderDT({
    almost <- find_almost_recipes(input$ingredient_select, max_missing = 2, input$buff_select, 
                                  input$energy_filter, input$health_filter)
    
    if (nrow(almost) == 0) {
      return(datatable(
        data.frame(Message = "Add more ingredients to see recipes you're close to making! 🔍"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    get_quantity <- function(rec_id, ing_name) {
      q <- ingredients_data %>% 
        filter(recipe_id == rec_id, ingredient == ing_name) %>% 
        pull(quantity)
      if(length(q) > 0) return(q[1]) else return(1)
    }
    
    # Create clickable missing ingredients
    # Iterates over the list of missing ingredients for every recipe row
    almost$Missing_Images <- mapply(function(missing_text, rec_id) {
      ingredients <- str_split(missing_text, ", ")[[1]]
      
      # Normalize naming conventions  to match file paths
      html_parts <- sapply(ingredients, function(ing) {
        qty <- get_quantity(rec_id, ing)
        
        ing_lower <- tolower(ing)
        if (ing_lower == "any fish") {
          ing_clean <- "sunfish"
        } else {
          ing_clean <- tolower(str_replace_all(ing, " ", "_"))
        }
        img_file <- paste0("images/ingredients/ingredient_", ing_clean, ".png")
        
        # Make clickable with link to explorer
        # Conditionally render image + link only if the image file actually exists
        if (file.exists(paste0("www/", img_file))) {
          sprintf('<img src="%s" height="25" style="vertical-align:middle; margin-right:5px;"><a href="#" onclick="Shiny.setInputValue(\'switch_to_explorer\', \'%s\', {priority: \'event\'}); return false;" style="color:#0066cc; text-decoration:underline; cursor:pointer;">%s</a> (%s)', 
                  img_file, ing, ing, qty)
        } else {
          sprintf('<span style="color:#666;">🥘 <a href="#" onclick="Shiny.setInputValue(\'switch_to_explorer\', \'%s\', {priority: \'event\'}); return false;" style="color:#0066cc; text-decoration:underline;">%s</a> (%s)</span>', 
                  ing, ing, qty)
        }
      })
      
      paste(html_parts, collapse = "<br>")
    }, almost$missing_list, almost$recipe_id)
    
    almost <- almost %>%
      mutate(
        Image = sprintf('<img src="images/recipes/%s" height="40" class="recipe-image">', basename(image_path)),
        buff = str_replace_all(buff, "\n", "<br>")
      ) %>%
      select(Image, name, Missing_Images, energy, health, buff, buff_duration, sell_price)
    
    datatable(
      almost,
      options = list(
        pageLength = 10, 
        dom = 'ftp',
        columnDefs = list(
          list(className = 'dt-center', targets = 0),
          list(width = '250px', targets = 2) 
        )
      ),
      rownames = FALSE,
      escape = FALSE,
      colnames = c("", "Recipe", "Missing Ingredients", "Energy", "Health", "Buff", "Duration", "Sell Price")
    )
  })
  
  # Table: All Recipes
  # Simple dump of all data, no logic required
  output$all_recipes_table <- renderDT({
    all_recipes_display <- recipes %>%
      left_join(recipe_ingredient_display, by = "recipe_id") %>%
      mutate(
        Image = sprintf('<img src="images/recipes/%s" height="40" class="recipe-image">', basename(image_path)),
        buff = str_replace_all(buff, "\n", "<br>"),
        ingredients_html = coalesce(ingredients_html, "<i>No ingredients listed</i>")
      ) %>%
      select(Image, name, ingredients_html, energy, health, buff, buff_duration, sell_price)
    
    datatable(
      all_recipes_display,
      options = list(
        pageLength = 15, 
        dom = 'ftp',
        columnDefs = list(
          list(className = 'dt-center', targets = 0),
          list(width = '300px', targets = 2) 
        )
      ),
      rownames = FALSE,
      escape = FALSE,
      colnames = c("", "Recipe", "Ingredients Needed", "Energy", "Health", "Buff", "Duration", "Sell Price")
    )
  })
  
  # Plot: Ingredient frequency
  # Standard ggplot bar chart for most common ingredients
  output$ingredient_freq_plot <- renderPlot({
    ingredient_counts <- ingredients_data %>%
      count(ingredient, sort = TRUE) %>%
      head(15)
    
    ggplot(ingredient_counts, aes(x = reorder(ingredient, n), y = n)) +
      geom_col(fill = "#667eea") +
      geom_text(aes(label = n), hjust = -0.3, size = 4) +
      coord_flip() +
      labs(
        title = "Most Common Ingredients in Recipes",
        x = NULL,
        y = "Number of Recipes"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
      )
  })
  
  # Plot: Coverage
  # Calculates stats on how many recipes are makeable/unmakeable and plots a stacked bar
  output$coverage_plot <- renderPlot({
    if (length(input$ingredient_select) == 0) {
      plot.new()
      text(0.5, 0.5, "Select ingredients to see your coverage!", cex = 1.5)
      return()
    }
    
    total_recipes <- nrow(recipes)
    can_make <- nrow(find_makeable_recipes(input$ingredient_select))
    almost <- nrow(find_almost_recipes(input$ingredient_select, max_missing = 2))
    cant_make <- total_recipes - can_make - almost
    
    coverage_data <- data.frame(
      category = c("Can Make Now", "Close (1-2 missing)", "Need More Ingredients"),
      count = c(can_make, almost, cant_make),
      color = c("#18BC9C", "#F39C12", "#E74C3C")
    )
    
    ggplot(coverage_data, aes(x = "", y = count, fill = category)) +
      geom_col(width = 1) +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 6, fontface = "bold") +
      coord_flip() +
      scale_fill_manual(values = coverage_data$color) +
      labs(
        title = paste("Recipe Coverage with", length(input$ingredient_select), "Ingredients"),
        x = NULL,
        y = "Number of Recipes",
        fill = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "bottom"
      )
  })
  
  # Ingredient Explorer (separated by category with formatting)
  # Loops through categories (Crops, Fish, etc.) and generates a separate table for each one
  # Allows us to display different columns for different categories
  output$ingredient_tables_ui <- renderUI({
    cat_choice <- input$ingredient_category
    name_choice <- input$ingredient_search
    
    # Determine which categories to show
    tables_to_show <- if (cat_choice == "All") ingredient_tables else ingredient_tables[cat_choice]
    
    all_tables <- tagList()
    
    # Loop through each category to display separately
    for (cat_name in names(tables_to_show)) {
      df <- as.data.frame(tables_to_show[[cat_name]])
      name_col <- grep("name", names(df), ignore.case = TRUE, value = TRUE)[1]
      
      # Filter by selected ingredient names
      if (!is.null(name_choice) && length(name_choice) > 0 && !is.na(name_col)) {
        df <- df %>% dplyr::filter(!!sym(name_col) %in% name_choice)
      }
      
      # Join images if available
      if (!is.na(name_col) && name_col %in% names(df)) {
        df <- df %>%
          dplyr::left_join(
            ingredient_imgs %>%
              dplyr::mutate(
                web_path = paste0("images/ingredients/", basename(image_path)),
                img_tag  = paste0('<img src="', web_path,
                                  '" height="35" style="vertical-align:middle;">')
              ),
            by = setNames("ingredient", name_col)
          ) %>%
          dplyr::mutate(Image = if_else(!is.na(img_tag), img_tag, "")) %>%
          dplyr::relocate(Image, .before = 1)
      }
      
      # Formatting and cleanup
      # Fix spacing in required_for and ingredients columns (adds a space after parentheses)
      if ("required_for" %in% names(df)) {
        df <- df %>%
          mutate(required_for = str_replace_all(required_for, "\\)([A-Z])", ") \\1"))
      }
      if ("ingredients" %in% names(df)) {
        df <- df %>%
          mutate(ingredients = str_replace_all(ingredients, "\\)([A-Za-z])", ") \\1"))
      }
      
      # Drop empty or duplicate columns
      df <- df %>%
        dplyr::select(where(~ any(!is.na(.x) & .x != ""))) %>%
        dplyr::select(!matches("(^image_url$|image_path|image_filename|web_path|img_tag|\\.x$|\\.y$|^extra_)"))
      
      # Rename columns for cleaner display
      rename_map <- c(
        "Category" = "category",
        "Season" = "season",
        "Growth Time" = "growth_time",
        "Notes" = "notes",
        "Requires for" = "required_for",
        "Amount Needed" = "amount_needed",
        "Location"  = "location",
        "Source" = "source",
        "Input Item" = "input_item",
        "Producing Time" = "producing_time",
        "Time" = "time",
        "Weather" = "weather",
        "Difficulty" = "difficulty",
        "Price per Unit Total" = "price_per_unittotal",
        "Ingredients" = "ingredients"
      )
      if (!is.na(name_col)) rename_map["Name"] <- name_col
      df <- df %>% dplyr::rename(any_of(rename_map))
      
      # Skip empty categories
      if (nrow(df) == 0) next
      
      # Add category heading and its table
      all_tables <- tagAppendChild(
        all_tables,
        tagList(
          h4(strong(cat_name)),
          DTOutput(paste0("table_", make.names(cat_name))),
          hr()
        )
      )
      
      # Separate renderDT for each table using local({}) which creates a new environment for each iteration of the loop
      local({
        my_cat <- cat_name
        my_df <- df
        output[[paste0("table_", make.names(my_cat))]] <- renderDT({
          datatable(
            my_df,
            escape = FALSE,
            rownames = FALSE,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              autoWidth = TRUE
            )
          )
        })
      })
    }
    
    # If no results found
    if (length(all_tables) == 0) {
      all_tables <- tagList(
        h5("No data found for this selection.", style = "color: #888; text-align:center;")
      )
    }
    
    all_tables
  })
}

shinyApp(ui = ui, server = server)
