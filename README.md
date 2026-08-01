# Stardew Valley Recipe Finder

An interactive R Shiny application that helps players discover which Stardew Valley recipes they can make from the ingredients they have, identify recipes that are only one or two ingredients away, browse the complete recipe catalog, and explore ingredient statistics.

## Live app

This repository is prepared for deployment on [Posit Connect Cloud](https://connect.posit.cloud/). A public app link will be added here after the initial deployment.

## R and Shiny skills demonstrated

- Reactive application design with `shiny`
- Interactive tables and client/server events with `DT`
- Data transformation with `dplyr`, `purrr`, and `stringr`
- Dynamic data visualization with `ggplot2`
- Responsive Bootstrap styling with `bslib`
- Reproducible deployment through a Posit Connect `manifest.json`
- Tidy, relational data modeling across recipes, ingredients, and categories

## Features

- **Can Make Now:** finds recipes whose requirements are satisfied by selected ingredients
- **Almost There:** identifies recipes missing only one or two ingredients
- **All Recipes:** provides a searchable catalog of 81 recipes
- **Statistics:** visualizes common ingredients and recipe coverage
- **Ingredient Explorer:** displays categorized ingredient metadata and supports navigation from recipe tables

## Run locally

Install the required packages, clone this repository, and run:

```r
shiny::runApp()
```

The app reads bundled `.rds` files from `data/` and serves image assets from `www/`; it does not require API keys or a database.

## Deploy

This repository follows Posit's Git-backed deployment layout:

1. Connect this public GitHub repository to Posit Connect Cloud.
2. Select **Shiny** as the content type.
3. Select `app.R` as the primary file.
4. Publish.

The checked-in `manifest.json` records the R and package dependencies needed to rebuild the app.

## Project history and attribution

This app was originally created as a Fall 2025 STA 523 final project at Duke University by:

- Franklin Zhou — <franklin.zhou@duke.edu>
- Simeng Wu — <simeng.wu@duke.edu>
- Carol Zhou — <carol.zhou@duke.edu>
- Cecilia Liu — <cecilia.liu@duke.edu>
- Wenjie Gong — <wenjie.gong@duke.edu>

Repackaged by Franklin Zhou in August 2026 for the purpose of a public portfolio. This repository contains only the files required to run and deploy the Shiny application; the original course repository and its history remain private.

Recipe data and images were sourced from the [Stardew Valley Wiki cooking page](https://stardewvalleywiki.com/Cooking). Stardew Valley and its visual assets are the property of their respective rights holders. This is an independent educational project and is not affiliated with or endorsed by ConcernedApe.

## License

The original application code and repository documentation are available under the [MIT License](LICENSE). Third-party data, names, and image assets remain subject to their respective owners' terms and are not relicensed by this repository.
