library(blogdown)

# See help for creating new site
?blogdown::new_site()

# NOT RUN
# For first time if directory was not created when creating the new project:
new_site(
  dir = ".",
  force = NA,
  install_hugo = TRUE,
  format = "yaml",
  sample = TRUE,
  theme = "color-your-world",
  hostname = "github.com",
  theme_example = TRUE,
  empty_dirs = FALSE,
  to_yaml = TRUE,
  netlify = TRUE,
  .Rprofile = TRUE,
  serve = if (interactive()) "ask" else FALSE
)

# New theme
blogdown::install_theme(theme = "hugo-sid/hugo-blog-awesome")

# Preview site
blogdown::serve_site()

# Stop preview
blogdown::stop_server()

# Check any troubleshooting
blogdown::check_site()


