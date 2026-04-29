# RAIPlot 🤖📊

**RAIPlot** is an RStudio Addin that uses OpenAI's GPT models to automatically generate beautiful, adaptive ggplot2 visualizations based on your data and natural language descriptions.

## Features

✨ **AI-Powered**: Uses OpenAI GPT-3.5 or GPT-4 to generate plotting code
🎨 **Adaptive**: Automatically adapts to your data structure and types
📊 **ggplot2-Based**: Creates publication-ready plots using ggplot2
🔒 **Safe Execution**: Reviews code before execution
⚡ **One-Click**: Generate plots directly from the RStudio Addins menu
📋 **Copy-Ready**: Automatically copy generated code to clipboard

## Installation

### Prerequisites

- R 4.0 or higher
- OpenAI API key (get one at https://platform.openai.com/account/api-keys)

### Install from GitHub

```r
# Install devtools if not already installed
install.packages("devtools")

# Install RAIPlot
devtools::install_github("mlaotou/RAIPlot")
```

## Quick Start

### 1. Set up your OpenAI API key

Add this to your `.Renviron` file:

```
OPENAI_API_KEY=sk-your-api-key-here
```

Or set it in your R session:

```r
Sys.setenv(OPENAI_API_KEY = "sk-your-api-key-here")
```

### 2. Load your data

```r
# Example: load built-in data
data(mtcars)
data(iris)
```

### 3. Launch the Addin

In RStudio:
- Go to **Addins** menu
- Select **AI Plot Generator**
- Describe the plot you want
- Click **Generate Code**

### 4. Review and use the generated code

The code will appear in the right panel. You can:
- Auto-execute it to see the plot immediately
- Copy it to your clipboard for later use
- Modify it as needed

## Examples

### Example 1: Simple Scatter Plot

**Data**: `mtcars`

**Description**: 
```
Show the relationship between miles per gallon (mpg) and weight (wt), 
colored by number of cylinders (cyl)
```

**Generated Plot**: A scatter plot with color-coded points

### Example 2: Statistical Comparison

**Data**: `iris`

**Description**:
```
Create a box plot comparing Sepal.Length across different Species, 
with jittered points overlaid
```

**Generated Plot**: A box plot with individual data points

### Example 3: Time Series

**Data**: `ChickWeight`

**Description**:
```
Show chicken weight progression over time, with separate lines for each diet
```

**Generated Plot**: A line plot with faceted or colored lines

## API Usage

### Direct Function Calls

```r
library(RAIPlot)

# Generate code for your data
code <- call_openai_for_plot(
  data_name = "mtcars",
  description = "Scatter plot of mpg vs wt, colored by cyl",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  model = "gpt-3.5-turbo"
)

# View the code
cat(code)

# Execute it safely
safe_execute_plot(code)
```

### Get Data Information

```r
# Check what columns are available
info <- get_data_info("mtcars")
print(info)

# List all available data frames
list_available_data()
```

## Functions

- `ai_plotter_addin()` - Launch the RStudio Addin
- `call_openai_for_plot()` - Call OpenAI API to generate plotting code
- `safe_execute_plot()` - Safely execute R plotting code
- `get_data_info()` - Get information about a data frame
- `list_available_data()` - List all data frames in the environment
- `raiplot_info()` - Display package information

## Troubleshooting

### "OPENAI_API_KEY not found"

Set your API key:
```r
Sys.setenv(OPENAI_API_KEY = "your-api-key")
```

### "API call failed"

- Check your internet connection
- Verify your API key is correct
- Check your OpenAI account has available credits

### Generated code doesn't work

- Review the generated code for syntax errors
- Modify your description to be more specific
- Check that your data frame has the columns you mentioned

## Best Practices

1. **Be specific**: More detailed descriptions lead to better plots
   - ❌ "Make a plot"
   - ✅ "Create a scatter plot of X vs Y with a trend line, colored by category"

2. **Reference columns correctly**: Use the exact column names
   - ❌ "The numeric columns"
   - ✅ "mpg and wt columns"

3. **Start simple**: Generate basic plots first, then add complexity
   - Start with simple scatter plots or bar charts
   - Add colors, facets, and themes incrementally

4. **Review the code**: Always check generated code before running
   - Understand what the code does
   - Make manual adjustments if needed

## Limitations

- Requires internet connection (for API calls)
- API calls cost money (see OpenAI pricing)
- Limited to ggplot2 visualizations
- Generated code quality depends on data quality and description clarity

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### How to contribute

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

Created by mlaotou

## Support

- 📖 GitHub Issues: [RAIPlot Issues](https://github.com/mlaotou/RAIPlot/issues)
- 💬 Discussions: [RAIPlot Discussions](https://github.com/mlaotou/RAIPlot/discussions)

## Acknowledgments

- Built with [Shiny](https://shiny.rstudio.com/) and [ggplot2](https://ggplot2.tidyverse.org/)
- AI powered by [OpenAI](https://openai.com/)
- RStudio Addin framework by [RStudio](https://rstudio.com/)

## Changelog

### v0.1.0 (2024)
- Initial release
- Basic Addin functionality
- OpenAI API integration
- ggplot2 code generation

---

**Note**: This project is not affiliated with OpenAI or RStudio. Use responsibly and respect API terms of service.