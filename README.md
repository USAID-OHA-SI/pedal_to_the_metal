# pedal_to_the_metal

Repo to host code for creating PEPFAR 1-pager for COP25 preparations.

### Instructions

Welcome to **petal_to_the_metal**!

This repo contains all the R coded needed to generate pre-COP25 one pagers. To get started, please clone this repo to your local machine and [create a development branch](https://stackoverflow.com/questions/40307960/how-to-create-a-branch-in-github) and set up your folder structure using `glamr::si_setup().`

To maintain consistency and high-quality contributions, please follow the guidelines below.

### 1. List of Countries to Loop Over

Dynamic paths allow the code to run seamlessly on different machines and environments without needing manual adjustments. Instead of hardcoding file paths, use relative paths or environment variables. This is especially helpful in collaborative projects where contributors may have different directory structures. For instance:

This approach helps the code remain portable, making it easy to clone the repository and run the analysis without needing to adjust paths.

### 2. List of Countries to Loop Over

This project is limited to all PEPFAR countries preparing a COP. To create this list, use the code chunk below:

``` r
# Pull the list of OUs for which visuals need to be generated
cop_ous <- glamr::pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Region", negate = T)) %>% 
  pull(operatingunit)
```

### 3. Set Up Script to Work for One Country

Before running scripts for multiple countries, ensure they work correctly for a single country. This step allows you to debug and refine the script on a smaller scale, minimizing errors when expanding to the full list of countries.

1.  **Choose a Test Country**: Select a representative country from the list.

2.  **Run and Debug**: Check for logical errors, data inconsistencies, and confirm the output is as expected.

3.  **Extend to All Countries**: Once the script works for one country, you can generalize it to all countries using the loop method.

4.  Ensure that All Countries in the `cop_ous` list can be processed with your code.

### 4. Use Consistent Colors

Color choices are important for visual clarity and data interpretation. Using a consistent color scheme helps communicate information effectively and aligns with UNAIDS’ standards.

-   **Achievement Colors**: Use the achievement colors from `glitr`. These can be automatically assigned to achievement using `gophr::adorn_achievement()`

-   **Logical Colors**: For binary or logical indicators (e.g., TRUE/FALSE values), maintain a consistent color scheme.

    -   For values that are TRUE use the hex code `#419164` or the color $\color{#419164}{\text{hunter}}$.

    -   FALSE values should be mapped to the hex code `#E14BA1` or the color $\color{#E14BA1}{\text{orchid bloom}}$

### 5. Convert Each Script to a Function

To improve reusability, modularity, and readability, each script should be refactored into functions. Functions make it easier to test specific parts of the code, reuse logic, and ensure each task is isolated. Functions should be thoroughly documented and include input parameters for flexibility.

#### Testing Functions for Every Country

After converting scripts to functions, test them across the full list of countries. This helps ensure that the function handles variations in data structure or content for each country. Testing also minimizes the risk of encountering unexpected errors later.

### 6. Expand Grid to Include Placeholders for Missing Data

To ensure comprehensive analysis and reporting, use `expand.grid()` or similar functions (`complete()`) to include all possible data points, even if some are missing. This approach highlights any gaps in the data and allows the use of placeholders to maintain a consistent structure across countries. Placeholders also simplify downstream analysis, as they allow you to differentiate between “missing data” and “zero” or “null” values explicitly.

Using `expand.grid()` also ensures that visualizations or summary tables remain consistent in size and format, regardless of data availability.

### 7. Follow `si_style` for Consistent Visuals

Adhering to `si_style` standards helps maintain a professional and consistent look across all graphics, aligning with UNAIDS or organizational branding. This style guide likely includes rules for font size, color schemes, title placements, and other visual elements. Implementing a consistent style not only improves readability but also enhances the credibility of the visualizations.

### 8. Use Source Sans Pro Typeface

All text should use the Source Sans Pro typeface to maintain consistency with UNAIDS’ brand guidelines. This font is clear, professional, and accessible, making it ideal for reports and presentations. Consistent typeface across visuals and reports helps maintain a unified and polished appearance.

### 9. For UNAIDS Data, Use the `dev_edms` Branch

The `dev_edms` branch is currently the active development branch for UNAIDS data. If you find yourself in need of using the latest UNAIDS data, please re-install `mindthegap` from the `dev_edms` branch.

``` r
  # install UNAIDS package from dev_edms branch
  remotes::install_github("USAID-OHA-SI/mindthegap", ref = "dev_edms")
```

------------------------------------------------------------------------

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
