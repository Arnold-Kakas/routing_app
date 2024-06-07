# Demo Example of Shiny Routing Application

This Shiny app helps users (e.g. sales representatives, regional managers etc.) optimize their daily routes by calculating the best route considering travel time, meeting durations, and traffic conditions. The app uses the `hereR` package to perform routing and geocoding.

## Features

- **User Input**: User can input addresses, activities, and activity durations through a table.
- **Route Optimization**: The app calculates the optimal route based on shortest travel time considering traffic conditions.
- **Geocoding**: Converts addresses to geographic coordinates.

## Installation

1. **Clone the Repository**

   ```sh
   git clone https://github.com/Arnold-Kakas/routing_app.git
   cd routing_app
   ```

2. **Set Up Rhino Framework**

   Follow the instructions on the [Rhino framework GitHub page](https://github.com/appsilon/rhino) to install and set up Rhino.

3. **Install Required Packages**

   Make sure you have R and RStudio installed. Then, install the necessary R packages:

   ```R
   install.packages(c("rhino", "shiny", "DT", "hereR", "dplyr", "tidygeocoder", "leaflet"))
   ```

4. **Set Up HERE API Key**

   Obtain an API key from [HERE Developer](https://developer.here.com/) and set it in the `global.R` file:

   ```R
   set_key("<your_here_api_key>")
   ```

## Usage

1. **Run the Shiny App**

   Open the project in RStudio and run the app:

   ```R
   shiny::runApp()
   ```

2. **Input Customer Data**

   - Enter your starting address in the provided text input.
   - Fill in the customer details including addresses, activities, and durations in the table. Default duration is 30 minutes if not specified.
   - Click the "Optimize Route" button to calculate the optimal route.

3. **View Optimized Route**

   The optimized route, including addresses and meeting durations, will be displayed in a table and a map.

## Project Structure

- tba

## Dependencies

- tba

## License

This project is licensed under the GNU License.

## Contributing

Contributions are welcome! Please create a pull request or open an issue to discuss your ideas.

## Contact

For any questions or suggestions, please open an issue or contact me at [kakas@cleandata.sk].
