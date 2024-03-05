The project titled "Cinematic Fortunes: An In-Depth Analysis of the Multifaceted Factors Impacting a Movie’s Box Office Success" by Group 9 explores the significant variables contributing to the financial success of movies. It utilizes the TMDb Movie Dataset for analysis and employs multiple linear regression models to investigate the relationship between various predictors (such as budget, runtime, genre, popularity, and lead actor gender) and movie revenue. The study's findings highlight the positive correlation between these variables and box office performance, with special emphasis on the genres of Adventure, Animation, Comedy, and Family for increased revenue. It also notes the negative impact of expanding a movie's release to multiple countries due to associated costs. The final model achieved an R-squared value of 0.710, indicating a good fit for predicting movie revenue based on the selected variables. The project acknowledges limitations and suggests future research directions, including considering non-linear models and external factors like social media trends and cultural events.

<img width="800" alt="image" src="https://github.com/balalabyte/Cinematic-Fortunes/assets/60688697/287a2ebb-110c-4083-b4f9-aa52ba706ebb">

## Project Overview
This project, titled "Cinematic Fortunes," is an in-depth statistical analysis aimed at uncovering the multifaceted factors that influence a movie's box office success. Utilizing the TMDb Movie Dataset, our team employed multiple linear regression models to explore the impact of various predictors such as budget, runtime, genre, popularity, and the gender of the lead actor on movie revenue.

## Data Source
The data was sourced from The Movie Database (TMDb) API, known for its comprehensive movie, actor, and director metadata.

## Key Findings
- Positive correlations were found between movie revenue and variables such as budget, runtime, genre, popularity, and vote count. Specifically, genres like Adventure, Animation, Comedy, and Family were strongly associated with higher revenue.
- Counter-Intuitive Results: Unexpectedly, the coefficient for the number of countries a movie was released in was negative, suggesting that international expansion costs may not always be offset by proportional revenue increases.
- Positive Correlation: Budget, runtime, genre, and popularity are found to be positively correlated with box office revenue. Specific genres such as Adventure, Animation, Comedy, and Family are notably more lucrative.
- Negative Correlation: Expanding a movie's release to multiple countries may not always result in proportional revenue increases, likely due to the higher costs associated with such expansions.
- Model Accuracy: The final model achieved an R-squared value of 0.710, indicating a strong ability to predict movie revenue based on the variables considered.

These insights could guide filmmakers and producers in making data-driven decisions to enhance box office performance.

## Models Used
Multiple linear regression models were iterated to find the best fit, with the final model achieving an R-squared value of 0.710.

## Limitations and Future Work
The study acknowledges the potential for omitted variable bias and the simplification of complex industry dynamics. Future research could incorporate non-linear models and external factors like social media trends and cultural events.


For more information on TMDb's API: [The Movie Database API](https://www.themoviedb.org/documentation/api)

