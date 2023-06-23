/* Query producing a table of demographic statistics related to COVID-19 mortality and vaccination rates across American counties, */
/* grouped into deciles of support for Donald Trump in the 2020 presidential election*/
/* Alaskan counties are excluded due to incomplete source data*/
WITH Election_2020 (county_state, GOP_2020) AS (
     SELECT CONCAT(county_name, '_', state_name),
            CAST(per_gop AS float)
       FROM COVID_practice_3.dbo.[2020_US_County_Level_Presidential_Results]
     ),

     Vaccination_Rates (county_state, Vax_Total, Vax_5Plus, Vax_12Plus, Vax_18Plus, Vax_65Plus) AS (
     SELECT CONCAT(Recip_County, '_', Recip_State),
            ISNULL(Series_Complete_Yes, 0),
            ISNULL(Series_Complete_5Plus, 0),
            ISNULL(Series_Complete_12Plus, 0),
            ISNULL(Series_Complete_18Plus, 0),
            ISNULL(Series_Complete_65Plus, 0)
       FROM COVID_practice_3.dbo.[COVID-19_Vaccinations_in_the_United_States_County]
     ),

     COVID_Mortality (county_state, COVID_Deaths, All_Deaths) AS (
     SELECT CONCAT(County_name, '_', State),
            CAST(ISNULL(Deaths_involving_COVID_19, 0) AS float),
            CAST(ISNULL(Deaths_from_All_Causes, 0) AS float)
       FROM COVID_practice_3.dbo.Provisional_COVID_19_Death_Counts_in_the_United_States_by_County
     ),

     County_Age_Population (county_state, median_age, county_pop, pop_under5, pop_over18, pop_over65) AS (
     SELECT CONCAT(CTYNAME, '_', STNAME),
            MEDIAN_AGE_TOT,
            POPESTIMATE,
            UNDER5_TOT,
            AGE18PLUS_TOT,
            AGE65PLUS_TOT
       FROM COVID_practice_3.dbo.[cc-est2021-agesex-all]
       WHERE YEAR = 3
     ),

     tbl1 (county_state, Avg_Trump_vote, Vax_Total, Vax_5Plus, Vax_12Plus, Vax_18Plus, Vax_65Plus, county_pop, pop_5plus, pop_12plus_est, pop_18plus, pop_65plus, COVID_Deaths, All_Deaths, Trump_vote_Decile) AS (
     SELECT c.county_state,
            CAST(b.GOP_2020 AS float),
            CAST(c.Vax_Total AS float),
            CAST(c.Vax_5Plus AS float),
            CAST(c.Vax_12Plus AS float),
            CAST(c.Vax_18Plus AS float),
            CAST(c.Vax_65Plus AS float),
            CAST(e.county_pop AS float),
            CAST(e.county_pop - e.pop_under5 AS float),
            CAST((e.county_pop - e.pop_under5 + e.pop_over18) / 2 AS float),
            CAST(e.pop_over18 AS float),
            CAST(e.pop_over65 AS float),
            d.COVID_Deaths,
            d.All_Deaths,
            NTILE(10) OVER (ORDER BY CAST(b.GOP_2020 AS float) DESC)
       FROM Election_2020 b
       JOIN Vaccination_Rates c ON b.county_state = c.county_state
       JOIN COVID_Mortality d ON b.county_state = d.county_state
       JOIN County_Age_Population e ON b.county_state = e.county_state
      WHERE Vax_Total > 0 AND COVID_Deaths > 0
       )

SELECT Trump_Vote_Decile,
       SUM(Vax_Total)/SUM(county_pop) AS Vax_Pct,
       SUM(Vax_5Plus)/SUM(pop_5plus) AS Vax_5Plus_Pct,
       SUM(Vax_12Plus)/SUM(pop_12plus_est) AS Vax_12Plus_Pct,
       SUM(Vax_18Plus)/SUM(pop_18plus) AS Vax_18Plus_Pct,
       SUM(Vax_65Plus)/SUM(pop_65plus) AS Vax_65Plus_Pct,
       SUM(COVID_Deaths)/SUM(county_pop)*10000 AS COVID_mortality_Per_10K,
       SUM(All_Deaths)/SUM(county_pop)*10000 AS All_mortality_Per_10K
  FROM tbl1
GROUP BY Trump_Vote_Decile;