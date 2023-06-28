/* Query producing a table that includes demographic, public health, and */
/* presidental election voting behavior data concerning the COVID-19 pandemic for each county in the United States*/
/* Alaskan counties are excluded due to incomplete source data*/
WITH Election_2020 (county_state, GOP_2020) AS (
     SELECT CONCAT(county_name, '_', state_name),
            CAST(per_gop AS float)
       FROM COVID_practice_3.dbo.[2020_US_County_Level_Presidential_Results]
     ),

     Vaccination_Rates (county_state, Vax_Pct, Vax_5Plus_Pct, Vax_12Plus_Pct, Vax_18Plus_Pct, Vax_65Plus_Pct) AS (
     SELECT CONCAT(Recip_County, '_', Recip_State),
            ISNULL(Series_Complete_Pop_Pct, 0),
            ISNULL(Series_Complete_5PlusPop_Pct, 0),
            ISNULL(Series_Complete_12PlusPop_Pct, 0),
            ISNULL(Series_Complete_18PlusPop_Pct, 0),
            ISNULL(Series_Complete_65PlusPop_Pct, 0)
       FROM COVID_practice_3.dbo.[COVID-19_Vaccinations_in_the_United_States_County]
     ),

     COVID_Mortality (county_state, COVID_Deaths, All_Deaths) AS (
     SELECT CONCAT(County_name, '_', State),
            CAST(ISNULL(Deaths_involving_COVID_19, 0) AS float),
            CAST(ISNULL(Deaths_from_All_Causes, 0) AS float)
       FROM COVID_practice_3.dbo.Provisional_COVID_19_Death_Counts_in_the_United_States_by_County
     ),

     County_Age_Population (county_state, median_age, county_pop) AS (
     SELECT CONCAT(CTYNAME, '_', STNAME),
            MEDIAN_AGE_TOT,
            POPESTIMATE
       FROM COVID_practice_3.dbo.[cc-est2021-agesex-all]
       WHERE YEAR = 3
     )

SELECT c.county_state,
       b.GOP_2020,
       c.Vax_Pct,
       c.Vax_5Plus_Pct,
       c.Vax_12Plus_Pct,
       c.Vax_18Plus_Pct,
       c.Vax_65Plus_Pct,
       CAST(d.COVID_Deaths / e.county_pop * 10000 AS DECIMAL(7,2)) AS COVID_Deaths_Per_10K,
       CAST(d.All_Deaths / e.county_pop * 10000 AS DECIMAL(7,2)) AS All_Deaths_Per_10K,
       e.median_age
  FROM Election_2020 b
  JOIN Vaccination_Rates c ON b.county_state = c.county_state
  JOIN COVID_Mortality d ON b.county_state = d.county_state
  JOIN County_Age_Population e ON b.county_state = e.county_state
  WHERE Vax_Pct > 0 AND COVID_Deaths > 0
  ORDER BY b.GOP_2020 DESC;
