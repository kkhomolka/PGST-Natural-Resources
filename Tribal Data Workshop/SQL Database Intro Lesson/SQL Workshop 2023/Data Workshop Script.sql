-- SQL is not case sensitive!

-- Read all the data in the surveys TABLE 
SELECT * -- asterisk is shorthand for selecting all  
FROM surveys; -- use a semicolon to end your command 

-- specify columns
SELECT year, month, day
FROM surveys;

-- limit the number of rows returned using the limit function
SELECT * FROM surveys LIMIT 10; 

-- unique values within a table 
SELECT DISTINCT species_id 
FROM surveys; 

-- calculated values 
SELECT year, month, day, weight/1000 as rounded_kg -- the as function allows you to rename the new column 
FROM surveys;  

-- use built in functions 
SELECT plot_id, species_id, sex, weight, round(weight/1000, 2)
FROM surveys; 

Workshop Challenge
Write a query that returns the year, month, day, specie_id and weight in mg?

-- answer
SELECT year, month, day, species_id, weight*1000 as weight_mg
FROM surveys; 

-- to save as a new table and save the new output 
CREATE TABLE weight_mg AS 
SELECT year, month, day, species_id, weight*1000 as weight_mg
FROM surveys; 