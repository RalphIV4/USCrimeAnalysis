<h1>United States Crime Story: An Exploratory Data Analysis</h1>
    <p>🔎 <em>Analyzing historical violent crime trends in the U.S. from 1975 to 2014</em></p>

<h2>📌 Overview</h2>
    <p>This project presents an exploratory analysis of violent crime trends across different regions and states in the U.S. between 1975 and 2014. Using FBI crime data, we examine crime patterns, correlations with population, and regional variations. Insights include the peak of violent crime in the early 1990s, regional differences, and the distribution of specific crimes such as homicides, robberies, rapes, and assaults.</p>

<h2>📊 Key Insights</h2>
    <ul>
        <li>Violent crime rates peaked in the early 1990s, followed by a steady decline.</li>
        <li>The <strong>Northeastern region</strong> showed the highest average crime per year, while <strong>Illinois</strong> had a higher crime rate than all Northeast states.</li>
        <li>Population density has a strong positive correlation with violent crimes—urban areas see higher crime rates.</li>
        <li>Crime composition shifted over time: <strong>robberies</strong> were most prevalent until the mid-1980s, then <strong>assaults</strong> took the lead.</li>
        <li><strong>Atlanta, GA</strong> had the highest crime per capita, while <strong>Fairfax County, VA</strong> had the lowest.</li>
        <li>Different regions showed distinct crime patterns—assaults were most common everywhere except the Northeast, where robberies dominated.</li>
        <li><strong>New York</strong> had the highest robbery and homicide reports, while <strong>California</strong> led in rape and assault reports.</li>
    </ul>

<h2>🗂 Dataset Details</h2>
    <ul>
        <li><strong>Source:</strong> FBI Uniform Crime Reports (1975-2014)</li>
        <li><strong>Columns Used for Analysis:</strong></li>
        <ul>
            <li><code>report_year</code> (Year)</li>
            <li><code>agency_jurisdiction</code> (City & State)</li>
            <li><code>population</code> (Total residents)</li>
            <li><code>violent_crime</code>, <code>homicides</code>, <code>rapes</code>, <code>assaults</code>, <code>robberies</code> (Crime counts)</li>
        </ul>
    </ul>


<h2>📈 Analysis Methods</h2>
    <ul>
        <li><strong>Programming Language and packages:</strong> R (tidyverse, usmap, scales, e1071, lubridate)
        <li><strong>Data Cleaning & Preprocessing:</strong> Handling missing values, categorizing regions, structuring datasets.</li>
        <li><strong>Exploratory Data Analysis (EDA):</strong> Distribution plots, time series trends, and regional comparisons.</li>
        <li><strong>Correlation Analysis:</strong> Examining crime rates vs. population density.</li>
        <li><strong>Geospatial Mapping:</strong> Visualizing crime per capita across U.S. states.</li>
    </ul>




<!--
 ```diff
- text in red
+ text in green
! text in orange
# text in gray
@@ text in purple (and bold)@@
```
--!>
