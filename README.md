# CLEANING AND VALIDATION OF DATA

This is an assignment of the course "Typology and life cycle of the data", belonging to Master of Data Science of the Open University of Catalonia. There are two types of the reports in this study, which are the <strong>general report</strong> and <strong>implementation report</strong>. The general report focuses on the written explanation of the research results. Moreover, the implementation report focuses on the exhaustive coding process and outputs from the R programming environment

In this research, we aimed to establish predictive models to forecast the pollution level in Fuerteventura, one of the seven islands in the Canary Islands. To reach this goal, we used the crawler that we had created in the PR1 to acquire the datasets we wanted, and later we used R programming language to integrate, clean, and analyse the data. Several statistical and machine leaning methods were used in this research. Firstly, we conducted descriptive statistical analysis to explore the relationships among the variables. Then, Shapiro-Wilk tests and Levene's tests were conducted to test the normality and homogeneity of the data. Additionally, Kruskal-Wallis tests were conducted to verify whether the mean value of the meteorological and pollution variables were significantly different across the year and month. In terms of the inferential statistic, we transformed dependent variables to run lineal regression models with ordinary least squares estimation. However, even the dependent variables were transformed, these lineal regression models did not meet the statistical assumptions. Due to this reason, Generalized additive model and Regression tree were used in this study as two alternative methods to establish predictive models. As the final step, we compare the fits of model using Normalised Mean Square Error value and visualisation. We found that the models to predict NO, NO2, and NOX were well fitted. The models to predict PM10 and PM2.5 were generally well fitted. Nevertheless, the models to predict SO2, O3, and CO were not well fitted. In conclusion, we partially reached our research goals

# Members of the group
This activity is realised collectively by two persons: Daura Hernández Díaz and Xiaowei Cai.

# Contents
<ul>
<li><strong>/Original datasets</strong>：This folder contains the original datasets scraped by our Python crawler from <a href="http://www.gobiernodecanarias.org/medioambiente/calidaddelaire/datosHistoricos.do">the governmental website of the Canary Islands </a>.</li>
<li><strong>Canarydataset.csv</strong>：It contains the cleaned dataset which is used in this study</li>
<li><strong>PR2 R code.R</strong>：Source code of data analysis</li>
<li><strong>PR2 Data analysis.rmd</strong>：R Markdown file of the code</li>
<li><strong>PR2 General Report ver 1.2.pdf</strong>：General report of the study</li>
<li><strong>PR2 Implementation Report ver 1.1.pdf</strong>：Implementation report of the study(PDF format)</li>
<li><strong>PR2 Implementation Report ver 1.1.html</strong>：Implementation report of the study(HTML format)</li>
</ul>
