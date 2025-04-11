[Home](./index.md) --- [Installation](./install.md) --- [Dataset](./data.md) --- [Methods](./methods.md) --- [Guide](./guide.md) --- [About](./about.md)

---

[CALIPER Dataset](#caliper) --- [AdRI_Generator Dataset](#adri_generator)

---

## CALIPER Dataset <a name = "caliper"></a>

The biomarker data from the [CALIPER study](https://doi.org/10.1373/clinchem.2011.177741) is available in this Shiny App in the [CALIPER folder](https://github.com/SandraKla/AdRI/tree/master/data) with the corresponding reference intervals. The data was brought into the appropriate format for the analysis.

* Albumin G (g/L)
* Albumin P (g/L)
* Alkaline Phosphatase (U/L)
* ALT (ACT) (U/L)
* ALT (U/L)
* Amylase (U/L)
* Apo A1 (g/L)
* Apo B (g/L)
* ASO (IU/mL)
* AST (ACT) (U/L)
* AST (U/L)
* Bilirubin Direct (µmol/L)
* Bilirubin-Total (T) (µmol/L)
* C3 (g/L)
* C4 (g/L)
* Calcium (mmol/L)
* ChE (U/L)
* Cholesterol (mmol/L)
* CO2 (carbon dioxide) (mmol/L)
* Creatinine (enzymatic) (μmol/L)
* Creatinine (Jaffe) (μmol/L)
* CRP (mg/L)
* GGT (U/L)
* Haptoglobin (g/L)
* IgA (g/L)
* IgG (g/L)
* IgM (g/L)
* Iron (μmol/L)
* LDH (LD) (U/L)
* Lipase (lip) (U/L)
* Magnesium (mmol/L)
* Phosphorus (mmol/L)
* Prealbumin (g/L)
* RF (rheumatoid factor) (IU/mL)
* Total Protein (g/L)
* Transferrin (TRF) (g/L)
* Triglyceride (mmol/L)
* UHDL (Ultra HDL) (mmol/L)
* Urea (mmol/L)
* Uric Acid (µmol/L)

## AdRI_Generator Dataset <a name = "adri_generator"></a>

The generated data from the Shiny App [AdRI_Generator](https://github.com/SandraKla/AdRI_Generator) can be used in this Shiny App no matter if it is generated with the help of the functions or for given reference intervals.

The Shiny App [AdRI_Generator](https://github.com/SandraKla/AdRI_Generator) is a generator for creating age-dependent analyte data. Available are the following distributions: Normal-Distribution (`NO`), Log-Normal-Distribution (`LOGNO`), Box-Cox Cole & Green Distribution (`BCCG`), Box-Cox _t_-Distribution (`BCT`) and Box-Cox Power Exponential Distribution (`BCPE`). The parameters μ (Mean), σ (Variance), ν (Skewness) and τ (Kurtosis) are changing over the patient age with a linear or an exponential function. 

The linear function is:  `y = m*x + b` and the exponential `y = a*e^(x*b)`. 

The data can be downloaded as a CSV file. Negative values are deleted automatically. The generator save "Generator" as the stations-name and determines that all values are unique and have no gender. To add pathological cases (only by Normal-Distribution) a factor can be added to μ.
