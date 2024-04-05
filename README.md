# Wallaby Consulting: 2024 Student Research Case Study Challenge

![bg image](https://github.com/Actuarial-Control-Cycle-T1-2024/group-page-showcase-wallaby-consulting/blob/main/Cover%20Page%20Image.jpg)

## SuperLife Saving Lives

Authors: Amelia Quinlan, Catherine Xie, Nicholas Wong, Nala Hong & Vivian Chan

<img width="250" alt="image" src="[https://github.com/Actuarial-Control-Cycle-T1-2024/group-page-showcase-wallaby-consulting/blob/main/Logos.png]">

---

### Table of Contents

---

## 1. Executive Summary

This report is presented by Wallaby Consulting for SuperLife’s chief actuary, Jes B. Zane, on suitable health incentive programs to be paired with SuperLife’s existing whole-life and 20-year term policies. The designed program is called EverCare and prioritises key incentive programs, which were selected based on their effectiveness in reducing mortality, as well as the net present value (NPV) of the estimated costs taken to implement each. To provide maximum value to as many policyholders as possible, EverCare encompasses four health intervention programs, as discussed more in Section 3. 

This program is designed to be implemented in January 2025 with a duration of 20 years (the full duration of a term policy). Wallaby Consulting have tested the economic viability of the EverCare program using given historical data and by conducting thorough sensitivity tests for key risks. EverCare has been designed to deliver maximum value to SuperLife, with a 95% certainty, of Č707 bn Net Profit (Č4,449 bn Premiums, Č3,742 bn Death Benefits) as well as an average mortality reduction of 22.23%.  


## 4. Pricing and Costs
SuperLife currently offers two long-term insurance products, a single-premium whole life insurance, and a level-premium 20-year term insurance. Our objectives were to price the premiums according to sound actuarial principles, balancing the economic value offered to SuperLife, and the competitiveness to the market. In general, our pricing process involved:
1. Analysis of the provided Inforce Data, and provided mortality tables,
2.  Creation of new mortality tables,
3. Base premium pricing,
4. Expense loading,
5. Cash back margins, and
6. Profitability projections.
7. 
### 4.1 Mortality Analysis
Our first concern when utilising the provided data is whether the provided mortality data (MORTDATA) is a suitable fit for the provided inforce data (INFORCE). We analysed the empirical death probabilities from INFORCE across the provided age range and found that for ages less than 58, the experienced mortality (INFORCE) is higher than predicted and vice versa for ages greater than 58 (Figure 2). To account for this, we needed to stratify the MORTDATA according to the Underwriting Class by calculating an average loading factor of the INFORCE mortality compared to the MORTDATA mortality.

<img width="250" alt="image" src="[]">

Next, we further stratified the mortality tables according to the projected mortality reductions due to the interventions. There are 4 base interventions and hence 15 combinations of the interventions, for which mortality tables were calculated. With a high degree of certainty, an average reduction of 22.23% is expected after implementing interventions.
The results of this analysis show clear mortality improvements, which can be seen by the life expectancy at birth, using the Low-Risk Underwriting Class, with and without interventions. Similar patterns hold for the other underwriting classes.

| Base    | Smoking Cessation | Wellness Programs | Chronic Disease Management | Cancer Prevention Initiatives | 
| ------- | ----------------- | ----------------- | -------------------------- | ----------------------------- | 
| 78.7753 |      81.2288      |      79.8376      |          80.0209           |             80.2480           |

### 4.2 Premium Pricing Methodology
The resulting mortality tables were utilised in the calculation of actuarial premiums. The raw base premiums were initially calculated, before loading on expenses to determine a final premium which considered age, underwriting risk class and intervention program participation. In terms of the methodology, the pricing impact of several aspects were first considered. This included economic factors (interest rate, inflation rate), decrement information (mortality rate, lapse rate) and cash flow amounts (premium, commission, expenses, death benefit, capital). The following assumptions surrounding these factors were made, with further explanation in Appendix 9.3

| Assumptions    | 20 Year Term  | Whole Life |
| ------- | ----------------- | ----------------- |
| Interest Rate | 3.6% real interest rate (6.7% nominal interest rate, 2.95% inflation rate) |
| Mortality Rate    | From provided ‘Lumaria mortality table.xlsx’|
| Lapse Rate | From INFORCE |
| Commission | 40% of premium in first year, 1% onwards | 2.5% of premium in first year, 0% onwards|
| Base Expenses    | 10% of premium every year | 0.5% of premium every year |
| Marketing Expenses    | 4% of premium in first year, 1% of premium onwards  | 0.5% of premium in first year, 0% onwards |
| Initiative Expenses    | As outlined in Program Design |
| Required Capital  | 1% of death benefit |

The death benefit was taken to be the historical average of death benefits provided in the inforce dataset. This was Č602,229 for a 20 Year Term policy and Č769,177 for a Whole life policy. 
An Excel model (Robinson, 2007) was produced to calculate the cash flow amounts with and without decrements. The resultant without decrement cashflows were then used in the calculation of the NPV of profit. Then, to maintain a consistent profit margin per individual policy, Excel Solver was utilised to obtain a final expense loaded premium that allowed for a selected 10% profit margin. Expense loaded premiums were found for each of the 15 possible combinations of the 4 EverCare interventions. This was repeated across each of the four underwriting risk classes. The final expense loaded premiums were calculated as a weighted percentage of each intervention and underwriting risk class. The full table of expense loaded premiums can be found in Appendix 9.6 and 9.7.
The expense loaded premiums were then used to calculate the aggregate premium and death benefits received and paid out by SuperLife. This was done by taking into consideration the projected population growth and the age distribution of policy holders across the next 20 years. These findings are analysed in Section 4.3 Profitability Analysis.

_Participation Analysis_
In terms of cost analysis, only the Smoking Cessation Program within our EverCare package is dependent on the number of participants. However, all four have their individual impacts on mortality savings. The following participation rates are based off three participation studies of healthcare programs (Harris, 2010; Bryant, Bonevski & Paul, 2011; Bobitt, Aguayo, Payne, Jansen & Schwingel, 2019). Expected participation is as follows:

| Smoking Cessation Program | Wellness Programs | Chronic Disease Management | Cancer Prevention Initiatives | 
| ----------------- | ----------------- | -------------------------- | ----------------------------- | 
| 55% of Smokers   |   17.5% of Inforce  |  60% of High-Risk Class   |  17.5% of Inforce|

In SuperLife’s dataset, it was found that approximately 70% of smokers belonged to the moderate risk class and 30% of smokers were designated high risk class (Appendix 9.3). We followed this proportion for the smoking cessation program participation rates for each underwriting class. We also assumed that individuals managing chronic disease would be of high risk underwriting class.

_Participant distribution projections_
For this analysis, each individual year had any lapsed or deceased participants filtered out to consider active participants only. The inforce population was projected out until 2043, the end of the EverCare 20-year period. The number of active participants was fitted to a second- degree polynomial (equation in Appendix 9.4).
The proportion of each type of policy also changes throughout time as evident in Figure 6. The proportion of whole life policies grew from 0.1969 in 2001 to 0.3767 in 2020. While examining this, we noted that it was a linear increase for the first 20 years. However, from 2021 onward, 20-year term life policies begin to expire. As such, the proportion of whole life policies began to increase more steeply. However, we expect this to slow down when the number of whole life policies being added reaches an equilibrium with the number of term life policies being added and lapsing. Since a linear trend would cause the proportion of whole life policies to overtake 100%, we assumed a logarithmic curve (equation in Appendix 9.4) and projected the proportion of whole life policies to increase at an eventually decaying rate.
Based on the issue ages in INFORCE, whole life policies are offered to those from 35 to 65 years of age, and Term Life policies to those from 25 to 55 years of age. Historically, active participants were found to have been uniformly distributed across each issue age for both policy types. We are only interested in a policyholder’s issue age, as issue age determines their premium throughout their policy duration. Thus, to distribute the participant volumes across each age for each policy type, we assumed uniform distributions for issue age, such that if Xpolicy type represents the issue age for an individual policyholder, X<sub>WL</sub> and X<sub>T</sub> are uniformly distributed on (35, 65) and (25, 55) respectively. This is shown in Figure 7 above.

### 4.3 Profitability Analysis
Once the expense loaded premiums had been calculated, we applied these premiums to the historical inforce data in order to calculate our profitability with and without the interventions. Please refer to Appendix 9.4 to see the revenue (premiums) and the cost (death benefits) per year according to the inforce data, but with projected deaths according to the mortality adjustments from the interventions (numbers in Č Millions).
With our EverCare interventions in place, within 95% certainty, the present value of our aggregate net cash flow forecasted for the next 20 years is 35.58% higher than without. This is driven by our 20-year forecasted aggregate premium (with interventions) being 0.42% lower than the calculated historical premium (without interventions) and our aggregate death benefits (with interventions) paid out at 5.18% less than historical death benefits (without interventions). Please refer to Appendix 9.8 for our 20-year aggregate forecasts. The reduction in premiums with interventions implemented will consequentially improve product competitiveness and increase life insurance sales for SuperLife. The reduction in death benefits also reflects the decrease in expected mortality which will add economic value to SuperLife.
It must be noted that the profit margin assumption of 10% that has been used in our premium pricing models is approximately 5.9% higher than the industry average of 4.1% (Brennan, 2023). As such, in the event of any catastrophic event or extreme downturn in the economy, SuperLife can make the decision to lower the target profit margin toward the industry average and thus optimise sales and the value of policies by reducing the premium price. Reducing the profit margin will also be a suitable solution if SuperLife chooses to further increase their competitiveness. Further pricing changes can be made by decreasing operating expenses or commission rates. However, the current premium pricing model has already optimised these expense percentages and as such, this would be a less effective solution than lowering the profit margin.


### 4.4 Sensitivity Analysis and Program Impact
The key assumptions for which we performed sensitivity analysis were the discount rate and the mortality loadings. For both, a sensitivity range was defined based on historical experience, and from there a recommended range was defined (to achieve the desired profit results). The 20-year aggregated premiums and death benefits for each scenario are summarised below (both a projected series and a historical series of values). An target profit margin of 10% was used for both. Refer to Appendix 9.5 on how to best interpret our sensitivity tables.

| Sensitivity Test | Low | Baseline | High | 
| ---------------- | --- | -------- | ---- | 
| Real Interest Rate Assumptions | 1.64% |  3.64%  | 5.64%  |
| Premiums  | Č9,994 bn (+124.65%) | Č4,449 bn  | Č2,202 bn (-50.50%) |
| Death Benefits |  Č4,704 bn (+25.71%) | Č3,742 bn | Č3,025 bn (-19.17%) |
| Net Profit |  Č5,290 bn (+648.37%) | Č707 bn |   Č-823 bn (-216.41%) |

| Sensitivity Test | Low | Baseline | High | 
| ---------------- | --- | -------- | ---- | 
| Real Interest Rate Assumptions | 1.64% |  3.64%  | 5.64%  |
| Premiums  | Č 286 bn (+30%) |   Č220 bn  | Č190 bn (-14.6%) |
| Death Benefits |   Č15 bn (-35.78%) | Č23 bn |  Č30 bn (+30.43%) |
| Net Profit |   Č271 bn (+37.56%) | Č197 bn |   Č160 bn (-18.78%) |


From the sensitivity analysis of real interest rate assumptions, changes in real interest rate have a significant impact on overall cashflows. In Figure 9 and 10, a lower real interest rate represents an optimistic scenario for SuperLife’s current profitability whilst a higher real interest rate represents a negative scenario. With a lower real interest rate, the future cashflows are discounted at a slower rate and thus the current NPV of cashflows is higher and vice versa.
As seen in our projected sensitivity testing, a real interest rate of 5.64% results in a negative NPV for cashflow. As such, if the real interest rates approach an average of 5% and over, SuperLife should consider pricing changes, such as lowering the 10% required profit margin, to ensure profitability in both the short and long term. Whilst a lower real interest rate of 1.64% is beneficial when considering SuperLife’s profitability, long term low interest rates could result in increased pressure for SuperLife to repay policyholders’ death benefits as investment returns decrease. Sustained higher premiums may also result in decreased competitiveness. Overall, SuperLife must maintain a careful balance between program profitability and liability management as real interest rates approach either extreme.

| Sensitivity Test | Low | Baseline | High | 
| ---------------- | --- | -------- | ---- | 
| Mortality Loading Assumptions | 0.8 |  1.0  | 1.2  |
| Premiums  | Č4,213 bn (-5.31%)  | Č4,449 bn  | Č4,660 bn (+4.75%) |
| Death Benefits |  Č3,684 bn (-1.54%) | Č3,742 bn |  Č4,131 bn (+10.41%)|
| Net Profit |  Č529 bn (-25.18%) | Č707 bn |  Č529 bn (-25.18%) |

| Sensitivity Test | Low | Baseline | High | 
| ---------------- | --- | -------- | ---- | 
| Mortality Loading Assumptions | 1.64% |  3.64%  | 5.64%  |
| Premiums  | Č207 bn (-5.9%) |   Č220 bn  | Č226 bn (+2.72%) |
| Death Benefits |  Č21bn (-9.7%) |  Č23 bn |  Č30 bn (+30.04%) |
| Net Profit |  Č183 bn (-7.11%) | Č197 bn |    Č204 bn (-0.51%) |


The sensitivity analysis on mortality loading suggests that mortality has a much smaller impact than real interest rates. Overall, a lower mortality rate represents an optimistic scenario whilst higher mortality rates negatively impact SuperLife.
Whilst the net profits are both in a similar percentage change range for low and high loading assumptions, the aggregate premiums and death benefits differ significantly. Although the difference between high and low mortality premiums is within an approximate +5%/-5% range, death benefits see a more significant change. Specifically, the lower mortality rate scenario improves death benefit pay-outs whilst the high mortality rate scenario worsens projected death benefits by 10.41%. In general, a lower death benefit value is more desirable than higher premiums as this translates to less future liabilities to fulfil for SuperLife and thus decreases liability risk.



