#' Capitalize R&D (-2 years)
#' @description
#' Capitalize R&D investments two years back (Non-tech service, Ads, Banks, Restaurant, Retail Store etc.).
#' @param cashflows **List of numbers** R&D spending the last 2 years
#' @param amortized **Boolean** Calculate amortized R&D
#' @return **Number** Amortized or unamortized portion of R&D
#' @examples
#' # List of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_2(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_2(cash_flow)
#' @export
get_amortization_2 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y2 <- cash_flows$y2 * 0
    un_y1 <- cash_flows$y1 * (1/2)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2)
    # Calculate amortized portion
  } else {
    am_y2 <- cash_flows$y2 * (1/2)
    am_y1 <- cash_flows$y1 * (1/2)
    current <- cash_flows$current * 0
    total <- as.integer(am_y2 + am_y1 + current)

  }
}

#' Capitalize R&D (-3 years)
#' @description Capitalize R&D investments three years back (Retail tech service, Apparel, Entertainment, Food processing, Household products, Newspapers, Publishing etc.).
#' @param cashflows **List of numbers** R&D spending the last 3 years
#' @param amortized **Boolean** Calculate amortized R&D
#' @return **Number** Amortized or unamortized portion of R&D
#' @examples
#' # List of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=555)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_3(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_3(cash_flow)
#' @export
get_amortization_3 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y3 <- cash_flows$y3 * 0
    un_y2 <- cash_flows$y2 * (1/3)
    un_y1 <- cash_flows$y1 * (2/3)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3)
    # Calculate amortized portion
  } else {
    am_y3 <- cash_flows$y3 * (1/3)
    am_y2 <- cash_flows$y2 * (1/3)
    am_y1 <- cash_flows$y1 * (1/3)
    current <- cash_flows$current * 0
    total <- as.integer(am_y3 + am_y2 + am_y1 + current)

  }
}

#' Capitalize R&D (-5 years)
#' @description Capitalize R&D investments five years back (Light manufacturing, Aluminum, Auto Parts, Building Materials, Computer Peripherals, Gold/Silver, Mining, Office supplies, Petroleum, Textile, Rubber, Tabacco).
#' @param cashflows **List of numbers** R&D spending the last 5 years
#' @param amortized **Boolean** Calculate amortized R&D
#' @return **Number** Amortized or unamortized portion of R&D
#' @examples
#' # List of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_5(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_5(cash_flow)
#' @export
get_amortization_5 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y5 <- cash_flows$y5 * 0
    un_y4 <- cash_flows$y4 * (1/5)
    un_y3 <- cash_flows$y3 * (2/5)
    un_y2 <- cash_flows$y2 * (3/5)
    un_y1 <- cash_flows$y1 * (4/5)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3 + un_y4 + un_y5)
    # Calculate amortized portion
  } else {
    am_y5 <- cash_flows$y5 * (1/5)
    am_y4 <- cash_flows$y4 * (1/5)
    am_y3 <- cash_flows$y3 * (1/5)
    am_y2 <- cash_flows$y2 * (1/5)
    am_y1 <- cash_flows$y1 * (1/5)
    current <- cash_flows$current * 0
    total <- as.integer(am_y5 + am_y4 + am_y3 + am_y2 + am_y1 + current)

  }
}


#' Capitalize R&D (-10 years)
#' @description Capitalize R&D investments 10 years back (Chemical, Auto/Truck, Drug, Machine, Maritime, Paper, Telecom Equipment, Water Utility, Heavy Manufacturing, Research with patenting).
#' @param cashflows **List of numbers** R&D spending the last 10 years
#' @param amortized **Boolean** Calculate amortized R&D
#' @return **Number** Amortized or Unamortized portion of R&D
#' @examples
#' # List of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666, y6=345, y7=556, y8=954, y9=3435, y10=111)
#' # Calculate unamortized portion of R&D
#' unamortized_rnd <- get_amortization_10(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_10(cash_flow)
#' @export
get_amortization_10 <- function(cash_flows, amortized) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y10 <- cash_flows$y10 * 0
    un_y9 <- cash_flows$y9 * (1/10)
    un_y8 <- cash_flows$y8 * (2/10)
    un_y7 <- cash_flows$y7 * (3/10)
    un_y6 <- cash_flows$y6 * (4/10)
    un_y5 <- cash_flows$y5 * (5/10)
    un_y4 <- cash_flows$y4 * (6/10)
    un_y3 <- cash_flows$y3 * (7/10)
    un_y2 <- cash_flows$y2 * (8/10)
    un_y1 <- cash_flows$y1 * (9/10)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3 + un_y4 + un_y5 + un_y6 + un_y7 + un_y8 + un_y9 + un_y10)
    # Calculate amortized portion
  } else {
    am_y10 <- cash_flows$y10 * (1/10)
    am_y9 <- cash_flows$y9 * (1/10)
    am_y8 <- cash_flows$y8 * (1/10)
    am_y7 <- cash_flows$y7 * (1/10)
    am_y6 <- cash_flows$y6 * (1/10)
    am_y5 <- cash_flows$y5 * (1/10)
    am_y4 <- cash_flows$y4 * (1/10)
    am_y3 <- cash_flows$y3 * (1/10)
    am_y2 <- cash_flows$y2 * (1/10)
    am_y1 <- cash_flows$y1 * (1/10)
    current <- cash_flows$current * 0
    total <- as.integer(am_y10 + am_y9 + am_y8 + am_y7 + am_y6 + am_y5 + am_y4 + am_y3 + am_y2 + am_y1 + current)

  }
}


#' Capitalize Acquisition (-5 years)
#' @description Capitalize acquisition investments five years back.
#' @param cashflows **List of numbers** Acquisition spending the 5 years
#' @param amortized **Boolean** Acquisition spending the last 10 years
#' @return **Number** Amortized or Unamortized portion of Acquisition
#' @examples
#' # List of cash flows
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Calculate unamortized portion of acquisition
#' unamortized_aq <- get_acquisition(cash_flow, amortized=FALSE)
#' # Calculate amortized portion of acquisition
#' amortized_aq <- get_acquisition(cash_flow)
#' @export
get_acquisition <- function(cash_flows, amortized=TRUE) {
  # Calculate unamortized portion
  if (amortized == FALSE) {
    un_y5 <- cash_flows$y5 * 0
    un_y4 <- cash_flows$y4 * (1/5)
    un_y3 <- cash_flows$y3 * (2/5)
    un_y2 <- cash_flows$y2 * (3/5)
    un_y1 <- cash_flows$y1 * (4/5)
    un_current <- cash_flows$current * 1
    total = as.integer(un_current + un_y1 + un_y2 + un_y3 + un_y4 + un_y5)
    # Calculate amortized portion
  } else {
    am_y5 <- cash_flows$y5 * (1/5)
    am_y4 <- cash_flows$y4 * (1/5)
    am_y3 <- cash_flows$y3 * (1/5)
    am_y2 <- cash_flows$y2 * (1/5)
    am_y1 <- cash_flows$y1 * (1/5)
    current <- cash_flows$current * 0
    total <- as.integer(am_y5 + am_y4 + am_y3 + am_y2 + am_y1 + current)

  }
}

#' Capitalize future leases
#' @description Transform future paid leases into debt.
#' @param lease_flow **List of numbers** Paid leases the next **n** years
#' @param beyond **Number** Paid leases beyond year five
#' @param cost_debt **Number** Cost of debt
#' @return **List of numbers** Debt value of leases **AND** years embedded in lease
#' @examples
#' # Paid leases 5Y in the future
#' cash_flow <- list(y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Paid leases beyond 5Y
#' beyond <- 45555
#' # Indicate the cost of debt (firm rating OR synthetic rating)
#' cost_debt <- 0.05
#' # Capitalize future leases
#' cap_leases <- get_leases(cash_flow, beyond, cost_debt)
#' # Capitalized leases
#' cap_leases$debt_value_lease
#' # Embedded lease years
#' cap_leases$years_embedded
#' @export
get_leases <- function(lease_flow, beyond, cost_debt) {
  # Calculate embedded years beyond 6
  years_embedded <- round(beyond/mean(unlist(lease_flow)))
  if (beyond > 0) {
    if (years_embedded > 1) {
      # Cash below year 6
      print("Beyond value > 1")
      value_beyond <- beyond / years_embedded
      l_beyond <- ((value_beyond*(1-1/(1+cost_debt)**years_embedded))/cost_debt)/(1+cost_debt)**5
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_beyond + l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
    } else {
      # Years embedded = 1
      print("Beyond value = 1")
      l_beyond <- beyond / (1+cost_debt)**6
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_beyond + l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
    }
  } else {
    print("No beyond value")
      l_y1 <- lease_flow$y1/(1+cost_debt)**1
      l_y2 <- lease_flow$y2/(1+cost_debt)**2
      l_y3 <- lease_flow$y3/(1+cost_debt)**3
      l_y4 <- lease_flow$y4/(1+cost_debt)**4
      l_y5 <- lease_flow$y5/(1+cost_debt)**5
      l_total <- as.integer(l_y1 + l_y2 + l_y3 + l_y4 + l_y5)
      result <- list(debt_value_lease=l_total, years_embedded=years_embedded)
  }
}

#' Calculate lease adjustment to EBIT
#' @description Calculate lease adjustment to EBIT (for ROIC calculation).
#' @param current_lease **Number** Current lease expenses
#' @param debt_value_lease **Number** Capitalized leases
#' @param lease_embedded_years **Number** Embedded years in leases
#' @return **Number** Lease adjustment for EBIT
#' @examples
#' # See get_leases function output for debt_value_lease and lease_embedded_years
#' # Current year leases
#' current_lease <- 555
#' # Paid leases 5Y in the future
#' cash_flow <- list(y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Paid leases beyond 5Y
#' beyond <- 45555
#' # Indicate the cost of debt (firm rating OR synthetic rating)
#' cost_debt <- 0.05
#' # Capitalize future leases
#' cap_leases <- get_leases(cash_flow, beyond, cost_debt)
#' lease_adj <- lease_adjustment(current_lease, cap_leases$debt_value_lease, cap_leases$years_embedded)
#' @export
lease_adjustment <- function(current_lease, debt_value_lease, lease_embedded_years) {
  as.integer(current_lease - debt_value_lease / (lease_embedded_years+5))
}

#' Calculate R&D adjustment to EBIT
#' @description Calculate R&D adjustment to EBIT (for ROIC calculation).
#' @param current_rnd **Number** Current year R&D expense
#' @param amortized_rnd **Number** Amortized R&D
#' @return **Number** R&D adjustment for EBIT
#' @examples
#' See get_amortization function output for amortized R&D
#' # Current year R&D
#' current_rnd <- 1771
#' # R&D spending last five years
#' cash_flow <- list(current=123, y1=333, y2=444, y3=444, y4=555, y5=666)
#' # Calculate amortized portion of R&D
#' amortized_rnd <- get_amortization_5(cash_flow)
#' lease_adj <- rnd_adjustment(current_rnd, amortized_rnd)
#' @export
rnd_adjustment <- function(current_rnd, amortized_rnd) {
  as.integer(current_rnd-amortized_rnd)
}

#' Calculate ROIC
#' @description Calculate Return on Invested Capital.
#' @param ebit **Number** Current year EBIT
#' @param curr_lease_adj **Number** Lease adjustment to EBIT
#' @param rnd_adj **Number** R&D adjustment to EBIT
#' @param eff_tax **Number** Effective tax rate (Tax Paid / Taxable Income)
#' @param short_debt **Number** Short term debt (Short term interest bearing debt (debt + leases))
#' @param long_debt **Number** Long term debt (debt + leases)
#' @param equity **Number** Equity
#' @param goodwill **Number** Goodwill
#' @param cash **Number** Cash
#' @param minority **Number** Minority interest
#' @param last_lease_debt **Number** Last year capitalized leases
#' @param rnd_asset **Number** The unamortized portion of R&D
#' @param goodwill_portion **Number** Part of goodwill added to Invested capital
#' @param cash_portion **Number** Part of cash added to Invested capital
#' @param current_year_rnd **Number** Current year R&D spending
#' @param amortized_rnd **Number** Amortized portion of R&D
#' @return **Tibble** with ROIC and other parameters
#' @examples
#' roic <- get_roic(ebit=2033, eff_tax=0.2, short_debt=333,
#'                  long_debt=4567, equity=4444, goodwill=333, cash=345)
#' @export
#' @importFrom tibble tibble
get_roic <- function(ebit, curr_lease_adj=0, rnd_adj=0, eff_tax,
                     short_debt, long_debt, equity, goodwill, cash,
                     last_lease_debt=0, rnd_asset=0, goodwill_portion=0.2, cash_portion=0.2,
                     minority=0) {
  # Portion of goodwill and cash to include in Invested capital
  goodwill_back = goodwill*goodwill_portion
  cash_back = cash*cash_portion
  goodwill_adj = goodwill*(1-goodwill_portion)
  cash_adj = cash*(1-cash_portion)
  # Calculate adjusted EBIT (leases, R&D)
  adj_ebit <- ebit + curr_lease_adj + rnd_adj
  # Calculate after-tax EBIT
  after_tax_ebit <- adj_ebit*(1-eff_tax) + rnd_adj*eff_tax
  # Calculate invested capital
  invested_capital <- short_debt+long_debt+equity+minority-goodwill_adj-cash_adj
  # Calculate adjusted invested capital
  adj_invested_capital <- invested_capital+last_lease_debt+rnd_asset+goodwill_back+cash_back
  # Calculate ROIC
  roic <- (ebit*(1-eff_tax))/invested_capital
  # Calculate adjusted ROIC
  adj_roic <- after_tax_ebit/adj_invested_capital
  # Return ROIC
  tibble(ebit=ebit, adj_ebit=adj_ebit, invested_capital=invested_capital,
                           after_tax_ebit=after_tax_ebit,
                           adj_invested_capital=adj_invested_capital, roic=round(roic*100, 1),
                           adj_roic=round(adj_roic*100, 1), goodwill_back=goodwill_back,
                           cash_back=cash_back)
}

#' Calculate ROE
#' @description Calculate Return on Equity
#' @param income **Number** Current year income
#' @param rnd_adj **Number** R&D adjustment
#' @param equity **Number** Current year Equity
#' @param goodwill **Number** Goodwill
#' @param goodwill_portion **Number** Portion of goodwill adjustment
#' @param rnd_asset **Number** Unamortized portion of R&D
#' @param eff_tax **Number** Effective tax rate
#' @return **Tibble** with ROE and the other parameters
#' @examples
#' ROE <- get_roe(income=333, goodwill=5555, equity=4555)
#' @export
#' @importFrom tibble tibble
get_roe <- function(income, rnd_adj=0, equity, goodwill,
                    goodwill_portion=0.2, rnd_asset=0,
                    eff_tax=0) {
  # Calculate goodwill
  goodwill_back = goodwill*goodwill_portion
  # Calculate adjusted net income
  adj_income <- income + rnd_adj + rnd_adj*eff_tax
  # Calculate book value of invested equity
  book_equity <- equity - goodwill + rnd_asset + goodwill_back
  # Calculate adjusted book value of equity
  adj_equity <- book_equity + rnd_asset + goodwill_back
  tibble(income=income, adj_income=adj_income,
                           equity=equity, adj_equity=adj_equity,
                           roe=round((income/book_equity),2)*100,
                           adj_roe=round((adj_income/adj_equity),2)*100,
                           rnd_adj=rnd_adj, goodwill_back=goodwill_back)
}

#' Calculate Levered Beta & Unlevered beta
#' @description Calculate unlevered and levered beta.
#' @param average_beta **Number** Industry average beta
#' @param industry_tax **Number** Industry effective tax rate
#' @param average_de **Number** Industry average Debt/Equity
#' @param firm_tax **Number** Firm effective tax
#' @param firm_debt **Number** Firm debt value (short + long term)
#' @param firm_equity **Number** Firm equity value
#' @return **Tibble** with bottom-up beta
#' @examples
#' beta <- get_beta(average_beta=0.77, industry_tax=0.05, average_de=0.083, firm_tax=0.35, firm_debt=333, firm_equity=444)
#' @export
#' @importFrom tibble tibble
get_beta <- function(average_beta, industry_tax, average_de,
                     firm_tax, firm_debt, firm_equity) {
  unlevered_beta <- average_beta / (1+(1-industry_tax)*average_de)
  levered_beta <- unlevered_beta*(1+(1-firm_tax)*(firm_debt/firm_equity))
  tibble(unlevered_beta = round(unlevered_beta,2),
                   levered_beta = round(levered_beta,2))
}

#' Calculate Cost of Equity
#' @description Calculate Cost of Equity.
#' @param risk_free **Number** Risk free rate
#' @param beta **Number** Bottom-up levered beta
#' @param risk_premium **Number** Risk premium
#' @return **Tibble** Cost of Equity
#' @export
#' @examples
#' cost_equity <- get_cost_equity(risk_free=0.015, beta=0.71, rick_premium=0.041)
#' @importFrom tibble tibble
get_cost_equity <- function(risk_free, beta, risk_premium) {
  cost_equity <- risk_free + beta*risk_premium
  tibble(risk_free=risk_free, beta=beta,
         risk_premium=risk_premium, cost_equity=round(cost_equity,3))
}

#' Calculate Cost of Debt
#' @description Calculate Cost of Debt based on interest coverage rate (EBIT/Interest expense).
#' @param risk_free **Number** Risk free rate
#' @param company_spread **Number** Firm company spread
#' @param country_spread **Number** Country spread
#' @return **Tibble** Cost of Debt
#' @export
#' @examples
#' cost_equity <- get_cost_debt(risk_free=0.015, company_spread=0.01)
#' @importFrom tibble tibble
get_cost_debt <- function(risk_free, company_spread, country_spread=0) {
  cost_debt <- risk_free + company_spread + country_spread
  tibble(risk_free=risk_free, company_spread=company_spread, cost_debt = round(cost_debt, 3))
}

#' Calculate Cost of Capital
#' @description Calculate Cost of Capital.
#' @param marginal_tax **Number** Marginal tax rate
#' @param cost_equity **Number** Cost of equity
#' @param cost_debt **Number** Cost of debt
#' @param equity **Number** Firm equity
#' @param debt **Number** Firm debt (short and long term)
#' @return **Tibble** Cost of Debt
#' @export
#' @examples
#' cost_equity <- get_cost_debt(cost_equity=0.107, cost_debt=0.0929, marginal_tax=0.35, equity=500, debt=300)
#' @importFrom tibble tibble
get_cost_capital <- function(marginal_tax, cost_equity,
                             cost_debt, equity, debt) {
  cost_capital <- cost_equity*(equity/(debt+equity)) + cost_debt*(1-marginal_tax)*(debt/(debt+equity))
  tibble(cost_equity=cost_equity, cost_debt=cost_debt, cost_capital=round(cost_capital,4))
}

#' Calculate Net Capital Expenditures (Net CapEx)
#' @description Calculate and adjust Net Capital Expenditure.
#' @param capex **Number** Current year capital expenditure
#' @param depreciation **Number** Depreciation
#' @param rnd_expense **Number** Current year R&D expense
#' @param rnd_amortization **Number** R&D amortization
#' @param acquisition **Number** Acquisition
#' @param acquisition_amortization **Number** Acquisition amortization
#' @param lease_current_year **Number** NPV of current leases
#' @param lease_last_year **Number** NPV of last year leases
#' @return **Tibble** Net CapEx
#' @export
#' @examples
#' get_net_capex(capex=584, depreciation=484)
#' @importFrom tibble tibble
get_net_capex <- function(capex, depreciation, rnd_expense=0, rnd_amortization=0,
                          acquisition=0, lease_last_year=0, lease_current_year=0,
                          acquisition_amortization=0) {
  net_capex <- capex - depreciation + rnd_expense - rnd_amortization + acquisition - acquisition_amortization + lease_last_year - lease_current_year
  tibble(net_capex = net_capex, depreciation=depreciation, rnd_expense=rnd_expense,
         rnd_amortization=rnd_amortization, acquisition=acquisition,
         acquisition_amortization = acquisition_amortization, lease_last_year=lease_last_year,
         lease_current_year=lease_current_year)
}

#' Calculate working capital
#' @description Calculate working capital.
#' @param inventory **Number** Inventory
#' @param accounts_receive **Number** Accounts receivable
#' @param accounts_payable **Number** Accounts payable
#' @return **Tibble** Working capital
#' @export
#' @examples
#' get_working_cap(inventory=40, account_receive=80, accounts_payable=40)
#' @importFrom tibble tibble
get_working_cap <- function(inventory, accounts_receive, accounts_payable) {
  working_capital <- (inventory + accounts_receive) - accounts_payable
  if (working_capital < 0) {
    working_capital <- 0
    tibble(inventory=inventory, accounts_receivable=accounts_receive,
           accounts_payable=accounts_payable, working_capital=working_capital)
  }
  else {
    tibble(inventory=inventory, accounts_receivable=accounts_receive,
           accounts_payable=accounts_payable, working_capital=working_capital)
  }
}

#' Calculate Reinvestment Rate
#' @description Calculate Reinvestment Rate (w/ adjusted EBIT).
#' @param net_capex **Number** Net Capital Expenditure
#' @param working_capital **Number** Change in Working Capital
#' @param after_tax_ebit **Number** Adjusted EBIT
#' @return **Tibble** Reinvestment rate
#' @export
#' @examples
#' get_rr(net_capex=100, working_capital=100, after_tax_ebit=200)
#' @importFrom tibble tibble
get_rr <- function(net_capex, working_capital, after_tax_ebit) {
  rr <- (net_capex + working_capital)/after_tax_ebit
  tibble(reinvestment_rate = rr, after_tax_ebit = after_tax_ebit, working_capital = working_capital)
}

#' Calculate Expected Growth Rate (stable ROIC)
#' @description Calculate expected growth rate (w/ stable ROIC across years)
#' @param reinvestment_rate **Number** Reinvestment rate
#' @param roic **Number** Return on Capital
#' @return **Tibble** Expected Growth Rate
#' @export
#' @examples
#' get_stable_growth(reinvestment_rate=0.4, roic=0.2)
#' @importFrom tibble tibble
get_stable_growth <- function(reinvestment_rate, roic) {
  stable_growth <- reinvestment_rate*roic
  tibble(stable_growth=stable_growth, reinvestment_rate=reinvestment_rate,
         roic=roic)
}

#' Calculate Expected Growth rate (changing ROIC)
#' @description Calculate expected growth rate (w/ changing ROIC across years), used in case of efficiency claim.
#' @param reinvestment_rate **Number** Reinvestment rate
#' @param roic_initial **Number** Current year Return on Capital
#' @param roic_target **Number** Target Return on Capital
#' @return **Tibble** Expected Growth Rate
#' @export
#' @examples
#' get_stable_growth(reinvestment_rate=0.4, roic_initial=0.2, roic_target=0.6, year_target=5)
#' @importFrom tibble tibble
get_dynamic_growth <- function(reinvestment_rate, roic_initial, roic_target, years_target=5) {
  formula_1 <- roic_target*reinvestment_rate
  formula_2 <- (1+(roic_target-roic_initial)/roic_initial)^(1/years_target)-1
  growth <- formula_1 + formula_2
  tibble(growth=round(growth, 2), reinvestment_rate=reinvestment_rate,
         roic_initial=roic_initial, roic_target=roic_target, years_target=years_target)
}

#' Calculate FCFF and Value of Operating Assets for stable growth firm
#' @description Calculate FCFF (w/ After Tax EBIT)
#' @param after_tax_ebit **Number** After-Tax EBIT (current year)
#' @param net_capex **Number** Net Capital Expenditures
#' @param working_capital **Number** Change in Working Capital
#' @param growth **Number** Growth rate
#' @param wacc **Number** Cost of Capital
#' @return **Tibble** FCFF
#' @export
#' @examples get_fcff(after_tax_ebit=2481, depreciation=1914, capex=1659, working_capital=1119, wacc=0.035, growth=0.03)
#' @importFrom tibble tibble
get_stable_operating_assets <- function(after_tax_ebit, net_capex, working_capital,
                                        wacc, growth) {
  fcff <- after_tax_ebit - net_capex - working_capital
  value_operating_assets <- fcff*(1+growth) / (wacc-growth)
  return(tibble(fcff=fcff, after_tax_ebit=after_tax_ebit,
         net_capex=net_capex, working_capital=working_capital,
         value_operating_assets=round(value_operating_assets)))
}

#' Calculate Value of Operating Assets
#' @description Calculate Value of Operating Assets (2-stage growth).
#' @param npv_fcff **Number** Free Cash Flow to Firm
#' @param terminal_value **Number** Terminal Value
#' @param wacc **Number** Cost of Capital
#' @return **Tibble** Value of operating assets
#' @export
#' @examples
#' get_operating_asses(npv_fcff=111, terminal_value=222, wacc=0.065)
#' @importFrom tibble tibble
get_operating_assets <- function(npv_fcff, terminal_value, wacc) {
  value_operating_assets <- npv_fcff + terminal_value / (1+wacc)^5
  tibble(npv_fcff=npv_fcff, wacc=wacc, terminal_value=terminal_value, value_operating_assets=round(value_operating_assets))
}

#' Calculate Terminal Value
#' @description Calculate Value of Terminal Value (2-stage growth).
#' @param ebit_year_five **Number** After-tax EBIT year 5
#' @param stable_growth **Number** Growth (Risk-free Rate)
#' @param roic **Number** Return of Capital
#' @param wacc **Number** Cost of Capital
#' @return **Tibble** Terminal Value
#' @export
#' @examples
#' get_terminal_value(ebit_year_five=1300, roic=0.046, wacc=0.1, stable_growth=0.03)
#' @importFrom tibble tibble
get_terminal_value <- function(ebit_year_five, stable_growth, wacc, roic) {
  # Terminal EBIT at year 6
  terminal_ebit <- round(ebit_year_five*(1+stable_growth))
  # Calculate Reinvestment Rate
  reinvestment_rate <- 1 - (stable_growth / roic)
  # Calculate Terminal Value
  terminal_value <- terminal_ebit*reinvestment_rate/(wacc-stable_growth)
  tibble(terminal_ebit=terminal_ebit,
         stable_growth=stable_growth, wacc=wacc, terminal_value=round(terminal_value))
}

#' Calculate cash flows
#' @description Calculate After-tax EBIT and FCFF flows and NPV (2-stage growth)
#' @param after_tax_ebit **Number** After-tax EBIT
#' @param reinvestment_rate **Number** Reinvestment rate
#' @param time_period **Vector of numbers** DCF years
#' @param wacc **Number** Cost of Capital
#' @param growth **Number** Estimated growth
#' @return **List of numbers** NPV FCFF, EBIT 5Y, etc.
#' @examples
#' cash_flows <- get_cash_flow(after_tax_ebit=333, reinvestment_rate=0.3, time_period=c(1,2,3,4,5), wacc=0.08, growth=0.03)
#' @export
#' @importFrom FinancialMath NPV
get_cash_flow <- function(after_tax_ebit, reinvestment_rate, time_period, wacc, growth) {
  # Calculate After-tax EBIT flow
  ebit_flow_1 <- after_tax_ebit*(1+growth)^1
  ebit_flow_2 <- after_tax_ebit*(1+growth)^2
  ebit_flow_3 <- after_tax_ebit*(1+growth)^3
  ebit_flow_4 <- after_tax_ebit*(1+growth)^4
  ebit_flow_5 <- after_tax_ebit*(1+growth)^5
  ebit_flow <- c(y1=round(ebit_flow_1), y2=round(ebit_flow_2),
                 y3=round(ebit_flow_3), y4=round(ebit_flow_4),
                 y5=round(ebit_flow_5))
  # Calculate reinvestment
  rr_1 <- ebit_flow_1*0.4
  rr_2 <- ebit_flow_2*0.4
  rr_3 <- ebit_flow_3*0.4
  rr_4 <- ebit_flow_4*0.4
  rr_5 <- ebit_flow_5*0.4
  rr_flow <- c(y1=round(rr_1), y2=round(rr_2),
                 y3=round(rr_3), y4=round(rr_4),
                 y5=round(rr_5))
  # Calculate FCFF
  fcff_1 <- ebit_flow_1-rr_1
  fcff_2 <- ebit_flow_2-rr_2
  fcff_3 <- ebit_flow_3-rr_3
  fcff_4 <- ebit_flow_4-rr_4
  fcff_5 <- ebit_flow_5-rr_5
  fcff_flow <- c(y1=round(fcff_1), y2=round(fcff_2),
                 y3=round(fcff_3), y4=round(fcff_4),
                 y5=round(fcff_5))
  # Calculate NPV
  fcff_npv <- NPV(0, cf = fcff_flow, time_period, wacc)
  # Output
  list(ebit_flow=ebit_flow, fcff_flow=fcff_flow, fcff_npv = round(fcff_npv), ebit_year_five=round(ebit_flow_5))

}

#' Calculate growth firm cash flows (start-ups)
#' @description Calculate After-tax EBIT and FCFF flows and NPV (high growth firm).
#' @param revenue **Number** Current year revenues
#' @param initial_margin **Number** Current year operating margin
#' @param final_margin **Number** Final year operating margin
#' @param revenue_growth_trend **List of numbers** Revenue growth next 10 years
#' @param ebit **Number** Current year EBIT
#' @param marginal_tax **Number** Marginal tax (industry specific)
#' @param nol **Number** Net Operating Losses
#' @param sales_capital_ratio **Number** Ratio between revenues and invested capital (industry)
#' @param current_capital_investment **Number** Current year capital investment
#' @return **Tibble** Full description of financial parameters
#' @examples
#' # Revenues growth in %
#' revenues_growth <- list(y1=0.55, y2=0.45, y3=0.40, y4=0.35, y5=0.30,y6=0.20, y7=0.15, y8=0.12, y9=0.08, y10=0.06)
#' res <- get_growth_flow(revenue = 1246, revenue_growth_trend = revenues_growth, initial_margin = -0.0418, final_margin = 0.14, tax_margin = 0.375, ebit = -52, nol = 1289, sales_capital_ratio = 0.5, current_capital_investment = 1000)
#' @export
#' @importFrom tibble tibble
get_growth_flow <- function(revenue,
                            revenue_growth_trend,
                            initial_margin,
                            final_margin,
                            tax_margin,
                            ebit,
                            nol,
                            sales_capital_ratio,
                            current_capital_investment) {

  # Calculate revenues the next 10Y
  revenue_initial <- revenue
  revenues_y1 <- round(revenue*(1+revenue_growth_trend$y1), 0)
  revenues_y2 <- round(revenues_y1*(1+revenue_growth_trend$y2), 0)
  revenues_y3 <- round(revenues_y2*(1+revenue_growth_trend$y3), 0)
  revenues_y4 <- round(revenues_y3*(1+revenue_growth_trend$y4), 0)
  revenues_y5 <- round(revenues_y4*(1+revenue_growth_trend$y5), 0)
  revenues_y6 <- round(revenues_y5*(1+revenue_growth_trend$y6), 0)
  revenues_y7 <- round(revenues_y6*(1+revenue_growth_trend$y7), 0)
  revenues_y8 <- round(revenues_y7*(1+revenue_growth_trend$y8), 0)
  revenues_y9 <- round(revenues_y8*(1+revenue_growth_trend$y9), 0)
  revenues_y10 <- round(revenues_y9*(1+revenue_growth_trend$y10), 0)
  revenues_10_years <- c(initial = revenue_initial,
                         y1=revenues_y1,
                            y2=revenues_y2,
                            y3=revenues_y3,
                            y4=revenues_y4,
                            y5=revenues_y5,
                            y6=revenues_y6,
                            y7=revenues_y7,
                            y8=revenues_y8,
                            y9=revenues_y9,
                            y10=revenues_y10)

  # Calculate increase in revenues the next 10Y
  revenue_increase <- 0
  rev_increase_y1 <- revenues_y1-revenue
  rev_increase_y2 <- revenues_y2-revenues_y1
  rev_increase_y3 <- revenues_y3-revenues_y2
  rev_increase_y4 <- revenues_y4-revenues_y3
  rev_increase_y5 <- revenues_y5-revenues_y4
  rev_increase_y6 <- revenues_y6-revenues_y5
  rev_increase_y7 <- revenues_y7-revenues_y6
  rev_increase_y8 <- revenues_y8-revenues_y7
  rev_increase_y9 <- revenues_y9-revenues_y8
  rev_increase_y10 <- revenues_y10-revenues_y9
  rev_increase_10_years <- c(revenue_increase=revenue_increase,
                             y1=rev_increase_y1, y2=rev_increase_y2,
                             y3=rev_increase_y3, y4=rev_increase_y4,
                             y5=rev_increase_y5, y6=rev_increase_y6,
                             y7=rev_increase_y7, y8=rev_increase_y8,
                             y9=rev_increase_y9, y10=rev_increase_y10)

  # Calculate Reinvestment
  current_reinv <- 0
  revin_y1 <- rev_increase_y1 * sales_capital_ratio
  revin_y2 <- rev_increase_y2 * sales_capital_ratio
  revin_y3 <- rev_increase_y3 * sales_capital_ratio
  revin_y4 <- rev_increase_y4 * sales_capital_ratio
  revin_y5 <- rev_increase_y5 * sales_capital_ratio
  revin_y6 <- rev_increase_y6 * sales_capital_ratio
  revin_y7 <- rev_increase_y7 * sales_capital_ratio
  revin_y8 <- rev_increase_y8 * sales_capital_ratio
  revin_y9 <- rev_increase_y9 * sales_capital_ratio
  revin_y10 <- rev_increase_y10 * sales_capital_ratio
  revin_10_years <- c(current_reinv=current_reinv,
                             y1=revin_y1, y2=revin_y2,
                             y3=revin_y3, y4=revin_y4,
                             y5=revin_y5, y6=revin_y6,
                             y7=revin_y7, y8=revin_y8,
                             y9=revin_y9, y10=revin_y10)

  # Calculate capital investment
  initial_capital_inv <- 0
  cap_y1 <- revin_y1 + current_capital_investment
  cap_y2 <- revin_y2 + cap_y1
  cap_y3 <- revin_y3 + cap_y2
  cap_y4 <- revin_y4 + cap_y3
  cap_y5 <- revin_y5 + cap_y4
  cap_y6 <- revin_y6 + cap_y5
  cap_y7 <- revin_y7 + cap_y6
  cap_y8 <- revin_y8 + cap_y7
  cap_y9 <- revin_y9 + cap_y8
  cap_y10 <- revin_y10 + cap_y9
  cap_10_years <- c(initial_capital=initial_capital_inv,
                             y1=cap_y1, y2=cap_y2,
                             y3=cap_y3, y4=cap_y4,
                             y5=cap_y5, y6=cap_y6,
                             y7=cap_y7, y8=cap_y8,
                             y9=cap_y9, y10=cap_y10)

  # Calculate % Margins the next 10Y
  margin_initial <- initial_margin
  margin_y1 <- round(((initial_margin*1.5)/2.5+final_margin/2.5),3)
  margin_y2 <- round(((margin_y1*1.5)/2.5+final_margin/2.5),3)
  margin_y3 <- round(((margin_y2*1.5)/2.5+final_margin/2.5),3)
  margin_y4 <- round(((margin_y3*1.5)/2.5+final_margin/2.5),3)
  margin_y5 <- round(((margin_y4*1.5)/2.5+final_margin/2.5),3)
  margin_y6 <- round(((margin_y5*1.5)/2.5+final_margin/2.5),3)
  margin_y7 <- round(((margin_y6*1.5)/2.5+final_margin/2.5),3)
  margin_y8 <- round(((margin_y7*1.5)/2.5+final_margin/2.5),3)
  margin_y9 <- round(((margin_y8*1.5)/2.5+final_margin/2.5),3)
  margin_y10 <- round(((margin_y9*1.5)/2.5+final_margin/2.5),3)
  margins_10_years <- c(margin_initial = margin_initial,
                        y1=margin_y1,
                           y2=margin_y2,
                           y3=margin_y3,
                           y4=margin_y4,
                           y5=margin_y5,
                           y6=margin_y6,
                           y7=margin_y7,
                           y8=margin_y8,
                           y9=margin_y9,
                           y10=margin_y10)

  # Calculate EBIT
  ebit_initial <- ebit
  ebit_y1 <- revenues_y1*margin_y1
  ebit_y2 <- revenues_y2*margin_y2
  ebit_y3 <- revenues_y3*margin_y3
  ebit_y4 <- revenues_y4*margin_y4
  ebit_y5 <- revenues_y5*margin_y5
  ebit_y6 <- revenues_y6*margin_y6
  ebit_y7 <- revenues_y7*margin_y7
  ebit_y8 <- revenues_y8*margin_y8
  ebit_y9 <- revenues_y9*margin_y9
  ebit_y10 <- revenues_y10*margin_y10
  ebit_10_years <- c(ebit_initial=ebit_initial,
                     y1=ebit_y1,
                     y2=ebit_y2,
                     y3=ebit_y3,
                     y4=ebit_y4,
                     y5=ebit_y5,
                     y6=ebit_y6,
                     y7=ebit_y7,
                     y8=ebit_y8,
                     y9=ebit_y9,
                     y10=ebit_y10)

  # Calculate NOL
  nol_initial <- nol
  nol_y1 <- round(get_nol(nol=nol_initial, ebit = ebit_y1),0)
  nol_y2 <- round(get_nol(nol=nol_y1, ebit = ebit_y2),0)
  nol_y3 <- round(get_nol(nol=nol_y2, ebit = ebit_y3),0)
  nol_y4 <- round(get_nol(nol=nol_y3, ebit = ebit_y4),0)
  nol_y5 <- round(get_nol(nol=nol_y4, ebit = ebit_y5),0)
  nol_y6 <- round(get_nol(nol=nol_y5, ebit = ebit_y6),0)
  nol_y7 <- round(get_nol(nol=nol_y6, ebit = ebit_y7),0)
  nol_y8 <- round(get_nol(nol=nol_y7, ebit = ebit_y8),0)
  nol_y9 <- round(get_nol(nol=nol_y8, ebit = ebit_y9),0)
  nol_y10 <- round(get_nol(nol=nol_y9, ebit = ebit_y10),0)
  nol_10_years <- c(nol_initial=nol_initial,
                    y1=nol_y1,
                    y2=nol_y2,
                    y3=nol_y3,
                    y4=nol_y4,
                    y5=nol_y5,
                    y6=nol_y6,
                    y7=nol_y7,
                    y8=nol_y8,
                    y9=nol_y9,
                    y10=nol_y10)

  # Calculate taxes
  tax_y1 <- get_growth_taxes(ebit=ebit_y1, last_nol=nol,
                           marginal_tax = tax_margin)

  tax_y2 <- get_growth_taxes(ebit=ebit_y2, last_nol=nol_y1,
                           marginal_tax = tax_margin)

  tax_y3 <- get_growth_taxes(ebit=ebit_y3, last_nol=nol_y2,
                           marginal_tax = tax_margin)

  tax_y4 <- get_growth_taxes(ebit=ebit_y4, last_nol=nol_y3,
                           marginal_tax = tax_margin)

  tax_y5 <- get_growth_taxes(ebit=ebit_y5, last_nol=nol_y4,
                           marginal_tax = tax_margin)

  tax_y6 <- get_growth_taxes(ebit=ebit_y6, last_nol=nol_y5,
                           marginal_tax = tax_margin)

  tax_y7 <- get_growth_taxes(ebit=ebit_y7, last_nol=nol_y6,
                           marginal_tax = tax_margin)

  tax_y8 <- get_growth_taxes(ebit=ebit_y8, last_nol=nol_y7,
                           marginal_tax = tax_margin)

  tax_y9 <- get_growth_taxes(ebit=ebit_y9, last_nol=nol_y8,
                           marginal_tax = tax_margin)

  tax_y10 <- get_growth_taxes(ebit=ebit_y10, last_nol=nol_y9,
                           marginal_tax = tax_margin)

  tax_10_years <- c(initial_tax=0, y1=tax_y1, y2=tax_y2, y3=tax_y3,
                    y4=tax_y4, y5=tax_y5, y6=tax_y6, y7=tax_y7, y8=tax_y8,
                    y9=tax_y9, y10=tax_y10)

  # Calculate After-tax EBIT
  initial_tax_ebit <- ebit_initial
  tax_ebit_y1 <- ebit_y1 - tax_y1
  tax_ebit_y2 <- ebit_y2 - tax_y2
  tax_ebit_y3 <- ebit_y3 - tax_y3
  tax_ebit_y4 <- ebit_y4 - tax_y4
  tax_ebit_y5 <- ebit_y5 - tax_y5
  tax_ebit_y6 <- ebit_y6 - tax_y6
  tax_ebit_y7 <- ebit_y7 - tax_y7
  tax_ebit_y8 <- ebit_y8 - tax_y8
  tax_ebit_y9 <- ebit_y9 - tax_y9
  tax_ebit_y10 <- ebit_y10 - tax_y10
  tax_ebit_10_years <- c(initial_tax_ebit=initial_tax_ebit,
                             y1=tax_ebit_y1, y2=tax_ebit_y2,
                             y3=tax_ebit_y3, y4=tax_ebit_y4,
                             y5=tax_ebit_y5, y6=tax_ebit_y6,
                             y7=tax_ebit_y7, y8=tax_ebit_y8,
                             y9=tax_ebit_y9, y10=tax_ebit_y10)

  # Calculate ROIC
  initial_fcff <- 0
  fcff_y1 <- tax_ebit_y1 - revin_y1
  fcff_y2 <- tax_ebit_y2 - revin_y2
  fcff_y3 <- tax_ebit_y3 - revin_y3
  fcff_y4 <- tax_ebit_y4 - revin_y4
  fcff_y5 <- tax_ebit_y5 - revin_y5
  fcff_y6 <- tax_ebit_y6 - revin_y6
  fcff_y7 <- tax_ebit_y7 - revin_y7
  fcff_y8 <- tax_ebit_y8 - revin_y8
  fcff_y9 <- tax_ebit_y9 - revin_y9
  fcff_y10 <- tax_ebit_y10 - revin_y10
  fcff_10_years <- c(initial_roic=initial_fcff,
                             y1=fcff_y1, y2=fcff_y2,
                             y3=fcff_y3, y4=fcff_y4,
                             y5=fcff_y5, y6=fcff_y6,
                             y7=fcff_y7, y8=fcff_y8,
                             y9=fcff_y9, y10=fcff_y10)


  tibble(revenues = revenues_10_years,
         revenues_increase = rev_increase_10_years,
         margins = margins_10_years,
         ebit = ebit_10_years,
         taxes_paid = tax_10_years,
         after_tax_ebit = tax_ebit_10_years,
         fcff = fcff_10_years,
         nol = nol_10_years,
         revinvestment = revin_10_years,
         capital_invested = cap_10_years)


}

#' Helper function for get_growth_flow
#' @description Calculate NOL
#' @param nol **Number** NOL
#' @param ebit **Number** EBIT
#' @return **Number**
get_nol <- function(nol, ebit) {
  if (ebit<nol) {
    nol <- nol-ebit
  } else {
   nol <- 0
  }
}

#' Helper function for get_growth_flow
#' @description Calculate Taxes paid
#' @param ebit A number EBIT
#' @param last_nol A number Last year NOL
#' @param marginal_tax A number Marginal tax (industry)
#' @return **Number**
get_growth_taxes <- function(ebit, last_nol, marginal_tax) {
  if (ebit < 0) {
    tax_paid <- 0
   } else if (ebit < last_nol) {
      tax_paid <- 0
   } else if (ebit > last_nol) {
      tax_paid <- (ebit-last_nol)*marginal_tax
    }
}














