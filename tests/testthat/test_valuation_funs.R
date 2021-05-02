library(fun.valuation)

# Test rnd_adjustment
test_that("Verify R&D adjustment to EBIT", {
  current_rnd <- 1771
  amortized_rnd <- 1409
  expect_equal(rnd_adjustment(current_rnd = current_rnd,
                              amortized_rnd = amortized_rnd), 362)
})

# Test get_amortization_2 function
test_that("Verify amortized and amortized R&D (2Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714)
  # Test amortized value
  expect_equal(get_amortization_2(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8878)
  # Test unamortized value
  expect_equal(get_amortization_2(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 14332)
})

# Test get_amortization_3 function
test_that("Verify amortized and amortized R&D (3Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010)
  # Test amortized value
  expect_equal(get_amortization_3(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8922)
  # Test unamortized value
  expect_equal(get_amortization_3(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 18744)
})


# Test get_amortization_5 function
test_that("Verify amortized and amortized R&D (5Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010, y4=6500, y5=4000)
  # Test amortized value
  expect_equal(get_amortization_5(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 7453)
  # Test unamortized value
  expect_equal(get_amortization_5(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 27177)
})


# Test get_amortization_10 function
test_that("Verify amortized and amortized R&D (10Y)", {
  cash_flows <- list(current=9811, y1=9043, y2=8714, y3=9010, y4=6500, y5=4000,
                     y6=4500, y7=3456, y8=9836, y9=23465, y10=4567)
  # Test amortized value
  expect_equal(get_amortization_10(
    cash_flows = cash_flows,
    amortized = TRUE
  ), 8309)
  # Test unamortized value
  expect_equal(get_amortization_10(
    cash_flows = cash_flows,
    amortized = FALSE
  ), 44278)
})

# Test get_leases function
test_that("Verify capitalized leases value and embedded years", {
  # Paid leases
  leases <- list(y1=471, y2=378, y3=295, y4=225, y5=163)
  # Leases paid beyond threshold
  beyond <- 791
  beyond_2 <- 310
  null_beyond <- 0
  # Cost of debt
  cost_debt <- 0.024
  # Test capitalized leases
  expect_equal(get_leases(lease_flow = leases,
                          beyond=beyond,
                          cost_debt = cost_debt)$debt_value_lease,
               2114)

  # Test embedded years
  expect_equal(get_leases(lease_flow = leases,
                          beyond=beyond,
                          cost_debt = cost_debt)$years_embedded,
               3)

  # Test capitalized leases, beyond mean(leases) = 1
  expect_equal(get_leases(lease_flow = leases,
                          beyond = beyond_2,
                          cost_debt = cost_debt)$debt_value_lease,
               1713)

  # Test embedded years, beyond mean(leases) = 1
  expect_equal(get_leases(lease_flow = leases,
                          beyond = beyond_2,
                          cost_debt = cost_debt)$years_embedded,
               1)

  # Test capitalized leases, beyond = 0
  expect_equal(get_leases(lease_flow = leases,
                          beyond = null_beyond,
                          cost_debt = cost_debt)$debt_value_lease,
               1444)

  # Test embedded years, beyond = 0
  expect_equal(get_leases(lease_flow = leases,
                          beyond = null_beyond,
                          cost_debt = cost_debt)$years_embedded,
               0)

})

# Test lease_adjustment function
test_that("Verify lease adjustment to EBIT", {
  current_year_leases <- 863
  lease_debt_value <- 2114
  lease_embedded_years <- 3
  expect_equal(lease_adjustment(current_lease = current_year_leases,
                                debt_value_lease = lease_debt_value,
                                lease_embedded_years = lease_embedded_years),
               598)
})

# Test get_roic function
test_that("Verify ROIC calculation", {
  ebit <- 27801
  lease_adj <- 1286
  rnd_adj <- 361
  tax_rate <- 0.31
  short_debt <- 6348
  long_debt <- 47079
  equity <- 71315
  minority <- 4446
  goodwill <- 16520
  cash <- 6550
  last_year_lease <- 14134
  # Unamortized portion R&D
  rnd_asset <- 4831
  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = rnd_adj, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = rnd_asset,
                        minority = minority)$roic), 17)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = rnd_adj, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = rnd_asset,
                        minority = minority)$adj_roic), 15)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = 0, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = 0,
                        minority = minority)$roic), 17)

  expect_equal(as.integer(get_roic(ebit=ebit, curr_lease_adj = lease_adj,
                        rnd_adj = 0, eff_tax = tax_rate,
                        short_debt = short_debt, long_debt = long_debt,
                        equity = equity, goodwill = goodwill, cash = cash,
                        last_lease_debt = last_year_lease, rnd_asset = 0,
                        minority = minority)$adj_roic), 15)

})


# Test get_roe function
test_that("Verify ROE calculation", {
  net_income <- 16999
  rnd_adj <- 361
  tax_rate <- 0.31
  equity <- 71315
  goodwill <- 16521
  # Unamortized portion R&D
  rnd_asset <- 4831
  expect_equal(get_roe(income = net_income, rnd_adj = rnd_adj, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = rnd_asset,
                       eff_tax = tax_rate, goodwill = goodwill)$roe, 27)

  expect_equal(get_roe(income = net_income, rnd_adj = rnd_adj, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = rnd_asset,
                       eff_tax = tax_rate, goodwill = goodwill)$adj_roe, 25)

  expect_equal(get_roe(income = net_income, rnd_adj = 0, equity = equity,
                       goodwill_portion = 0.2, rnd_asset = 0,
                       eff_tax = tax_rate, goodwill = goodwill)$roe, 29)

})

# Calculate bottom-up beta
test_that("Verify bottom-up beta", {
  average_beta <- 0.77
  industry_tax <- 0.0597
  average_de <- 0.083
  firm_tax <- 0.16
  firm_debt <- 13.03
  firm_equity <- 212.92
  expect_equal(get_beta(average_beta = average_beta,
                        industry_tax = industry_tax,
                        average_de = average_de,
                        firm_tax = firm_tax,
                        firm_debt = firm_debt,
                        firm_equity = firm_equity)$unlevered_beta, 0.71)
  expect_equal(get_beta(average_beta = average_beta,
                        industry_tax = industry_tax,
                        average_de = average_de,
                        firm_tax = firm_tax,
                        firm_debt = firm_debt,
                        firm_equity = firm_equity)$levered_beta, 0.75)
})

# Calculate Cost of Equity
test_that("Verift cost of equity", {
  risk_free <- 0.015
  beta <- 0.7
  risk_premium <- 0.041
  expect_equal(get_cost_equity(risk_free = risk_free,
                               beta = beta,
                               risk_premium = risk_premium)$cost_equity, 0.044)
})

# Calculate Cost of Debt
test_that("Verify cost of equity", {
  risk_free <- 0.015
  company_spread <- 0.01
  expect_equal(get_cost_debt(risk_free = risk_free,
                               company_spread = company_spread)$cost_debt, 0.025)
})

# Calculate Cost of Capital
test_that("Verify cost of capital", {
    marginal_tax <- 0.34
    cost_equity <- 0.107
    cost_debt <- 0.0929
    equity <- 11
    debt <- 2
    expect_equal(get_cost_capital(marginal_tax = marginal_tax,
                                  cost_equity = cost_equity,
                                  cost_debt = cost_debt,
                                  equity = equity,
                                  debt = debt)$cost_capital, 0.1)
})
