# Currency helpers ------------------------------------------------------------
# v1: the currency is a display label only. No exchange rates, no PPP.

CURRENCIES <- c(
  "USD - US dollar"            = "USD",
  "EUR - Euro"                 = "EUR",
  "GBP - Pound sterling"       = "GBP",
  "CHF - Swiss franc"          = "CHF",
  "Int$ - International dollar" = "Int$",
  "ARS - Argentine peso"       = "ARS",
  "BOB - Boliviano"            = "BOB",
  "BRL - Brazilian real"       = "BRL",
  "CLP - Chilean peso"         = "CLP",
  "COP - Colombian peso"       = "COP",
  "CRC - Costa Rican colon"    = "CRC",
  "GTQ - Guatemalan quetzal"   = "GTQ",
  "HNL - Honduran lempira"     = "HNL",
  "MXN - Mexican peso"         = "MXN",
  "NIO - Nicaraguan cordoba"   = "NIO",
  "PAB - Panamanian balboa"    = "PAB",
  "PEN - Peruvian sol"         = "PEN",
  "PYG - Paraguayan guarani"   = "PYG",
  "UYU - Uruguayan peso"       = "UYU",
  "VES - Venezuelan bolivar"   = "VES",
  "DOP - Dominican peso"       = "DOP",
  "CAD - Canadian dollar"      = "CAD",
  "AUD - Australian dollar"    = "AUD",
  "JPY - Japanese yen"         = "JPY",
  "CNY - Chinese yuan"         = "CNY",
  "INR - Indian rupee"         = "INR",
  "IDR - Indonesian rupiah"    = "IDR",
  "PHP - Philippine peso"      = "PHP",
  "VND - Vietnamese dong"      = "VND",
  "THB - Thai baht"            = "THB",
  "BDT - Bangladeshi taka"     = "BDT",
  "PKR - Pakistani rupee"      = "PKR",
  "ZAR - South African rand"   = "ZAR",
  "NGN - Nigerian naira"       = "NGN",
  "KES - Kenyan shilling"      = "KES",
  "ETB - Ethiopian birr"       = "ETB",
  "TZS - Tanzanian shilling"   = "TZS",
  "UGX - Ugandan shilling"     = "UGX",
  "GHS - Ghanaian cedi"        = "GHS",
  "XOF - West African CFA franc" = "XOF",
  "XAF - Central African CFA franc" = "XAF",
  "CDF - Congolese franc"      = "CDF",
  "MZN - Mozambican metical"   = "MZN",
  "EGP - Egyptian pound"       = "EGP",
  "MAD - Moroccan dirham"      = "MAD"
)

#' Format a monetary amount with the currency label
format_money <- function(x, currency, digits = 0) {
  ifelse(is.na(x), "",
         paste(currency, formatC(x, format = "f", digits = digits, big.mark = ",")))
}

#' Format a plain number with thousands separators
format_num <- function(x, digits = 0) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}
