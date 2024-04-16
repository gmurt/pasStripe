unit pasStripe.ParamTypes;

interface

type
  TpsParamName = (id, arrival_date, amount, amount_received, amount_refunded, application_fee_amount, brand, business_type,
  cancel_url, charge, charges_enabled, city,
                  client_secret, confirm, created, customer,customer_email, currency, default_source,
                  deleted, description, email, exp_month, exp_year,
                  has_more, invoice_pdf, last4, &message, mode, name, number, payment_intent, payment_method,
                  payment_status, price_id, setup_future_usage,
                  reason, setup_intent,
                  status, success_url, total, url, &type);

  TpsParamNames = set of TpsParamName;


implementation

end.
