unit pasStripe.Utils;

interface

uses pasStripe;

function ParamToString(AParam: TpsParamName): string;
function PaymentMethodToString(APaymentMethod: TpsPaymentMethodType): string;
function CheckoutModeToString(ACheckoutMode: TpsCheckoutMode): string;
function IntervalToString(AInterval: TpsRecurring): string;

function StringToCheckoutMode(AValue: string): TpsCheckoutMode;

function CurrencyToString(ACurrency: TpsCurrency): string;
function StringToCurrency(AValue: string): TpsCurrency;

implementation

uses SysUtils, RTTI;

function ParamToString(AParam: TpsParamName): string;
begin
  Result := '';
  case AParam of
    amount: Result := 'amount';
    cancelUrl: Result := 'cancel_url';
    currency: Result := 'currency';
    description: Result := 'description';
    email: Result := 'email';
    mode: Result := 'mode';
    successUrl: Result := 'success_url';
  end;
  if Result = '' then
    raise Exception.Create('ParamtoString error! unknown parameter: '+TRttiEnumerationType.GetName(AParam));
end;
   {

function CurrencyToString(ACurrency: TpsCurrency): string;
begin
  case ACurrency of
    scUnknown: Result := '';
    scGbp: Result := 'gbp';
    scEur: Result := 'eur';
    scUsd: Result := 'usd';
  end;
end;            }


function PaymentMethodToString(APaymentMethod: TpsPaymentMethodType): string;
begin
  Result := '';
  if APaymentMethod = pmDirectDebit then
    Result := 'bacs_debit';
  if APaymentMethod = pmCard then
    Result := 'card';
end;

function CheckoutModeToString(ACheckoutMode: TpsCheckoutMode): string;
begin
  case ACheckoutMode of
    cmSetup: Result := 'setup';
    cmPayment: Result := 'payment';
    cmSubscription: Result := 'subscription';
  end;
end;

function IntervalToString(AInterval: TpsRecurring): string;
begin
  case AInterval of
    rDaily: Result := 'day';
    rWeekly: Result := 'week';
    rMonthly: Result := 'month';
    rYearly: Result := 'year';
  end;
end;

function StringToCheckoutMode(AValue: string): TpsCheckoutMode;
begin
  Result := cmPayment;
  AValue := Trim(AValue).ToLower;
  if AValue = 'setup' then Result := cmSetup;
  if AValue = 'payment' then Result := cmPayment;
  if AValue = 'subscription' then Result := cmSubscription;

end;

function CurrencyToString(ACurrency: TpsCurrency): string;
begin
  Result := '';
  case ACurrency of
    scGbp: Result := 'gbp';
    scEur: Result := 'eur';
    scUsd: Result := 'usd';
  end;
end;

function StringToCurrency(AValue: string): TpsCurrency;
begin
  Result := scGbp; // default
  AValue := Trim(AValue).ToLower;
  if AValue = 'gbp' then Result := scGbp;
  if AValue = 'eur' then Result := scEur;
  if AValue = 'usd' then Result := scUsd;

end;

end.
