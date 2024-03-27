unit pasStripe.Utils;

interface

uses pasStripe;

function PaymentMethodToString(APaymentMethod: TpsPaymentMethodType): string;
function CheckoutModeToString(ACheckoutMode: TpsCheckoutMode): string;

function CurrencyToString(ACurrency: TpsCurrency): string;
function StringToCurrency(AValue: string): TpsCurrency;

implementation

uses SysUtils;

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
