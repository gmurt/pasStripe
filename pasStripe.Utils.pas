{*******************************************************************************
*                                                                              *
*  pasStripe - Stripe Interfaces for Delphi                                    *
*                                                                              *
*  https://github.com/gmurt/pasStripe                                          *
*                                                                              *
*  Copyright 2024 Graham Murt                                                  *
*                                                                              *                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit pasStripe.Utils;

interface

uses Classes, pasStripe, pasStripe.Constants, pasStripe.ParamTypes;

function ParamToString(AParam: TpsParamName): string;
function PaymentMethodToString(APaymentMethod: TpsPaymentMethodType): string;
procedure PaymentMethodsToStrings(APaymentMethods: TpsPaymentMethodsTypes; AStrings: TStrings);
function CheckoutModeToString(ACheckoutMode: TpsCheckoutMode): string;
function IntervalToString(AInterval: TpsRecurring): string;

function StringToCheckoutMode(AValue: string): TpsCheckoutMode;

function CurrencyToString(ACurrency: TpsCurrency): string;
function StringToCurrency(AValue: string): TpsCurrency;

implementation

uses SysUtils, RTTI;

function ParamToString(AParam: TpsParamName): string;
begin
  Result := TRttiEnumerationType.GetName(AParam)
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

procedure PaymentMethodsToStrings(APaymentMethods: TpsPaymentMethodsTypes; AStrings: TStrings);
var
  APaymentMethod: TpsPaymentMethodType;
  AIndex: integer;
begin
  AIndex := 0;
  for APaymentMethod in APaymentMethods do
  begin
    AStrings.Values['payment_method_types[' + AIndex.ToString + ']'] := PaymentMethodToString(APaymentMethod);
    Inc(AIndex);
  end;
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
    Daily: Result := 'day';
    Weekly: Result := 'week';
    Monthly: Result := 'month';
    Yearly: Result := 'year';
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
  Result := 'gbp';
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
