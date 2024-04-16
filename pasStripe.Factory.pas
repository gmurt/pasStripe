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

unit pasStripe.Factory;

interface

uses pasStripe;

type
  TpsFactory = class(TInterfacedObject, IpsFactory)
  protected
    function PasStripe(ASecretKey: string; const AAccount: string = ''): IPasStripe;
    function Account: IpsAccount;
    function PaymentMethod: IpsPaymentMethod;
    function Customer: IpsCustomer;
    function Charge: IpsCharge;
    function ChargeList: IpsChargeList;
    function ChargeListOptions: IpsChargeListOptions;

    function Payout: IpsPayout;
    function PayoutList: IpsPayoutList;
    function PayoutListOptions: IpsPayoutListOptions;

    function Invoice: IpsInvoice;
    function InvoiceList: IpsInvoiceList;
    function InvoiceListOptions: IpsInvoiceListOptions;

    function PaymentIntent: IpsPaymentIntent;
    function SetupIntent: IpsSetupIntent;
    function CheckoutSession: IpsCheckoutSession;
    function CheckoutSessionList: IpsCheckoutSessionList;

    // params
    function CreateAccountParams: IpsCreateAccountParams;
    function CreateCheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): IpsCreateCheckoutParams;
    function CreateChargeParams(AAmount: integer; ACurrency: TpsCurrency): IpsCreateChargeParams;
    function CreateCustomerParams: IpsCreateCustomerParams;

    function CreatePaymentIntentParams(AAmount: integer; ADesc: string; ACurrency: TpsCurrency): IpsCreatePaymentIntentParams;
    function CreateRefundParams: IpsCreateRefundParams;

    function UpdateAccountParams: IpsUpdateAccountParams;
    function UpdateChargeParams: IpsUpdateChargeParams;
    function UpdateCustomerParams: IpsUpdateCustomerParams;
  end;


implementation

uses
  pasStripe.Core,
  pasStripe.Account,
  pasStripe.Metadata,
  pasStripe.PaymentMethod,
  pasStripe.Customer,
  pasStripe.Charge,
  pasStripe.Payout,

  pasStripe.Intents,
  pasStripe.Checkout,
  pasStripe.Invoice,
  pasStripe.Utils,
  pasStripe.Params,
  pasStripe.Refund;

{ TpsFactory }

function TpsFactory.Account: IpsAccount;
begin
  Result := TpsAccount.Create;
end;

function TpsFactory.Charge: IpsCharge;
begin
  Result := TpsCharge.Create;
end;

function TpsFactory.ChargeList: IpsChargeList;
begin
  Result := TpsChargeList.Create;
end;

function TpsFactory.ChargeListOptions: IpsChargeListOptions;
begin
  Result := TpsChargeListOptions.Create;
end;


function TpsFactory.Payout: IpsPayout;
begin
  Result := TpsPayout.Create;
end;

function TpsFactory.PayoutList: IpsPayoutList;
begin
  Result := TpsPayoutList.Create;
end;

function TpsFactory.PayoutListOptions: IpsPayoutListOptions;
begin
  Result := TpsPayoutListOptions.Create;
end;

function TpsFactory.CreateAccountParams: IpsCreateAccountParams;
begin
  Result := TpsCreateAccountParams.Create(nil);
end;

function TpsFactory.CreateChargeParams(AAmount: integer; ACurrency: TpsCurrency): IpsCreateChargeParams;
begin
  Result := TpsCreateChargeParams.Create(nil);
  Result.Amount := AAmount;
  Result.Currency := ACurrency;
end;

function TpsFactory.CreateCheckoutParams(AMode: TpsCheckoutMode; ACurrency: TpsCurrency): IpsCreateCheckoutParams;
begin
  Result := TpsCreateCheckoutParams.Create(nil);
  Result.Mode := AMode;
  Result.Currency := ACurrency;
end;

function TpsFactory.CreateCustomerParams: IpsCreateCustomerParams;
begin
  Result := TpsCustomerParams.Create(nil);
end;

function TpsFactory.CreatePaymentIntentParams(AAmount: integer; ADesc: string; ACurrency: TpsCurrency): IpsCreatePaymentIntentParams;
begin
  Result := TpsCreatePaymentIntentParams.Create(nil);
  Result.Amount := AAmount;
  Result.Currency := ACurrency;
  Result.Description := ADesc;
end;

function TpsFactory.CreateRefundParams: IpsCreateRefundParams;
begin
  Result := TpsCreateRefundParams.Create(nil);
end;

function TpsFactory.CheckoutSession: IpsCheckoutSession;
begin
  Result := TpsCheckoutSession.Create;
end;

function TpsFactory.CheckoutSessionList: IpsCheckoutSessionList;
begin
    Result := TpsCheckoutSessionList.Create;
end;

function TpsFactory.Customer: IpsCustomer;
begin
  Result := TpsCustomer.Create;
end;

function TpsFactory.Invoice: IpsInvoice;
begin
  Result := TpsInvoice.Create;
end;

function TpsFactory.InvoiceList: IpsInvoiceList;
begin
  Result := TpsInvoiceList.Create;
end;

function TpsFactory.InvoiceListOptions: IpsInvoiceListOptions;
begin
  Result := TpsInvoiceListOptions.Create;
end;

function TpsFactory.PasStripe(ASecretKey: string; const AAccount: string = ''): IPasStripe;
begin
  Result := TPasStripe.Create(ASecretKey, AAccount);
end;

function TpsFactory.PaymentIntent: IpsPaymentIntent;
begin
  Result := TpsPaymentIntent.Create;
end;

function TpsFactory.PaymentMethod: IpsPaymentMethod;
begin
  Result := TpsPaymentMethod.Create;
end;


function TpsFactory.SetupIntent: IpsSetupIntent;
begin
  Result := TpsSetupIntent.Create;
end;


function TpsFactory.UpdateAccountParams: IpsUpdateAccountParams;
begin
  Result := TpsUpdateAccountParams.Create(nil);
end;

function TpsFactory.UpdateChargeParams: IpsUpdateChargeParams;
begin
  Result := TpsUpdateChargeParams.Create(nil);
end;

function TpsFactory.UpdateCustomerParams: IpsUpdateCustomerParams;
begin
  Result := TpsCustomerParams.Create(nil);
end;

end.
