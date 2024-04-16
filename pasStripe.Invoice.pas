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

unit pasStripe.Invoice;

interface

uses pasStripe, pasStripe.Base, pasStripe.Json;

type
  TpsInvoiceListOptions = class(TInterfacedObject, IpsInvoiceListOptions)
  private
    FFrom: TDateTime;
    FTo: TDateTime;
    Flimit: integer;
    FCustomer: string;
    function GetCustomer: string;
    function GetFromDate: TDateTime;
    function GetToDate: TDateTime;
    function GetLimit: integer;
    procedure SetFromDate(const Value: TDateTime);
    procedure SetLimit(const Value: integer);
    procedure SetToDate(const Value: TDateTime);
    procedure SetCustomer(const Value: string);
   public
    constructor Create; virtual;
  end;


  TpsInvoice = class(TInterfacedObject, IpsInvoice)
  private
    FID: string;
    FJson: string;
    FPdfUrl: string;
    FStatus: string;
    FCreated: TDateTime;
    FTotal: integer;
    FCurrency: string;
    FNumber: string;
    function GetJson: string;
    function GetID: string;
    function GetTotal: integer;
    function GetPdfUrl: string;
    function GetStatus: string;
    function GetCreated: TDateTime;
    function GetCurrency: string;
    function GetNumber: string;
  protected
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TpsJsonObject); overload;
  end;

  TpsInvoiceList = class(TpsBaseList<IpsInvoice>, IpsInvoiceList);

implementation

uses pasStripe.Constants, pasStripe.Params, pasStripe.ParamTypes, DateUtils;

{ TpsInvoice }

function TpsInvoice.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsInvoice.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsInvoice.GetID: string;
begin
  Result := FID;
end;

function TpsInvoice.GetJson: string;
begin
  Result := FJson;
end;

function TpsInvoice.GetNumber: string;
begin
  Result := FNumber;
end;

function TpsInvoice.GetPdfUrl: string;
begin
  Result := FPdfUrl;
end;

function TpsInvoice.GetStatus: string;
begin
  Result := FStatus;
end;

function TpsInvoice.GetTotal: integer;
begin
  Result := FTotal;
end;

procedure TpsInvoice.LoadFromJson(AJson: string);
var
  AObj: TpsJsonObject;
begin
  AObj := TpsJsonObject.ParseJSONValue(AJson) as TpsJsonObject;
  try
    LoadFromJson(AObj);
  finally
    AObj.Free;
  end;
end;

procedure TpsInvoice.LoadFromJson(AJson: TpsJsonObject);
begin
  FJson := AJson.ToJSON;
  FID := AJson.S[id];
  FPdfUrl := AJson.S[invoice_pdf];
  FStatus := AJson.S[status];
  FCreated := UnixToDateTime(AJson.I[created]);
  FCurrency := AJson.S[currency];
  FTotal := AJson.I[total];
  FNumber := AJson.S[number];
end;

{ TpsInvoiceListOptions }

constructor TpsInvoiceListOptions.Create;
begin
  Flimit := -1;
  FFrom := -1;
  FTo := -1;
end;

function TpsInvoiceListOptions.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsInvoiceListOptions.GetFromDate: TDateTime;
begin
  Result := FFrom;
end;

function TpsInvoiceListOptions.GetLimit: integer;
begin
  Result := Flimit;
end;

function TpsInvoiceListOptions.GetToDate: TDateTime;
begin
  Result := FTo;
end;

procedure TpsInvoiceListOptions.SetCustomer(const Value: string);
begin
  FCustomer := Value;
end;

procedure TpsInvoiceListOptions.SetFromDate(const Value: TDateTime);
begin
  FFrom := Value;
end;

procedure TpsInvoiceListOptions.SetLimit(const Value: integer);
begin
  Flimit := Value;
end;

procedure TpsInvoiceListOptions.SetToDate(const Value: TDateTime);
begin
  FTo := Value;
end;

end.
