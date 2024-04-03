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

uses pasStripe, pasStripe.Json;

type
  TpsInvoice = class(TInterfacedObject, IpsInvoice)
  private
    FID: string;
    FJson: string;
    FPdfUrl: string;
    function GetID: string;
    function GetJson: string;
    function GetPdfUrl: string;
  protected
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
  end;


implementation

uses pasStripe.Constants, pasStripe.Params, System.Json;

{ TpsInvoice }

function TpsInvoice.GetID: string;
begin
  Result := FID;
end;

function TpsInvoice.GetJson: string;
begin
  Result := FJson;
end;

function TpsInvoice.GetPdfUrl: string;
begin
  Result := FPdfUrl;
end;

procedure TpsInvoice.LoadFromJson(AJson: string);
var
  AObj: TJsonObject;
begin
  AObj := TJsonObject.Create;
  try
    AObj.FromJSON(AJson);
    LoadFromJson(AObj);
  finally
    AObj.Free;
  end;
end;

procedure TpsInvoice.LoadFromJson(AJson: TJsonObject);
begin
  FJson := AJson.ToJSON;
  FID := AJson.S[id];
  FPdfUrl := AJson.S[invoice_pdf];
end;

end.
