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

unit pasStripe.Params;

interface

uses Classes, System.Generics.Collections, pasStripe;

type
  TpsParamName = (id, amount, amount_received, amount_refunded, application_fee_amount, brand, business_type,
  cancel_url, charge, charges_enabled, city,
                  client_secret, confirm, created, customer,customer_email, currency, default_source,
                  deleted, description, email, exp_month, exp_year,
                  has_more, invoice_pdf, last4, &message, mode, name, payment_intent, payment_method,
                  payment_status, price_id, setup_future_usage,
                  reason, setup_intent,
                  status, success_url, url, &type);

  TpsParamNames = set of TpsParamName;



  TpsBaseParams = class;
  TpsBaseParamsWithMetaData = class;

  TpsBaseParams = class(TInterfacedObject, IpsBaseParams)
  private
    FName: string;
    FParent: TpsBaseParams;
    FParams: TStrings;
  protected
    function GetInteger(AParam: TpsParamName): integer;
    function GetString(AParam: TpsParamName): string;
    function GetBoolean(AParam: TpsParamName): Boolean;
    procedure SetString(AParam: TpsParamName; const AValue: string);
    procedure SetInteger(AParam: TpsParamName; const AValue: integer);
    procedure SetBoolean(AParam: TpsParamName; const AValue: Boolean);
    function GetAsString: string;
    function GetObjectName: string; virtual;
    procedure PopulateStrings(AStrings: TStrings); virtual;
  public
    constructor Create(AParent: TpsBaseParams); virtual;
    destructor Destroy; override;
  end;

  TpsBaseParamsWithMetaData = class(TpsBaseParams)
  private
    FMetaData: IpsMetadata;
  protected
    function GetMetadata: IpsMetadata;
    procedure PopulateStrings(AStrings: TStrings); override;
  public
    constructor Create(AParent: TpsBaseParams); override;
    property MetaData: IpsMetadata read GetMetaData;
  end;

  TpsAddressParams = class(TpsBaseParams, IpsAddressParams)
  private
    function GetCity: string;
    procedure SetCity(const Value: string);
  protected
    function GetObjectName: string; override;
  end;

  TpsCompanyParams = class(TpsBaseParams, IpsCompanyParams)
  private
    FAddress: IpsAddressParams;
  protected
    function GetObjectName: string; override;
    function GetName: string;
    function GetAddress: IpsAddressParams;
    procedure SetName(const Value: string);
    procedure PopulateStrings(AStrings: TStrings); override;
  public
    constructor Create(AParent: TpsBaseParams); override;
  end;

  TpsCreateAccountParams = class(TpsBaseParamsWithMetaData, IpsCreateAccountParams)
  private
    FCompany: IpsCompanyParams;
  protected
    function GetObjectName: string; override;
    function GetBusinessType: string;
    function GetEmail: string;
    function GetCompany: IpsCompanyParams;
    function GetType: string;
    procedure SetBusinessType(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetType(const Value: string);
    procedure PopulateStrings(AStrings: TStrings); override;
  public
    constructor Create(AParent: TpsBaseParams); override;
  end;


implementation

uses SysUtils, Rtti, pasStripe.Utils, pasStripe.MetaData;

 { TpsBaseParams }

constructor TpsBaseParams.Create(AParent: TpsBaseParams);
begin
  inherited Create;
  FParams := TStringList.Create;
  FParent := AParent;
  FName := GetObjectName;
end;

destructor TpsBaseParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TpsBaseParams.GetAsString: string;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    PopulateStrings(AStrings);
    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;

function TpsBaseParams.GetBoolean(AParam: TpsParamName): Boolean;
begin
  Result := GetString(AParam).ToLower = 'true';
end;

function TpsBaseParams.GetInteger(AParam: TpsParamName): integer;
begin
  Result := StrToIntDef(GetString(AParam), 0);
end;

function TpsBaseParams.GetObjectName: string;
begin
  Result := '';
end;

function TpsBaseParams.GetString(AParam: TpsParamName): string;
begin
  Result := FParams.Values[TRttiEnumerationType.GetName(AParam)];
end;

procedure TpsBaseParams.PopulateStrings(AStrings: TStrings);

  function NameStringsToName(ANameStrings: TStrings): string;
  var
    ICount: integer;
  begin
    Result := '';
    for ICount := 1 to ANameStrings.Count-1 do
    begin

      if ICount = 1 then
        Result := Result + ANameStrings[ICount]
      else
        Result := Result + '['+ANameStrings[ICount]+']'
    end;
  end;

var
  ICount: integer;
  ANames: TStrings;
  AParent: TpsBaseParams;
begin
  for ICount := 0 to FParams.Count-1 do
  begin
    ANames := TStringList.Create;
    try
      ANames.Add(FParams.Names[ICount]);
      ANames.Insert(0, FName);
      AParent := FParent;
      while AParent <> nil do
      begin
        ANames.Insert(0, AParent.FName);
        AParent := AParent.FParent;
      end;

      AStrings.Values[NameStringsToName(ANames)] := FParams.ValueFromIndex[ICount];

    finally
      ANames.Free;
    end;
  end;
end;

procedure TpsBaseParams.SetBoolean(AParam: TpsParamName; const AValue: Boolean);
begin
  case AValue of
    True: SetString(AParam, 'true');
    False: SetString(AParam, 'false');
  end;
end;

procedure TpsBaseParams.SetInteger(AParam: TpsParamName; const AValue: integer);
begin
  SetString(AParam, IntToStr(AValue));
end;

procedure TpsBaseParams.SetString(AParam: TpsParamName; const AValue: string);
begin
  FParams.Values[TRttiEnumerationType.GetName(AParam)] := AValue;
end;

{ TpsCreateAccountParams }


constructor TpsCreateAccountParams.Create(AParent: TpsBaseParams);
begin
  inherited;
  FCompany := TpsCompanyParams.Create(Self);
end;

function TpsCreateAccountParams.GetBusinessType: string;
begin
  Result := GetString(business_type);
end;

function TpsCreateAccountParams.GetCompany: IpsCompanyParams;
begin
  Result := FCompany;
end;

function TpsCreateAccountParams.GetEmail: string;
begin
  Result := GetString(email);
end;

function TpsCreateAccountParams.GetObjectName: string;
begin
  Result := 'account';
end;

function TpsCreateAccountParams.GetType: string;
begin
  Result := GetString(&type);
end;

procedure TpsCreateAccountParams.PopulateStrings(AStrings: TStrings);
begin
  inherited;
  FCompany.PopulateStrings(AStrings);
end;

procedure TpsCreateAccountParams.SetBusinessType(const Value: string);
begin
  SetString(business_type, Value);
end;

procedure TpsCreateAccountParams.SetEmail(const Value: string);
begin
  SetString(email, Value);
end;

procedure TpsCreateAccountParams.SetType(const Value: string);
begin
  SetString(&type, Value);
end;

{ TpsAddressParams }

function TpsAddressParams.GetCity: string;
begin
  Result := GetString(TpsParamName.city);
end;

function TpsAddressParams.GetObjectName: string;
begin
  Result := 'address';
end;

procedure TpsAddressParams.SetCity(const Value: string);
begin
  SetString(TpsParamName.city, Value);
end;

{ TpsCompanyParams }


constructor TpsCompanyParams.Create(AParent: TpsBaseParams);
begin
  inherited;
  FAddress := TpsAddressParams.Create(Self);
end;

function TpsCompanyParams.GetAddress: IpsAddressParams;
begin
  Result := FAddress;
end;

function TpsCompanyParams.GetName: string;
begin
  Result := GetString(TpsParamName.name);
end;

function TpsCompanyParams.GetObjectName: string;
begin
  Result := 'company';
end;

procedure TpsCompanyParams.PopulateStrings(AStrings: TStrings);
begin
  inherited;
  FAddress.PopulateStrings(AStrings);
end;

procedure TpsCompanyParams.SetName(const Value: string);
begin
  SetString(TpsParamName.name, Value);
end;

constructor TpsBaseParamsWithMetaData.Create(AParent: TpsBaseParams);
begin
  inherited;
  FMetaData := TpsMetaData.Create;
end;

{ TpsBaseParamsWithMetaData }

function TpsBaseParamsWithMetaData.GetMetaData: IpsMetadata;
begin
  Result := FMetaData;
end;

procedure TpsBaseParamsWithMetaData.PopulateStrings(AStrings: TStrings);
begin
  inherited;
  FMetaData.Enumerate(
    procedure(AMeta: IpsMetaDataRecord)
    begin
      AStrings.Values['metadata['+AMeta.Name+']'] := AMeta.Value;
    end
  );

end;

end.
