unit pasStripe.Intents;

interface

uses pasStripe, pasStripe.Json;

type
  TpsPaymentIntent = class(TInterfacedObject, IpsPaymentIntent)
  private
    Fid: string;
    FAmount: integer;
    FApplicationFee: integer;
    FMetadata: string;
    FPaid: Boolean;
    FCreated: TDateTime;
    FClientSecret: string;
    FPaymentMethod: string;
    function GetMetaData(AName: string): string;
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCreated: TDateTime;
    function GetId: string;
    function GetPaid: Boolean;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
  protected
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
  end;

  TpsSetupIntent = class(TInterfacedObject, IpsSetupIntent)
  private
    FID: string;
    FClientSecret: string;
    FPaymentMethod: string;
    function GetID: string;
    function GetClientSecret: string;
    function GetPaymentMethod: string;
  protected
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
  end;

implementation

uses SysUtils, DateUtils;

{ TpsPaymentIntent }

function TpsPaymentIntent.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsPaymentIntent.GetApplicationFee: integer;
begin
  Result := FApplicationFee;
end;

function TpsPaymentIntent.GetClientSecret: string;
begin
  Result := FClientSecret;
end;

function TpsPaymentIntent.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsPaymentIntent.GetId: string;
begin
  Result := Fid;
end;

function TpsPaymentIntent.GetMetaData(AName: string): string;
var
  AJson: TJsonObject;
begin
  Result := '';
  if FMetadata <> '' then
  begin
    AJson := TJsonObject.Create;// Parse(FMetadata) as TJsonObject;
    try
      AJson.FromJSON(FMetaData);
      Result := AJson.S[AName];
    finally
      AJson.Free;
    end;
  end;
end;


function TpsPaymentIntent.GetPaid: Boolean;
begin
  Result := FPaid;
end;

function TpsPaymentIntent.GetPaymentMethod: string;
begin
  Result := FPaymentMethod;
end;

procedure TpsPaymentIntent.LoadFromJson(AJson: string);
var
  AJsonObj: TJsonObject;
begin
  AJsonObj := TJsonObject.Create;
  try
    AJsonObj.FromJSON(AJson);
    LoadFromJson(AJsonObj);
  finally
    AJsonObj.Free;
  end;
end;

procedure TpsPaymentIntent.LoadFromJson(AJson: TJsonObject);
begin
  Fid := AJson.S['id'];
  FAmount := AJson.I['amount'];
  if AJson.IsNull('application_fee_amount') = False then
    FApplicationFee := AJson.I['application_fee_amount'];
  if AJson.IsNull('metadata') = False then
    FMetadata := AJson.O['metadata'].ToJSON;
  FPaid := AJson.I['amount_received'] >= AJson.I['amount'];
  FCreated := UnixToDateTime(StrToInt(AJson.S['created']));
  FClientSecret := AJson.S['client_secret'];
  if not AJson.IsNull('payment_method') then FPaymentMethod := AJson.S['payment_method'];
  
end;

{ TpsSetupIntent }


function TpsSetupIntent.GetClientSecret: string;
begin
  Result := FClientSecret;
end;

function TpsSetupIntent.GetID: string;
begin
  Result := FID;
end;

function TpsSetupIntent.GetPaymentMethod: string;
begin
  Result := FPaymentMethod;
end;

procedure TpsSetupIntent.LoadFromJson(AJson: string);
var
  AJsonObj: TJsonObject;
begin
  AJsonObj := TJsonObject.Create;
  try
    AJsonObj.FromJSON(AJson);
    LoadFromJson(AJsonObj);
  finally
    AJsonObj.Free;
  end;
end;

procedure TpsSetupIntent.LoadFromJson(AJson: TJsonObject);
begin
  FID := AJson.S['id'];
  if not AJson.IsNull('payment_method') then FPaymentMethod := AJson.S['payment_method'];
  if not AJson.IsNull('client_secret') then FClientSecret := AJson.S['client_secret'];
end;


end.
