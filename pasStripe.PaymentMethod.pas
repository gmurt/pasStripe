unit pasStripe.PaymentMethod;

interface

uses pasStripe, pasStripe.Json;

type
  TpsPaymentMethod = class(TInterfacedObject, IpsPaymentMethod)
  private
    FID: string;
    FCustomer: string;
    FExpiryMonth: integer;
    FExpiryYear: integer;
    FBrand: string;
    FLast4: string;
    FJson: string;
    function GetID: string;
    function GetCustomer: string;
    function GetBrand: string;
    function GetExpiryMonth: integer;
    function GetExpiryYear: integer;
    function GetExpiryStr: string;
    function GetLast4: string;
    function GetJson: string;
  protected
    procedure LoadFromJson(AJson: TJsonObject);
  end;

implementation

uses SysUtils;

{ TpsPaymentMethod }


function TpsPaymentMethod.GetBrand: string;
begin
  Result := FBrand;
end;

function TpsPaymentMethod.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsPaymentMethod.GetExpiryMonth: integer;
begin
  Result := FExpiryMonth;
end;

function TpsPaymentMethod.GetExpiryStr: string;
begin
  Result := FormatFloat('00', FExpiryMonth)+' / '+ FormatFloat('0000', FExpiryYear);
end;

function TpsPaymentMethod.GetExpiryYear: integer;
begin
  Result := FExpiryYear;
end;

function TpsPaymentMethod.GetID: string;
begin
  Result := FID;
end;

function TpsPaymentMethod.GetJson: string;
begin
  Result := FJson;
end;

function TpsPaymentMethod.GetLast4: string;
begin
  Result := FLast4;
end;

procedure TpsPaymentMethod.LoadFromJson(AJson: TJsonObject);
var
  ACard: TJsonObject;
begin
  FID := AJson.S['id'];
  if AJson.Types['customer'] = jvtString then FCustomer := AJson.S['customer'];

  if AJson.Contains('card') then
  begin
    ACard := AJson.O['card'];
    FExpiryMonth := ACard.I['exp_month'];
    FExpiryYear := ACard.I['exp_year'];
    FExpiryYear := ACard.I['exp_year'];
    FBrand := ACard.S['brand'];
    FLast4 := ACard.S['last4'];
  end;
  FJson := AJson.ToJSON;
end;

end.
