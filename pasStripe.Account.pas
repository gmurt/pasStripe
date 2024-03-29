unit pasStripe.Account;

interface

uses pasStripe;

type
  TpsAccount = class(TInterfacedObject, IpsAccount)
  private
    Fid: string;
    Fcharges_enabled: Boolean;
    FName: string;
    Fjson: string;
    FMetaData: IpsMetaData;
    function GetChargesEnabled: Boolean;
    function GetID: string;
    function GetJson: string;
    function GetMetaData: IpsMetaData;
    function GetName: string;
  protected
    procedure LoadFromJson(AJson: string);
  public
    constructor Create; virtual;
  end;


implementation

uses pasStripe.Json, pasStripe.Constants;

{ TpsAccount }

function TpsAccount.GetName: string;
begin
  Result := FName;
end;

procedure TpsAccount.LoadFromJson(AJson: string);
var
  AObj: TJsonObject;
begin
  Fjson := AJson;
  AObj := TJsonObject.Create;
  try
    AObj.FromJSON(AJson);
    Fid := AObj.S[id];
    Fcharges_enabled := AObj.B[charges_enabled];
    if not AObj.O['business_profile'].IsNull('name') then
      Fname := AObj.O['business_profile'].S[name];

    FMetaData.LoadFromJson(AObj.O['metadata'] );
  finally
    AObj.Free;
  end;
end;

constructor TpsAccount.Create;
begin
  FMetaData := TpsFactory.MetaData;
end;

function TpsAccount.GetChargesEnabled: Boolean;
begin
  Result := Fcharges_enabled;
end;

function TpsAccount.GetID: string;
begin
  Result := Fid;
end;

function TpsAccount.GetJson: string;
begin
  Result := Fjson;
end;

function TpsAccount.GetMetaData: IpsMetaData;
begin
  Result := FMetaData;
end;


end.
