unit pasStripe.Customer;

interface

uses
  Classes, pasStripe, pasStripe.Json, System.Generics.Collections, pasStripe.Params;

type
  TpsCustomer = class(TInterfacedObject, IpsCustomer)
  private
    FId: string;
    FName: string;
    FDescription: string;
    FEmail: string;
    FJson: string;
    FMetaData: IpsMetaData;
    function GetDescription: string;
    function GetEmail: string;
    function GetID: string;
    function GetJson: string;
    function GetMetaData: IpsMetaData;
    function GetName: string;
  protected
    procedure LoadFromJson(AJson: string); overload;
    procedure LoadFromJson(AJson: TJsonObject); overload;
  public
    constructor Create; virtual;
  end;

 { TpsUpdateCustomerParams = class(TpsBaseParams, IpsUpdateCustomerParams)
  private
    function GetDescription: string;
    function GetName: string;
    procedure SetDescription(const Value: string);
    procedure SetName(const Value: string);
  protected
    property description: string read GetDescription write SetDescription;
  end;  }

implementation

{ TpsCustomer }

constructor TpsCustomer.Create;
begin
  FMetaData := TpsFactory.Metadata;
end;

function TpsCustomer.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCustomer.GetEmail: string;
begin
  Result := FEmail;
end;

function TpsCustomer.GetID: string;
begin
  Result := FId;
end;

function TpsCustomer.GetJson: string;
begin
  Result := FJson;
end;

function TpsCustomer.GetMetaData: IpsMetaData;
begin
  Result := FMetaData;
end;

function TpsCustomer.GetName: string;
begin
  Result := FName;
end;

procedure TpsCustomer.LoadFromJson(AJson: TJsonObject);
begin
  FId := AJson.S['id'];
  if not AJson.IsNull('name') then Fname := AJson.S['name'];
  if not AJson.IsNull('description') then FDescription := AJson.S['description'];
  if not AJson.IsNull('email') then FEmail := AJson.S['email'];
  FMetaData.LoadFromJson(AJson.O['metadata']);
end;

procedure TpsCustomer.LoadFromJson(AJson: string);
var
  AObj: TJsonObject;
begin
  FJson := AJson;
  AObj := TJsonObject.Create;
  try
    AObj.FromJSON(AJson);
    LoadFromJson(AObj);
  finally
    AObj.Free;
  end;
end;


{ TpsUpdateCustomerParams }
         {
function TpsUpdateCustomerParams.GetDescription: string;
begin
  Result := GetParam(TpsParamName.description);
end;

function TpsUpdateCustomerParams.GetName: string;
begin
  Result := GetParam(name);
end;

procedure TpsUpdateCustomerParams.SetDescription(const Value: string);
begin
  SetParam(TpsParamName.description, Value);
end;

procedure TpsUpdateCustomerParams.SetName(const Value: string);
begin
  SetParam(name, Value);
end;     }

end.
