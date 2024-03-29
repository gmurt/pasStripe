unit pasStripe.Checkout;

interface

uses
  pasStripe, pasStripe.Json, pasStripe.Params;

type

                                               {
  TpsCheckoutParams = class(TInterfacedObject, IpsCheckoutParams)
  private
    FCurrency: string;
    FEmail: string;
    FCustomer: string;
    FDescription: string;
    FMode: TpsCheckoutMode;
    FPriceID: string;
    FSuccessUrl: string;
    FCancelUrl: string;
    FAmount: integer;
    FPaymentMethods: TpsPaymentMethodsTypes;
    FMetaData: IpsMetaData;
    FApplicationFee: integer;
    FClientReferenceID: string;
    FTaxID: string;
    function GetAmount: integer;
    function GetApplicationFee: integer;
    function GetCancelUrl: string;
    function GetClientReferenceID: string;
    function GetCurrency: string;
    function GetCustomer: string;
    function GetDescription: string;
    function GetEmail: string;
    function GetMetaData: IpsMetaData;
    function GetMode: TpsCheckoutMode;
    function GetPaymentMethodsTypes: TpsPaymentMethodsTypes;
    function GetSuccessUrl: string;
    procedure SetAmount(const Value: integer);
    procedure SetApplicationFee(const Value: integer);
    procedure SetCancelUrl(const Value: string);
    procedure SetClientReferenceID(const Value: string);
    procedure SetCurrency(const Value: string);
    procedure SetCustomer(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetMode(const Value: TpsCheckoutMode);
    procedure SetPaymentMethodsTypes(const Value: TpsPaymentMethodsTypes);
    procedure SetSuccessUrl(const Value: string);
    function GetTaxID: string;
    procedure SetTaxID(const Value: string);
    function GetPriceID: string;
    procedure SetPriceID(const Value: string);
  public
    constructor Create; virtual;
    procedure Clear;
  end;
                    }

  TpsCheckoutSession = class(TInterfacedObject, IpsCheckoutSession)
  private
    FID: string;
    FCreated: TDateTime;
    FUrl: string;
    FPaymentIntentID: string;
    FSetupIntentID: string;
    FStatus: string;
    FPaymentStatus: string;
    FMetadata: IpsMetadata;
    FJson: string;

    function GetID: string;
    function GetCreated: TDateTime;
    function GetPaymentIntentID: string;
    function GetSetupIntentID: string;
    function GetJson: string;
    function GetPaymentStatus: string;
    function GetStatus: string;
    function GetUrl: string;
    function GetMetadata: IpsMetadata;
    procedure SetID(const Value: string);
    procedure SetPaymentIntentID(const Value: string);
    procedure SetSetupIntentID(const Value: string);
    procedure SetJson(const Value: string);
    procedure SetPaymentStatus(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetUrl(const Value: string);
  protected
    procedure Clear;
    procedure LoadFromJson(AData: string);
  public
    constructor Create; virtual;
  end;


implementation

uses SysUtils, DateUtils;

{ TpsCheckoutSession }

procedure TpsCheckoutSession.Clear;
begin
  FID := '';
  FUrl := '';
  FCreated := 0;
  FPaymentIntentID := '';
  FSetupIntentID := '';
  FStatus := '';
  FPaymentStatus := '';
  FJson := '';
  FMetadata.Clear;
end;

constructor TpsCheckoutSession.Create;
begin
  FMetadata := TpsFactory.Metadata;
end;

function TpsCheckoutSession.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsCheckoutSession.GetID: string;
begin
  Result := FID;
end;

function TpsCheckoutSession.GetPaymentIntentID: string;
begin
  Result := FPaymentIntentID;
end;

function TpsCheckoutSession.GetJson: string;
begin
  Result := FJson;
end;

function TpsCheckoutSession.GetMetadata: IpsMetadata;
begin
  Result := FMetadata;
end;

function TpsCheckoutSession.GetPaymentStatus: string;
begin
  Result := FPaymentStatus;
end;

function TpsCheckoutSession.GetSetupIntentID: string;
begin
  Result := FSetupIntentID;
end;

function TpsCheckoutSession.GetStatus: string;
begin
  Result := FStatus;
end;

function TpsCheckoutSession.GetUrl: string;
begin
  Result := FUrl;
end;

procedure TpsCheckoutSession.LoadFromJson(AData: string);
var
  AJson: TJsonObject;
begin
  Clear;
  AJson := TJsonObject.Create;  //Parse(AData) as TJsonObject;
  try
    AJson.FromJSON(AData);
    FID := AJson.S['id'];
    FCreated := UnixToDateTime(StrToInt(AJson.S['created']));
    if not AJson.IsNull('url') then FUrl := AJson.S['url'];
    if AJson.Types['payment_intent'] = jvtString then FPaymentIntentID := AJson.S['payment_intent'];
    if AJson.Types['setup_intent'] = jvtString then FSetupIntentID := AJson.S['setup_intent'];
    FStatus := AJson.S['status'];
    FPaymentStatus := AJson.S['payment_status'];
    FMetadata.LoadFromJson(AJson.O['metadata']);
    FJson := AData;
  finally
    AJson.Free;
  end;
end;



procedure TpsCheckoutSession.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TpsCheckoutSession.SetPaymentIntentID(const Value: string);
begin
  FPaymentIntentID := Value;
end;

procedure TpsCheckoutSession.SetSetupIntentID(const Value: string);
begin
  FSetupIntentID := Value;
end;

procedure TpsCheckoutSession.SetJson(const Value: string);
begin
  FJson := Value;
end;

procedure TpsCheckoutSession.SetPaymentStatus(const Value: string);
begin
  FPaymentStatus := Value;
end;

procedure TpsCheckoutSession.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TpsCheckoutSession.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

{ TpsCheckoutParams }
      {
procedure TpsCheckoutParams.Clear;
begin
  FCurrency := '';
  FEmail := '';
  FCustomer := '';
  FDescription := '';
  FMode := cmSetup;
  FPriceID := '';
  FSuccessUrl := '';
  FCancelUrl := '';
  FAmount := 0;
  FPaymentMethods := [];
  FMetaData.Clear;
  FApplicationFee := 0;
  FClientReferenceID := '';
  FTaxID := '';
end;

constructor TpsCheckoutParams.Create;
begin
  FMetaData := TpsFactory.Metadata;
end;

function TpsCheckoutParams.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsCheckoutParams.GetApplicationFee: integer;
begin
  Result := FApplicationFee;
end;

function TpsCheckoutParams.GetCancelUrl: string;
begin
  Result := FCancelUrl;
end;

function TpsCheckoutParams.GetClientReferenceID: string;
begin
  Result := FClientReferenceID;
end;

function TpsCheckoutParams.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsCheckoutParams.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsCheckoutParams.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCheckoutParams.GetEmail: string;
begin
  Result := FEmail;
end;

function TpsCheckoutParams.GetMetaData: IpsMetaData;
begin
  Result := FMetaData;
end;

function TpsCheckoutParams.GetMode: TpsCheckoutMode;
begin
  Result := FMode;
end;

function TpsCheckoutParams.GetPaymentMethodsTypes: TpsPaymentMethodsTypes;
begin
  Result := FPaymentMethods;
end;

function TpsCheckoutParams.GetPriceID: string;
begin
  Result := FPriceID;
end;

function TpsCheckoutParams.GetSuccessUrl: string;
begin
  Result := FSuccessUrl;
end;

function TpsCheckoutParams.GetTaxID: string;
begin
  Result := FTaxID;
end;

procedure TpsCheckoutParams.SetAmount(const Value: integer);
begin
  FAmount := Value;
end;

procedure TpsCheckoutParams.SetApplicationFee(const Value: integer);
begin
  FApplicationFee := Value;
end;

procedure TpsCheckoutParams.SetCancelUrl(const Value: string);
begin
  FCancelUrl := Value;
end;

procedure TpsCheckoutParams.SetClientReferenceID(const Value: string);
begin
  FClientReferenceID := Value;
end;

procedure TpsCheckoutParams.SetCurrency(const Value: string);
begin
  FCurrency := Value;
end;

procedure TpsCheckoutParams.SetCustomer(const Value: string);
begin
  FCustomer := Value;
end;

procedure TpsCheckoutParams.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TpsCheckoutParams.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TpsCheckoutParams.SetMode(const Value: TpsCheckoutMode);
begin
  FMode := Value;
end;

procedure TpsCheckoutParams.SetPaymentMethodsTypes(const Value: TpsPaymentMethodsTypes);
begin
  FPaymentMethods := Value;
end;

procedure TpsCheckoutParams.SetPriceID(const Value: string);
begin
  FPriceID := Value;
end;

procedure TpsCheckoutParams.SetSuccessUrl(const Value: string);
begin
  FSuccessUrl := Value;
end;

procedure TpsCheckoutParams.SetTaxID(const Value: string);
begin
  FTaxID := Value;
end;            }

end.
