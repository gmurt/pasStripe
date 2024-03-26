unit pasStripe.Charge;

interface

uses pasStripe, pasStripe.Json;

type
  TpsChargeParams = class(TInterfacedObject, IpsChargeParams)
  private
    FAmount: integer;
    FCurrency: string;
    FPaymentMethodID: string;
    FCustomerID: string;
    FDescription: string;
    FMetaData: IpsMetaData;
    function GetAmount: integer;
    function GetCurrency: string;
    function GetCustomerID: string;
    function GetDescription: string;
    function GetMetadata: IpsMetadata;
    function GetPaymentMethodID: string;
    procedure SetPaymentMethodID(const Value: string);
    procedure SetCustomerID(const Value: string);
    procedure SetDescription(const Value: string);

  public
    constructor Create(AAmount: integer; ACurrency: string);  virtual;
    destructor Destroy; override;
  end;



  TpsChargeListOptions = class(TInterfacedObject, IpsChargeListOptions)
  private
    FFrom: TDateTime;
    FTo: TDateTime;
    FpaymentIntentID: string;
    Flimit: integer;
    FQuery: string;
    function GetFromDate: TDateTime;
    function GetToDate: TDateTime;
    function GetPaymentIntentID: string;
    function GetLimit: integer;
    function GetQuery: string;
    procedure SetFromDate(const Value: TDateTime);
    procedure SetLimit(const Value: integer);
    procedure SetToDate(const Value: TDateTime);
    procedure SetQuery(const Value: string);
    procedure SetPaymentIntentID(const Value: string);
  public
    constructor Create; virtual;
  end;


  TpsCharge = class(TInterfacedObject, IpsCharge)
  private
    FID: string;
    FCustomer: string;
    FMetadata: IpsMetaData;
    FDescription: string;
    FJson: string;
    FAmount: integer;
    FRefunded: integer;
    FCreated: TDateTime;
    FCurrency: string;
    FStatus: string;
    FCardBrand: string;
    FLast4: string;
    FPaymentIntentID: string;
    function GetID: string;
    function GetAmount: integer;
    function GetCardBrand: string;
    function GetCreated: TDateTime;
    function GetCurrency: string;
    function GetCustomer: string;
    function GetDescription: string;
    function GetJson: string;
    function GetLast4: string;
    function GetRefunded: integer;
    function GetStatus: string;
    function GetPaymentIntentID: string;
    function GetMetaData: IpsMetaData;
  public
    constructor Create; virtual;
    procedure LoadFromJson(AJson: TJsonObject);

  end;

implementation

uses SysUtils, DateUtils;

{ TpsChargeParams }

constructor TpsChargeParams.Create(AAmount: integer; ACurrency: string);
begin
  inherited Create;
  FMetaData := TpsFactory.Metadata;
  FAmount := AAmount;
  FCurrency := ACurrency;
end;

destructor TpsChargeParams.Destroy;
begin
  inherited;
end;

function TpsChargeParams.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsChargeParams.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsChargeParams.GetCustomerID: string;
begin
  Result := FCustomerID;
end;

function TpsChargeParams.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsChargeParams.GetMetadata: IpsMetadata;
begin
  Result := FMetaData;
end;

function TpsChargeParams.GetPaymentMethodID: string;
begin
  Result := FPaymentMethodID;
end;

procedure TpsChargeParams.SetCustomerID(const Value: string);
begin
  FCustomerID := Value;
end;

procedure TpsChargeParams.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TpsChargeParams.SetPaymentMethodID(const Value: string);
begin
  FPaymentMethodID := Value;
end;

{ TpsChargeListOptions }

constructor TpsChargeListOptions.Create;
begin
  Flimit := 100;
  FFrom := -1;
  FTo := -1;
end;

function TpsChargeListOptions.GetFromDate: TDateTime;
begin
  Result := FFrom;
end;

function TpsChargeListOptions.GetLimit: integer;
begin
  Result := Flimit;
end;

function TpsChargeListOptions.GetPaymentIntentID: string;
begin
  Result := FpaymentIntentID;
end;

function TpsChargeListOptions.GetQuery: string;
begin
  Result := FQuery;
end;

function TpsChargeListOptions.GetToDate: TDateTime;
begin
  Result := FTo;
end;

procedure TpsChargeListOptions.SetFromDate(const Value: TDateTime);
begin
  FFrom := Value;
end;

procedure TpsChargeListOptions.SetLimit(const Value: integer);
begin
  Flimit := Value;
end;

procedure TpsChargeListOptions.SetPaymentIntentID(const Value: string);
begin
  FPaymentIntentID := Value;
end;

procedure TpsChargeListOptions.SetQuery(const Value: string);
begin
  FQuery := Value;
end;

procedure TpsChargeListOptions.SetToDate(const Value: TDateTime);
begin
  FTo := Value;
end;


{ TpsCharge }

constructor TpsCharge.Create;
begin
  FMetaData := TpsFactory.MetaData;
end;

function TpsCharge.GetAmount: integer;
begin
  Result := FAmount;
end;

function TpsCharge.GetCardBrand: string;
begin
  Result := FCardBrand;
end;

function TpsCharge.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TpsCharge.GetCurrency: string;
begin
  Result := FCurrency;
end;

function TpsCharge.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TpsCharge.GetDescription: string;
begin
  Result := FDescription;
end;

function TpsCharge.GetID: string;
begin
  Result := FID;
end;

function TpsCharge.GetJson: string;
begin
  Result := FJson;
end;

function TpsCharge.GetLast4: string;
begin
  Result := FLast4;
end;

function TpsCharge.GetMetaData: IpsMetaData;
begin
  Result := FMetaData;
end;

function TpsCharge.GetPaymentIntentID: string;
begin
  Result := FPaymentIntentID;
end;

function TpsCharge.GetRefunded: integer;
begin
  Result := FRefunded;
end;

function TpsCharge.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TpsCharge.LoadFromJson(AJson: TJsonObject);
begin
  FJson := AJson.ToJSON;
  FID := AJson.S['id'];
  FAmount := AJson.I['amount'];
  FCurrency := AJson.S['currency'];

  if AJson.IsNull('customer') = False then
  begin
    case AJson.Types['customer'] of
      jvtObject: FCustomer := AJson.O['customer'].S['id'];
      jvtString: FCustomer := AJson.S['customer'];
    end;
  end;

  FRefunded := AJson.I['amount_refunded'];

  if AJson.IsNull('payment_method_details') = False then
  begin
    if AJson.O['payment_method_details'].IsNull('card') = False then
    begin
      FLast4 := AJson.O['payment_method_details'].O['card'].S['last4'];
      FCardBrand := AJson.O['payment_method_details'].O['card'].S['brand'];
    end;
  end;

  FMetadata.LoadFromJson(AJson.O['metadata']);
  FCreated := UnixToDateTime(StrToInt(AJson.S['created']));
  if AJson.IsNull('payment_intent') = False then
    FpaymentIntentID := AJson.S['payment_intent'];
  if AJson.IsNull('description') = False then
    FDescription := AJson.S['description'];

  FStatus := AJson.S['status'];
end;


end.
