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

uses pasStripe.Constants;

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
