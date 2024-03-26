# pasStripe

## Important! Please Read!

These interfaces are far from complete and only expose the properties/methods I have required for my project. 

These classes/interfaces barely scratch the surface of the Stripe API but it help others get a head start with integrating with Stripe.

I've just converted this to use System.Json from JsonDataObjects (which is awesome and my prefered json library) via the use of a class helper which mimicks some of the JsonDataObjects property conventions.  
Hopefully this will make it easier for others to use, but is also likely to have introduced a few bugs which I may have missed.

To use, add the pasStripe.pas file to your uses clause, then use the TpsFactory class to create the pasStripe interface.

e.g.

```
// to display the account name...

var
  AStripe: IPasStripe;
  AAccount: IpsAccount;
begin
  AStripe :=  TpsFactory.PasStripe('sk_test_fFt5YRlBI*********', 'acct_1O2AWuQ********');
  if AStripe.TestCredentials then
  begin
    AAccount := Stripe.GetAccount;
    ShowMessage(AAccount.Name);
  end;
end;
```
```
// to load an ivoice and show the URL...

var
  AStripe: IPasStripe;
  ACharges: TpsChargeList;
  AInvoice: IpsInvoice;
begin
  AStripe := TpsFactory.PasStripe('sk_test_fFt5YRlBI*********', 'acct_1O2AWuQ********');
  AInvoice := AStripe.GetInvoice('in_1OyXC4QppGf99akRBlkO2E8f');
  showmessage(AInvoice.PdfUrl);
end;
```
