{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ETradeDocuments;

{$warn 5023 off : no warning about unused units}
interface

uses
  ETradeDoc, AdditionalInfo, AdressInfo, InvoceExchangeFile, InvoceDocument, 
  InvoiceItem, OrganizationInfo, Signer, TransferInfo, ClientExchangeFile, 
  xml_doc, ExchangeDocument, ExchangeInformation, TreasuryInformation, 
  ExchangeSigner, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ETradeDoc', @ETradeDoc.Register);
end;

initialization
  RegisterPackage('ETradeDocuments', @Register);
end.
