{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ETradeDocuments;

{$warn 5023 off : no warning about unused units}
interface

uses
  ETradeDoc, AdditionalInfo, AdressInfo, InvoceExchangeFile, InvoceDocument, 
  InvoiceItem, OrganizationInfo, Signer, TransferInfo, ClientExchangeFile, 
  ExchangeDocument, ExchangeInformation, TreasuryInformation, ExchangeSigner, 
  ImportGoodsAndIndirectTaxesExchangeFile, 
  ImportGoodsAndIndirectTaxesDocument, EImportAndPayTaxDoc, EAbstractDoc, 
  eDocsRegisterUnit, InvoceExchangeFile_5_02, AbstractExchangeFileUnit, 
  et_torg2, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('eDocsRegisterUnit', @eDocsRegisterUnit.Register);
end;

initialization
  RegisterPackage('ETradeDocuments', @Register);
end.
