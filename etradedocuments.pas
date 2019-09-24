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
  ExchangeSigner, ImportGoodsAndIndirectTaxesExchangeFile, 
  ImportGoodsAndIndirectTaxesDocument, xml_doc_resource, EImportAndPayTaxDoc, 
  EAbstractDoc, eDocsRegisterUnit, InvoceExchangeFile_5_02, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('eDocsRegisterUnit', @eDocsRegisterUnit.Register);
end;

initialization
  RegisterPackage('ETradeDocuments', @Register);
end.
