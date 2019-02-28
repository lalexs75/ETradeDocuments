unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ETradeDoc,
  InvoceExchangeFile, Signer, InvoiceItem;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ETradeDoc1: TETradeDoc;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses xmliconv;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  E: TExchangeFile;
  Ch: TSigner;
  II: TInvoiceItem;
begin
  E:=TExchangeFile.Create;
  E.FileID:='AASSDSADADAD-ADDAD-ADAD';
  E.FormatVersion:='5.1';
  E.AppVersion:='TestAPP XML 1.0';
  E.ParticipantsInformation.SenderInfo:='GUID-Отправитель';
  E.ParticipantsInformation.RecipientInfo:='GUID-Получатель';
  E.ParticipantsInformation.SellerExchangeInformation.FullName:='Организация отправитель';
  E.ParticipantsInformation.SellerExchangeInformation.Inn:='100000009';
  E.ParticipantsInformation.SellerExchangeInformation.IdentifierSenderOperator:='2DD';

  E.Document.KND:='1115125';
  E.Document.DocFunction:='СЧФДОП';
  E.Document.DocumentNameEL:='Документ об отгрузке товара';
  E.Document.DocumentName:='Документ об отгрузке товара';
  E.Document.InvoceDateCreate:='01.01.2019';
  E.Document.InvoceTimeCreate:='15.00.25';
  E.Document.DocumentCreator:='Организация';
(*  E.Document.DocumentCreatorBase:string read FDocumentCreatorBase write SetDocumentCreatorBase;
  E.Document.InformationAvailabilityStructureAdditionalInfo:string read FInformationAvailabilityStructureAdditionalInfo write SetInformationAvailabilityStructureAdditionalInfo;
  E.Document.InvoiceInformation:TInvoiceInformation read FInvoiceInformation; *)

  II:=E.Document.InvoiceItems.InvoiceItemList.CreateChild;
  II.LineNumber:=1;
  II.Product:='Товар 1';
  II.UnitCode:='615';
  //II.UnitCodeDef:string read FUnitCodeDef write SetUnitCodeDef;
  //II.Quantity:string read FQuantity write SetQuantity;
  //II.Price:string read FPrice write SetPrice;
  //II.SubtotalWithVatExcluded:string read FSubtotalWithVatExcluded write SetSubtotalWithVatExcluded;
  //II.TaxRate:string read FTaxRate write SetTaxRate;
  //II.Subtotal:string read FSubtotal write SetSubtotal;
  //II.SubtotalDef:string read FSubtotalDef write SetSubtotalDef;
  //II.Excise:TExciseSum read FExcise;
  //II.Vat:TVatSum read FVat;
  //II.CustomsDeclaration:TCustomsDeclarationList read FCustomsDeclaration;
  //II.InvoiceItemAdditional:TInvoiceItemAdditional read FInvoiceItemAdditional;
  //II.AdditionalInfo:TTextInfoList read FAdditionalInfo;


//  E.Document.TransferInfo:TTransferInfo read FTransferInfo;
  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='1';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.LegalEntityEmployee.RegistrationInfo:='Организация';
  CH.LegalEntityEmployee.Inn:='123456789';
  CH.LegalEntityEmployee.OrganizationName:='ООО Чебурашка';
  CH.LegalEntityEmployee.Position:='Сотрудник';
  CH.LegalEntityEmployee.OtherInfo:='Нет';
  CH.LegalEntityEmployee.Person.Surname:='Иванов';
  CH.LegalEntityEmployee.Person.FirstName:='Иван';
  CH.LegalEntityEmployee.Person.Patronymic:='Иванович';


  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='2';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.PhysicalPersonEntity.CertificateStateRegistration:='Свидетельство о гос регистрации';
  CH.PhysicalPersonEntity.INN:='123456789123';
  CH.PhysicalPersonEntity.OtherInfo:='Примечание';
  CH.PhysicalPersonEntity.Person.Surname:='Иванов';
  CH.PhysicalPersonEntity.Person.FirstName:='Иван';
  CH.PhysicalPersonEntity.Person.Patronymic:='Иванович';


  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='3';
  CH.SignerStatus:='1';
  CH.SignerPowersBase:='Доверенность';
  CH.SignerOrgPowersBase:='Документ';
  CH.IndividualEntrepreneurInformation.INN:='123456789';
  //CH.IndividualEntrepreneurInformation.INNDef:string read FINNDef write SetINNDef;
  CH.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='Сертификаты';
  CH.IndividualEntrepreneurInformation.OtherInfo:='Примечание';
  CH.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  CH.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  CH.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';


  ETradeDoc1.SaveInvoce(E, 'test1_invoce.xml');
  E.Free;
  ShowMessage('Успешно');
end;

end.

