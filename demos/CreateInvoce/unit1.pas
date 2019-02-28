unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ETradeDoc,
  InvoceExchangeFile, Signer;

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
begin
  E:=TExchangeFile.Create;
  E.FileID:='AASSDSADADAD-ADDAD-ADAD';
  E.FormatVersion:='5.1';
  E.AppVersion:='123-123';
  E.ParticipantsInformation.SenderInfo:='Отправитель';
  E.ParticipantsInformation.RecipientInfo:='Получатель';
  E.ParticipantsInformation.SellerExchangeInformation.FullName:='Организация отправитель';
  E.ParticipantsInformation.SellerExchangeInformation.Inn:='100000009';
  E.ParticipantsInformation.SellerExchangeInformation.IdentifierSenderOperator:='AAA';

  E.Document.KND:='123';
  E.Document.DocFunction:='УПД';
(*  E.Document.DocumentNameEL:string read FDocumentNameEL write SetDocumentNameEL;
  E.Document.DocumentName:string read FDocumentName write SetDocumentName;
  E.Document.InvoceDateCreate:string read FInvoceDateCreate write SetInvoceDateCreate;
  E.Document.InvoceTimeCreate:string read FInvoceTimeCreate write SetInvoceTimeCreate;
  E.Document.DocumentCreator:string read FDocumentCreator write SetDocumentCreator;
  E.Document.DocumentCreatorBase:string read FDocumentCreatorBase write SetDocumentCreatorBase;
  E.Document.InformationAvailabilityStructureAdditionalInfo:string read FInformationAvailabilityStructureAdditionalInfo write SetInformationAvailabilityStructureAdditionalInfo;
  E.Document.InvoiceInformation:TInvoiceInformation read FInvoiceInformation;
  E.Document.InvoiceItems:TInvoiceItems read FInvoiceItems;
  E.Document.TransferInfo:TTransferInfo read FTransferInfo; *)
  Ch:=E.Document.Signers.CreateChild;
  CH.SignerPowers:='1';
(*  CH.SignerStatus:string read FSignerStatus write SetSignerStatus;
  CH.SignerPowersBase:string read FSignerPowersBase write SetSignerPowersBase;
  CH.SignerOrgPowersBase:string read FSignerOrgPowersBase write SetSignerOrgPowersBase;
  CH.PhysicalPersonEntity:TPhysicalPersonEntity read FPhysicalPersonEntity;
  CH.IndividualEntrepreneurInformation:TIndividualEntrepreneurInformation read FIndividualEntrepreneurInformation;
  CH.LegalEntityEmployee:TLegalEntityEmployee read FLegalEntityEmployee; *)

  ETradeDoc1.SaveInvoce(E, 'test1_invoce.xml');
  E.Free;
end;

end.

