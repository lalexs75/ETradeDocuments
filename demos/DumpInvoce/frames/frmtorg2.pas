unit frmTorg2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TfrmTorg2Frame }

  TfrmTorg2Frame = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    CLabel: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

implementation
uses et_torg2;

{$R *.lfm}

{ TfrmTorg2Frame }

procedure TfrmTorg2Frame.Button1Click(Sender: TObject);
var
  F: TTorg2ExchangeFile;
  FN: String;
begin
  FN:='DP_PRIRASXPRIN'+'MARK'+'_'+'2610001001'+'_'+'2610001002'+'_'+'2019'+'11'+'20'+'ED914F28-C18C-48F4-999C-5CA294E76B9F';
  F:=TTorg2ExchangeFile.Create;
  F.FileID:=FN;
  F.FormatVersion:='5.01';
  F.ApplicationVersion:='DemoTST_1.0';

  F.ParticipantsInformation.RecipientInfo:='001';
  F.ParticipantsInformation.SenderInfo:='002';
  F.ParticipantsInformation.SellerExchangeInformation.FullName:='Оператор ЭДО №1';
  F.ParticipantsInformation.SellerExchangeInformation.Inn:='2610001003';
  F.ParticipantsInformation.SellerExchangeInformation.IdentifierSenderOperator:='881';

  F.Document.KND:='1175014';
  F.Document.DocumentDateCreate:='12.12.2019';
  F.Document.DocumentTimeCreate:='12:12:12';
  F.Document.DocumentNameEL:='Документ о приемке ценностей и (или) расхождениях, выявленных при их приемке';
  F.Document.DocumentName:='Документ о приемке ценностей и (или) расхождениях или иное';
  F.Document.DocumentCreator:='ООО Плюшка';
  F.Document.DocumentCreatorBase:='Покупатель';
  F.Document.DocumentCreateConditions:='1';
  //F.Document.DocumentDestination:=
  F.Document.AcceptanceDocumentDateNumber.DocumentNumber:='A0000123/12';
  F.Document.AcceptanceDocumentDateNumber.DocumentDate:='01.01.2019';
  F.Document.CorrectionDocumentDateNumber.CorrectionNumber:='1';
  F.Document.CorrectionDocumentDateNumber.CorrectionDate:='01.02.2019';

  F.Document.AcceptanceInformation1.GenerateCodeDocument:='1000';
  F.Document.AcceptanceInformation1.GovernmentContractInfo:='# 12312 от 25.11.2019';
  F.Document.AcceptanceInformation1.Seller.OKPO:='123123';
  F.Document.AcceptanceInformation1.Seller.Department:='склад 1';
  F.Document.AcceptanceInformation1.Seller.InformationWorkflow:='Обратите внимание на то и на это';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.INN:='123456789012';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='sad asgd as asgasdg asgd';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.OtherInfo:='Прочая информация';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.PhysicalPerson.INN:='123456789012';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.PhysicalPerson.OtherInfo:='afd rtwqe qwt qwt 123123123';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.PhysicalPerson.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.PhysicalPerson.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.PhysicalPerson.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.LegalEntityInformation.FullName:='AAA ksdfkgkg';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.LegalEntityInformation.INN:='123456789';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.LegalEntityInformation.KPP:='123123';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.FullName:='sfg asfgasg a';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.Identifier:='123';
  F.Document.AcceptanceInformation1.Seller.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.OtherInfo:='123 123 12 12';
  //F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.
  //F.Document.AcceptanceInformation1.Seller.Adress.AdressInfo
  //F.Document.AcceptanceInformation1.Seller.ContactInformation:TContactInformation read FContactInformation;
  //F.Document.AcceptanceInformation1.Seller.BankInformation:TBankInformation read FBankInformation;

  F.Document.AcceptanceInformation1.Buyer.OKPO:='1131231';
  F.Document.AcceptanceInformation1.Buyer.Department:='asfdasdf';
  F.Document.AcceptanceInformation1.Buyer.InformationWorkflow:='asdfasdf';
  //F.Document.AcceptanceInformation1.Buyer.IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
  //F.Document.AcceptanceInformation1.Buyer.Adress:TAdress read FAdress;
  //F.Document.AcceptanceInformation1.Buyer.ContactInformation:TContactInformation read FContactInformation;
  //F.Document.AcceptanceInformation1.Buyer.BankInformation:TBankInformation read FBankInformation;

  F.Document.AcceptanceInformation1.Shipper.OKPO:='123123123';
  F.Document.AcceptanceInformation1.Shipper.Department:='as dfa sfdasdf';
  F.Document.AcceptanceInformation1.Shipper.InformationWorkflow:='kasjf sadksd jal';
  //F.Document.AcceptanceInformation1.Shipper.IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
  //F.Document.AcceptanceInformation1.Shipper.Adress:TAdress read FAdress;
  //F.Document.AcceptanceInformation1.Shipper.ContactInformation:TContactInformation read FContactInformation;
  //F.Document.AcceptanceInformation1.Shipper.BankInformation:TBankInformation read FBankInformation;

  F.Document.AcceptanceInformation1.Consignee.OKPO:='321312312';
  F.Document.AcceptanceInformation1.Consignee.Department:='asfas asfasdf asdf asd';
  F.Document.AcceptanceInformation1.Consignee.InformationWorkflow:='asd asfasdf';
  //F.Document.AcceptanceInformation1.Consignee.IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
  //F.Document.AcceptanceInformation1.Consignee.Adress:TAdress read FAdress;
  //F.Document.AcceptanceInformation1.Consignee.ContactInformation:TContactInformation read FContactInformation;
  //F.Document.AcceptanceInformation1.Consignee.BankInformation:TBankInformation read FBankInformation;

  F.Document.AcceptanceInformation1.InsuranceCompany.OKPO:='23456789';
  F.Document.AcceptanceInformation1.InsuranceCompany.Department:='sdf asf asdf';
  F.Document.AcceptanceInformation1.InsuranceCompany.InformationWorkflow:='12312 3123';
  //F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
  //F.Document.AcceptanceInformation1.InsuranceCompany.Adress:TAdress read FAdress;
  //F.Document.AcceptanceInformation1.InsuranceCompany.ContactInformation:TContactInformation read FContactInformation;
  //F.Document.AcceptanceInformation1.InsuranceCompany.BankInformation:TBankInformation read FBankInformation;

  F.Document.AcceptanceInformation1.CommisionDocument.OrderDate:='01.01.2019';
  F.Document.AcceptanceInformation1.CommisionDocument.OrderNumber:='#1234 jjfjg sdfg';

  F.Document.AcceptanceInformation1.ResultsInspectionCargo.InspectDate:='01.01.2019';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.InspectionPlace:='Склад № 123';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AcceptanceTime:='11:00:00';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AcceptanceTimeEnd:='12:00:00';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ShipmentDate:='01.01.2019';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ConformityCertificate.Add('12 jn 1231212313');
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ConformityCertificate.Add('11 от 12,12,12');
  //F.Document.AcceptanceInformation1.ResultsInspectionCargo.AccompanyingDocument:TAccompanyingDocument read FAccompanyingDocument;
  //F.Document.AcceptanceInformation1.ResultsInspectionCargo.InformationResult:TAdditionalInformationOfLife1 read FInformationResult;

  //F.Document.AcceptanceInformation1.GoodsItemList:TGoodsItemList read FGoodsItemList;
  //F.Document.AcceptanceInformation1.AcceptanceDateInfo:TAcceptanceDateInfo read FAcceptanceDateInfo;
  //F.Document.AcceptanceInformation1.OtherAcceptanceInfo:TOtherAcceptanceInfo read FOtherAcceptanceInfo;
  //F.Document.AcceptanceInformation1.AcceptedPersonInformation:TAcceptedPersonInformation read FAcceptedPersonInformation;
  //F.Document.AcceptanceInformation1.AdditionalInformationOfLife1:TAdditionalInformationOfLife1 read FAdditionalInformationOfLife1;

  //F.Document.AcceptanceInformation2:TAcceptanceInformation2 read FAcceptanceInformation2;
  //F.Document.AdditionalInformationState:string read FAdditionalInformationState write SetAdditionalInformationState;
  //F.Document.Signer


  F.SaveToFile('/tmp/'+FN+'.xml');
  F.Free;
end;

end.

