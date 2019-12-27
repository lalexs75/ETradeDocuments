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
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.ZipCode:='321123';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Region:='26';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Territory:='Ставропольский';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.City:='Ставрополь';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Locality:='Ставрополь';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Street:='Мира';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Building:='1';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Block:='a';
  F.Document.AcceptanceInformation1.Seller.Adress.RussianAdress.Apartment:='123';
  F.Document.AcceptanceInformation1.Seller.Adress.AdressInfo.CountryCode:='863';
  F.Document.AcceptanceInformation1.Seller.Adress.AdressInfo.Address:='Россия, Ставропольский край, г.Ставрополь';
  F.Document.AcceptanceInformation1.Seller.ContactInformation.PhoneNumber:='8-865-2-55-55-55';
  F.Document.AcceptanceInformation1.Seller.ContactInformation.EMail:='email@mail.ru';
  F.Document.AcceptanceInformation1.Seller.BankInformation.BankAccount:='12345678901234567890';
  F.Document.AcceptanceInformation1.Seller.BankInformation.Bank.BankBIK:='123456789';
  F.Document.AcceptanceInformation1.Seller.BankInformation.Bank.BankName:='AAA AAAA AAA AAAA AAA AAAA AAA AAAA AAA AAAA';
  F.Document.AcceptanceInformation1.Seller.BankInformation.Bank.CorrAccount:='12345678901234567890';

  F.Document.AcceptanceInformation1.Buyer.OKPO:='1131231';
  F.Document.AcceptanceInformation1.Buyer.Department:='asfdasdf';
  F.Document.AcceptanceInformation1.Buyer.InformationWorkflow:='asdfasdf';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.INN:='123456789012';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='sad asgd as asgasdg asgd';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.OtherInfo:='Прочая информация';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.PhysicalPerson.INN:='123456789012';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.PhysicalPerson.OtherInfo:='afd rtwqe qwt qwt 123123123';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.PhysicalPerson.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.PhysicalPerson.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.PhysicalPerson.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.LegalEntityInformation.FullName:='AAA ksdfkgkg';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.LegalEntityInformation.INN:='123456789';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.LegalEntityInformation.KPP:='123123';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.FullName:='sfg asfgasg a';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.Identifier:='123';
  F.Document.AcceptanceInformation1.Buyer.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.OtherInfo:='123 123 12 12';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.ZipCode:='321123';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Region:='26';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Territory:='Ставропольский';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.City:='Ставрополь';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Locality:='Ставрополь';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Street:='Мира';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Building:='1';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Block:='a';
  F.Document.AcceptanceInformation1.Buyer.Adress.RussianAdress.Apartment:='123';
  F.Document.AcceptanceInformation1.Buyer.Adress.AdressInfo.CountryCode:='863';
  F.Document.AcceptanceInformation1.Buyer.Adress.AdressInfo.Address:='Россия, Ставропольский край, г.Ставрополь';
  F.Document.AcceptanceInformation1.Buyer.ContactInformation.PhoneNumber:='8-865-2-55-55-55';
  F.Document.AcceptanceInformation1.Buyer.ContactInformation.EMail:='email@mail.ru';
  F.Document.AcceptanceInformation1.Buyer.BankInformation.BankAccount:='12345678901234567890';
  F.Document.AcceptanceInformation1.Buyer.BankInformation.Bank.BankBIK:='123456789';
  F.Document.AcceptanceInformation1.Buyer.BankInformation.Bank.BankName:='AAA AAAA AAA AAAA AAA AAAA AAA AAAA AAA AAAA';
  F.Document.AcceptanceInformation1.Buyer.BankInformation.Bank.CorrAccount:='12345678901234567890';

  F.Document.AcceptanceInformation1.Shipper.OKPO:='123123123';
  F.Document.AcceptanceInformation1.Shipper.Department:='as dfa sfdasdf';
  F.Document.AcceptanceInformation1.Shipper.InformationWorkflow:='kasjf sadksd jal';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.INN:='123456789012';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='sad asgd as asgasdg asgd';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.OtherInfo:='Прочая информация';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.PhysicalPerson.INN:='123456789012';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.PhysicalPerson.OtherInfo:='afd rtwqe qwt qwt 123123123';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.PhysicalPerson.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.PhysicalPerson.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.PhysicalPerson.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.LegalEntityInformation.FullName:='AAA ksdfkgkg';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.LegalEntityInformation.INN:='123456789';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.LegalEntityInformation.KPP:='123123';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.FullName:='sfg asfgasg a';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.Identifier:='123';
  F.Document.AcceptanceInformation1.Shipper.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.OtherInfo:='123 123 12 12';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.ZipCode:='321223';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Region:='26';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Territory:='Ставропольский';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.City:='Ставрополь';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Locality:='Ставрополь';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Street:='Мира';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Building:='1';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Block:='a';
  F.Document.AcceptanceInformation1.Shipper.Adress.RussianAdress.Apartment:='123';
  F.Document.AcceptanceInformation1.Shipper.Adress.AdressInfo.CountryCode:='863';
  F.Document.AcceptanceInformation1.Shipper.Adress.AdressInfo.Address:='Россия, Ставропольский край, г.Ставрополь';
  F.Document.AcceptanceInformation1.Shipper.ContactInformation.PhoneNumber:='8-865-2-55-55-55';
  F.Document.AcceptanceInformation1.Shipper.ContactInformation.EMail:='email@mail.ru';
  F.Document.AcceptanceInformation1.Shipper.BankInformation.BankAccount:='12345678901234567890';
  F.Document.AcceptanceInformation1.Shipper.BankInformation.Bank.BankBIK:='123456789';
  F.Document.AcceptanceInformation1.Shipper.BankInformation.Bank.BankName:='AAA AAAA AAA AAAA AAA AAAA AAA AAAA AAA AAAA';
  F.Document.AcceptanceInformation1.Shipper.BankInformation.Bank.CorrAccount:='12345678901234567890';

  F.Document.AcceptanceInformation1.Consignee.OKPO:='321312312';
  F.Document.AcceptanceInformation1.Consignee.Department:='asfas asfasdf asdf asd';
  F.Document.AcceptanceInformation1.Consignee.InformationWorkflow:='asd asfasdf';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.INN:='123456789012';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='sad asgd as asgasdg asgd';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.OtherInfo:='Прочая информация';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.PhysicalPerson.INN:='123456789012';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.PhysicalPerson.OtherInfo:='afd rtwqe qwt qwt 123123123';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.PhysicalPerson.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.PhysicalPerson.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.PhysicalPerson.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.LegalEntityInformation.FullName:='AAA ksdfkgkg';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.LegalEntityInformation.INN:='123456789';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.LegalEntityInformation.KPP:='123123';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.FullName:='sfg asfgasg a';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.Identifier:='123';
  F.Document.AcceptanceInformation1.Consignee.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.OtherInfo:='123 123 12 12';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.ZipCode:='321123';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Region:='26';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Territory:='Ставропольский';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.City:='Ставрополь';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Locality:='Ставрополь';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Street:='Мира';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Building:='1';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Block:='a';
  F.Document.AcceptanceInformation1.Consignee.Adress.RussianAdress.Apartment:='123';
  F.Document.AcceptanceInformation1.Consignee.Adress.AdressInfo.CountryCode:='863';
  F.Document.AcceptanceInformation1.Consignee.Adress.AdressInfo.Address:='Россия, Ставропольский край, г.Ставрополь';
  F.Document.AcceptanceInformation1.Consignee.ContactInformation.PhoneNumber:='8-865-2-55-55-55';
  F.Document.AcceptanceInformation1.Consignee.ContactInformation.EMail:='email@mail.ru';
  F.Document.AcceptanceInformation1.Consignee.BankInformation.BankAccount:='12345678901234567890';
  F.Document.AcceptanceInformation1.Consignee.BankInformation.Bank.BankBIK:='123456789';
  F.Document.AcceptanceInformation1.Consignee.BankInformation.Bank.BankName:='AAA AAAA AAA AAAA AAA AAAA AAA AAAA AAA AAAA';
  F.Document.AcceptanceInformation1.Consignee.BankInformation.Bank.CorrAccount:='12345678901234567890';

  F.Document.AcceptanceInformation1.InsuranceCompany.OKPO:='23456789';
  F.Document.AcceptanceInformation1.InsuranceCompany.Department:='sdf asf asdf';
  F.Document.AcceptanceInformation1.InsuranceCompany.InformationWorkflow:='12312 3123';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.INN:='123456789012';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.IndividualEntityRegistrationCertificate:='sad asgd as asgasdg asgd';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.OtherInfo:='Прочая информация';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.IndividualEntrepreneurInformation.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.PhysicalPerson.INN:='123456789012';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.PhysicalPerson.OtherInfo:='afd rtwqe qwt qwt 123123123';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.PhysicalPerson.Person.FirstName:='Иван';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.PhysicalPerson.Person.Surname:='Иванов';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.PhysicalPerson.Person.Patronymic:='Иванович';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.LegalEntityInformation.FullName:='AAA ksdfkgkg';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.LegalEntityInformation.INN:='123456789';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.LegalEntityInformation.KPP:='123123';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.FullName:='sfg asfgasg a';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.Identifier:='123';
  F.Document.AcceptanceInformation1.InsuranceCompany.IdentificationInformation.OrganizationInformation.ForeignEntityInformation.OtherInfo:='123 123 12 12';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.ZipCode:='321123';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Region:='26';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Territory:='Ставропольский';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.City:='Ставрополь';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Locality:='Ставрополь';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Street:='Мира';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Building:='1';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Block:='a';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.RussianAdress.Apartment:='123';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.AdressInfo.CountryCode:='863';
  F.Document.AcceptanceInformation1.InsuranceCompany.Adress.AdressInfo.Address:='Россия, Ставропольский край, г.Ставрополь';
  F.Document.AcceptanceInformation1.InsuranceCompany.ContactInformation.PhoneNumber:='8-865-2-55-55-55';
  F.Document.AcceptanceInformation1.InsuranceCompany.ContactInformation.EMail:='email@mail.ru';
  F.Document.AcceptanceInformation1.InsuranceCompany.BankInformation.BankAccount:='12345678901234567890';
  F.Document.AcceptanceInformation1.InsuranceCompany.BankInformation.Bank.BankBIK:='123456789';
  F.Document.AcceptanceInformation1.InsuranceCompany.BankInformation.Bank.BankName:='AAA AAAA AAA AAAA AAA AAAA AAA AAAA AAA AAAA';
  F.Document.AcceptanceInformation1.InsuranceCompany.BankInformation.Bank.CorrAccount:='12345678901234567890';

  F.Document.AcceptanceInformation1.CommisionDocument.OrderDate:='01.01.2019';
  F.Document.AcceptanceInformation1.CommisionDocument.OrderNumber:='#1234 jjfjg sdfg';

  F.Document.AcceptanceInformation1.ResultsInspectionCargo.InspectDate:='01.01.2019';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.InspectionPlace:='Склад № 123';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AcceptanceTime:='11:00:00';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AcceptanceTimeEnd:='12:00:00';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ShipmentDate:='01.01.2019';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ConformityCertificate.Add('12 jn 1231212313');
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.ConformityCertificate.Add('11 от 12,12,12');

  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AccompanyingDocument.DocumentName:='Сопроводительный';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AccompanyingDocument.DocumentNumber:='12aA11';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AccompanyingDocument.DocumentDate:='01.01.2019';
  F.Document.AcceptanceInformation1.ResultsInspectionCargo.AccompanyingDocument.DocumentID:='35A9A00A-383C-46B4-A2EE-4B6511AF3545';

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

