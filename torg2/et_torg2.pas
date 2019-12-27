{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании

  Приказ Федеральной налоговой службы от 27 августа 2019 г. № ММВ-7-15/423@
  "Об утверждении формата представления документа о приемке материальных ценностей
   и (или) расхождениях, выявленных при их приемке, в электронной форме"

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit et_torg2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractExchangeFileUnit, xmlobject;

type
  TPhysicalPersonEntity = class;
  TIndividualEntrepreneurInformation = class;
  TAdress = class;
  TContactInformation = class;

  { TPerson }

  TPerson = class(TXmlSerializationObject) //%Таблица 5.45
  private
    FFirstName: string;
    FPatronymic: string;
    FSurname: string;
    procedure SetFirstName(AValue: string);
    procedure SetPatronymic(AValue: string);
    procedure SetSurname(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Surname:string read FSurname write SetSurname;
    property FirstName:string read FFirstName write SetFirstName;
    property Patronymic:string read FPatronymic write SetPatronymic;
  end;

  { TBank }

  TBank = class(TXmlSerializationObject) //%Таблица 5.44
  private
    FBankBIK: string;
    FBankName: string;
    FCorrAccount: string;
    procedure SetBankBIK(AValue: string);
    procedure SetBankName(AValue: string);
    procedure SetCorrAccount(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property BankName:string read FBankName write SetBankName;
    property BankBIK:string read FBankBIK write SetBankBIK;
    property CorrAccount:string read FCorrAccount write SetCorrAccount;
  end;

  { TBankInformation }

  TBankInformation = class(TXmlSerializationObject) //%Таблица 5.43
  private
    FBank: TBank;
    FBankAccount: string;
    procedure SetBankAccount(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property BankAccount:string read FBankAccount write SetBankAccount;
    property Bank:TBank read FBank;
  end;

  { TForeignEntityInformation }

  TForeignEntityInformation = class(TXmlSerializationObject) //%Таблица 5.42
  private
    FFullName: string;
    FIdentifier: string;
    FOtherInfo: string;
    procedure SetFullName(AValue: string);
    procedure SetIdentifier(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property Identifier:string read FIdentifier write SetIdentifier;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
  end;


  { TLegalEntityInformation }

  TLegalEntityInformation = class(TXmlSerializationObject) //%Таблица 5.41
  private
    FFullName: string;
    FINN: string;
    FKPP: string;
    procedure SetFullName(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetKPP(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property FullName:string read FFullName write SetFullName;
    property INN:string read FINN write SetINN;
    property KPP:string read FKPP write SetKPP;
  end;


  { TOrganizationInformation }

  TOrganizationInformation = class(TXmlSerializationObject) //%Таблица 5.40
  private
    FForeignEntityInformation: TForeignEntityInformation;
    FLegalEntityInformation: TLegalEntityInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property LegalEntityInformation:TLegalEntityInformation read FLegalEntityInformation;
    property ForeignEntityInformation:TForeignEntityInformation read FForeignEntityInformation;
  end;


  { TIdentificationInformation }

  TIdentificationInformation = class(TXmlSerializationObject) //%Таблица 5.39
  private
    FIndividualEntrepreneurInformation: TIndividualEntrepreneurInformation;
    FOrganizationInformation: TOrganizationInformation;
    FPhysicalPerson: TPhysicalPersonEntity;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PhysicalPerson:TPhysicalPersonEntity read FPhysicalPerson;
    property IndividualEntrepreneurInformation:TIndividualEntrepreneurInformation read FIndividualEntrepreneurInformation;
    property OrganizationInformation:TOrganizationInformation read FOrganizationInformation;
  end;


  { TOrganizationInfo }

  TOrganizationInfo = class(TXmlSerializationObject) //%Таблица 5.38
  private
    FAdress: TAdress;
    FBankInformation: TBankInformation;
    FContactInformation: TContactInformation;
    FDepartment: string;
    FIdentificationInformation: TIdentificationInformation;
    FInformationWorkflow: string;
    FOKPO: string;
    procedure SetDepartment(AValue: string);
    procedure SetInformationWorkflow(AValue: string);
    procedure SetOKPO(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OKPO:string read FOKPO write SetOKPO;
    property Department:string read FDepartment write SetDepartment;
    property InformationWorkflow:string read FInformationWorkflow write SetInformationWorkflow;
    property IdentificationInformation:TIdentificationInformation read FIdentificationInformation;
    property Adress:TAdress read FAdress;
    property ContactInformation:TContactInformation read FContactInformation;
    property BankInformation:TBankInformation read FBankInformation;
  end;


  { TAccompanyingDocument }

  TAccompanyingDocument = class(TXmlSerializationObject) //%Таблица 5.36
  private
    FDocumentDate: string;
    FDocumentID: string;
    FDocumentName: string;
    FDocumentNumber: string;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentID(AValue: string);
    procedure SetDocumentName(AValue: string);
    procedure SetDocumentNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property DocumentName:string read FDocumentName write SetDocumentName;
    property DocumentNumber:string read FDocumentNumber write SetDocumentNumber;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    property DocumentID:string read FDocumentID write SetDocumentID;
  end;

  { TPhysicalPersonEntity }

  TPhysicalPersonEntity = class(TXmlSerializationObject) //%Таблица 5.35
  private
    FINN: string;
    FOtherInfo: string;
    FPerson: TPerson;
    procedure SetINN(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property INN:string read FINN write SetINN;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TPerson read FPerson;
  end;

  { TIndividualEntrepreneurInformation }

  TIndividualEntrepreneurInformation = class(TXmlSerializationObject) //%Таблица 5.34
  private
    FIndividualEntityRegistrationCertificate: string;
    FINN: string;
    FOtherInfo: string;
    FPerson: TPerson;
    procedure SetIndividualEntityRegistrationCertificate(AValue: string);
    procedure SetINN(AValue: string);
    procedure SetOtherInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property INN:string read FINN write SetINN;
    property IndividualEntityRegistrationCertificate:string read FIndividualEntityRegistrationCertificate write SetIndividualEntityRegistrationCertificate;
    property OtherInfo:string read FOtherInfo write SetOtherInfo;
    property Person:TPerson read FPerson;
  end;



  { TAdditionalInformationOfLife1 }

  TAdditionalInformationOfLife1 = class(TXmlSerializationObject) //%Таблица 5.29
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;

  { TContactInformation }

  TContactInformation = class(TXmlSerializationObject) //%Таблица 5.28
  private
    FEMail: string;
    FPhoneNumber: string;
    procedure SetEMail(AValue: string);
    procedure SetPhoneNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PhoneNumber:string read FPhoneNumber write SetPhoneNumber;
    property EMail:string read FEMail write SetEMail;
  end;


  { TRussianAdress }

  TRussianAdress = class(TXmlSerializationObject) //%Таблица 5.27
  private
    FApartment: string;
    FBlock: string;
    FBuilding: string;
    FCity: string;
    FLocality: string;
    FRegion: string;
    FStreet: string;
    FTerritory: string;
    FZipCode: string;
    procedure SetApartment(AValue: string);
    procedure SetBlock(AValue: string);
    procedure SetBuilding(AValue: string);
    procedure SetCity(AValue: string);
    procedure SetLocality(AValue: string);
    procedure SetRegion(AValue: string);
    procedure SetStreet(AValue: string);
    procedure SetTerritory(AValue: string);
    procedure SetZipCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ZipCode:string read FZipCode write SetZipCode;
    property Region:string read FRegion write SetRegion;
    property Territory:string read FTerritory write SetTerritory;
    property City:string read FCity write SetCity;
    property Locality:string read FLocality write SetLocality;
    property Street:string read FStreet write SetStreet;
    property Building:string read FBuilding write SetBuilding;
    property Block:string read FBlock write SetBlock;
    property Apartment:string read FApartment write SetApartment;
  end;

  { TAdressInfo }

  TAdressInfo = class(TXmlSerializationObject) //%Таблица 5.26
  private
    FAddress: string;
    FCountryCode: string;
    procedure SetAddress(AValue: string);
    procedure SetCountryCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CountryCode:string read FCountryCode write SetCountryCode;
    property Address:string read FAddress write SetAddress;
  end;

  { TAdress }

  TAdress = class(TXmlSerializationObject) //%Таблица 5.25
  private
    FAdressInfo: TAdressInfo;
    FCodeGAR: string;
    FRussianAdress: TRussianAdress;
    procedure SetCodeGAR(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property RussianAdress:TRussianAdress read FRussianAdress;
    property AdressInfo:TAdressInfo read FAdressInfo;
    //property CodeGAR:string read FCodeGAR write SetCodeGAR;
  end;


  { TBuyerSignerInformation }

  TBuyerSignerInformation = class(TXmlSerializationObject) //%Таблица 5.23
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;

  { TBuyerSignerInformationList }

  TBuyerSignerInformationList = class(TXmlSerializationObjectList) //%Таблица 5.23
  private
    function GetItem(AIndex: Integer): TBuyerSignerInformation; inline;
  public
    constructor Create;
    function CreateChild:TBuyerSignerInformation;
    property Item[AIndex:Integer]:TBuyerSignerInformation read GetItem; default;
  end;

  { TAcceptanceInformation2 }

  TAcceptanceInformation2 = class(TXmlSerializationObject) //%Таблица 5.18
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;

  { TAcceptedPersonInformation }

  TAcceptedPersonInformation = class(TXmlSerializationObject) //%Таблица 5.13
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;


  { TOtherAcceptanceInfo }

  TOtherAcceptanceInfo = class(TXmlSerializationObject) //%Таблица 5.12
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;


  { TAcceptanceDateInfo }

  TAcceptanceDateInfo = class(TXmlSerializationObject) //%Таблица 5.11
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;


  { TGoodsItem }

  TGoodsItem = class(TXmlSerializationObject) //%Таблица 5.10
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
  end;

  { TGoodsItemList }

  TGoodsItemList = class(TXmlSerializationObjectList) //%Таблица 5.10
  private
    function GetItem(AIndex: Integer): TGoodsItem; inline;
  public
    constructor Create;
    function CreateChild:TGoodsItem;
    property Item[AIndex:Integer]:TGoodsItem read GetItem; default;
  end;

  { TResultsInspectionCargo }

  TResultsInspectionCargo = class(TXmlSerializationObject) //%Таблица 5.9
  private
    FAcceptanceTime: string;
    FAcceptanceTimeEnd: string;
    FAccompanyingDocument: TAccompanyingDocument;
    FConformityCertificate: TStringList;
    FInformationResult: TAdditionalInformationOfLife1;
    FInspectDate: string;
    FInspectionPlace: string;
    FShipmentDate: string;
    procedure SetAcceptanceTime(AValue: string);
    procedure SetAcceptanceTimeEnd(AValue: string);
    procedure SetInspectDate(AValue: string);
    procedure SetInspectionPlace(AValue: string);
    procedure SetShipmentDate(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property InspectDate:string read FInspectDate write SetInspectDate;
    property InspectionPlace:string read FInspectionPlace write SetInspectionPlace;
    property AcceptanceTime:string read FAcceptanceTime write SetAcceptanceTime;
    property AcceptanceTimeEnd:string read FAcceptanceTimeEnd write SetAcceptanceTimeEnd;
    property ShipmentDate:string read FShipmentDate write SetShipmentDate;
    property ConformityCertificate:TStringList read FConformityCertificate;
    property AccompanyingDocument:TAccompanyingDocument read FAccompanyingDocument;
    property InformationResult:TAdditionalInformationOfLife1 read FInformationResult;
  end;


  { TCommisionDocument }

  TCommisionDocument = class(TXmlSerializationObject) //%Таблица 5.8
  private
    FOrderDate: string;
    FOrderNumber: string;
    procedure SetOrderDate(AValue: string);
    procedure SetOrderNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property OrderDate:string read FOrderDate write SetOrderDate;
    property OrderNumber:string read FOrderNumber write SetOrderNumber;
  end;


  { TAcceptanceInformation1 }

  TAcceptanceInformation1 = class(TXmlSerializationObject) //%Таблица 5.7
  private
    FAcceptanceDateInfo: TAcceptanceDateInfo;
    FAcceptedPersonInformation: TAcceptedPersonInformation;
    FAdditionalInformationOfLife1: TAdditionalInformationOfLife1;
    FBuyer: TOrganizationInfo;
    FCommisionDocument: TCommisionDocument;
    FConsignee: TOrganizationInfo;
    FGenerateCodeDocument: string;
    FGoodsItemList: TGoodsItemList;
    FGovernmentContractInfo: string;
    FInsuranceCompany: TOrganizationInfo;
    FOtherAcceptanceInfo: TOtherAcceptanceInfo;
    FResultsInspectionCargo: TResultsInspectionCargo;
    FSeller: TOrganizationInfo;
    FShipper: TOrganizationInfo;
    procedure SetGenerateCodeDocument(AValue: string);
    procedure SetGovernmentContractInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property GenerateCodeDocument:string read FGenerateCodeDocument write SetGenerateCodeDocument;
    property GovernmentContractInfo:string read FGovernmentContractInfo write SetGovernmentContractInfo;
    property Seller:TOrganizationInfo read FSeller;
    property Buyer:TOrganizationInfo read FBuyer;
    property Shipper:TOrganizationInfo read FShipper;
    property Consignee:TOrganizationInfo read FConsignee;
    property InsuranceCompany:TOrganizationInfo read FInsuranceCompany;
    property CommisionDocument:TCommisionDocument read FCommisionDocument;
    property ResultsInspectionCargo:TResultsInspectionCargo read FResultsInspectionCargo;
    property GoodsItemList:TGoodsItemList read FGoodsItemList;
    property AcceptanceDateInfo:TAcceptanceDateInfo read FAcceptanceDateInfo;
    property OtherAcceptanceInfo:TOtherAcceptanceInfo read FOtherAcceptanceInfo;
    property AcceptedPersonInformation:TAcceptedPersonInformation read FAcceptedPersonInformation;
    property AdditionalInformationOfLife1:TAdditionalInformationOfLife1 read FAdditionalInformationOfLife1;
  end;

  { TCorrectionDocumentDateNumber }

  TCorrectionDocumentDateNumber = class(TXmlSerializationObject) //%Таблица 5.6
  private
    FCorrectionDate: string;
    FCorrectionNumber: string;
    procedure SetCorrectionDate(AValue: string);
    procedure SetCorrectionNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property CorrectionNumber:string read FCorrectionNumber write SetCorrectionNumber;
    property CorrectionDate:string read FCorrectionDate write SetCorrectionDate;
  end;

  { TAcceptanceDocumentDateNumber }

  TAcceptanceDocumentDateNumber = class(TXmlSerializationObject) //%Таблица 5.5
  private
    FDocumentDate: string;
    FDocumentNumber: string;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentNumber(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property DocumentNumber:string read FDocumentNumber write SetDocumentNumber;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
  end;

  { TAcceptanceDocument }

  TAcceptanceDocument = class(TXmlSerializationObject) //%Таблица 5.4
  private
    FAcceptanceDocumentDateNumber: TAcceptanceDocumentDateNumber;
    FAcceptanceInformation1: TAcceptanceInformation1;
    FAcceptanceInformation2: TAcceptanceInformation2;
    FAdditionalInformationState: string;
    FCorrectionDocumentDateNumber: TCorrectionDocumentDateNumber;
    FDocumentCreateConditions: string;
    FDocumentCreator: string;
    FDocumentCreatorBase: string;
    FDocumentDateCreate: string;
    FDocumentDestination: string;
    FDocumentName: string;
    FDocumentNameEL: string;
    FDocumentTimeCreate: string;
    FKND: string;
    FSigner: TBuyerSignerInformationList;
    procedure SetAdditionalInformationState(AValue: string);
    procedure SetDocumentCreateConditions(AValue: string);
    procedure SetDocumentCreator(AValue: string);
    procedure SetDocumentCreatorBase(AValue: string);
    procedure SetDocumentDateCreate(AValue: string);
    procedure SetDocumentDestination(AValue: string);
    procedure SetDocumentName(AValue: string);
    procedure SetDocumentNameEL(AValue: string);
    procedure SetDocumentTimeCreate(AValue: string);
    procedure SetKND(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property KND:string read FKND write SetKND;
    property DocumentDateCreate:string read FDocumentDateCreate write SetDocumentDateCreate;
    property DocumentTimeCreate:string read FDocumentTimeCreate write SetDocumentTimeCreate;
    property DocumentNameEL:string read FDocumentNameEL write SetDocumentNameEL;
    property DocumentName:string read FDocumentName write SetDocumentName;
    property DocumentCreator:string read FDocumentCreator write SetDocumentCreator;
    property DocumentCreatorBase:string read FDocumentCreatorBase write SetDocumentCreatorBase;
    property DocumentCreateConditions:string read FDocumentCreateConditions write SetDocumentCreateConditions;
    property DocumentDestination:string read FDocumentDestination write SetDocumentDestination;
    property AcceptanceDocumentDateNumber:TAcceptanceDocumentDateNumber read FAcceptanceDocumentDateNumber;
    property CorrectionDocumentDateNumber:TCorrectionDocumentDateNumber read FCorrectionDocumentDateNumber;
    property AcceptanceInformation1:TAcceptanceInformation1 read FAcceptanceInformation1;
    property AcceptanceInformation2:TAcceptanceInformation2 read FAcceptanceInformation2;
    property AdditionalInformationState:string read FAdditionalInformationState write SetAdditionalInformationState;
    property Signer:TBuyerSignerInformationList read FSigner;
  end;

  { TSellerExchangeInformation }

  TSellerExchangeInformation = class(TXmlSerializationObject) //%Таблица 5.3
  private
    FFullName: string;
    FIdentifierSenderOperator: string;
    FInn: string;
    procedure SetFullName(AValue: string);
    procedure SetIdentifierSenderOperator(AValue: string);
    procedure SetInn(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
  public
  published
    property FullName:string read FFullName write SetFullName;
    property Inn:string read FInn write SetInn;
    property IdentifierSenderOperator:string read FIdentifierSenderOperator write SetIdentifierSenderOperator;
  end;

  { TParticipantsInformation }

  TParticipantsInformation = class(TXmlSerializationObject)  //%Таблица 5.2
  private
    FRecipientInfo: string;
    FSellerExchangeInformation: TSellerExchangeInformation;
    FSenderInfo: string;
    procedure SetRecipientInfo(AValue: string);
    procedure SetSenderInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SenderInfo:string read FSenderInfo write SetSenderInfo;
    property RecipientInfo:string read FRecipientInfo write SetRecipientInfo;
    property SellerExchangeInformation:TSellerExchangeInformation read FSellerExchangeInformation;
  end;

  { TTorg2ExchangeFile }

  TTorg2ExchangeFile = class(TAbstractExchangeFile)   //%Таблица 5.1
  private
    FDocument: TAcceptanceDocument;
    FParticipantsInformation: TParticipantsInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ParticipantsInformation:TParticipantsInformation read FParticipantsInformation;
    property Document:TAcceptanceDocument read FDocument;
  end;

implementation

{ TBank }

procedure TBank.SetBankName(AValue: string);
begin
  if FBankName=AValue then Exit;
  FBankName:=AValue;
  ModifiedProperty('BankName');
end;

procedure TBank.SetBankBIK(AValue: string);
begin
  if FBankBIK=AValue then Exit;
  FBankBIK:=AValue;
  ModifiedProperty('BankBIK');
end;

procedure TBank.SetCorrAccount(AValue: string);
begin
  if FCorrAccount=AValue then Exit;
  FCorrAccount:=AValue;
  ModifiedProperty('CorrAccount');
end;

procedure TBank.InternalRegisterPropertys;
begin
  RegisterProperty('BankName', 'НаимБанк', 'Н', 'Наименование банка', 1, 1000);
  RegisterProperty('BankBIK', 'БИК', 'НК', 'Банковский идентификационный код (БИК) в соответствии со "Справочником БИК РФ"', 9, 9);
  RegisterProperty('CorrAccount', 'КорСчет', 'Н', 'Корреспондентский счет банка', 1, 20);
end;

procedure TBank.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TBank.Destroy;
begin
  inherited Destroy;
end;

{ TRussianAdress }

procedure TRussianAdress.SetApartment(AValue: string);
begin
  if FApartment=AValue then Exit;
  FApartment:=AValue;
  ModifiedProperty('Apartment');
end;

procedure TRussianAdress.SetBlock(AValue: string);
begin
  if FBlock=AValue then Exit;
  FBlock:=AValue;
  ModifiedProperty('Block');
end;

procedure TRussianAdress.SetBuilding(AValue: string);
begin
  if FBuilding=AValue then Exit;
  FBuilding:=AValue;
  ModifiedProperty('Building');
end;

procedure TRussianAdress.SetCity(AValue: string);
begin
  if FCity=AValue then Exit;
  FCity:=AValue;
  ModifiedProperty('City');
end;

procedure TRussianAdress.SetLocality(AValue: string);
begin
  if FLocality=AValue then Exit;
  FLocality:=AValue;
  ModifiedProperty('Locality');
end;

procedure TRussianAdress.SetRegion(AValue: string);
begin
  if FRegion=AValue then Exit;
  FRegion:=AValue;
  ModifiedProperty('Region');
end;

procedure TRussianAdress.SetStreet(AValue: string);
begin
  if FStreet=AValue then Exit;
  FStreet:=AValue;
  ModifiedProperty('Street');
end;

procedure TRussianAdress.SetTerritory(AValue: string);
begin
  if FTerritory=AValue then Exit;
  FTerritory:=AValue;
  ModifiedProperty('Territory');
end;

procedure TRussianAdress.SetZipCode(AValue: string);
begin
  if FZipCode=AValue then Exit;
  FZipCode:=AValue;
  ModifiedProperty('ZipCode');
end;

procedure TRussianAdress.InternalRegisterPropertys;
begin
  RegisterProperty('ZipCode', 'Индекс', 'Н', 'Индекс', 6, 6);
  RegisterProperty('Region', 'КодРегион', 'ОК', 'Код региона', 2, 2);
  RegisterProperty('Territory', 'Район', 'Н', 'Район', 1, 50);
  RegisterProperty('City', 'Город', 'Н', 'Город', 1, 50);
  RegisterProperty('Locality', 'НаселПункт', 'Н', 'Населенный пункт', 1, 50);
  RegisterProperty('Street', 'Улица', 'Н', 'Улица', 1, 50);
  RegisterProperty('Building', 'Дом', 'Н', 'Дом', 1, 20);
  RegisterProperty('Block', 'Корпус', 'Н', 'Корпус', 1, 20);
  RegisterProperty('Apartment', 'Кварт', 'Н', 'Квартира', 1, 20);
end;

procedure TRussianAdress.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TRussianAdress.Destroy;
begin
  inherited Destroy;
end;

{ TAdressInfo }

procedure TAdressInfo.SetAddress(AValue: string);
begin
  if FAddress=AValue then Exit;
  FAddress:=AValue;
  ModifiedProperty('Address');
end;

procedure TAdressInfo.SetCountryCode(AValue: string);
begin
  if FCountryCode=AValue then Exit;
  FCountryCode:=AValue;
  ModifiedProperty('CountryCode');
end;

procedure TAdressInfo.InternalRegisterPropertys;
begin
  RegisterProperty('CountryCode', 'КодСтр', 'ОК', 'Код страны', 3, 3);
  RegisterProperty('Address', 'АдрТекст', 'О', 'Адрес', 1, 255);
end;

procedure TAdressInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAdressInfo.Destroy;
begin
  inherited Destroy;
end;

{ TForeignEntityInformation }

procedure TForeignEntityInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TForeignEntityInformation.SetIdentifier(AValue: string);
begin
  if FIdentifier=AValue then Exit;
  FIdentifier:=AValue;
  ModifiedProperty('Identifier');
end;

procedure TForeignEntityInformation.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TForeignEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование полное', 1, 1000);
  RegisterProperty('Identifier', 'Идентиф', 'Н', 'Идентификатор юридического лица', 1, 255);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие организацию', 1, 255);
end;

procedure TForeignEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TForeignEntityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TLegalEntityInformation }

procedure TLegalEntityInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TLegalEntityInformation.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TLegalEntityInformation.SetKPP(AValue: string);
begin
  if FKPP=AValue then Exit;
  FKPP:=AValue;
  ModifiedProperty('KPP');
end;

procedure TLegalEntityInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование полное', 1, 100);
  RegisterProperty('INN', 'ИННЮЛ', 'О', 'ИНН', 10, 10);
  RegisterProperty('KPP', 'КПП', 'Н', 'КПП', 9, 9);
end;

procedure TLegalEntityInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TLegalEntityInformation.Destroy;
begin
  inherited Destroy;
end;

{ TPerson }

procedure TPerson.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
  ModifiedProperty('FirstName');
end;

procedure TPerson.SetPatronymic(AValue: string);
begin
  if FPatronymic=AValue then Exit;
  FPatronymic:=AValue;
  ModifiedProperty('Patronymic');
end;

procedure TPerson.SetSurname(AValue: string);
begin
  if FSurname=AValue then Exit;
  FSurname:=AValue;
  ModifiedProperty('Surname');
end;

procedure TPerson.InternalRegisterPropertys;
begin
  RegisterProperty('FirstName', 'Фамилия', 'О', 'Фамилия', 1, 60);
  RegisterProperty('Patronymic', 'Имя', 'О', 'Имя', 1, 60);
  RegisterProperty('Surname', 'Отчество', 'Н', 'Отчество', 1, 60);
end;

procedure TPerson.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

{ TOrganizationInformation }

procedure TOrganizationInformation.InternalRegisterPropertys;
begin
  RegisterProperty('LegalEntityInformation', 'СвОргУч', 'О', 'Сведения об организации, состоящей на учете в налоговом органе', -1, -1);
  RegisterProperty('ForeignEntityInformation', 'СвИнНеУч', 'О', 'Сведения об иностранном лице, не состоящем на учете в налоговых органах в качестве налогоплательщика', -1, -1);
end;

procedure TOrganizationInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FForeignEntityInformation:=TForeignEntityInformation.Create;
  FLegalEntityInformation:=TLegalEntityInformation.Create;
end;

destructor TOrganizationInformation.Destroy;
begin
  FreeAndNil(FForeignEntityInformation);
  FreeAndNil(FLegalEntityInformation);
  inherited Destroy;
end;

{ TIndividualEntrepreneurInformation }

procedure TIndividualEntrepreneurInformation.SetIndividualEntityRegistrationCertificate
  (AValue: string);
begin
  if FIndividualEntityRegistrationCertificate=AValue then Exit;
  FIndividualEntityRegistrationCertificate:=AValue;
  ModifiedProperty('IndividualEntityRegistrationCertificate');
end;

procedure TIndividualEntrepreneurInformation.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TIndividualEntrepreneurInformation.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TIndividualEntrepreneurInformation.InternalRegisterPropertys;
begin
  RegisterProperty('INN', 'ИННФЛ', 'О', 'ИНН', 12, 12);
  RegisterProperty('IndividualEntityRegistrationCertificate', 'СвГосРегИП', 'Н', 'Реквизиты свидетельства о государственной регистрации индивидуального предпринимателя', 1, 100);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TIndividualEntrepreneurInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TIndividualEntrepreneurInformation.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TPhysicalPersonEntity }

procedure TPhysicalPersonEntity.SetINN(AValue: string);
begin
  if FINN=AValue then Exit;
  FINN:=AValue;
  ModifiedProperty('INN');
end;

procedure TPhysicalPersonEntity.SetOtherInfo(AValue: string);
begin
  if FOtherInfo=AValue then Exit;
  FOtherInfo:=AValue;
  ModifiedProperty('OtherInfo');
end;

procedure TPhysicalPersonEntity.InternalRegisterPropertys;
begin
  RegisterProperty('INN', 'ИННФЛ', 'Н', 'ИНН', 12, 12);
  RegisterProperty('OtherInfo', 'ИныеСвед', 'Н', 'Иные сведения, идентифицирующие физическое лицо', 1, 255);
  RegisterProperty('Person', 'ФИО', 'О', 'Фамилия, имя, отчество', -1, -1);
end;

procedure TPhysicalPersonEntity.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPerson:=TPerson.Create;
end;

destructor TPhysicalPersonEntity.Destroy;
begin
  FreeAndNil(FPerson);
  inherited Destroy;
end;

{ TAccompanyingDocument }

procedure TAccompanyingDocument.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TAccompanyingDocument.SetDocumentID(AValue: string);
begin
  if FDocumentID=AValue then Exit;
  FDocumentID:=AValue;
  ModifiedProperty('DocumentID');
end;

procedure TAccompanyingDocument.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
  ModifiedProperty('DocumentName');
end;

procedure TAccompanyingDocument.SetDocumentNumber(AValue: string);
begin
  if FDocumentNumber=AValue then Exit;
  FDocumentNumber:=AValue;
  ModifiedProperty('DocumentNumber');
end;

procedure TAccompanyingDocument.InternalRegisterPropertys;
begin
  RegisterProperty('DocumentName', 'НаимСопрДок', 'О', 'Наименование сопроводительного документа', 1, 200);
  RegisterProperty('DocumentNumber', 'НомСопрДок', 'Н', 'Номер сопроводительного документа', 1, 100);
  RegisterProperty('DocumentDate', 'ДатаСопрДок', 'Н', 'Дата сопроводительного документа', 10, 10);
  RegisterProperty('DocumentID', 'ДопИдСопрДок', 'Н', 'Дополнительный идентификатор сопроводительного документа', 1, 255);
end;

procedure TAccompanyingDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAccompanyingDocument.Destroy;
begin
  inherited Destroy;
end;

{ TIdentificationInformation }

procedure TIdentificationInformation.InternalRegisterPropertys;
begin
  RegisterProperty('PhysicalPerson', 'СвФЛ', 'О', 'Сведения о физическом лице', -1, -1);
  RegisterProperty('IndividualEntrepreneurInformation', 'СвИП', 'О', 'Сведения об индивидуальном предпринимателе', -1, -1);
  RegisterProperty('OrganizationInformation', 'СвОрг', 'О', 'Сведения об организации', -1, -1);
end;

procedure TIdentificationInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FPhysicalPerson:=TPhysicalPersonEntity.Create;
  FIndividualEntrepreneurInformation:=TIndividualEntrepreneurInformation.Create;
  FOrganizationInformation:=TOrganizationInformation.Create;
end;

destructor TIdentificationInformation.Destroy;
begin
  FreeAndNil(FPhysicalPerson);
  FreeAndNil(FIndividualEntrepreneurInformation);
  FreeAndNil(FOrganizationInformation);
  inherited Destroy;
end;

{ TBankInformation }

procedure TBankInformation.SetBankAccount(AValue: string);
begin
  if FBankAccount=AValue then Exit;
  FBankAccount:=AValue;
end;

procedure TBankInformation.InternalRegisterPropertys;
begin
  RegisterProperty('BankAccount', 'НомерСчета', 'Н', 'Номер банковского счета', 1, 20);
  RegisterProperty('Bank', 'СвБанк', 'Н', 'Сведения о банке', -1, -1);
end;

procedure TBankInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FBank:=TBank.Create;
end;

destructor TBankInformation.Destroy;
begin
  FreeAndNil(FBank);
  inherited Destroy;
end;

{ TContactInformation }

procedure TContactInformation.SetEMail(AValue: string);
begin
  if FEMail=AValue then Exit;
  FEMail:=AValue;
  ModifiedProperty('EMail');
end;

procedure TContactInformation.SetPhoneNumber(AValue: string);
begin
  if FPhoneNumber=AValue then Exit;
  FPhoneNumber:=AValue;
  ModifiedProperty('PhoneNumber');
end;

procedure TContactInformation.InternalRegisterPropertys;
begin
  RegisterProperty('PhoneNumber', 'Тлф', 'Н', 'Номер контактного телефона/факс', 1, 255);
  RegisterProperty('EMail', 'ЭлПочта', 'Н', 'Адрес электронной почты', 1, 255);
end;

procedure TContactInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TContactInformation.Destroy;
begin
  inherited Destroy;
end;

{ TAdress }

procedure TAdress.SetCodeGAR(AValue: string);
begin
  if FCodeGAR=AValue then Exit;
  FCodeGAR:=AValue;
  ModifiedProperty('CurrencyCode');
end;

procedure TAdress.InternalRegisterPropertys;
begin
  RegisterProperty('RussianAdress', 'АдрРФ', 'О', 'Адрес, указанный в Едином государственном реестре юридических лиц/почтовый адрес/адрес места жительства индивидуального предпринимателя (реквизиты адреса на территории Российской Федерации)', -1, -1);
  RegisterProperty('AdressInfo', 'АдрИнф', 'О', 'Адрес, указанный в Едином государственном реестре юридических лиц/почтовый адрес/адрес места жительства индивидуального предпринимателя (информация об адресе, в том числе об адресе за пределами территории Российской Федерации)', -1, -1);
  //property CodeGAR:string read FCodeGAR write SetCodeGAR;
  (*
  ----------------------------------------------------
  Уникальный номер адреса объекта адресации в государственном адресном реестре
  КодГар
  П
  Т(1-36)
  о
  Типовой элемент <string-36>
  *)

end;

procedure TAdress.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FRussianAdress:=TRussianAdress.Create;
  FAdressInfo:=TAdressInfo.Create;
end;

destructor TAdress.Destroy;
begin
  FreeAndNil(FRussianAdress);
  FreeAndNil(FAdressInfo);
  inherited Destroy;
end;

{ TGoodsItem }

procedure TGoodsItem.InternalRegisterPropertys;
begin

end;

procedure TGoodsItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TGoodsItem.Destroy;
begin
  inherited Destroy;
end;

{ TAcceptanceDateInfo }

procedure TAcceptanceDateInfo.InternalRegisterPropertys;
begin

end;

procedure TAcceptanceDateInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAcceptanceDateInfo.Destroy;
begin
  inherited Destroy;
end;

{ TOtherAcceptanceInfo }

procedure TOtherAcceptanceInfo.InternalRegisterPropertys;
begin

end;

procedure TOtherAcceptanceInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TOtherAcceptanceInfo.Destroy;
begin
  inherited Destroy;
end;

{ TAcceptedPersonInformation }

procedure TAcceptedPersonInformation.InternalRegisterPropertys;
begin

end;

procedure TAcceptedPersonInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAcceptedPersonInformation.Destroy;
begin
  inherited Destroy;
end;

{ TAdditionalInformationOfLife1 }

procedure TAdditionalInformationOfLife1.InternalRegisterPropertys;
begin

end;

procedure TAdditionalInformationOfLife1.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAdditionalInformationOfLife1.Destroy;
begin
  inherited Destroy;
end;

{ TGoodsItemList }

function TGoodsItemList.GetItem(AIndex: Integer): TGoodsItem;
begin
  Result:=TGoodsItem(InternalGetItem(AIndex));
end;

constructor TGoodsItemList.Create;
begin
  inherited Create(TGoodsItem)
end;

function TGoodsItemList.CreateChild: TGoodsItem;
begin
  Result:=InternalAddObject as TGoodsItem;
end;

{ TCommisionDocument }

procedure TCommisionDocument.SetOrderDate(AValue: string);
begin
  if FOrderDate=AValue then Exit;
  FOrderDate:=AValue;
  ModifiedProperty('OrderDate');
end;

procedure TCommisionDocument.SetOrderNumber(AValue: string);
begin
  if FOrderNumber=AValue then Exit;
  FOrderNumber:=AValue;
  ModifiedProperty('OrderNumber');
end;

procedure TCommisionDocument.InternalRegisterPropertys;
begin
  RegisterProperty('OrderDate', 'ДатаПрик', 'О', 'Дата приказа (распоряжения)', 10, 10);
  RegisterProperty('OrderNumber', 'НомПрик', 'О', 'Номер приказа (распоряжения)', 1, 255);
end;

procedure TCommisionDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCommisionDocument.Destroy;
begin
  inherited Destroy;
end;

{ TResultsInspectionCargo }

procedure TResultsInspectionCargo.SetAcceptanceTime(AValue: string);
begin
  if FAcceptanceTime=AValue then Exit;
  FAcceptanceTime:=AValue;
  ModifiedProperty('AcceptanceTime');
end;

procedure TResultsInspectionCargo.SetAcceptanceTimeEnd(AValue: string);
begin
  if FAcceptanceTimeEnd=AValue then Exit;
  FAcceptanceTimeEnd:=AValue;
  ModifiedProperty('AcceptanceTimeEnd');
end;

procedure TResultsInspectionCargo.SetInspectDate(AValue: string);
begin
  if FInspectDate=AValue then Exit;
  FInspectDate:=AValue;
  ModifiedProperty('InspectDate');
end;

procedure TResultsInspectionCargo.SetInspectionPlace(AValue: string);
begin
  if FInspectionPlace=AValue then Exit;
  FInspectionPlace:=AValue;
  ModifiedProperty('InspectionPlace');
end;

procedure TResultsInspectionCargo.SetShipmentDate(AValue: string);
begin
  if FShipmentDate=AValue then Exit;
  FShipmentDate:=AValue;
  ModifiedProperty('ShipmentDate');
end;

procedure TResultsInspectionCargo.InternalRegisterPropertys;
begin
  RegisterProperty('InspectDate', 'ДатаОсм', 'Н', 'Дата осмотра прибывшего груза', 10, 10);
  RegisterProperty('InspectionPlace', 'МестоСост', 'Н', 'Место составления документа о приемке и (или) расхождениях', 1, 100);
  RegisterProperty('AcceptanceTime', 'ВремяНач', 'Н', 'Время начала приемки', 8, 8);
  RegisterProperty('AcceptanceTimeEnd', 'ВремяОконч', 'Н', 'Время окончания приемки', 8, 8);
  RegisterProperty('ShipmentDate', 'ДатаОтпр', 'Н', 'Дата отправки груза со станции (пристани, порта)', 10, 10);
  RegisterProperty('ConformityCertificate', 'СертСоотв','НМ', 'Номер сертификата соответствия', 1, 50);
  RegisterProperty('AccompanyingDocument', 'СопрДок', 'О', 'Наименование, номер и дата сопроводительного документа (идентификатор сопроводительного документа)', -1, -1);
  RegisterProperty('InformationResult', 'ИнфПолСвОсм', 'Н', 'Информационное поле сведений о результатах осмотра', -1, -1);
end;

procedure TResultsInspectionCargo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FAccompanyingDocument:=TAccompanyingDocument.Create;
  FInformationResult:=TAdditionalInformationOfLife1.Create;
  FConformityCertificate:=TStringList.Create;
end;

destructor TResultsInspectionCargo.Destroy;
begin
  FreeAndNil(FAccompanyingDocument);
  FreeAndNil(FInformationResult);
  FreeAndNil(FConformityCertificate);
  inherited Destroy;
end;

{ TOrganizationInfo }

procedure TOrganizationInfo.SetDepartment(AValue: string);
begin
  if FDepartment=AValue then Exit;
  FDepartment:=AValue;
  ModifiedProperty('Department');
end;

procedure TOrganizationInfo.SetInformationWorkflow(AValue: string);
begin
  if FInformationWorkflow=AValue then Exit;
  FInformationWorkflow:=AValue;
  ModifiedProperty('InformationWorkflow');
end;

procedure TOrganizationInfo.SetOKPO(AValue: string);
begin
  if FOKPO=AValue then Exit;
  FOKPO:=AValue;
  ModifiedProperty('OKPO');
end;

procedure TOrganizationInfo.InternalRegisterPropertys;
begin
  RegisterProperty('OKPO', 'ОКПО', 'Н', 'Код в общероссийском классификаторе предприятий и организаций', 1, 10);
  RegisterProperty('Department', 'СтруктПодр', 'Н', 'Структурное подразделение', 1, 1000);
  RegisterProperty('InformationWorkflow', 'ИнфДляУчаст', 'Н','Информация для участника документооборота', 1, 255);
  RegisterProperty('IdentificationInformation', 'ИдСв', 'О', 'Идентификационные сведения', -1, -1);
  RegisterProperty('Adress', 'Адрес', 'Н', 'Адрес', -1, -1);
  RegisterProperty('ContactInformation', 'Контакт', 'Н', 'Контактные данные', -1, -1);
  RegisterProperty('BankInformation', 'БанкРекв', 'Н', 'Банковские реквизиты', -1, -1);
end;

procedure TOrganizationInfo.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FIdentificationInformation:=TIdentificationInformation.Create;
  FAdress:=TAdress.Create;
  FContactInformation:=TContactInformation.Create;
  FBankInformation:=TBankInformation.Create;
end;

destructor TOrganizationInfo.Destroy;
begin
  FreeAndNil(FIdentificationInformation);
  FreeAndNil(FAdress);
  FreeAndNil(FContactInformation);
  FreeAndNil(FBankInformation);
  inherited Destroy;
end;

{ TAcceptanceDocumentDateNumber }

procedure TAcceptanceDocumentDateNumber.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TAcceptanceDocumentDateNumber.SetDocumentNumber(AValue: string);
begin
  if FDocumentNumber=AValue then Exit;
  FDocumentNumber:=AValue;
  ModifiedProperty('DocumentNumber');
end;

procedure TAcceptanceDocumentDateNumber.InternalRegisterPropertys;
begin
  RegisterProperty('DocumentNumber', 'НомДокПР', 'О', 'Номер документа о приемке и (или) расхождениях', 1, 255);
  RegisterProperty('DocumentDate', 'ДатаДокПР', 'О', 'Дата составления (выписки) документа о приемке и (или) расхождениях', 10, 10);
end;

procedure TAcceptanceDocumentDateNumber.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAcceptanceDocumentDateNumber.Destroy;
begin
  inherited Destroy;
end;

{ TCorrectionDocumentDateNumber }

procedure TCorrectionDocumentDateNumber.SetCorrectionDate(AValue: string);
begin
  if FCorrectionDate=AValue then Exit;
  FCorrectionDate:=AValue;
  ModifiedProperty('CorrectionDate');
end;

procedure TCorrectionDocumentDateNumber.SetCorrectionNumber(AValue: string);
begin
  if FCorrectionNumber=AValue then Exit;
  FCorrectionNumber:=AValue;
  ModifiedProperty('CorrectionNumber');
end;

procedure TCorrectionDocumentDateNumber.InternalRegisterPropertys;
begin
  RegisterProperty('CorrectionNumber', 'НомИспрДокПР', 'О', 'Исправление: №', 1, 3);
  RegisterProperty('CorrectionDate', 'ДатаИспрДокПР', 'О', 'Исправление: Дата', 10, 10);
end;

procedure TCorrectionDocumentDateNumber.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TCorrectionDocumentDateNumber.Destroy;
begin
  inherited Destroy;
end;

{ TAcceptanceInformation1 }

procedure TAcceptanceInformation1.SetGenerateCodeDocument(AValue: string);
begin
  if FGenerateCodeDocument=AValue then Exit;
  FGenerateCodeDocument:=AValue;
  ModifiedProperty('GenerateCodeDocument');
end;

procedure TAcceptanceInformation1.SetGovernmentContractInfo(AValue: string);
begin
  if FGovernmentContractInfo=AValue then Exit;
  FGovernmentContractInfo:=AValue;
  ModifiedProperty('GovernmentContractInfo');
end;

procedure TAcceptanceInformation1.InternalRegisterPropertys;
begin
  RegisterProperty('GenerateCodeDocument', 'ОбстИсп', 'Обозначение (код) обстоятельств формирования (использования) документа', 'Н', 4, 4);
  RegisterProperty('GovernmentContractInfo', 'ИдГосКон', 'Идентификатор государственного контракта', 'Н', 1, 255);
  RegisterProperty('Seller', 'Продавец', 'Продавец (поставщик, исполнитель)', 'О', -1, -1);
  RegisterProperty('Buyer', 'Покупатель', 'Покупатель (заказчик)', 'О', -1, -1);
  RegisterProperty('Shipper', 'Грузоотправитель', 'Грузоотправитель (отправитель)', 'Н', -1, -1);
  RegisterProperty('Consignee', 'Грузополучатель', 'Грузополучатель (получатель)', 'Н', -1, -1);
  RegisterProperty('InsuranceCompany', 'СтрахКом', 'Страховая компания', 'Н', -1, -1);
  RegisterProperty('CommisionDocument', 'Приказ', 'Дата и номер приказа (распоряжения) о назначении комиссии', 'Н', -1, -1);
  RegisterProperty('ResultsInspectionCargo', 'СвОсмГруз', 'Сведения о событиях, связанных с осмотром груза (о результатах осмотра прибывшего груза)', 'Н', -1, -1);
  RegisterProperty('GoodsItemList', 'СвСопрДок', 'Сведения о грузе по сопроводительным транспортным документам', 'НМ', -1, -1);
  RegisterProperty('AcceptanceDateInfo', 'СвВремПрием', 'Сведения о дате и времени событий, связанных с приемкой груза', 'Н', -1, -1);
  RegisterProperty('OtherAcceptanceInfo', 'ДрОбстПрием', 'Другие обстоятельства приемки ценностей', 'Н', -1, -1);
  RegisterProperty('AcceptedPersonInformation', 'СвЛицПрин', 'Сведения о лице, принявшем товар (получившем груз) (в том числе на ответственное хранение)', 'Н', -1, -1);
  RegisterProperty('AdditionalInformationOfLife1', 'ИнфПолФХЖ1', 'Информационное поле события (факта хозяйственной жизни) 1', 'Н', -1, -1);
end;

procedure TAcceptanceInformation1.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSeller:=TOrganizationInfo.Create;
  FBuyer:=TOrganizationInfo.Create;
  FShipper:=TOrganizationInfo.Create;
  FConsignee:=TOrganizationInfo.Create;
  FInsuranceCompany:=TOrganizationInfo.Create;
  FCommisionDocument:=TCommisionDocument.Create;
  FResultsInspectionCargo:=TResultsInspectionCargo.Create;
  FGoodsItemList:=TGoodsItemList.Create;
  FAcceptanceDateInfo:=TAcceptanceDateInfo.Create;
  FOtherAcceptanceInfo:=TOtherAcceptanceInfo.Create;
  FAcceptedPersonInformation:=TAcceptedPersonInformation.Create;
  FAdditionalInformationOfLife1:=TAdditionalInformationOfLife1.Create;
end;

destructor TAcceptanceInformation1.Destroy;
begin
  FreeAndNil(FSeller);
  FreeAndNil(FBuyer);
  FreeAndNil(FShipper);
  FreeAndNil(FConsignee);
  FreeAndNil(FInsuranceCompany);
  FreeAndNil(FCommisionDocument);
  FreeAndNil(FResultsInspectionCargo);
  FreeAndNil(FGoodsItemList);
  FreeAndNil(FAcceptanceDateInfo);
  FreeAndNil(FOtherAcceptanceInfo);
  FreeAndNil(FAcceptedPersonInformation);
  FreeAndNil(FAdditionalInformationOfLife1);
  inherited Destroy;
end;

{ TAcceptanceInformation2 }

procedure TAcceptanceInformation2.InternalRegisterPropertys;
begin

end;

procedure TAcceptanceInformation2.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TAcceptanceInformation2.Destroy;
begin
  inherited Destroy;
end;

{ TBuyerSignerInformation }

procedure TBuyerSignerInformation.InternalRegisterPropertys;
begin

end;

procedure TBuyerSignerInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TBuyerSignerInformation.Destroy;
begin
  inherited Destroy;
end;

{ TBuyerSignerInformationList }

function TBuyerSignerInformationList.GetItem(AIndex: Integer
  ): TBuyerSignerInformation;
begin
  Result:=TBuyerSignerInformation(InternalGetItem(AIndex));
end;

constructor TBuyerSignerInformationList.Create;
begin
  inherited Create(TBuyerSignerInformation);
end;

function TBuyerSignerInformationList.CreateChild: TBuyerSignerInformation;
begin
  Result:=InternalAddObject as TBuyerSignerInformation;
end;

{ TAcceptanceDocument }

procedure TAcceptanceDocument.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
  ModifiedProperty('KND');
end;

procedure TAcceptanceDocument.SetDocumentDateCreate(AValue: string);
begin
  if FDocumentDateCreate=AValue then Exit;
  FDocumentDateCreate:=AValue;
  ModifiedProperty('DocumentDateCreate');
end;

procedure TAcceptanceDocument.SetDocumentDestination(AValue: string);
begin
  if FDocumentDestination=AValue then Exit;
  FDocumentDestination:=AValue;
  ModifiedProperty('DocumentDestination');
end;

procedure TAcceptanceDocument.SetDocumentCreator(AValue: string);
begin
  if FDocumentCreator=AValue then Exit;
  FDocumentCreator:=AValue;
  ModifiedProperty('DocumentCreator');
end;

procedure TAcceptanceDocument.SetDocumentCreateConditions(AValue: string);
begin
  if FDocumentCreateConditions=AValue then Exit;
  FDocumentCreateConditions:=AValue;
  ModifiedProperty('DocumentCreateConditions');
end;

procedure TAcceptanceDocument.SetAdditionalInformationState(AValue: string);
begin
  if FAdditionalInformationState=AValue then Exit;
  FAdditionalInformationState:=AValue;
  ModifiedProperty('AdditionalInformationState');
end;

procedure TAcceptanceDocument.SetDocumentCreatorBase(AValue: string);
begin
  if FDocumentCreatorBase=AValue then Exit;
  FDocumentCreatorBase:=AValue;
  ModifiedProperty('DocumentCreatorBase');
end;

procedure TAcceptanceDocument.SetDocumentName(AValue: string);
begin
  if FDocumentName=AValue then Exit;
  FDocumentName:=AValue;
  ModifiedProperty('DocumentName');
end;

procedure TAcceptanceDocument.SetDocumentNameEL(AValue: string);
begin
  if FDocumentNameEL=AValue then Exit;
  FDocumentNameEL:=AValue;
  ModifiedProperty('DocumentNameEL');
end;

procedure TAcceptanceDocument.SetDocumentTimeCreate(AValue: string);
begin
  if FDocumentTimeCreate=AValue then Exit;
  FDocumentTimeCreate:=AValue;
  ModifiedProperty('DocumentTimeCreate');
end;

procedure TAcceptanceDocument.InternalRegisterPropertys;
begin
  RegisterProperty('KND', 'КНД', 'ОК', 'Код документа по КНД', 7, 7);
  RegisterProperty('DocumentDateCreate', 'ДатаИнфПок', 'О', 'Дата формирования файла обмена информации покупателя', 10, 10);
  RegisterProperty('DocumentTimeCreate', 'ВремИнфПок', 'О', 'Время формирования файла обмена информации покупателя', 8, 8);
  RegisterProperty('DocumentNameEL', 'ПоФактХЖ', 'О', 'Наименование документа по событию (факту хозяйственной жизни)', 1, 255);
  RegisterProperty('DocumentName', 'НаимДокОпр', 'О', 'Наименование документа, определенное организацией (согласованное сторонами сделки)', 1, 255);
  RegisterProperty('DocumentCreator', 'НаимЭконСубСост', 'О', 'Наименование экономического субъекта - составителя файла обмена информации покупателя', 1, 1000);
  RegisterProperty('DocumentCreatorBase', 'ОснДоверОргСост', 'Н', 'Основание, по которому экономический субъект является составителем файла обмена информации покупателя', 1, 120);
  RegisterProperty('DocumentCreateConditions', 'ОбстСостДок', 'НК', 'Обстоятельства составления документа', 1, 1);
  RegisterProperty('DocumentDestination', 'НазнДопСв', 'Н', 'Назначение и подписанты дополнительных сведений', 1, 2000);
  RegisterProperty('AcceptanceDocumentDateNumber', 'ИдентДокПР', 'О', 'Дата и номер документа о приемке и (или) расхождениях', -1, -1);
  RegisterProperty('CorrectionDocumentDateNumber', 'ИспрДокПР', 'Н', 'Исправление документа о приемке и (или) расхождениях', -1, -1);
  RegisterProperty('AcceptanceInformation1', 'СодФХЖ1', 'О', 'Содержание события (факта хозяйственной жизни (1)) - сведения об обстоятельствах приемки', -1, -1);
  RegisterProperty('AcceptanceInformation2', 'СодФХЖ2', 'О', 'Содержание события (факта хозяйственной жизни (2)) - сведения о факте приемки и (или) о расхождениях', -1, -1);
  RegisterProperty('AdditionalInformationState', 'ИнфДопСв', 'ПОКМ', 'Информация о формировании дополнительных сведений к документу', -1, -1);
  RegisterProperty('Signer', 'Подписант', 'ОМ', 'Сведения о лице, подписавшем файл обмена информации покупателя в электронной форме', -1, -1);
end;

procedure TAcceptanceDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FAcceptanceDocumentDateNumber:=TAcceptanceDocumentDateNumber.Create;
  FCorrectionDocumentDateNumber:=TCorrectionDocumentDateNumber.Create;
  FAcceptanceInformation1:=TAcceptanceInformation1.Create;
  FAcceptanceInformation2:=TAcceptanceInformation2.Create;
  FSigner:=TBuyerSignerInformationList.Create;
end;

destructor TAcceptanceDocument.Destroy;
begin
  FreeAndNil(FAcceptanceDocumentDateNumber);
  FreeAndNil(FCorrectionDocumentDateNumber);
  FreeAndNil(FAcceptanceInformation1);
  FreeAndNil(FAcceptanceInformation2);
  FreeAndNil(FSigner);
  inherited Destroy;
end;

{ TSellerExchangeInformation }

procedure TSellerExchangeInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TSellerExchangeInformation.SetIdentifierSenderOperator(AValue: string
  );
begin
  if FIdentifierSenderOperator=AValue then Exit;
  FIdentifierSenderOperator:=AValue;
  ModifiedProperty('IdentifierSenderOperator');
end;

procedure TSellerExchangeInformation.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TSellerExchangeInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование', 1, 1000);
  RegisterProperty('Inn', 'ИННЮЛ', 'О', 'ИНН', 10, 10);
  RegisterProperty('IdentifierSenderOperator', 'ИдЭДО', 'О', 'Идентификатор оператора электронного документооборота отправителя файла обмена счета-фактуры (информации продавца)', 3, 3);
end;

{ TParticipantsInformation }

procedure TParticipantsInformation.SetRecipientInfo(AValue: string);
begin
  if FRecipientInfo=AValue then Exit;
  FRecipientInfo:=AValue;
  ModifiedProperty('RecipientInfo');
end;

procedure TParticipantsInformation.SetSenderInfo(AValue: string);
begin
  if FSenderInfo=AValue then Exit;
  FSenderInfo:=AValue;
  ModifiedProperty('SenderInfo');
end;

procedure TParticipantsInformation.InternalRegisterPropertys;
begin
  RegisterProperty('SenderInfo', 'ИдОтпр', 'О', 'Идентификатор участника документооборота - отправителя файла обмена счета-фактуры (информации продавца)', 4, 46);
  RegisterProperty('RecipientInfo', 'ИдПол', 'О', 'Идентификатор участника документооборота - получателя файла обмена счета-фактуры (информации продавца)', 4, 46);
  RegisterProperty('SellerExchangeInformation', 'СвОЭДОтпр', 'Н', 'Сведения об операторе электронного документооборота отправителя файла обмена счета-фактуры (информации продавца)', -1, -1);
end;

procedure TParticipantsInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSellerExchangeInformation:=TSellerExchangeInformation.Create;
end;

destructor TParticipantsInformation.Destroy;
begin
  FreeAndNil(FSellerExchangeInformation);
  inherited Destroy;
end;

{ TTorg2ExchangeFile }

procedure TTorg2ExchangeFile.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ParticipantsInformation', 'СвУчДокОбор', 'О', 'Сведения об участниках электронного документооборота', -1, -1);
  RegisterProperty('Document', 'Документ', 'О', 'Документ о приемке и (или) расхождениях (информация покупателя) (Документ)', -1, -1);
end;

procedure TTorg2ExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FParticipantsInformation:=TParticipantsInformation.Create;
  FDocument:=TAcceptanceDocument.Create;
end;

destructor TTorg2ExchangeFile.Destroy;
begin
  FreeAndNil(FParticipantsInformation);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.

